
# README ------------------------------------------------------------------

# This script is for archiving various simulation scenarios for Module 3 of SMURP. 

# Requirements ------------------------------------------------------------


source("GlobalParameters.R")
source("Functions.R")


wd <- getwd()



# Incident Scenario Generation --------------------------------------------


realcell = nrow(cell[!is.na(cell$eff_e),])

inc.time = unique(total.input$time)[seq(49,169,2)] # incident generation 
duration = seq(5, 30, 5)
dmg.cell = c(3:(realcell-2))
dmg.lane = cell[3:(realcell-2),]$eff.lane-1

pm.lane = cbind(dmg.cell, dmg.lane)
for (i in 1:max(dmg.lane)) {
  tmp = cbind(dmg.cell, pmax(dmg.lane-i, 1))
  pm.lane = rbind(pm.lane, tmp)
}
pm.lane = as.data.frame(pm.lane)
pm.lane = unique(pm.lane)

tmp2 = cbind(rep(duration, each = nrow(pm.lane)), do.call("rbind", replicate(length(duration), pm.lane, simplify = F)))
tmp3 = cbind(rep(inc.time, each = nrow(tmp2)), do.call("rbind", replicate(length(inc.time), tmp2, simplify = F)))
vir.incident = cbind(1:nrow(tmp3), tmp3)

vir.inc.colname = c("id", "time", "duration", "cell", "dmg.lane")

colnames(vir.incident) = vir.inc.colname



# Incident Generation -----------------------------------------------------


#### CTM parameters obtained through GA ####
# for SUN
c.vf <- 69.89341 # mph
c.qmax <- 2274.94558 # veh/hr/lane
c.w <- 17.12081 # mph
c.kjam <- 229.87848  #veh/mile/lane

##### parameter initialization #####
n_max <- matrix(0, nrow = wholeStep, ncol = nrow(cell))
q_max.lane <- matrix(c.qmax, nrow = wholeStep, ncol = nrow(cell))
vf <- matrix(c.vf, nrow = wholeStep, ncol = nrow(cell))
k_jam.lane <- matrix(c.kjam, nrow = wholeStep, ncol = nrow(cell))
w <- matrix(c.w, nrow = wholeStep, ncol = nrow(cell))

length <- matrix(1, nrow = wholeStep, ncol = nrow(cell))
lane <- matrix(5, nrow = wholeStep, ncol = nrow(cell))
eff.lane <- matrix(5, nrow = wholeStep, ncol = nrow(cell))

for (i in 1:nrow(cell)) {
  length[,i] <- cell[i,]$length # mile
  lane[,i] <- cell[i,]$Lane # lane
  eff.lane[,i] <- cell[i,]$eff.lane # lane
}

q_max <- q_max.lane * lane /60 * dt # #veh(/min) (#veh/hr/lane * lane * dt)
#q_max[,8] <- c.qmax/ 60 *dt * 4 # #veh(/min) (#veh/hr/lane * lane * dt)
q_max[,ncol(q_max)] <- c.qmax / 60 *dt * 5 # #veh(/min) (#veh for dt (lane: 5))
k_c.lane <- q_max.lane / vf # #veh/mile
#k_c.lane[,8] <- 65
k_jam.lane <- q_max.lane/w + k_c.lane
#k_jam.lane[,8] <- c.qmax/w[1,1] + c.qmax/vf[1,8]

n_max <- k_jam.lane * eff.lane * length # #veh (for cell) n=k*l , l=v*dt
n_max[,c(1, ncol(n_max))] <- 1000000 # V1, V22


##### Incident DB Generation #####

incidentDB <- list()
#for (wdn in 1:length(wkd.names)) {
for (wdn in 5) {
  wdn = 5
  wkd = wkd.names[wdn]
  
  ###### Simulation output ######
  n.sim.input = c() ## number of vehicles in cells
  y.sim.input = c() ## vehicle flow between cells
  
  ###### input flow and ramp flow setting by target weekdays ######
  onFlow <- avg.on.fwy[which(avg.on.fwy$wkd == wkd),]
  offFlow <- avg.off.fwy[which(avg.off.fwy$wkd == wkd),]
  fwyFlow <- avg.fwy[which(avg.fwy$wkd == wkd),]
  dvgRatio <- dvg.ratio[which(dvg.ratio$wkd == wkd),]
  dvgRatio$ratio.main <- dvgRatio$ratio.main / max(dvgRatio$ratio.main) * .9
  outFlow <- avg.outflow[which(avg.outflow$wkd == wkd),]
  if (sum(is.na(outFlow)) > 0) {
    outFlow[which(is.na(outFlow$Flow)),]$Flow <- 0
  }
  
  q_on <- matrix(0, nrow = wholeStep, ncol = nrow(cell))
  q_off <- matrix(0, nrow = wholeStep, ncol = nrow(cell))
  for (t in 1:wholeStep) {
    for (i in 1:nrow(cell)) {
      q_on[t, i] <- calRampFlow(onPM.fwy, onFlow, cell, t, i) # #veh(/min) (#veh/5min * dt) for all lane
      q_off[t, i] <- calRampFlow(offPM.fwy, offFlow, cell, t, i) # #veh(/min) (#veh/5min * dt) for all lane
    }
  }
  
  ###### Fill the most upstream cell's number of vehicle ######
  cell1.VDS <- unique(mainPM[which(mainPM$cellID == 1),]$VDS)[1]
  cell1.VDS.flow <- avg.input2018[which(avg.input2018$wkd == wkd & avg.input2018$VDS == cell1.VDS),]
  
  for (t in 1:simulationstep) {
    step <- startStep + (t-1)
    n.sim.input[t] <- calEnterFlow(cell1.VDS.flow, step) + q_on[step,1] - q_off[step,1] # #veh(/min) (#veh/5min * dt) for all lane
  }
  
  ###### Fill the cells' initial flow ######
  t = 1
  step = startStep + (t-1)
  for (i in 1:realcellnum) {
    cell.VDS <- unique(mainPM[which(mainPM$cellID == i),]$VDS)[1]
    cell.VDS.flow <- avg.input2018[which(avg.input2018$VDS == cell.VDS & avg.input2018$wkd == wkd),]
    y.sim.input[i] <- calEnterFlow(cell.VDS.flow, step) + q_on[step,i] - q_off[step,i] # #veh(/min) (#veh/5min * dt) for all lane
  }
  cell.VDS <- 1215109
  cell.VDS.flow <- avg.fwy[which(avg.fwy$VDS == cell.VDS & avg.fwy$wkd == wkd),] ################### CHECK 1215109 in avg.fwy!!!!
  y.sim.input[fwycellID] <- cell.VDS.flow[((step-1) %/% inputDiv)+1,]$Flow / inputDiv
  
  
  ###### START DB Generation ######
  start_incDB <- Sys.time()
  incidentDB_twt3 <- list()
  
  for (vi in 1:nrow(vir.incident)){
    
    n.sim <- n.sim_init # CTM_x[1] from CTM_calibration.R
    y.sim <- y.sim_init # CTM_x[2]
    speed <- speed_init # CTM_x[3]
    
    tmp.inc = vir.incident[vi,]
    tmp.time <- max(strptime(tmp.inc$time, "%H:%M"), startTime)
    tmp.init <- as.numeric(difftime(tmp.time, initialTime, units = "mins")) / dt + 1
    tmp.duration <- tmp.inc$duration / dt
    tmp.end <- tmp.init + tmp.duration - 1
    tmp.cell <- tmp.inc$cell
    tmp.dlane = tmp.inc$dmg.lane
    q_max[c(tmp.init:(min(tmp.end, nrow(q_max)))), tmp.cell] <- q_max.lane[c(tmp.init:(min(tmp.end, nrow(q_max)))), tmp.cell] *
      (lane[c(tmp.init:(min(tmp.end, nrow(q_max)))), tmp.cell] - tmp.dlane) / 60 * dt
    
    for (t in tmp.init:simulationstep) {
      for (i in 2:(nrow(cell))) {
        out <- cell[which(cell$prevID == i),]$cellID
        step <- startStep + (t-1)
        n.sim[t,i] <- max(0, n.sim[t-1,i] + y.sim[t-1,i] - sum(y.sim[t-1,out]) + q_on[step-1,i] - q_off[step-1,i] )#- q_off.8[t-1,i]/2)
      }
      
      for (i in 2:(nrow(cell))) {
        step <- startStep + (t-1)
        prev <- cell[i,]$prevID
        if (i == dvgcellID) {
          ratio <- dvgRatio[( (t-1) %/% inputDiv)+1,]$ratio.main
        } else if (i == fwycellID) {
          ratio <- 1 - dvgRatio[( (t-1) %/% inputDiv)+1,]$ratio.main
        } else {
          ratio <- cell[i,]$ratio
        }
        y.sim[t,i] <- min(  n.sim[t,prev] * ratio, # * vf[t,i] / 60 * dt ,
                            q_max[step,i] ,
                            w[step,i] / 60 * (n_max[t,i] - n.sim[t,i] ) / length[step,i] * dt # /l*dt = v
        )
      }
    }
    
    k.sim.lane <- n.sim/eff.lane[c(startStep:(endStep)),]/length[c(startStep:(endStep)),]
    
    
    for (t in tmp.init:simulationstep) {
      step <- startStep + (t-1)
      for (i in 1:nrow(cell)) {
        speed[t,i] <- min(min( -w[step,i] * (k.sim.lane[t,i] - k_jam.lane[step,i]), q_max.lane[step,i]) / k.sim.lane[t,i], vf[t,i])
        
      }
    }
    
    incidentDB[[length(incidentDB)+1]] =  list(wkd = wkd, inc = tmp.inc, speed = speed)
    print(wkd)
    print(vi)
    
    if (length(incidentDB) %% 1000 == 0) {
      saveRDS(incidentDB, "incidentDB.rds")
    }
  }
  
  end_incDB <- Sys.time()
  DB_time <- start_incDB - end_incDB
  print(DB_time)
  
}


# Data check --------------------------------------------------------------

speed_test = incidentDB[[13176]]$speed # check
filled.contour(speed_test[c(2:nrow(speed_test)),c(1:realcellnum)])

pems.result <- avg.input2018[which(avg.input2018$time > "10:00" & avg.input2018$time <= "22:00" & avg.input2018$wkd == wkd),]
pems.result <- aggregate(.~ time + VDS + Abs.PM + CA.PM + wkd , pems.result, mean)
pems.n <- matrix(0, nrow = length(unique(pems.result$time)), ncol = nrow(cell[!is.na(cell$eff_b),]))
#pems.y <- matrix(0, nrow = length(unique(pems.result$time)), ncol = nrow(cell[!is.na(cell$eff_b),]))
pems.speed <- matrix(0, nrow = length(unique(pems.result$time)), ncol = nrow(cell[!is.na(cell$eff_b),]))

for (i in 1:ncol(pems.n)) {
  tmp.cell <- cell[i,]
  tmp.pems <- pems.result[which(pems.result$Abs.PM >= tmp.cell$eff_b & pems.result$Abs.PM < tmp.cell$eff_e),]
  tmp.pems <- aggregate(tmp.pems, list(time = tmp.pems$time), max)
  #pems.n[,i] <- tmp.pems$AggDensity * tmp.cell$length
  #pems.y[,i] <- tmp.pems$AggFlow
  pems.speed[,i] <- tmp.pems$AggSpeed
}

filled.contour(pems.speed[c(2:nrow(pems.speed)),c(1:realcellnum)])

#n.sim.5min <- matrix(0, nrow = simulationstep/5, ncol = ncol(n.sim))
#y.sim.5min <- matrix(0, nrow = simulationstep/5, ncol = ncol(y.sim))
speed.5min <- matrix(0, nrow = simulationstep/inputDiv, ncol = ncol(speed))
for (i in 1:(simulationstep/inputDiv)) {
  agg.rows <- c( ((i-1)*inputDiv+1):(i*inputDiv) )
  #n.sim.5min[i,] <- colMeans(n.sim[agg.rows,])
  #y.sim.5min[i,] <- colSums(y.sim[agg.rows,])
  speed.5min[i,] <- colMeans(speed[agg.rows,])
  
}

