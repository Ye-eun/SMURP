
# README ------------------------------------------------------------------

# This script is for calibration of the CTM parameters. A main calibration algorithm is GA, one of the heuristic algorithms that is convinced to be useful for traffic model parameter calibration.
# Calibrated parameters are used for CTM in IncidentGeneration.r.

# Requirements ------------------------------------------------------------

source("GlobalParameters.R") # use: Default CTM parameters
source("Functions.R")

wd <- getwd()


# Functions ---------------------------------------------------------------

CTM_x <- function(c.vf, c.qmax, c.w, c.kjam, n.sim.input, y.sim.input){
  # perform CTM
  # input: CTM parameters, initial values
  # result: SPEED 
  speed <- c()
  
  ## Parameters
  # CTM parameter to the matrix
  q_max.lane <- matrix(c.qmax, nrow = simulationstep, ncol = nrow(cell))
  n_max <- matrix(0, nrow = simulationstep, ncol = nrow(cell))
  vf <- matrix(c.vf, nrow = simulationstep, ncol = nrow(cell))
  k_jam.lane <- matrix(c.kjam, nrow = simulationstep, ncol = nrow(cell))
  w <- matrix(c.w, nrow = simulationstep, ncol = nrow(cell))
  
  # road geometry
  length <- matrix(1, nrow = simulationstep, ncol = nrow(cell))
  lane <- matrix(5, nrow = simulationstep, ncol = nrow(cell))
  eff.lane <- matrix(5, nrow = simulationstep, ncol = nrow(cell))
  
  for (i in 1:nrow(cell)) {
    length[,i] <- cell[i,]$length # mile
    lane[,i] <- cell[i,]$Lane # lane
    eff.lane[,i] <- cell[i,]$eff.lane # lane
  }
  
  
  q_max <- q_max.lane * lane /60 * dt # #veh(/min) (#veh/hr/lane * lane * dt)
  q_max[,ncol(q_max)] <- c.qmax / 60 *dt * 5 # #veh(/min) (#veh for dt (lane: 5))
  k_c.lane <- q_max.lane / vf # #veh/mile
  k_jam.lane <- q_max.lane/w + k_c.lane
  
  n_max <- k_jam.lane * eff.lane * length #/ 0.621371 # #veh (for cell) to mile
  n_max[,c(1, ncol(n_max))] <- 1000000
  
  # START #
  
  time <- startTime
  n.sim <- matrix(0, nrow = simulationstep, ncol = nrow(cell))
  k.sim <- matrix(0, nrow = simulationstep, ncol = nrow(cell))
  y.sim <- matrix(0, nrow = simulationstep, ncol = nrow(cell))
  speed <- matrix(0, nrow = simulationstep, ncol = nrow(cell))
  
  n.sim[,1] = n.sim.input
  n.sim[1,c(1:length(y.sim.input))] = y.sim.input
  
  y.sim[,1] <- n.sim[,1]
  
  for (i in 2:(nrow(cell))) {
    prev <- cell[i,]$prevID
    if (i == dvgcellID) {
      ratio <- dvgRatio[(( (t-1) %/% inputDiv)+1),]$ratio.main
    } else if (i == fwycellID) {
      ratio <- 1 - dvgRatio[( (t-1) %/% inputDiv)+1,]$ratio.main
    } else {
      ratio <- cell[i,]$ratio
    }
    y.sim[1,i] <- min(  n.sim[1,prev] * ratio,
                        q_max[1,i] ,
                        w[1,i] / 60 * (n_max[1,i] - n.sim[1,i] ) / length[1,i] * dt
    )
  }
  
  for (t in 2:simulationstep) {
    for (i in 2:(nrow(cell))) {
      out <- cell[which(cell$prevID == i),]$cellID
      step <- startStep + (t-1)
      n.sim[t,i] <- max(0, n.sim[t-1,i] + y.sim[t-1,i] - sum(y.sim[t-1,out]) + q_on[step-1,i] - q_off[step-1,i] )
    }
    
    for (i in 2:(nrow(cell))) {
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
                          w[step,i] / 60 * (n_max[t,i] - n.sim[t,i] ) / length[step,i] * dt
      )
    }
  }
  
  speed <- vf[c(startStep:(endStep)),]
  k.sim.lane <- n.sim/eff.lane[c(startStep:(endStep)),]/length[c(startStep:(endStep)),]
  
  for (t in 1:simulationstep) {
    step <- startStep + (t-1)
    for (i in 1:nrow(cell)) {
      speed[t,i] <- min(min( -w[step,i] * (k.sim.lane[t,i] - k_jam.lane[step,i]), q_max.lane[step,i]) / k.sim.lane[t,i], vf[t,i])
      
    }
  }
  
  return (speed)
  
}

getRMSE <- function(c.vf, c.qmax, c.w, c.kjam, pems.speed){ 
  # get RMSE value between speed by CTM and actual data
  # input: CTM parameters, PEMS speed with corresponding weekdays
  # result: RMSE of link speed
  ctmSpeed <- CTM_x(c.vf, c.qmax, c.w, c.kjam, n.sim.input, y.sim.input)
  
  speed.5min <- matrix(0, nrow = simulationstep/inputDiv, ncol = ncol(ctmSpeed))
  for (i in 1:(simulationstep/inputDiv)) {
    agg.rows <- c( ((i-1)*inputDiv+1):(i*inputDiv) )
    speed.5min[i,] <- colMeans(ctmSpeed[agg.rows,])
  }
  
  ## calculate RMSE between ground truth (actual speed) and simulated results (CTM) ##
  RMSE= sqrt(mean(pems.speed - speed.5min[c(1:nrow(speed.5min)),c(1:realcellnum)]) ^2)
  
  return(RMSE)
  
}



# Main --------------------------------------------------------------------

#### PEMS speed (ground truth) ####

pems.result <- avg.input2018[which(avg.input2018$time > initialTime & avg.input2018$time <= "22:00" & avg.input2018$wkd == wkd),]
pems.result <- aggregate(.~ time + VDS + Abs.PM + CA.PM + wkd , pems.result, mean)
pems.speed <- matrix(0, nrow = length(unique(pems.result$time)), ncol = nrow(cell[!is.na(cell$eff_b),]))

for (i in 1:ncol(pems.n)) {
  tmp.cell <- cell[i,]
  tmp.pems <- pems.result[which(pems.result$Abs.PM >= tmp.cell$eff_b & pems.result$Abs.PM < tmp.cell$eff_e),]
  tmp.pems <- aggregate(tmp.pems, list(time = tmp.pems$time), max)
  pems.speed[,i] <- tmp.pems$AggSpeed
}


#### CTM initialization ####

# select wkd
wdn = 5
wkd = wkd.names[wdn]

##### Simulation output #####
n.sim.input = c() ## number of vehicles in cells
y.sim.input = c() ## vehicle flow between cells

##### input flow and ramp flow setting by target weekdays #####
onFlow <- avg.on.fwy2018[which(avg.on.fwy2018$wkd == wkd),]
offFlow <- avg.off.fwy2018[which(avg.off.fwy2018$wkd == wkd),]
fwyFlow <- avg.fwy2018[which(avg.fwy2018$wkd == wkd),]
dvgRatio <- dvg.ratio[which(dvg.ratio$wkd == wkd),]
dvgRatio$ratio.main <- dvgRatio$ratio.main / max(dvgRatio$ratio.main) * .9
outFlow <- avg.outflow2018[which(avg.outflow2018$wkd == wkd),]
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

##### Fill the most upstream cell's number of vehicle #####
cell1.VDS <- unique(mainPM[which(mainPM$cellID == 1),]$VDS)[1]
cell1.VDS.flow <- avg.input2018[which(avg.input2018$wkd == wkd & avg.input2018$VDS == cell1.VDS),]

for (t in 1:simulationstep) {
  step <- startStep + (t-1)
  n.sim.input[t] <- calEnterFlow(cell1.VDS.flow, step) + q_on[step,1] - q_off[step,1] # #veh(/min) (#veh/5min * dt) for all lane
}

##### Fill the cells' initial flow #####
t=1
step = startStep + (t-1)
for (i in 1:realcellnum) {
  cell.VDS <- unique(mainPM[which(mainPM$cellID == i),]$VDS)[1]
  cell.VDS.flow <- avg.input2018[which(avg.input2018$VDS == cell.VDS & avg.input2018$wkd == wkd),]
  y.sim.input[i] <- calEnterFlow(cell.VDS.flow, step) + q_on[step,i] - q_off[step,i] # #veh(/min) (#veh/5min * dt) for all lane
}
cell.VDS <- 1215109
cell.VDS.flow <- avg.fwy[which(avg.fwy$VDS == cell.VDS & avg.fwy$wkd == wkd),] ################### CHECK 1215109 in avg.fwy!!!!
y.sim.input[fwycellID] <- cell.VDS.flow[((step-1) %/% inputDiv)+1,]$Flow / inputDiv

#### CTM parameter Calibration ####
## fitness function: minimize RMSE (as ga function find maximum of convex function, fitness function is -getRMSE()) ##
## result ##
# c.vf :mph
# c.qmax : veh/hr/lane
# c.w : mph
# c.kjam : veh/mile/lane

start_time <- Sys.time()
garesult <- ga(type = "real-valued" ,
               fitness = function(x) -getRMSE(x[1],x[2],x[3],x[4], pems.speed),
               # c.vf, c.qmax, c.w, c.kjam 
               lower = c( 50 , 2000 , 12 , 200) ,
               upper = c( 70 , 2300 , 20 , 250) ,
               popSize = 50,
               maxiter = 20)
end_time <- Sys.time()

TotalTime = end_time - start_time
garesult@solution[1,]
plot(garesult)




