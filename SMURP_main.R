
# README ------------------------------------------------------------------

# This script is a main prediction model.


# Requirements ------------------------------------------------------------


source("GlobalParameters.R")
source("Functions.R")


# Parameters --------------------------------------------------------------

k=3
w = rep(1, k+1)/(k+1)

quantile = 80
max.std = 2.2  ## prediction time: 10:00-20:00 ##  4.7 (90) 3.1 (85) 2.2 (80) 1.7 (75)
min.std = -1.6 ## prediction time: 10:00-20:00 ## -3.2 -2.2 -1.6 -1.2

wkd = "twt"
VDSlist<-as.character(unique(mainPM$VDS))


# Main --------------------------------------------------------------------

#### Select DB ####


incidentDB = readRDS(paste0(incidentDBDir, "/incidentDB_",tolower(wkd),".rds"))
incidentDB3 = lapply(incidentDB, function(x) agg5min(x))


#### main SMURP ####
# mainPM: VDS, cell ID link
# knn.speed: from IncidentThresholdDetermination.R
# actual.speed: from IncidentThresholdDetermination.R


knn.speed.15int = knn.speed[which(difftime(strptime(knn.speed$time, "%H%M"), strptime(knn.speed$ptime, "%H%M")) <= 15),]
actual.speed.15int = actual.speed[which(actual.speed$time <= 2000),]
knn.speed.15int = knn.speed.15int[which(knn.speed.15int$time <= 2000),]

vds.name = colnames(knn.speed.15int)[4:ncol(knn.speed.15int)]

date = unique(knn.speed.15int$date)
date.wkd <- weekdays(as.Date(as.character(date), format = "%Y%m%d"))
date.wkd[which(date.wkd %in% c("Tuesday", "Wednesday", "Thursday"))] = "twt"
date.wkd = data.frame(date, date.wkd)

date.tmp <- date.wkd[which(date.wkd$date.wkd == "twt"),] # select day
date = unique(date.tmp$date)

knn.speed.15int.result = c()
result = c()

det_vds = c()

for (d in 1:length(date)) {
  
  cat(date[d])
  wkd = extractWkd(date[d], wkd.names)
  target.knn = knn.speed.15int[which(knn.speed.15int$date == date[d]),]
  target.act = actual.speed.15int[which(actual.speed.15int$date == date[d]),]
  target.knn = target.knn[which(target.knn$time <= 2000),] ## 5min interval
  target.act = target.act[which(target.act$time <= 2000),] ## 5min interval
  
  target.knn.rev = target.knn
  
  ptime = unique(target.knn$ptime)
  
  for (pt in 1:(length(ptime)-1)) {
    
    startTime <- "10:00"
    startTime <- strptime(startTime, "%H:%M")
    startStep <- 1
    
    x <- c(startStep, startStep+5)
    
    ##### Agg_target.knn before event detection
    ano.rows_pre = which(target.knn.rev$ptime == ptime[pt])[2]
    ano.rows = which(target.knn.rev$ptime == ptime[pt])[3]
    
    aggSpeed = c()  # avg. Area
    for (n in 1:(length(vds.name))) {
      speed_1=target.knn.rev[ano.rows_pre, c(4:ncol(target.knn.rev))][n]
      speed_2=target.knn.rev[ano.rows, c(4:ncol(target.knn.rev))][n]
      y <- c(speed_1, speed_2)
      area=integrate(approxfun(x,y), range(x)[1], range(x)[2])
      aggSpeed[n]=area$value / 5 #min
    }
    
    err.speed = aggSpeed - target.act[ano.rows, c(3:ncol(target.act))]
    target.ano = replace(err.speed, err.speed >= min.std.VDS & err.speed <= max.std.VDS, 0) # SMURP 2
    #target.ano = replace(err.speed, err.speed >= min.std & err.speed <= max.std, 0) # SMURP 1
    
    time = strptime(ptime[pt], "%H%M")
    
    ano.vc.df = c()
    if (sum(abs(target.ano)) > 0) {
      ano.vds = vds.name[abs(target.ano)>0]
      ano.cell = mainPM[match(ano.vds, mainPM$VDS),]$cellID
      ano.vc.df = data.frame(ptime[pt],ano.vds, ano.cell)
      ano.vc.df$cond <- 0
      
      his.start = startStep + as.numeric(difftime(time, startTime, units = "mins"))/5
      his.end = his.start + 15/5 - 1
      pred.start = startStep + as.numeric(difftime(time+15*60, startTime, units = "mins"))/5
      pred.end = pred.start + 15/5 - 1
      
      for (i in 1:nrow(ano.vc.df)) {
        cand.db = lapply(incidentDB3, function(x) subset.time.sp(x, his.start, pred.end))
        
        ##### 1. select detection space #####
        ano.cell.adj <- c() # cell to upstream direction
        if (ano.cell[i] - 2 <= 0) { # number + 1 = cell number
          ano.cell.adj = c(1:ano.cell[i])
        } else { # cell 
          ano.cell.adj = c((ano.cell[i] - 2):ano.cell[i])
        }
        
        ano.vds.start = as.character(mainPM[which(ano.cell.adj[1] == mainPM$cellID),]$VDS)[1]
        ano.vds.end = as.character(last(mainPM[which(ano.cell.adj[length(ano.cell.adj)] == mainPM$cellID),]$VDS))
        idx.start = which(VDSlist==ano.vds.start)+2
        idx.end = which(VDSlist==ano.vds.end)+2
        
        tmp.act = target.act[c(his.start:pred.end),c(idx.start:idx.end)]
        tmp.knn = target.knn[c(his.start:pred.end),c((idx.start+1):(idx.end+1))]
        
        if (length(ano.cell.adj) == 1){
          tmp.act.his = tmp.act[c(1:3)]
          tmp.knn.his = tmp.knn[c(1:3)]
          tmp.act.pred = tmp.act[c(4:6)]
          tmp.knn.pred = tmp.knn[c(4:6)]
          
          knn.dist = abs(tmp.knn.his - tmp.act.his)
          
        } else {
          tmp.act.his = tmp.act[c(1:3),]
          tmp.knn.his = tmp.knn[c(1:3),]
          tmp.act.pred = tmp.act[c(4:6),]
          tmp.knn.pred = tmp.knn[c(4:6),]  
          
          knn.dist = (colSums(abs(tmp.knn.his - tmp.act.his)))
        }
        
        #### 2. get simulation DB and calculate dist. btw actual speed #####
        DB_check <- list()
        
        for (j in 1:length(ano.cell.adj)) {
          vdsinCell <-  as.character(mainPM[which(ano.cell.adj[j] == mainPM$cellID),]$VDS)
          cand.db_tmp = lapply(cand.db, function(x) subset.cell(x, ano.cell.adj[j]))
          
          if (length(ano.cell.adj) == 1){
            dist.db = sapply(cand.db_tmp, function(x) cal.dist(x, tmp.act.his))
          } else {
            vdsLastinCell<-last(vdsinCell)
            dist.db = sapply(cand.db_tmp, function(x) cal.dist(x, tmp.act.his[,vdsLastinCell])) 
          }
          
          dist.order = order(abs(dist.db))[1]
          cond = dist.db[dist.order]
          sim.pred = do.call("rbind", lapply(cand.db_tmp[dist.order], function(x) x$speed))
          DB_check[[j]] = list(ano.cell = ano.cell.adj[j], cond = cond, sim.pred = sim.pred) # dist.order, sim.pred
        }
        
        cond = mean(unlist(do.call("rbind", DB_check)[,2])) < mean(knn.dist)
        #cond
        
        #### 3. revise Mk-NN to sim #####
        
        if (cond) {
          w = c(0.5, 0.5)
          sim.pred.tmp = do.call("rbind", DB_check)[,3]
          sim.pred <- array(unlist(sim.pred.tmp), dim = c(6,length(ano.cell.adj)))
          
          rev.pred<-tmp.knn.pred
          for (j in 1:length(ano.cell.adj)) {
            vdsinCell <-  as.character(mainPM[which(ano.cell.adj[j] == mainPM$cellID),]$VDS)
            if (length(ano.cell.adj) == 1){
              rev.pred = sim.pred[c(4:6),] * w[1] + tmp.knn.pred * w[2] 
              knn.pred.dist = mean(sum(abs(tmp.knn.pred - tmp.act.pred)))
              rev.pred.dist = mean(sum(abs(rev.pred - tmp.act.pred)))
            } else {
              rev.pred[,vdsinCell] = sim.pred[c(4:6),][,j] * w[1] + tmp.knn.pred[,vdsinCell] * w[2] 
              knn.pred.dist = mean(colSums(abs(tmp.knn.pred - tmp.act.pred)))
              rev.pred.dist = mean(colSums(abs(rev.pred - tmp.act.pred)))
            }
          }
          
          cond2 = rev.pred.dist <= knn.pred.dist
          #cond2
          
          if (cond2) { 
            ano.vc.df[i,]$cond <- cond2
            
            for (j in 1:length(ano.cell.adj)) { # cell to vds
              vdsinCell = as.character(mainPM[which(ano.cell.adj[j] == mainPM$cellID),]$VDS)
              
              if (length(ano.cell.adj) == 1) {
                target.knn.rev[c(pred.start:pred.end), vdsinCell] = rev.pred
              } else {
                target.knn.rev[c(pred.start:pred.end), vdsinCell] = rev.pred[,vdsinCell]
              } 
              
            }
          }
          
        }
      }
      
      
    }
    
    det_vds = rbind(det_vds, cbind(date[d], ano.vc.df))
  }
  
  vds.length <- pm.postmile[match(vds.name, pm.postmile$VDS),]$length
  tmp.knn.sec.s <- rowSums(mapply('/', vds.length, target.knn[,vds.name]) * 60 * 60)
  tmp.knn.rev.sec.s <- rowSums(mapply('/', vds.length, target.knn.rev[,vds.name]) * 60 * 60)
  tmp.act.s <- rowSums(mapply('/', vds.length, target.act[,vds.name]) * 60 * 60)
  
  result = rbind(result, cbind(target.knn.rev[,c(1:3)], "actual" = tmp.act.s, "knn" = tmp.knn.sec.s, "knn.rev" = tmp.knn.rev.sec.s))
  knn.speed.15int.result = rbind(knn.speed.15int.result, target.knn.rev)

}

#### Save Results ####
saveRDS(knn.speed.15int.result, file="Agg.knn.speed.15int.result_Grev3_",quantile,"Q_",tolower(wkd),".rds")

result$mape.knn = abs(result$knn - result$actual)/result$actual * 100
result$mape.knn.rev = abs(result$knn.rev - result$actual)/result$actual * 100


result <-result %>% mutate(diff = mape.knn- mape.knn.rev)
saveRDS(result, file="Agg.mape.result_Grev3_,",quantile,"Q_,",tolower(wkd),".rds")


#### Visualize results ####

knn.speed.15int.result_tmp <- readRDS("Agg.knn.speed.15int.result_mod3rev3_75Q_sat.rds")

lvls <- pretty(range(20, 80),28)

target.knn = knn.speed.15int[which(knn.speed.15int$date == date[d]),]
target.act = actual.speed.15int[which(actual.speed.15int$date == date[d]),]
target.knn = target.knn[which(target.knn$time <= 2000),] ## 5min interval
target.act = target.act[which(target.act$time <= 2000),] ## 5min interval

target.sim = knn.speed.15int.result[which(knn.speed.15int.result$date == date[d]),]

filled.contour(as.matrix(target.act[,c(3:ncol(target.act))]), levels = lvls)
filled.contour(as.matrix(target.knn[,c(4:ncol(target.knn))]), levels = lvls)
filled.contour(as.matrix(target.sim[,c(4:ncol(target.sim))]), levels = lvls)
cat(date[d])

#### MAPE results ####
mean(result$mape.knn)
mean(result$mape.knn.rev)
sd(result$mape.knn)
sd(result$mape.knn.rev)


#### compare MAPE when the incidents happened ####

incident4result <- incident[which(incident$duration>0),]
incident4result$endTime <- format(strptime(incident4result$time, "%H:%M") + as.difftime(incident4result$duration, units = "mins"), "%H:%M")
incident4result <- incident4result[which(incident4result$Abs.PM > 2.537),]

incident4result$stime = incident4result$time
incident4result$stime = format(floor_date(strptime(incident4result$stime, "%H:%M"), unit = "5 minuits"), "%H:%M")
incident4result$stime = str_remove(incident4result$stime, ":")

incident4result$etime = format(strptime(incident4result$endTime, "%H:%M") + as.difftime(0, units = "mins"), "%H:%M")
incident4result$etime = format(ceiling_date(strptime(incident4result$etime, "%H:%M"), unit = "10 minuits"), "%H:%M")
incident4result$etime = str_remove(incident4result$etime, ":")

result$inc = 0

for (i in 1:nrow(incident4result)) {
  tmp.inc = incident4result[i,]
  
  tmp.date = tmp.inc$date
  tmp.stime = tmp.inc$stime
  tmp.etime = tmp.inc$etime
  
  included = which(result$date == tmp.date & result$time >= as.numeric(tmp.stime) & result$time <= as.numeric(tmp.etime))
  
  if (length(included) >0 ) {
    result[included,]$inc = tmp.inc$id
  }
}

colMeans(result[which(result$inc > 0),c("mape.knn", "mape.knn.rev")])
sd(result[which(result$inc > 0),c("mape.knn")])
sd(result[which(result$inc > 0),c("mape.knn.rev")])



#### compare MAPE (cross validation CV btw Mk-NN and Proposed model) ####

limit=tail(sort(result$mape.knn), n=nrow(result)*.10)[1]
result4knn <- result[result$mape.knn>=limit,]

mean(result4knn$mape.knn)
mean(result4knn$mape.knn.rev)
#sd(result4knn$mape.knn)
#sd(result4knn$mape.knn.rev)


limit=tail(sort(result$mape.knn.rev), n=nrow(result)*.10)[1]
result4proposed <- result[result$mape.knn.rev>=limit,]

mean(result4proposed$mape.knn.rev)
mean(result4proposed$mape.knn)
#sd(result4proposed$mape.knn.rev)
#sd(result4proposed$mape.knn)



#### Detect anormal VDS/Cell for date ####

det_vds = c()

for (d in 1:length(date)) {

  cat(date[d])
  wkd = extractWkd(date[d], wkd.names)
  target.knn = knn.speed.15int[which(knn.speed.15int$date == date[d]),]
  target.act = actual.speed.15int[which(actual.speed.15int$date == date[d]),]
  target.knn = target.knn[which(target.knn$time <= 2000),] ## 5min interval
  target.act = target.act[which(target.act$time <= 2000),] ## 5min interval

  target.knn.rev = target.knn

  ptime = unique(target.knn$ptime)

 for (pt in 1:(length(ptime)-1)) {
  # for (pt in 1:2) {
    startTime <- "10:00"
    startTime <- strptime(startTime, "%H:%M")
    startStep <- 1

    x <- c(startStep, startStep+5)

    ##### Agg_target.knn before event detection #####
    ano.rows_pre = which(target.knn.rev$ptime == ptime[pt])[2]
    ano.rows = which(target.knn.rev$ptime == ptime[pt])[3]

    aggSpeed = c()  # avg. Area
    for (n in 1:(length(vds.name))) {
      speed_1=target.knn.rev[ano.rows_pre, c(4:ncol(target.knn.rev))][n]
      speed_2=target.knn.rev[ano.rows, c(4:ncol(target.knn.rev))][n]
      y <- c(speed_1, speed_2)
      area=integrate(approxfun(x,y), range(x)[1], range(x)[2])
      aggSpeed[n]=area$value / 5 #min
    }

    err.speed = aggSpeed - target.act[ano.rows, c(3:ncol(target.act))]
    target.ano = replace(err.speed, err.speed >= min.std.VDS & err.speed <= max.std.VDS, 0) # SMURP 2
    #target.ano = replace(err.speed, err.speed >= min.std & err.speed <= max.std, 0) # SMURP 1


    if (sum(abs(target.ano)) > 0) {
      ano.vds = vds.name[abs(target.ano)>0]
      ano.cell = mainPM[match(ano.vds, mainPM$VDS),]$cellID
      ano.vc.df = data.frame(ptime[pt],ano.vds, ano.cell)
      ano.vc.df$cond <- 0

      det_vds = rbind(det_vds, cbind(date[d], ano.vc.df))
    }

  }

}


det_vds2 = det_vds[which(det_vds$cond == 1),]



ggplot(det_vds2) +
  geom_point(aes(x = ptime.pt., y = ano.cell, color = ano.cell),shape = 15) +  scale_colour_gradientn(colours=terrain.colors(10))
ggplot(det_vds) +
  geom_point(aes(x = ptime.pt., y = ano.cell, color = ano.cell),shape = 15) +  scale_colour_gradientn(colours=terrain.colors(10))
