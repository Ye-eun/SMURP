
# README ------------------------------------------------------------------

# This script is to determine prediction failure threshold by capturing specific quantile value of the difference between observed link speed and mknn-based predicted speed.



# Requirements ------------------------------------------------------------


source("GlobalParameters.R")
source("Functions.R")



# Main --------------------------------------------------------------------

#### Get Mk-nn predicted speed for each VDS ####

ddoutPDir <- paste0(BaseDir, "/Data/data-driven prediction/out/prediction")
outFilesP <- list.files(path = ddoutPDir, pattern = "201908|201909.+.rdata")

vds.use = as.character(pm.info.use$VDS)
knn.speed = c()
for (i in 1:length(outFilesP)) {
  names <- outFilesP[i]
  date <- substr(names, 1, 8)
  ptime <- substr(names, 9, 12)
  print(date)
  print(ptime)
  tmp.knn.speed.P <- get(load(paste0(ddoutPDir, "/", names)))
  tmp.knn.speed.P[,3:ncol(tmp.knn.speed.P)] <- sapply(tmp.knn.speed.P[,3:ncol(tmp.knn.speed.P)], as.numeric)
  
  knn.speed = rbind(knn.speed, tmp.knn.speed.P)
}

colnames(knn.speed)[c(1, 3)] = c("date", "time")
vds.name = colnames(knn.speed)[4:ncol(knn.speed)]



#### Get Observed link speed for each VDS ####

speedFiles <- list.files(path = paste0(dataDir, "/speed"), pattern = "201908|201909.+speed.xlsx")

result.names <-c("date", "time", vds.name)
result <- data.frame(matrix(ncol = length(result.names), nrow = 0))
names(result) <- result.names

diff <- as.difftime("00:05", format = "%H:%M")
for (d in 1:length(speedFiles)) {
  target.file <- speedFiles[d]
  speed <- read_xlsx(paste0(dataDir, "/speed/", target.file))
  speed <- data.frame(speed)
  colnames(speed) <- c("time", "Abs.PM", "CA.PM", "VDS", "AggSpeed", "Lane", "Observed")
  speed$time <- format(strptime(speed$time, "%H:%M"), "%H:%M")
  if (nrow(speed)>0) {
    date <- substr(target.file, 1, 8)
    print(date)
    time <- strptime("10:05", "%H:%M")
    while (format(time, "%H:%M") < "22:05") {
      tmp.speed <- speed[which(speed$time == format(time, "%H:%M")),]
      colname.vds = tmp.speed$VDS
      result = rbind(result, c(date, format(time, "%H:%M"), tmp.speed[match(vds.name, tmp.speed$VDS),]$AggSpeed))
      
      print(time)
      time <- time + diff
    }
  }
}
colnames(result) <- result.names
actual.speed = result
actual.speed[,2] = as.numeric(str_replace_all(actual.speed[,2],":", ""))
actual.speed[,3:ncol(actual.speed)] <- sapply(actual.speed[,3:ncol(actual.speed)], as.numeric)


#### compare actual and knn speed ####

err.speed = knn.speed

for (i in 1:nrow(err.speed)) {
  date = knn.speed[i,"date"]
  time = knn.speed[i,"time"]
  
  tmp = actual.speed[which(actual.speed$date == date & actual.speed$time == time),]
  err.speed[i,4:ncol(err.speed)] = err.speed[i,c(4:ncol(err.speed))] - tmp[,c(3:ncol(tmp))]
}
err.speed[,3:ncol(err.speed)] <- sapply(err.speed[,3:ncol(err.speed)], as.numeric)

##### different standard for every VDSs (90, 10% quantile) #####
err.speed2 = err.speed[which(difftime(strptime(err.speed$time, "%H%M"), strptime(err.speed$ptime, "%H%M")) <= 15),] # Selected 15min interval
err.speed.15int = err.speed2[which(err.speed2$time <= 2000 & err.speed2$time > 1000),]
max.std = quantile(unlist(err.speed.15int[,c(4:ncol(err.speed.15int))]),.75) #  85,80, 75
min.std = quantile(unlist(err.speed.15int[,c(4:ncol(err.speed.15int))]),.25)
# # 
max.std.VDS=apply(err.speed.15int[,c(4:ncol(err.speed.15int))], 2, quantile, probs=.90) # 85,80, 75
min.std.VDS=apply(err.speed.15int[,c(4:ncol(err.speed.15int))], 2, quantile, probs=.10)
# 

hist(unlist(err.speed[,c(4:ncol(err.speed))]))
summary(unlist(err.speed[,c(4:ncol(err.speed))]))
max.std = quantile(unlist(err.speed2[,c(4:ncol(err.speed))]),.90) # 80(o), 85, 75
min.std = quantile(unlist(err.speed2[,c(4:ncol(err.speed))]),.10)
max.err = max(unlist(err.speed[,c(4:ncol(err.speed))]))
min.err = min(unlist(err.speed[,c(4:ncol(err.speed))]))

#### other analysis ####

anomal.speed = err.speed
anomal.speed[,2:ncol(anomal.speed)] <- sapply(anomal.speed[,2:ncol(anomal.speed)], as.numeric)
anomal.speed[(anomal.speed >= max.std & anomal.speed <= max.err) | (anomal.speed <= min.std & anomal.speed >= min.err)] = 100
anomal.speed[anomal.speed < 100] = 0
anomal.speed[,c(1:3)] = err.speed[,c(1:3)]

anomal.speed$anomality = rowSums(anomal.speed[,c(4:ncol(anomal.speed))])/100
anomal.speed$ptime <- as.numeric(anomal.speed$ptime)

anomal.speed$timediff = anomal.speed$time - anomal.speed$ptime
anomal.table = as.data.frame(table(anomal.speed[,c("anomality", "timediff")]))
anomal.table$Freq = as.numeric(anomal.table$Freq)
anomal.table$anomality = as.numeric(anomal.table$anomality)
ggplot() + geom_tile(data = anomal.table, aes(x = anomality, y = timediff, fill = Freq))
plot(aggregate(anomality ~ timediff, anomal.speed, sum), type = 'l')


incident.speed = err.speed
incident.speed[,4:ncol(incident.speed)] = 0
for (i in 1:nrow(incident.tot)) {
  tmp.inc = incident.tot[i,]
  date = tmp.inc$date
  stime = tmp.inc$time
  etime = tmp.inc$endTime
  pm = tmp.inc$Abs.PM
  roundstime = as.numeric(str_replace_all(stime,":", "")) %/% 5 * 5
  roundetime = (as.numeric(str_replace_all(etime,":", "")) %/% 5 + 1) * 5
  vds = pm.postmile[max(which(pm.postmile$eff_e < pm)),]$VDS
  incident.speed[which(incident.speed$date == date & incident.speed$time >= roundstime & incident.speed$time <= roundetime), which(colnames(incident.speed) == vds)] = 1
}

ano.eve.speed = anomal.speed
ano.eve.speed[,4:ncol(ano.eve.speed)] = anomal.speed[,4:ncol(anomal.speed)] + incident.speed[,4:ncol(incident.speed)]
table(ano.eve.speed[,4:ncol(ano.eve.speed)])

length(ano.eve.speed[ano.eve.speed==100])
length(ano.eve.speed[ano.eve.speed==1])
length(ano.eve.speed[ano.eve.speed==101])


