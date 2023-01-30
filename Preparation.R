
# README ------------------------------------------------------------------

# This script is for preparation of the whole project. Most of the data used in this project is quite huge and not recommended to load them at every script. Some of scripts share same data, so, this script load all data we need in whole process.

## Load input value
# AggFlow.lane: #veh/5min/lane
# AggSpeed.5min: miles/5min
# density: #veh/mile/lane


# Requirements ------------------------------------------------------------

Sys.setlocale("LC_TIME", "C") # in English

source("GlobalParameters.R")
source("Functions.R")


# Info Load ---------------------------------------------------------------

pm.info <- read_excel(path = paste0(infoDir, "/info_sh.xlsx"), sheet = "postmile", col_names = T)
pm.info <- data.frame(pm.info)
pm.info.use <- pm.info[which(pm.info$postmile >= start.pm & pm.info$postmile <= end.pm),]

# CTM Network Initialization ----------------------------------------------
cell <- cell.generate(dt)
dvgcellID = which(cell$ratio<1)[1]
fwycellID = which(cell$ratio<1)[2]
realcellnum = nrow(cell[!is.na(cell$eff_b),]) 

PMlist <- vdsAllocation(cell)
onPM <- PMlist[[1]]
offPM <- PMlist[[2]]
mainPM <- PMlist[[3]]
fwyPM <- PMlist[[4]]
fwyPM.8 <- fwyPM[1,]
onPM = na.omit(onPM)
offPM = na.omit(offPM)
mainPM = na.omit(mainPM)


# 2018 Data ---------------------------------------------------------------
##### Mainline data (aggregated by weekdays) #####
flowDir <- paste0(dataDir, "/flow")
speedDir <- paste0(dataDir, "/speed")
incidentDir <- paste0(dataDir, "/incident")
flowFiles <- list.files(path = flowDir, pattern = "2018.+flow.xlsx")
speedFiles <- list.files(path = speedDir, pattern = "2018.+speed.xlsx")

flowFiles <- flowFiles[!grepl("~", flowFiles)]
speedFiles <- speedFiles[!grepl("~", speedFiles)]
incidentFiles <- incidentFiles[!grepl("~", incidentFiles)]

total.input2018 = loadInput(flowFiles, speedFiles, flowDir, speedDir)
total.input2018$wkd = extractWkd(total.input2018$date, wkd.names)
total.input2018[,c(5:13)] = sapply(total.input2018[,c(5:13)], as.numeric)

avg.input2018 = aggregate(.~ date + time + VDS + Abs.PM + CA.PM + wkd , total.input2018, mean)
avg.input2018 = avg.input2018[which(avg.input2018$time >= "10:00"),]

##### Incidents #####

incidentFiles <- list.files(path = incidentDir, pattern = "2018.+incident.xlsx")
incidentFiles <- incidentFiles[!grepl("~", incidentFiles)]
incident2018 = loadIncident(incidentFiles, incidentDir)
incident2018 = incident2018[which(incident2018$Abs.PM>4.5 & incident2018$duration >=5),]


#### Outflow data ####

finOutDir <- paste0(dataDir, "/finOutFlow/flow")

flowDirs <- list.dirs(path = finOutDir, recursive = F)

flowDirN <- flowDirs[1]
flowDirS <- flowDirs[2]

flowNfiles <- list.files(path = paste0(flowDirN, "/hour"), pattern = "flow.xlsx")
flowSfiles <- list.files(path = paste0(flowDirS, "/hour"), pattern = "flow.xlsx")


flowS <- c()
flowN <- c()
tmp.names <- c("time", "flow.1", "flow.2", "AggFlow", "lane", "Observed")
for (i in 1:length(flowSfiles)) {
  target.flow <- flowSfiles[i]
  tmp <- read_xlsx(paste0(flowDirS, "/hour/", target.flow))
  tmp <- data.frame(tmp)
  colnames(tmp) <- tmp.names
  tmp$time <- substr(tmp$time, 12, 16)
  tmp$date <- substr(target.flow, 1, 8)
  
  flowS <- rbind(flowS, tmp)
}
for (i in 1:length(flowNfiles)) {
  target.flow <- flowNfiles[i]
  tmp <- read_xlsx(paste0(flowDirN, "/hour/", target.flow))
  tmp <- data.frame(tmp)
  colnames(tmp) <- tmp.names
  tmp$time <- substr(tmp$time, 12, 16)
  tmp$date <- substr(target.flow, 1, 8)
  
  flowN <- rbind(flowN, tmp)
}

flowS$Observed <- as.numeric(flowS$Observed)
flowN$Observed <- as.numeric(flowN$Observed)

flowS$weekdays <- weekdays(as.Date(flowS$date, format = "%Y%m%d"))
flowN$weekdays <- weekdays(as.Date(flowN$date, format = "%Y%m%d"))

flowS <- flowS[which(flowS$time >= "06:00"),]
flowN <- flowN[which(flowN$time >= "06:00"),]

rmdate <- unique(c(flowN[which(flowN$Observed < 100),]$date, flowS[which(flowS$Observed < 100),]$date, "20200703", "20200704"))

date.cand <- substr(flowSfiles, 1, 8)
date.cand <- date.cand[-which(date.cand %in% rmdate)]

flowS <- flowS[which(flowS$date %in% date.cand),]
flowN <- flowN[which(flowN$date %in% date.cand),]


wkd.names <- c("Sun", "Sat", "Mon", "Fri", "twt")

flow.ratio.names <- c("time", "flow.s", "flow.n", "wkd")
flow.ratio <- as.data.frame(matrix(0, ncol = length(flow.ratio.names)))
colnames(flow.ratio) <- flow.ratio.names
flow.ratio <- flow.ratio[-1,]

for (i in 1:5) { #length(wkd) 
  tmp.wkd <- wkd.list[[i]]
  
  tmp.s <- flowS[which(flowS$weekdays %in% tmp.wkd),]
  tmp.n <- flowN[which(flowN$weekdays %in% tmp.wkd),]
  
  tmp.s.agg <- aggregate(AggFlow ~ time, tmp.s, mean)  
  tmp.n.agg <- aggregate(AggFlow ~ time, tmp.n, mean)
  
  tmp <- merge(tmp.s.agg, tmp.n.agg, by = "time")
  tmp$wkd <- wkd.names[i]
  colnames(tmp) <- flow.ratio.names
  
  flow.ratio <- rbind(flow.ratio, tmp)
}

flow.ratio$ratio <- (flow.ratio$flow.n + flow.ratio$flow.s) / flow.ratio$flow.s

# plot(flowS$AggFlow, type = 'l')
# lines(flowN$AggFlow, col = 'red')


# Target Data (201908, 09) ------------------------------------------------

#### Mainline data ####

flowDir <- paste0(dataDir, "/flowCTM")
speedDir <- paste0(dataDir, "/speedCTM")
flowFiles <- list.files(path = flowDir, pattern = "201909|201908.+flow.xlsx")
speedFiles <- list.files(path = speedDir, pattern = "201909|201908.+speed.xlsx")

flowFiles <- flowFiles[!grepl("~", flowFiles)]
speedFiles <- speedFiles[!grepl("~", speedFiles)]

total.input <- c()
for (i in 1:length(flowFiles)) {
  target.flow <- flowFiles[i]
  target.speed <- speedFiles[i]
  inputFlow <- read_xlsx(paste0(flowDir, "/", target.flow))
  inputSpeed <- read_xlsx(paste0(speedDir, "/", target.speed))
  inputFlow <- data.frame(inputFlow)
  inputSpeed <- data.frame(inputSpeed)
  colnames(inputFlow) <- c("time", "Abs.PM", "CA.PM", "VDS", "AggFlow", "Lane", "Observed")
  colnames(inputSpeed) <- c("time", "Abs.PM", "CA.PM", "VDS", "AggSpeed", "Lane", "Observed")
  inputVal <- merge(inputFlow, inputSpeed[,c(1,4,5)], by = c("time", "VDS"))
  inputVal$AggFlow.lane <- inputVal$AggFlow / inputVal$Lane
  inputVal$AggSpeed.5min <- inputVal$AggSpeed / 60 * 5
  inputVal$AggDensity.lane <- inputVal$AggFlow.lane / inputVal$AggSpeed.5min
  if (nrow(inputVal)>0) {
    inputVal$date <- substr(target.flow, 1, 8)
  }
  total.input <- rbind(total.input, inputVal)
}
total.input$AggDensity <- total.input$AggDensity.lane * total.input$Lane


#### Ramp data (aggregated by weekdays) ####

cell <- cell.generate(dt)
dvgcellID = which(cell$ratio<1)[1]
fwycellID = which(cell$ratio<1)[2]
realcellnum = nrow(cell[!is.na(cell$eff_b),]) 
PMlist <- vdsAllocation(cell)

onPM <- PMlist[[1]]
offPM <- PMlist[[2]]
mainPM <- PMlist[[3]]
fwyPM <- PMlist[[4]]
fwyPM.8 <- fwyPM[1,]
onPM = na.omit(onPM)
offPM = na.omit(offPM)
mainPM = na.omit(mainPM)

RampFwy.input <- RampFwyProcess(PMlist)
total.on.fwy <- RampFwy.input[[1]]
total.off.fwy <- RampFwy.input[[2]]
total.fwy <- RampFwy.input[[3]]

vds.info.tot = loadVDSInfo(mainPM)

total.on.fwy$wkd = extractWkd(total.on.fwy$date, wkd.names)
total.off.fwy$wkd = extractWkd(total.off.fwy$date, wkd.names)
total.fwy$wkd = extractWkd(total.fwy$date, wkd.names)
# total.outflow$wkd = extractWkd(total.outflow$date, wkd.names)

total.on.fwy[,c(3:7)] = sapply(total.on.fwy[,c(3:7)], as.numeric)
total.off.fwy[,c(3:7)] = sapply(total.off.fwy[,c(3:7)], as.numeric)
total.fwy[,c(3:7)] = sapply(total.fwy[,c(3:7)], as.numeric)
# total.outflow[,c(3:7)] = sapply(total.outflow[,c(3:7)], as.numeric)

avg.on.fwy = aggregate(.~time + VDS + wkd, total.on.fwy, mean)
avg.off.fwy = aggregate(.~time + VDS + wkd, total.off.fwy, mean)

tmp.total.fwy <- total.fwy[,-c(8)] # remove type
avg.fwy = aggregate(.~time + VDS + wkd, tmp.total.fwy, mean)
# avg.outflow = aggregate(.~time + VDS + wkd, total.outflow, mean)

avg.on.fwy = avg.on.fwy[which(avg.on.fwy$time >= "10:00"),]
avg.off.fwy = avg.off.fwy[which(avg.off.fwy$time >= "10:00"),]
avg.fwy = avg.fwy[which(avg.fwy$time >= "10:00"),]
# avg.outflow = avg.outflow[which(avg.outflow$time >= "10:00"),]



#### Diverging ratio ####

premain.id <- 1215043
fwy.id <- 1215109
main.id <- 1202885
off.id <- 1202883

cell8.main <- total.input[which(total.input$VDS == premain.id),]
cell9.fwy <- total.fwy[which(total.fwy$VDS == fwy.id),]
cell9.main <- total.input[which(total.input$VDS == main.id),]
cell9.off <- total.off.fwy[which(total.off.fwy$VDS == off.id),]

dvg.flow <- merge(cell9.main[,c("time", "date", "AggFlow", "Lane")], cell9.fwy[,c("time", "date", "Flow", "Lane")], by = c("date", "time"))
dvg.flow <- merge(dvg.flow, cell9.off[,c("time", "date", "Flow", "Lane")], by = c("date", "time"))
dvg.flow <- merge(dvg.flow, cell8.main[,c("time", "date", "AggFlow", "Lane")], by = c("date", "time"))

colnames(dvg.flow)[3:10] <- c("Flow.main", "Lane.main", "Flow.fwy", "Lane.fwy", "Flow.off", "Lane.off", "Flow.prev", "Lane.prev")

dvg.flow$ratio.main <- (dvg.flow$Flow.main + dvg.flow$Flow.off) / (dvg.flow$Flow.main + dvg.flow$Flow.fwy + dvg.flow$Flow.off)
dvg.flow$weekdays <- weekdays(as.Date(dvg.flow$date, format = "%Y%m%d"))

#dvg.flow$weekdays[(dvg.flow$weekdays == '일요일')] <- 'Sunday'
#dvg.flow$weekdays[(dvg.flow$weekdays == '토요일')] <- 'Saturday'
#dvg.flow$weekdays[(dvg.flow$weekdays == '월요일')] <- 'Monday'
#dvg.flow$weekdays[(dvg.flow$weekdays == '화요일')] <- 'Tuesday'
#dvg.flow$weekdays[(dvg.flow$weekdays == '수요일')] <- 'Wednesday'
#dvg.flow$weekdays[(dvg.flow$weekdays == '목요일')] <- 'Thursday'
#dvg.flow$weekdays[(dvg.flow$weekdays == '금요일')] <- 'Friday'


dvg.ratio <- c()
for (i in 1:length(wkd.names)) {
  tmp.wkd <- wkd.list[[i]]
  
  tmp.dvg <- dvg.flow[which(dvg.flow$weekdays %in% tmp.wkd),]
  tmp.dvg.agg <- aggregate(ratio.main ~ time, tmp.dvg, mean)
  tmp.dvg.agg$wkd <- wkd.names[i]
  
  # if (i==1) {
  #   plot(tmp.dvg.agg$ratio.main, type = 'l', ylim = c(0.3, 0.7))
  # } else {
  #   lines(tmp.dvg.agg$ratio.main, col = i)
  # }
  
  dvg.ratio <- rbind(dvg.ratio, tmp.dvg.agg)
}

###### check data (plot) ######
# plot(tmp.dvg.agg$ratio.main, type = 'l')
# plot(dvg.ratio$ratio.main, type = 'l')
# 
# table(dvg.flow[which((dvg.flow$Flow.main+dvg.flow$Flow.fwy+dvg.flow$Flow.off)/dvg.flow$Flow.prev < 0.9),]$date)
# 
# summary((dvg.flow$Flow.main+dvg.flow$Flow.fwy+dvg.flow$Flow.off)/dvg.flow$Flow.prev)
# 
# date = target.date[d]
# 
# tmp.8.main <- cell8.main[which(cell8.main$date == date),]
# tmp.9.main <- cell9.main[which(cell9.main$date == date),]
# tmp.9.fwy <- cell9.fwy[which(cell9.fwy$date == date),]
# tmp.9.off <- cell9.off[which(cell9.off$date == date),]
# 
# plot(tmp.8.main$AggFlow, type = "l", ylim = c(0,800))
# lines(tmp.9.main$AggFlow, col = 'red')
# lines(tmp.9.fwy$Flow, col = 'blue')
# lines(tmp.9.off$Flow, col = 'orange')
# lines(tmp.9.main$AggFlow + tmp.9.fwy$Flow + tmp.9.off$Flow, col = "green")


#### Incidents ####


# Load incidentDB ---------------------------------------------------------

incidentFiles <- list.files(path = incidentDir, pattern = "201909|201908.+incident.xlsx")
incidentFiles <- incidentFiles[!grepl("~", incidentFiles)]
incident.tot = loadIncident(incidentFiles,incidentDir)

