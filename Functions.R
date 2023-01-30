
# Packages ----------------------------------------------------------------

library(readxl)
library(ggplot2)
library(dplyr)
library(zoo)
library(reshape2)
library(dplyr)
library(stringr)
library(RColorBrewer)
library(lubridate)
library(plotly)
library(reshape)
library(ggpubr)


# Functions ---------------------------------------------------------------

#### load input files ####
loadTotalInput <- function() {
  ## Load input value
  # AggFlow.lane: #veh/5min/lane
  # AggSpeed.5min: miles/5min
  # density: #veh/mile/lane
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
  
  return(total.input)
}

loadVDSInfo <- function(mainPM) {
  vds.info <- read_excel(path = paste0(infoDir, "/info_sh.xlsx"),
                         sheet = "vds",
                         col_names = T)
  vds.info <- data.frame(vds.info)
  vds.info.tot <- merge(vds.info, mainPM[,c(7,13)], by = c("VDS"))
  vds.info.tot <- vds.info.tot[order(vds.info.tot$postmile),]
  return(vds.info.tot)
}

load_ramp <- function(onPM) {
  target.date <- unique(total.input$date)
  input <- deparse(substitute(onPM))
  type <- substr(input, 1, nchar(input)-2)
  rampVDS <- unique(onPM$VDS)
  
  tmp.names <- c("time", "Flow", "rampObserved", "mainObserved")
  ramp.names <- c("time", "VDS", "Lane", "Flow", "rampObserved", "mainObserved", "date")
  rampData <- data.frame(matrix(ncol = length(ramp.names), nrow = 0))
  for (i in 1:length(rampVDS)) {
    for (j in 1:length(target.date)) {
      path = paste0(dataDir, "/", type, "Ramp/flow/", rampVDS[i], "/", target.date[j],"flow.xlsx")
      tmp = read_xlsx(path)
      tmp = data.frame(tmp[c(73:nrow(tmp)),c(1:4)])
      colnames(tmp) = tmp.names
      tmp$time <- substr(tmp$time, 12, 16)  
      tmp.ramp <- data.frame(cbind(tmp$time, rampVDS[i], onPM[i,]$Lane, tmp$Flow, tmp$rampObserved, tmp$mainObserved, target.date[j]))
      
      rampData <- rbind(rampData, tmp.ramp)
    }
  }
  names(rampData) <- ramp.names
  
  rampData$Lane <- as.numeric(rampData$Lane)
  rampData$Flow <- as.numeric(rampData$Flow)
  
  return(rampData)
}

load_fwy <- function(fwyPM) {
  target.date <- unique(total.input$date)
  fwyPM <- fwyPM
  #fwyPM <- fwyPM[!is.na(fwyPM$ramp),]
  fwyVDS <- unique(fwyPM$ID)
  
  tmp.names <- c("time", "Flow", "Lane", "rampObserved")
  fwy.names <- c("time", "VDS", "Lane", "Flow", "rampObserved", "mainObserved", "date", "type")
  # ramp.names <- c("time", "L1.Flow", "L2.Flow", "Flow", "date")
  # ramp.names <- c("time", "VDS", "Lane", "Flow", "rampObserved", "mainObserved", "date")
  fwyData <- data.frame(matrix(ncol = length(fwy.names), nrow = 0))
  for (i in 1:length(fwyVDS)) {
    for (j in 1:length(target.date)) {
      path = paste0(dataDir, "/", "fwy/flow/", fwyVDS[i], "/", target.date[j],"flow.xlsx")
      tmp = read_xlsx(path)
      tmp = data.frame(tmp[c(73:nrow(tmp)),c(1, (ncol(tmp)-2):ncol(tmp))])
      colnames(tmp) = tmp.names
      tmp$time <- substr(tmp$time, 12, 16)  
      tmp.fwy <- data.frame(cbind(tmp$time, fwyVDS[i], tmp[i,]$Lane, tmp$Flow, tmp$rampObserved, 100, target.date[j], fwyPM[i,]$ramp))
      
      fwyData <- rbind(fwyData, tmp.fwy)
    }
  }
  names(fwyData) <- fwy.names
  
  fwyData$Lane <- as.numeric(fwyData$Lane)
  fwyData$Flow <- as.numeric(fwyData$Flow)
  
  return(fwyData)
}

load_outflow <- function() {
  outflowPM <- read_excel(path = paste0(infoDir, "/SR55S_detectorID.xlsx"),
                          col_names = T)
  outflowPM <- data.frame(outflowPM)
  outflowPM <- outflowPM[grep("22E", outflowPM$Name),]
  
  target.date <- unique(total.input$date)
  
  outflowVDS <- unique(outflowPM$ID)
  
  tmp.names <- c("time", "Flow", "Lane", "rampObserved")
  outflow.names <- c("time", "VDS", "Lane", "Flow", "rampObserved", "mainObserved", "date")
  outflowData <- data.frame(matrix(ncol = length(outflow.names), nrow = 0))
  for (i in 1:length(outflowVDS)) {
    for (j in 1:length(target.date)) {
      path = paste0(dataDir, "/", "finOutFlow/flow/", outflowVDS[i], "/", target.date[j],"flow.xlsx")
      tmp = read_xlsx(path)
      tmp = data.frame(tmp[c(73:nrow(tmp)),c(1, (ncol(tmp)-2):ncol(tmp))])
      colnames(tmp) = tmp.names
      tmp$time <- substr(tmp$time, 12, 16)  
      tmp.outflow <- data.frame(cbind(tmp$time, outflowVDS[i], tmp[i,]$Lane, tmp$Flow, tmp$rampObserved, 100, target.date[j], outflowPM[i,]$ramp))
      
      outflowData <- rbind(outflowData, tmp.outflow)
    }
  }
  names(outflowData) <- outflow.names
  
  outflowData$Lane <- as.numeric(outflowData$Lane)
  outflowData$Flow <- as.numeric(outflowData$Flow)
  
  return(outflowData)
}

loadInput = function(flowFiles, speedFiles, flowDir, speedDir) {
  total.input2018 <- c()
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
    if (nrow(inputVal)>0) {
      inputVal$AggFlow.lane <- inputVal$AggFlow / inputVal$Lane
      inputVal$AggSpeed.5min <- inputVal$AggSpeed / 60 * 5
      inputVal$AggDensity.lane <- inputVal$AggFlow.lane / inputVal$AggSpeed.5min
      inputVal$date <- substr(target.flow, 1, 8)
      
      total.input2018 <- rbind(total.input2018, inputVal)
    }
    
    
  }
  total.input2018$AggDensity <- total.input2018$AggDensity.lane * total.input2018$Lane
  return(total.input2018)  
}

loadIncident = function(incidentFiles,incidentDir) {
  # incidentDir <- paste0(dataDir, "/incident")
  # incidentFiles <- list.files(path = incidentDir, pattern = "2018.+incident.xlsx")
  
  # incidentFiles <- incidentFiles[!grepl("~", incidentFiles)]
  
  incident <- c()
  tmp.names <- c("id", "time", "duration", "freeway", "CA.PM", "Abs.PM", "source", "area", "location", "destription")
  for (i in 1:length(incidentFiles)) {
    target.incident <- incidentFiles[i]
    tmp <- read_xlsx(paste0(incidentDir, "/", target.incident))
    tmp <- data.frame(tmp)
    colnames(tmp) <- tmp.names
    if (nrow(tmp) > 0) {
      tmp$date <- substr(target.incident, 1, 8)
      tmp$time <- substr(tmp$time, 10, 14)
    }
    
    incident <- rbind(incident, tmp)
  }
  
  incident.tot <- incident[which(incident$duration>0),]
  incident.tot$endTime <- format(strptime(incident.tot$time, "%H:%M") + as.difftime(incident.tot$duration, units = "mins"), "%H:%M")
  incident.tot <- incident.tot[which(incident.tot$endTime >= "10:05"),]
  incident.tot <- incident.tot[which(incident.tot$time <= "22:00"),]
  incident.tot <- incident.tot[which(incident.tot$Abs.PM > start.PM),]
  
  
  if (length(which(as.numeric(substr(incident.tot$time, 1, 2)) < 6 & as.numeric(substr(incident.tot$endTime, 1, 2)) < 6))>0) {
    incident.tot <- incident.tot[-which(as.numeric(substr(incident.tot$time, 1, 2)) < 6 & as.numeric(substr(incident.tot$endTime, 1, 2)) < 6),]
  }
  
  incident.tot <- incident.tot[which(incident.tot$destription != "1166-Defective Traffic Signals"),]
  incident.tot <- incident.tot[which(incident.tot$destription != "ANIMAL-Live or Dead Animal"),]
  incident.tot <- incident.tot[which(incident.tot$destription != "JUMPER"),]
  incident.tot <- incident.tot[which(incident.tot$destription != "23114-Object Flying From Veh"),]
  return(incident.tot)
  
}

filterPM <- function(onPM) {
  for (i in 1:nrow(onPM)) {
    onPM[i,]$cellID = cell[max(which(cell$eff_b <= onPM[i,]$Abs.PM)),]$cellID
  }
  onPM <- onPM[order(onPM$Abs.PM, -onPM$Lane),]
  if (sum(duplicated(onPM$VDS))>0){
    onPM <- onPM[-which(duplicated(onPM$VDS)),]
  }
  return(onPM)
}

#### CTM ####
##### Preparation #####
cell.generate <- function(dt) {
  
  cell <- read_excel(path = paste0(infoDir, "/Cell.xlsx"),
                     col_names = T)
  pm.info <- read_excel(path = paste0(infoDir, "/info_sh.xlsx"),
                        sheet = "postmile",
                        col_names = T)
  
  cell <- data.frame(cell)
  pm.info <- data.frame(pm.info)
  cell$eff.lane <- cell$Lane
  for (i in 1:nrow(cell)) {
    tmp.cell <- cell[i,]
    tmp.pm <- pm.info[which(pm.info$postmile >= tmp.cell$eff_b & pm.info$postmile < tmp.cell$eff_e),]
    tmp.pm[1,]$eff_b <- tmp.cell$eff_b
    tmp.pm[nrow(tmp.pm),]$eff_e <- tmp.cell$eff_e
    tmp.pm$length <- tmp.pm$eff_e - tmp.pm$eff_b
    cell[i,]$eff.lane <- sum(tmp.pm$X.lane * tmp.pm$length) / sum(tmp.pm$length)
  }
  cell[is.na(cell$eff.lane),]$eff.lane = cell[is.na(cell$eff.lane),]$Lane
  
  return(cell)
} # (using this)

vdsAllocation <- function(cell) {
  linkPM <- read_excel(path = paste0(infoDir, "/pems_linkPM.xlsx"),
                       col_names = T)
  fwyPM <- read_excel(path = paste0(infoDir, "/pems_fwyPM.xlsx"),
                      col_names = T)
  linkPM <- data.frame(linkPM)
  linkPM <- linkPM[which(linkPM$Abs.PM>2),]
  fwyPM <- data.frame(fwyPM)
  fwyPM <- fwyPM[which(fwyPM$Abs.PM>2),]
  # linkPM <- linkPM[which(linkPM$Abs.PM < cell[nrow(cell),]$Abs.PM + 0.0006214*cell[nrow(cell),]$cellLength), ]
  linkPM$cellID <- NA
  linkPM[which(grepl("Opposite", linkPM$Type)),]$Type <- substr(linkPM[which(grepl("Opposite", linkPM$Type)),]$Type, 15, 22)
  onPM <- linkPM[which(linkPM$Type == "On Ramp"),]
  offPM <- linkPM[which(linkPM$Type == "Off Ramp"),]
  mainPM <- linkPM[which(linkPM$Type == "Mainline"),]
  # fwyPM <- linkPM[which(linkPM$Type == "Freeway to Freeway"),]
  
  onPM <- filterPM(onPM)
  offPM <- filterPM(offPM)
  mainPM <- filterPM(mainPM)
  
  fwyPM$cellID <- NA
  fwyPM$ramp <- NA
  for (i in 1:nrow(fwyPM)) {
    fwyPM[i,]$cellID = cell[max(which(cell$eff_b <= fwyPM[i,]$Abs.PM)),]$cellID
  }
  
  fwyPM[which(grepl("TO E22", fwyPM$Name)),]$ramp <- "on"
  fwyPM[which(grepl("E22 TO", fwyPM$Name)),]$ramp <- "off"
  # fwyPM[which(fwyPM$ID == 1215109),]$ramp <- "off"
  fwyPM[which(fwyPM$ID == 1215109),]$cellID <- 12
  
  return(list(onPM, offPM, mainPM, fwyPM))
}

##### Ramp Functions ####
calEnterFlow <- function(inputVDSflow, t) {
  inputflowRow = ( (t-1) %/% inputDiv)+1
  q_in = inputVDSflow[inputflowRow,]$AggFlow / inputDiv # #veh/min (#veh/5min * dt) for all lane
  return(q_in)
}

calOutFlow <- function(outFlow, t, wkd.idx) {
  outputrow = ( (t-1) %/% inputDiv)+1
  q_out = outFlow[outputrow,]$Flow / inputDiv # #veh/min (#veh/5min * dt) for all lane
  
  outputflowRow = ( (t-1) %/% (inputDiv*12))+1
  ratio = flow.ratio[outputflowRow,]$ratio
  
  q_out.tot <- q_out * ratio
  return(q_out.tot)
}

calRampFlow <- function(onPM, onFlow, cell, t, i) {
  which.vds <- which(onPM$cellID == cell[i,]$cellID)
  inputflowRow = ( (t-1) %/% inputDiv)+1
  r_on = 0
  if (length(which.vds) > 0) {
    for (j in which.vds) {
      tmp.on.vds <- onFlow[which(onFlow$VDS == onPM[j,]$VDS),]
      r_on = r_on + tmp.on.vds[inputflowRow,]$Flow / inputDiv # #veh/min (#veh/5min * dt) for all lane
    }
  }
  return(r_on)
}

RampFwyProcess <- function(PMlist) {
  onPM <- PMlist[[1]]
  offPM <- PMlist[[2]]
  mainPM <- PMlist[[3]]
  fwyPM <- PMlist[[4]]
  fwyPM.8 <- fwyPM[1,]
  # fwyPM <- fwyPM[-1,]
  
  total.on <- load_ramp(onPM)
  total.off <- load_ramp(offPM)
  total.fwy <- load_fwy(fwyPM)
  
  fwyPM.on <- fwyPM[which(fwyPM$ramp == "on"),]
  fwyPM.off <- fwyPM[which(fwyPM$ramp == "off"),]
  fwyPM.on <- cbind(fwyPM.on[,c(1:6,8)], NA, fwyPM.on[,c(10,11,13)], NA, fwyPM.on[,16])
  fwyPM.off <- cbind(fwyPM.off[,c(1:6,8)], NA, fwyPM.off[,c(10,11,13)], NA, fwyPM.off[,16])
  fwyPM.8 <- cbind(fwyPM.8[,c(1:6,8)], NA, fwyPM.8[,c(10,11,13)], NA, fwyPM.8[,16])
  colnames(fwyPM.on) <- colnames(onPM)
  colnames(fwyPM.off) <- colnames(offPM)
  colnames(fwyPM.8) <- colnames(offPM)
  
  onPM.fwy <- rbind(onPM, fwyPM.on)
  offPM.fwy <- rbind(offPM, fwyPM.off)
  total.fwy.on <- total.fwy[which(total.fwy$type == "on"),]
  total.fwy.off <- total.fwy[which(total.fwy$type == "off"),]
  
  total.on.fwy <- rbind(total.on, total.fwy.on[,c(1:7)])
  total.off.fwy <- rbind(total.off, total.fwy.off[,c(1:7)])
  
  return(list(total.on.fwy, total.off.fwy, total.fwy))
}

#### Etc ####
extractWkd = function(date, wkd.names) {
  date.wkd <- weekdays(as.Date(as.character(date), format = "%Y%m%d"))
  date.wkd[which(date.wkd %in% c("Tuesday", "Wednesday", "Thursday"))] = "twt"
  # wkd.idx <- match(substr(date.wkd, 1, 3), wkd.names)
  return(substr(date.wkd,1,3))
}

subset.time.sp = function(x, his.start, pred.end) {
  x$speed = x$speed[c(his.start:pred.end),]
  return(x)
}

subset.cell = function(x, cell) {
  x$speed = x$speed[,c(cell)]
  return(x)
}

cal.dist = function(x, actual) {
  his = x$speed[1:3]
  act = actual
  dist = sum(abs(his - act))
  return(dist)
}

agg5min = function(x) {
  data = x$speed
  agg.data = as.matrix(aggregate(data, by=list((seq(nrow(data))-1) %/% (5/dt)), FUN=mean)[,-1])
  x$speed = agg.data
  return(x)
}

resultToList <- function(speed.data) {
  date = unique(speed.data$date)
  speed.result = list()
  for (i in 1:length(date)) {
    tmp = speed.data[which(speed.data$date == date[i]),]
    vds = colnames(tmp)[-c(1:3)]
    tmp2 = melt(data = tmp, id.vars = c("date", "time"), measure.vars = vds)
    colnames(tmp2) = c("date", "time", "VDS", "AggSpeed")
    tmp3 = merge(tmp2, info, by="VDS", all = TRUE)
    tmp3$time = format(strptime(tmp3$time, "%H%M"), "%H:%M")
    
    speed.result[[i]] = tmp3
    
  }
  return(speed.result)
  
}
