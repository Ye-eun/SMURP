
# Global parameters -------------------------------------------------------
#### Directories ####

BaseDir = "C:/Users/Yeeun/Dropbox/STSlab/0.나도참여하는진행중과제/연구재단중견/2020"
dataDir = paste0(BaseDir, "/Data/Data")
infoDir = paste0(BaseDir, "/SMURP/Info")
incidentDir = paste0(dataDir, "/incident")
incidentDBDir = paste0(BaseDir, "/SMURP/incidentDB/DB")

#### Simulation parameters ####
dt = 0.5 # unit: min --> 30sec
simulationstep = 60*12 / dt # 10:00-22:00
inputDiv <- 5/dt
initialTime <- "10:00" ## Simulation start time
initialTime <- strptime(initialTime, "%H:%M")
startTime <- "10:00" ## Simulation record start time
startTime <- strptime(startTime, "%H:%M")
startStep <- as.numeric(difftime(startTime, initialTime, units = "mins")) + 1
endStep <- startStep + simulationstep - 1
endTime <- "22:00"
wholeStep <- 60*14 / dt
diff <- as.difftime("00:05", format = "%H:%M")

#### Etc ####
wkd.names = c("Sun", "Sat", "Mon", "Fir", "twt")

wkd.list <- list()
wkd.list[[1]] <- c("Sunday")
wkd.list[[2]] <- c("Saturday")
wkd.list[[3]] <- c("Monday")
wkd.list[[4]] <- c("Friday")
wkd.list[[5]] <- c("Tuesday", "Wednesday", "Thursday")

#### PM value ####
## Start PM for db generation
start.PM = 2.537

## Start and End PM of target region ####
start.pm <- 4.62 # 7.087
end.pm <- 14.229
