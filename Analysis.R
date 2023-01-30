


# Requirements ------------------------------------------------------------

source("GlobalParameters.R")
source("Functions.R")



# Main --------------------------------------------------------------------

#### preparation for load actual data ####

dataDir <- "C:/Users/Yeeun/Dropbox/STSlab/0.나도참여하는진행중과제/연구재단중견/2020/Data/Data"
speedFiles <- list.files(path = paste0(dataDir, "/speed"), pattern = "201909|201908.+speed.xlsx")
speedFiles = speedFiles[order(speedFiles)]
speedFiles = speedFiles[-c(17)]

real.speed.tot = list()
diff <- as.difftime("00:05", format = "%H:%M")
for (d in 1:length(speedFiles)) {
  target.file <- speedFiles[d]
  speed <- read_xlsx(paste0(dataDir, "/speed/", target.file))
  speed <- data.frame(speed)
  colnames(speed) <- c("time", "Abs.PM", "CA.PM", "VDS", "AggSpeed", "Lane", "Observed")
  speed$time <- format(strptime(speed$time, "%H:%M"), "%H:%M")
  speed = speed[which(speed$time > "10:00" & speed$time <= "20:00"),]
  speed = speed[which(speed$VDS %in% vds.id),]
  
  real.speed.tot[[d]] = speed
}

saveRDS(real.speed.tot, "real.speed.tot.rds")


#### preparation for load mknn results ####

mknn.tt = readRDS(paste0(getwd(), "/knnTT.rds"))
mknn.result.path = "C:/Users/Yeeun/Dropbox/STSlab/0.나도참여하는진행중과제/연구재단중견/2020/Data/data-driven prediction/out/prediction"

mknn.result.file.lists = list.files(path = mknn.result.path, pattern = ".rdata", full.names = TRUE)

mknn.speed.tot = list()
for (i in 1:length(date)) {
  
  target.date.lists = mknn.result.file.lists[grep(date[i], mknn.result.file.lists)]
  tmp1 = lapply(target.date.lists, function(x) get(load(x))[c(1:3),])
  tmp = do.call("rbind", tmp1)
  tmp2 = melt(data = tmp[,-2], id.vars = c("pdate", "ftime"))
  # tmp2 = melt(data = tmp, id.vars = c("pdate", "ftime"), measure.vars = pm.vds$VDS)
  colnames(tmp2) = c("date", "time", "VDS", "AggSpeed")
  tmp3 = merge(tmp2, info, by="VDS", all = TRUE)
  
  tmp3$time = format(strptime(tmp3$time, "%H%M"), "%H:%M")
  tmp3 = tmp3[which(tmp3$time > "10:00" & tmp3$time <= "20:00"),]
  tmp3$AggSpeed = as.numeric(tmp3$AggSpeed)
  
  mknn.speed.tot[[i]] = tmp3
}

saveRDS(mknn.speed.tot, "mknn.speed.tot.rds")


#### Load Actual and MkNN data ####
real.speed.tot = readRDS("real.speed.tot.rds")
mknn.speed.tot = readRDS("mknn.speed.tot.rds")

info = unique(real.speed.tot[[1]][,c("Abs.PM", "VDS")])

#### load SMURP results ####

qt=80
# wkd = "fri"
newWd = paste0("C:/Users/Yeeun/Dropbox/STSlab/0.나도참여하는진행중과제/연구재단중견/2020/temp/8. reviseModule3/DATA/revise3_result/SMURP_G/",qt,"Q/")

speed.15int = c()
mape = c()
for (wkd in wkd.names) {
  tmp.speed.15int = readRDS((paste0(newWd, "Agg.knn.speed.15int.result_Grev3_",qt,"Q_",tolower(wkd),".rds")))
  speed.15int = rbind(speed.15int, tmp.speed.15int)
  
  tmp.mape = readRDS(paste0(newWd, "Agg.mape.result_Grev3_",qt,"Q_",tolower(wkd),".rds"))
  tmp.mape = tmp.mape[,c(1:8)]
  mape = rbind(mape, tmp.mape)
}

mape = mape[order(mape$date),]
speed.15int = speed.15int[order(speed.15int$date),]
speed.15int.list = resultToList(speed.15int)

date = unique(mape$date)

for (i in 1:length(unique(mape$date))) {
  
  # show.date = which(date==check.date[i])
  
  speed.actual = real.speed.tot[[i]]
  speed.mknn = mknn.speed.tot[[i]]
  speed.smurp =speed.15int.list[[i]]
  
  p1 = ggplot(data = speed.actual, aes(x = time, y = as.factor(Abs.PM), fill = AggSpeed)) + 
    geom_tile() + scale_fill_gradient(limits = c(0,80)) +
    geom_hline(yintercept = c(6.5, 14.5, 20.5, 25.5))
  p2 = ggplot(data = speed.mknn, aes(x = time, y = as.factor(Abs.PM), fill = AggSpeed)) + 
    geom_tile() + scale_fill_gradient(limits = c(0,80)) +
    geom_hline(yintercept = c(6.5, 14.5, 20.5, 25.5))
  p3 = ggplot(data = speed.smurp, aes(x = time, y = as.factor(Abs.PM), fill = AggSpeed)) + 
    geom_tile() + scale_fill_gradient(limits = c(0,80)) +
    geom_hline(yintercept = c(6.5, 14.5, 20.5, 25.5))
  
  speed.error = merge(speed.actual, speed.mknn, by = c("VDS", "time", "Abs.PM"))
  speed.error$error = speed.error$AggSpeed.y - speed.error$AggSpeed.x
  p2.e = ggplot(data = speed.error, aes(x = time, y = as.factor(Abs.PM), fill = error)) + 
    geom_tile() + scale_fill_gradient2(limits = c(-50,50)) +
    geom_hline(yintercept = c(6.5, 14.5, 20.5, 25.5))
  
  speed.error = merge(speed.actual, speed.smurp, by = c("VDS", "time", "Abs.PM"))
  speed.error$error = speed.error$AggSpeed.y - speed.error$AggSpeed.x
  p3.e = ggplot(data = speed.error, aes(x = time, y = as.factor(Abs.PM), fill = error)) + 
    geom_tile() + scale_fill_gradient2(limits = c(-50,50)) +
    geom_hline(yintercept = c(6.5, 14.5, 20.5, 25.5))
  
  png(paste0(getwd(),"/compare_new/test",qt, "_", date[i],".png"), width = 1600, height = 1000)
  print(ggarrange(p2, p2.e, p3, p3.e, p1, label.y = 4, ncol = 2, nrow = 3,
                  labels = c("actual", "mknn", "mknn.error", "smurp", "smurp.error")
  ))
  
  dev.off()
  
}

# speed.smurp2 =speed.15int.75Q_pro2.list[[show.date]]
# speed.smurp.lw =speed.15int.75Q_lw.list[[show.date]]
# speed.smurp.lw = speed.smurp.lw[which(speed.smurp.lw$time %in% speed.mknn$time),]
# speed.smurp.c3 =speed.15int.75Q_c3.list[[show.date]]
# speed.smurp.c3 = speed.smurp.c3[which(speed.smurp.c3$time %in% speed.mknn$time),]
# speed.smurp.cAll =speed.15int.75Q_cAll.list[[show.date]]
# speed.smurp.cAll = speed.smurp.cAll[which(speed.smurp.cAll$time %in% speed.mknn$time),]

# speed.15int.75Q_pro1 = readRDS((paste0(getwd(), "/knn.rev.Result/Model1_ITSC/knn.rev.speed.",qt,"Q_Pro1.rds")))
# speed.15int.75Q_pro1.list = resultToList(speed.15int.75Q_pro1)
# speed.15int.75Q_pro2 = readRDS((paste0(getwd(), "/knn.rev.Result/Model1_ITSC/knn.rev.speed.",qt,"Q_Pro2.rds")))
# speed.15int.75Q_pro2.list = resultToList(speed.15int.75Q_pro2)
# speed.15int.75Q_lw = readRDS((paste0(getwd(), "/knn.rev.Result/Model3/Agg.knn.rev.speed.",qt,"Q_weightA.rds")))
# speed.15int.75Q_lw = readRDS((paste0("/Users/yeeunkim/Dropbox/STSlab/0.나도참여하는진행중과제/연구재단중견/2020/temp/7. WeightDecision/1. Modelresult/Agg.knn.rev.speed.",qt,"Q_W.rds")))
# speed.15int.75Q_lw.list = resultToList(speed.15int.75Q_lw)
# speed.15int.75Q_c3 = readRDS((paste0("/Users/yeeunkim/Dropbox/STSlab/0.나도참여하는진행중과제/연구재단중견/2020/temp/7. WeightDecision/1. Modelresult/Agg.knn.rev.speed.",qt,"Q_W_cluster3.rds")))
# speed.15int.75Q_c3.list = resultToList(speed.15int.75Q_c3)

# mape1 = readRDS(paste0(getwd(), "/mape.tot.Result/Model1_ITSC/mape.result_",qt,"Q_Pro1.rds"))
# mape2 = readRDS(paste0(getwd(), "/mape.tot.Result/Model1_ITSC/mape.result_",qt,"Q_Pro2.rds"))
# mape3 = readRDS(paste0(getwd(), "/mape.tot.Result/Model2/Agg.mape.result.",qt,"Q_weight.rds"))
# mape3 = readRDS(paste0(newWd, "Agg.mape.result_Grev3_",qt,"Q_",wkd,".rds"))
# mape4 = readRDS(paste0("/Users/yeeunkim/Dropbox/STSlab/0.나도참여하는진행중과제/연구재단중견/2020/temp/7. WeightDecision/1. Modelresult/Agg.mape.result.",qt,"Q_W_cluster3.rds"))
# mape5 = readRDS(paste0("/Users/yeeunkim/Dropbox/STSlab/0.나도참여하는진행중과제/연구재단중견/2020/temp/7. WeightDecision/1. Modelresult/Agg.mape.result.",qt,"Q_W_clusterAll_k8.rds"))
# mape3 = mape3[,c(1:6,9,7,8)]


#### compare proposed 1 and proposed 2 ####
mape = merge(mape1, mape2, by = colnames(mape1)[c(1:5,7)], all=TRUE)
colnames(mape)[7:10] = c("smurp_L", "mape.smurp_L", "smurp_G", "mape.smurp_G")
mape = merge(mape, mape3, by = colnames(mape)[c(1:6)], all.x = TRUE)
colnames(mape)[which(colnames(mape) %in% c("knn.rev", "mape.knn.rev"))] = c("smurp_Gw", "mape.smurp_Gw")
mape = merge(mape, mape4[,c(1:8)], by = colnames(mape)[c(1:6)], all.x = TRUE)
colnames(mape)[which(colnames(mape) %in% c("knn.rev", "mape.knn.rev"))] = c("smurp_Gwc3", "mape.smurp_Gwc3")
mape = merge(mape, mape5, by = colnames(mape)[c(1:6)], all.x = TRUE)
colnames(mape)[which(colnames(mape) %in% c("knn.rev", "mape.knn.rev"))] = c("smurp_GwcAll", "mape.smurp_GwcAll")
mape$date = as.numeric(mape$date)
mape$ptime = as.numeric(mape$ptime)
mape$time = as.numeric(mape$time)
mape.agg = aggregate(mape, list(mape$date), mean)

## find the date when SMURP has low accuracy than MKNN and load the data##
check.date = mape.agg[which(mape.agg$mape.smurp_L < mape.agg$mape.knn &
                              mape.agg$mape.smurp_G < mape.agg$mape.knn &
                              mape.agg$mape.smurp_Gw < pmin(mape.agg$mape.smurp_L, mape.agg$mape.smurp_G) &
                              mape.agg$mape.smurp_Gwc3 < mape.agg$mape.smurp_Gw),]$date
# & mape.agg$mape.smurp_GwcAll < mape.agg$mape.smurp_Gw),]$date
check.date = mape.agg[which(mape.agg$mape.smurp_Gwc3 < 
                              pmin(mape.agg$mape.knn, mape.agg$mape.smurp_G,
                                   mape.agg$mape.smurp_Gw, mape.agg$mape.smurp_L)),]$date
# check.date = c("20190808")
# nice case: 20190927

for (i in 1:length(unique(mape$date))) {
  
  # show.date = which(date==check.date[i])
  show.date = which(date==check.date[i])
  
  speed.actual = real.speed.tot[[show.date]]
  speed.mknn = mknn.speed.tot[[show.date]]
  speed.smurp =speed.15int.list[[show.date]]
  # speed.smurp2 =speed.15int.75Q_pro2.list[[show.date]]
  # speed.smurp.lw =speed.15int.75Q_lw.list[[show.date]]
  # speed.smurp.lw = speed.smurp.lw[which(speed.smurp.lw$time %in% speed.mknn$time),]
  # speed.smurp.c3 =speed.15int.75Q_c3.list[[show.date]]
  # speed.smurp.c3 = speed.smurp.c3[which(speed.smurp.c3$time %in% speed.mknn$time),]
  # speed.smurp.cAll =speed.15int.75Q_cAll.list[[show.date]]
  # speed.smurp.cAll = speed.smurp.cAll[which(speed.smurp.cAll$time %in% speed.mknn$time),]
  
  p1 = ggplot(data = speed.actual, aes(x = time, y = as.factor(Abs.PM), fill = AggSpeed)) + 
    geom_tile() + scale_fill_gradient(limits = c(0,80)) +
    geom_hline(yintercept = c(6.5, 14.5, 20.5, 25.5))
  p2 = ggplot(data = speed.mknn, aes(x = time, y = as.factor(Abs.PM), fill = AggSpeed)) + 
    geom_tile() + scale_fill_gradient(limits = c(0,80)) +
    geom_hline(yintercept = c(6.5, 14.5, 20.5, 25.5))
  p3 = ggplot(data = speed.smurp1, aes(x = time, y = as.factor(Abs.PM), fill = AggSpeed)) + 
    geom_tile() + scale_fill_gradient(limits = c(0,80)) +
    geom_hline(yintercept = c(6.5, 14.5, 20.5, 25.5))
  p4 = ggplot(data = speed.smurp2, aes(x = time, y = as.factor(Abs.PM), fill = AggSpeed)) + 
    geom_tile() + scale_fill_gradient(limits = c(0,80)) +
    geom_hline(yintercept = c(6.5, 14.5, 20.5, 25.5))
  p5 = ggplot(data = speed.smurp.lw, aes(x = time, y = as.factor(Abs.PM), fill = AggSpeed)) + 
    geom_tile() + scale_fill_gradient(limits = c(0,80)) +
    geom_hline(yintercept = c(6.5, 14.5, 20.5, 25.5))
  p6 = ggplot(data = speed.smurp.c3, aes(x = time, y = as.factor(Abs.PM), fill = AggSpeed)) + 
    geom_tile() + scale_fill_gradient(limits = c(0,80)) +
    geom_hline(yintercept = c(6.5, 14.5, 20.5, 25.5))
  # p7 = ggplot(data = speed.smurp.cAll, aes(x = time, y = as.factor(Abs.PM), fill = AggSpeed)) + geom_tile() + scale_fill_gradient(limits = c(0,80))
  
  p8 = ggplot(data = speed.actual, aes(x = time, y = as.factor(Abs.PM), fill = AggSpeed)) + geom_tile() + scale_fill_gradient(limits = c(0,80))
  speed.error = merge(speed.actual, speed.mknn, by = c("VDS", "time", "Abs.PM"))
  speed.error$error = speed.error$AggSpeed.y - speed.error$AggSpeed.x
  p9 = ggplot(data = speed.error, aes(x = time, y = as.factor(Abs.PM), fill = error)) + geom_tile() + scale_fill_gradient2(limits = c(-50,50)) +
    geom_hline(yintercept = c(6.5, 14.5, 20.5, 25.5))
  speed.error = merge(speed.actual, speed.smurp1, by = c("VDS", "time", "Abs.PM"))
  speed.error$error = speed.error$AggSpeed.y - speed.error$AggSpeed.x
  p10 = ggplot(data = speed.error, aes(x = time, y = as.factor(Abs.PM), fill = error)) + geom_tile() + scale_fill_gradient2(limits = c(-50,50)) +
    geom_hline(yintercept = c(6.5, 14.5, 20.5, 25.5))
  speed.error = merge(speed.actual, speed.smurp2, by = c("VDS", "time", "Abs.PM"))
  speed.error$error = speed.error$AggSpeed.y - speed.error$AggSpeed.x
  p11 = ggplot(data = speed.error, aes(x = time, y = as.factor(Abs.PM), fill = error)) + geom_tile() + scale_fill_gradient2(limits = c(-50,50)) +
    geom_hline(yintercept = c(6.5, 14.5, 20.5, 25.5))
  speed.error = merge(speed.actual, speed.smurp.lw, by = c("VDS", "time", "Abs.PM"))
  speed.error$error = speed.error$AggSpeed.y - speed.error$AggSpeed.x
  p12 = ggplot(data = speed.error, aes(x = time, y = as.factor(Abs.PM), fill = error)) + geom_tile() + scale_fill_gradient2(limits = c(-50,50))+
    geom_hline(yintercept = c(6.5, 14.5, 20.5, 25.5))
  speed.error = merge(speed.actual, speed.smurp.c3, by = c("VDS", "time", "Abs.PM"))
  speed.error$error = speed.error$AggSpeed.y - speed.error$AggSpeed.x
  p13 = ggplot(data = speed.error, aes(x = time, y = as.factor(Abs.PM), fill = error)) + geom_tile() + scale_fill_gradient2(limits = c(-50,50))+
    geom_hline(yintercept = c(6.5, 14.5, 20.5, 25.5))
  speed.error = merge(speed.actual, speed.smurp.cAll, by = c("VDS", "time", "Abs.PM"))
  speed.error$error = speed.error$AggSpeed.y - speed.error$AggSpeed.x
  # p14 = ggplot(data = speed.error, aes(x = time, y = as.factor(Abs.PM), fill = error)) + geom_tile() + scale_fill_gradient2(limits = c(-50,50))+
  # geom_hline(yintercept = c(6.5, 14.5, 20.5, 25.5))
  
  
  
  # png(paste0(getwd(),"/comparePro1Pro2/",qt,"better",check.date[i],".png"), width = 1600, height = 1350)
  png(paste0(getwd(),"/compareG_L_Lw/test",qt,"better",check.date[i],".png"), width = 1600, height = 1500)
  # print(ggarrange(p1, p2, p3, p4, p5, labels = c("actual", "mknn", "smurp-g", "smurp-l", "smurp-lw"), label.y = 3, ncol = 2, nrow = 3))
  print(ggarrange(p1, p2, p3, p4, p5, p6, label.y = 4, ncol = 2, nrow = 3,
                  labels = c("actual", "mknn", "smurp-g", "smurp-l", "smurp-lw", "smurp-lwc3")
  ))
  
  dev.off()
  
  png(paste0(getwd(),"/compareG_L_Lw/test",qt,"better",check.date[i],"error.png"), width = 1600, height = 1500)
  # print(ggarrange(p1, p2, p3, p4, p5, labels = c("actual", "mknn", "smurp-g", "smurp-l", "smurp-lw"), label.y = 3, ncol = 2, nrow = 3))
  print(ggarrange(p8, p9, p10, p11, p12, p13, label.y = 4, ncol = 2, nrow = 3,
                  labels = c("actual", "mknn", "smurp-g", "smurp-l", "smurp-lw", "smurp-lwc3")
  ))
  
  dev.off()
}

## plot vds speed ##
for (i in 1:length(check.date)) {
  
  show.date = which(date==check.date[i])
  # show.data = which(date=="20190927")
  
  speed.actual = real.speed.tot[[show.date]]
  speed.mknn = mknn.speed.tot[[show.date]]
  speed.smurp1 =speed.15int.75Q_pro1.list[[show.date]]
  speed.smurp2 =speed.15int.75Q_pro2.list[[show.date]]
  speed.smurp.lw =speed.15int.75Q_lw.list[[show.date]]
  speed.smurp.lw = speed.smurp.lw[which(speed.smurp.lw$time %in% speed.mknn$time),]
  speed.smurp.c3 =speed.15int.75Q_c3.list[[show.date]]
  speed.smurp.c3 = speed.smurp.c3[which(speed.smurp.c3$time %in% speed.mknn$time),]
  speed.smurp.cAll =speed.15int.75Q_lw.cAll[[show.date]]
  speed.smurp.cAll = speed.smurp.cAll[which(speed.smurp.cAll$time %in% speed.mknn$time),]
  
  
  # pdf(paste0(getwd(),"/comparePro1Pro2/",qt,"better_vds",check.date[i],".pdf"), width = 16, height = 9)
  pdf(paste0(getwd(),"/compareG_L_Lw/NEW",qt,"better_vds",check.date[i],".pdf"), width = 16, height = 9)
  vds.id = unique(speed.actual$VDS)
  for (j in 1:length(vds.id)) {
    vds = vds.id[j]
    plot(speed.actual[which(speed.actual$VDS == vds),]$AggSpeed, type = 'l', ylim = c(0,80))
    grid(90,10)
    lines(speed.mknn[which(speed.mknn$VDS == vds),]$AggSpeed, col = 'blue')
    lines(speed.smurp1[which(speed.smurp1$VDS == vds),]$AggSpeed, col = 'red')
    lines(speed.smurp2[which(speed.smurp2$VDS == vds),]$AggSpeed, col = 'orange')
    lines(speed.smurp.lw[which(speed.smurp.lw$VDS == vds),]$AggSpeed, col = 'green')
    lines(speed.smurp.c3[which(speed.smurp.lc3$VDS == vds),]$AggSpeed, col = 'green')
    lines(speed.smurp.cAll[which(speed.smurp.cAll$VDS == vds),]$AggSpeed, col = 'green')
    
  }
  
  dev.off()
}


tot.real = do.call("rbind", real.speed.tot)
vds.speed.var = aggregate(tot.real$AggSpeed, list(tot.real$VDS), FUN = var)


