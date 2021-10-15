library(magick)
library(foreach)
library(data.table)
library(raster)
library(corrplot)

#use magick to quickly extra file attributes####
path<-"~/Downloads/petfinder-pawpularity-score/train/"
trainimgs<-paste0(path,list.files("~/Downloads/petfinder-pawpularity-score/train/"))
x<-foreach(i=1:length(trainimgs)) %do% image_read(trainimgs[i])
xx<-lapply(x,image_info)
xx.dt<-as.data.table(do.call("rbind",xx))
xx.dt$imgid<-list.files("~/Downloads/petfinder-pawpularity-score/train/")
xx.dt$imgid<-gsub(".jpg","",xx.dt$imgid,fixed=TRUE)
xx.dt$imgid<-as.factor(xx.dt$imgid)

#append file attributes to training data & compute addl photo-y metrics (mp)####
train.data<-read.csv("~/Downloads/petfinder-pawpularity-score/train.csv")
train.data.m<-merge(xx.dt,train.data, by.x = "imgid", by.y="Id")
train.data.m$mp<-((train.data.m$width*train.data.m$height)/1000000)
train.data.m$compress<-((train.data.m$filesize/1000000)/train.data.m$mp)
train.data.m.t<-train.data.m[,.(imgid,Pawpularity,filesize,compress,mp,Subject.Focus,
                                Eyes,Face,Near,Action,Accessory,Group,Collage,Human,Occlusion,Info,Blur)]

#create ib vs oob data sets (overall & stratified)####
samples<-round(length(train.data.m$imgid)*0.3)
oobid<-as.integer(sample(rownames(train.data.m.t),samples))
oob<-train.data.m.t[oobid]
ib<-train.data.m.t[!oobid]

thirds<-quantile(train.data.m$Pawpularity,c(0.1,0.9,1))
train.data.m.t[Pawpularity<=thirds[1],paw_rank:=1,]
train.data.m.t[Pawpularity>thirds[1] & Pawpularity<=thirds[2],paw_rank:=2,]
train.data.m.t[Pawpularity>thirds[2] & Pawpularity<=thirds[3],paw_rank:=3,]
train.pawranks<-list(train.data.m.t[paw_rank==1,],train.data.m.t[paw_rank==2,],
     train.data.m.t[paw_rank==3,])

oob.strat<-foreach(i=1:3, .combine = "rbind") %do% {
  samples<-round(length(train.pawranks[[i]]$imgid)*0.3)
  oobid.bypawrank<-as.integer(sample(rownames(train.pawranks[[i]]),samples))
  oob.bypawrank<-train.pawranks[[i]][oobid.bypawrank]
  oob.bypawrank$sampletype<-as.factor("oob")
  ib.bypawrank<-train.pawranks[[i]][-oobid.bypawrank]
  ib.bypawrank$sampletype<-as.factor("ib")
  rbind(oob.bypawrank,ib.bypawrank)
}

#Check correlations (& potential multicollinearity)####
M = cor(ib[,2:17])
testRes = cor.mtest(ib[,2:17], conf.level = 0.95)
corrplot(M, p.mat = testRes$p, method = 'color', diag = FALSE, type = 'upper',
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9, insig = 'label_sig', 
         pch.col = 'grey20', order = 'AOE')
#Run models & compare RMSE(overall & stratified)####
#Overall####
formulas.m0m00m1m2<-c(m00=("Pawpularity~Subject.Focus+Eyes+Face+Near+Action+Accessory+
                        Group+Collage+Human+Occlusion+Info+Blur"),
          m0=("Pawpularity~filesize+compress+mp+Subject.Focus+Eyes+Face+Near+Action+
              Accessory+Group+Collage+Human+Occlusion+Info+Blur"),
          m1=("Pawpularity~filesize+compress+Subject.Focus+Eyes+Face+Near+
                         Action+Accessory+Group+Collage+Human+Occlusion+Info+Blur"),
          m2=("Pawpularity~filesize+compress+Subject.Focus+Face+Near+Action+
                       Accessory+Group+Collage+Human+Info+Blur"))
m0m00m1m2<-foreach(i=1:length(formulas.m0m00m1m2)) %do% lm(formulas.m0m00m1m2[i], data=ib)
step.m0m00m1m2<-foreach(i=1:length(formulas.m0m00m1m2)) %do% stepAIC(m0m00m1m2[[i]], 
                                                                     direction = "both", trace = FALSE)
lapply(step.m0m00m1m2,summary)
lapply(m0m00m1m2,car::vif)
cor(ib[,.(Pawpularity,filesize,compress,mp)])#drop mp

data<-foreach (i=1:3) %do% oob.strat[paw_rank==i & sampletype=="ib",]
ib.l<-data[[1]]
ib.m<-data[[2]]
ib.h<-data[[3]]
ib.lmh<-list(ib.l=ib.l,ib.m=ib.m,ib.h=ib.h)
m3lmh<-list(lm(formulas.m0m00m1m2[3], data=ib.l),lm(formulas.m0m00m1m2[3], data=ib.m),lm(formulas.m0m00m1m2[3], data=ib.h))
step.m3lmh<-foreach (i=1:3) %do% stepAIC(m3lmh[[i]], direction = "both",trace = FALSE)
lapply(step.m3lmh,summary)

#tried cubes & cubes of predictors & binomial regression; doesn't matter much or worse RMSE

#Stratified####
oob.lmh<-foreach(i=1:3) %do% oob.strat[paw_rank==i & sampletype=="oob",]
m00m0m1m2_pred<-foreach(i=1:4) %do% predict(step.m0m00m1m2[[i]],oob)
m3lmh_pred<- foreach(i=1:3) %do% predict(step.m3lmh[[i]],oob.lmh[[i]])

RMSE_m00m0m1m2<-foreach(i=1:4,.combine="c") %do% {mean((oob$Pawpularity - m00m0m1m2_pred[[i]])^2) %>% sqrt()}
names(RMSE_m00m0m1m2)<-c("m00","m0","m1","m2")

RMSE_m3lmh<-foreach(i=1:3,.combine="c") %do% {mean((oob.lmh[[i]]$Pawpularity - m3lmh_pred[[i]])^2) %>% sqrt()}
names(RMSE_m3lmh)<-c("m3l","m3m","m3h")
RMSE_all<-c(RMSE_m00m0m1m2,RMSE_m3lmh)

barplot(RMSE_all-mean(RMSE_all),names.arg=c("Kaggle","Kaggle+us","noVIF","noMC",
                                            "l + noVIF","m + noVIF","h + noVIF"),cex.names=0.5,cex.axis=0.5)

#convert image into raster - only tested on 1 image so far####
test<-as.raster(x[[1]])
dim(test)
rgb.array<-apply(test,1:2,col2rgb)
dim(rgb.array)
test.stack<-stack(raster(rgb.array[1,,]),raster(rgb.array[2,,]),raster(rgb.array[3,,]))
plotRGB(test.stack)

#various ways to possibly detect edges####
train.data.m[1:30]
x[[20]]
image_lat(x[[20]])
image_threshold(x[[20]])
image_edge(x[[20]],radius=2)
image_median(x[[20]],radius=10)
image_quantize(x[[20]],treedepth = 1)
image_level(x[[20]],black_point=100,white_point=0,mid_point=1)