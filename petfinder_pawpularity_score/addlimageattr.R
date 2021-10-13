library(magick)
library(foreach)
library(data.table)
library(raster)

#use magick to quickly extra file attributes
path<-"~/Downloads/petfinder-pawpularity-score/train/"
trainimgs<-paste0(path,list.files("~/Downloads/petfinder-pawpularity-score/train/"))
x<-foreach(i=1:length(trainimgs)) %do% image_read(trainimgs[i])
xx<-lapply(x,image_info)
xx.dt<-as.data.table(do.call("rbind",xx))
xx.dt$imgid<-list.files("~/Downloads/petfinder-pawpularity-score/train/")
xx.dt$imgid<-gsub(".jpg","",xx.dt$imgid,fixed=TRUE)
xx.dt$imgid<-as.factor(xx.dt$imgid)

#append file attributes to training data & compute addl photo-y metrics (mp)
train.data<-read.csv("~/Google Drive/My Drive/PhD/Other/ecoforecasting/prizes/kaggle/data/train.csv")
train.data.m<-merge(xx.dt,train.data, by.x = "imgid", by.y="Id")
train.data.m$mp<-((train.data.m$width*train.data.m$height)/1000000)
train.data.m$compress<-((train.data.m$filesize/1000000)/train.data.m$mp)

m0<-lm(train.data.m$Pawpularity~train.data.m$filesize)
summary(m0)

m1<-lm(train.data.m$Pawpularity~train.data.m$mp)
summary(m1)

m2<-lm(train.data.m$Pawpularity~train.data.m$compress)
summary(m2)


#convert image into raster - only tested on 1 image so far
test<-as.raster(x[[1]])
dim(test)
rgb.array<-apply(test,1:2,col2rgb)
dim(rgb.array)
test.stack<-stack(raster(rgb.array[1,,]),raster(rgb.array[2,,]),raster(rgb.array[3,,]))
plotRGB(test.stack)

#various ways to possibly detect edges
train.data.m[1:30]
x[[20]]
image_lat(x[[20]])
image_threshold(x[[20]])
image_edge(x[[20]],radius=2)
image_median(x[[20]],radius=10)
image_quantize(x[[20]],treedepth = 1)
image_level(x[[20]],black_point=100,white_point=0,mid_point=1)


