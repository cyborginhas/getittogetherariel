library(magick)
library(foreach)
library(data.table)
library(raster)
library(corrplot)
library(MASS)
library(sf) # to call st_* functions

#use magick to quickly extra file attributes####
path<-"~/Desktop/github/getittogetherariel/petfinder-pawpularity-score/train/"
trainimgs<-paste0(path,list.files(path))
x<-foreach(i=1:length(trainimgs)) %do% magick::image_read(trainimgs[i])
xx<-lapply(x,image_info)
xx.dt<-as.data.table(do.call("rbind",xx))
xx.dt$imgid<-list.files(path)[1:length(trainimgs)]
xx.dt$imgid<-gsub(".jpg","",xx.dt$imgid,fixed=TRUE)
xx.dt$imgid<-as.factor(xx.dt$imgid)


#append file attributes to training data & compute addl photo-y metrics (mp)####
train.data<-read.csv("~/Google Drive/My Drive/PhD/organize/Other/ecoforecasting/prizes/kaggle/petfinder_pawpularityscore/data/train.csv")
train.data.m<-merge(xx.dt,train.data, by.x = "imgid", by.y="Id")
train.data.m$mp<-((train.data.m$width*train.data.m$height)/1000000)
train.data.m$compress<-((train.data.m$filesize/1000000)/train.data.m$mp)
train.data.m.t<-train.data.m[,.(imgid,Pawpularity,filesize,compress,mp,Subject.Focus,
                                Eyes,Face,Near,Action,Accessory,Group,Collage,Human,Occlusion,Info,Blur)]

#create ib vs oob data sets (overall & stratified)####
catdog<-read.csv("~/Google Drive/My Drive/PhD/organize/Other/ecoforecasting/prizes/kaggle/petfinder_pawpularityscore/data/cat_or_dog_classified.csv")
catdog.m<-as.data.table(merge(catdog,train.data.m.t, by.x="Id", by.y="imgid"))
write.csv(catdog.m,"~/Desktop/meowmeow.csv")
catdog.m<-as.data.table(read.csv("~/Desktop/meowmeow.csv"))
cat<-catdog.m[cat_or_dog=="cat",]
dog<-catdog.m[cat_or_dog!="cat",]
names(cat)
cat<-cat[,-c(1,3,4)]
dog<-dog[,-c(1,3,4)]

#ib/oob cats
cat.samples<-round(length(cat$Id)*0.15)
oobid.cat<-as.integer(sample(rownames(cat),cat.samples))
oob.cat<-cat[oobid.cat]
ib.cat<-cat[!oobid.cat]
hist(cat$Pawpularity)

#ib/oob dogs
dog.samples<-round(length(dog$Id)*0.15)
oobid.dog<-as.integer(sample(rownames(dog),dog.samples))
oob.dog<-dog[oobid.dog]
ib.dog<-dog[!oobid.dog]
hist(dog$Pawpularity)

#Check correlations (& potential multicollinearity)####
#cats
M = cor(ib.cat[,2:17])
testRes = cor.mtest(ib.cat[,2:17], conf.level = 0.95)
corrplot(M, p.mat = testRes$p, method = 'color', diag = FALSE, type = 'upper',
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9, insig = 'label_sig', 
         pch.col = 'grey20', order = 'AOE',title = "Meow Meows",mar = c(0,0,1,0))


#Dogs
M = cor(ib.dog[,2:17])
testRes = cor.mtest(ib.dog[,2:17], conf.level = 0.95)
corrplot(M, p.mat = testRes$p, method = 'color', diag = FALSE, type = 'upper',
         sig.level = c(0.001, 0.01, 0.05), pch.cex = 0.9, insig = 'label_sig', 
         pch.col = 'grey20', order = 'AOE',title = "Peppies",mar = c(0,0,1,0))


#Run models & compare RMSE(overall & stratified)####
#Overall####
#cats
names(cat)
formulas.m0m00m1m2<-c(m00=("Pawpularity~Subject.Focus+Eyes+Face+Near+Action+Accessory+
                        Group+Collage+Human+Occlusion+Info+Blur"),
          m0=("Pawpularity~filesize+compress+mp+Subject.Focus+Eyes+Face+Near+Action+
              Accessory+Group+Collage+Human+Occlusion+Info+Blur"),
          m1=("Pawpularity~filesize+compress+Subject.Focus+Eyes+Face+Near+Action+
              Accessory+Group+Collage+Human+Occlusion+Info+Blur"))
m0m00m1m2.cat<-foreach(i=1:length(formulas.m0m00m1m2)) %do% lm(formulas.m0m00m1m2[i], data=ib.cat)

lapply(m0m00m1m2.cat,summary)

lapply(m0m00m1m2.cat,car::vif)


m00m0m1m2_pred.cat<-foreach(i=1:3) %do% predict(m0m00m1m2.cat[[i]],oob.cat)

RMSE_m00m0m1m2.cat<-foreach(i=1:3,.combine="c") %do% {mean((oob.cat$Pawpularity - m00m0m1m2_pred.cat[[i]])^2) %>% sqrt()}
names(RMSE_m00m0m1m2.cat)<-c("m00","m0","m1")


barplot(RMSE_m00m0m1m2.cat-mean(RMSE_m00m0m1m2.cat),names.arg=c("Kaggle(19.567)","Kaggle+(19.561)","no VIF(19.562)"),cex.names=0.5,cex.axis=0.5,main="Meow Meows")

#Dogs
names(dog)
m0m00m1m2.dog<-foreach(i=1:length(formulas.m0m00m1m2)) %do% lm(formulas.m0m00m1m2[i], data=ib.dog)
lapply(m0m00m1m2.dog,car::vif)
m00m0m1m2_pred.dog<-foreach(i=1:3) %do% predict(m0m00m1m2.dog[[i]],oob.dog)
RMSE_m00m0m1m2.dog<-foreach(i=1:3,.combine="c") %do% {mean((oob.dog$Pawpularity - m00m0m1m2_pred.dog[[i]])^2) %>% sqrt()}
names(RMSE_m00m0m1m2.dog)<-c("m00","m0","m1")
barplot(RMSE_m00m0m1m2.dog-mean(RMSE_m00m0m1m2.dog),names.arg=c("Kaggle(20.761)","Kaggle+(20.676)","no VIF(20.689)"),cex.names=0.5,cex.axis=0.5,main="Peppies")

