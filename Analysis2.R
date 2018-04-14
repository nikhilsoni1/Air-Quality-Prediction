# header----
.libPaths( c( .libPaths(), "/home/sonin/Rlibs") )
save(list=ls(all=T),file='Analysis2.RData')
resp<-"PM2.5"
wd<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)
load(paste0(wd, '/Analysis2.Rdata'))
rm(wd)

install.packages('caret')
install.packages('gam')
install.packages('rJava')
install.packages('bartMachine')
install.packages('ModelMetrics')
install.packages('stats')
library(ggplot2)
library(caret)
library(gam)
library(rJava)
library(bartMachine)
library(ModelMetrics)
library(stats)
library(corrplot)
library(Hmisc)
library(gam)
# functions----

completeness<-function(dat)
{
  dat<-as.data.frame(sapply(dat,as.character))
  dat[dat=="."]<-NA
  a=nrow(dat)
  b=length(dat)
  c=sum(is.na(dat))/(a*b)
  return(round(100*(1-c),digits=2))
}

crossValidate<-function(cvtype,folds,dataset,model,resp)
{
  df<-dataset
  l <- vector("list", 2)
  if (cvtype=="kfold")
  {
    df$knum<-sample(1:folds,nrow(df),replace = TRUE)
    rmse_kfold<-0
    for (i in 1:folds)
    {
      df.test<-df[df$knum==i,]
      df.train<-df[!df$knum==i,]
      pred<-predict(model,df.test)
      pred[is.na(pred)]<-mean(pred,na.rm = T)
      rmse_kfold<-cbind(rmse_kfold,rmse(df.test[,resp],pred))
    }
    l[[1]]<-rmse_kfold[,-1]
    l[[2]]<-mean(rmse_kfold[,-1])
    return (l)
  }
  else if (cvtype=="LOOCV"||cvtype=="loocv")
  {
    rmse_loocv<-0
    for (i in 1:nrow(df))
    {
      df.test<-df[i,]
      df.train<-df[-i,]
      pred<-predict(model,df.test)
      pred[is.na(pred)]<-mean(df.train[,resp])
      rmse_loocv<-cbind(rmse_loocv,rmse(df.test[,resp],pred))
    }
    l[[1]]<-rmse_loocv[,-1]
    l[[2]]<-mean(rmse_loocv[,-1])
    return(l)
  }
}

# data----

# AOT Data and processing (1)
df.aot<-read.csv('/home/sonin/Harmanik/AOT.csv')
df.aot$Date<-as.character(df.aot$Date)
df.aot$Date<-as.Date(df.aot$Date, format='%m/%d/%Y')

# Environmental variables data and processing  (2)
df.init<-read.csv('/home/sonin/Harmanik/EnvVar.csv')
df.store<-df.init[,-2]
colnames(df.store)[1]<-'Date'
colnames(df.store)[2]<-'Station'
df.store$Date<-as.character(df.store$Date)
df.store$Date<-as.Date(df.store$Date, format='%d-%m-%Y')
summary(is.na(df.store$Date))
df<-merge(df.store, df.aot, by=c('Date', 'Station'), all.x = TRUE)  # (3) -> Merging (1) & (2)
df<-df[,-c(9, 11, 12, 13, 14, 15, 16, 18, 19, 20)]

# Additional temperature data and processing  (4)
df.temp<-read.csv('//home/sonin/Harmanik/TemperatureData.csv')
df.temp$Date<-as.character(df.temp$Date)
df.temp$Date<-as.Date(df.temp$Date, format='%m/%d/%y')
df<-merge(df, df.temp, by='Date', all.x=T)  # (5) -> Merging (3) & (4)

df$GC<-0
df[which(df$Station=='AV'),'GC']<-3
df[which(df$Station=='IHBAS'),'GC']<-0
df[which(df$Station=='ITO'),'GC']<-3
df[which(df$Station=='MM'),'GC']<-3
df[which(df$Station=='NSIT'),'GC']<-1
df[which(df$Station=='PUSA'),'GC']<-4
df[which(df$Station=='RKP'),'GC']<-4
df[which(df$Station=='LODHI'),'GC']<-3
df[which(df$Station=='CRRI'),'GC']<-2
df[which(df$Station=='DTU'),'GC']<-1
df[which(df$Station=='SIRI'),'GC']<-2
df[which(df$Station=='AYA'),'GC']<-2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
df[which(df$Station=='BC'),'GC']<-3
df[which(df$Station=='EAN'),'GC']<-1
df[which(df$Station=='IGI'),'GC']<-1
df[which(df$Station=='NC'),'GC']<-4
df[which(df$Station=='PB'),'GC']<-1
df[which(df$Station=='SHAD'),'GC']<-4
df$GC<-as.factor(df$GC)
rm(df.aot,df.init,df.store,df.temp)
summary(df)
#df$GC<-factor(df$GC, labels = c("Non Forest","Open Forest","Moderate Forest","Dense Forest"))


# data cleaning intense----
##PM 2.5

df[which(df$PM2.5<0),'PM2.5']<- NA
summary(df$PM2.5)

#Looks ok. PM 2.5 of 1000 was recorded in Delhi. PM2.5 = 3897.31 can be a outlier.
#Cleaned bitch!

##AT
summary(df$AT)
#Since here we get lowest value of AT as -66648.01, which is obviously nonsense. So, we need to delete the large negative values and large positive values.
df[order(df$AT),'AT']
df[which(df$AT<0),'AT']<- NA

df[order(-df$AT),'AT']
#Here, 631.98 is obviously nonsense value which we remove from the dataset.
df[which(df$AT==631.98),'AT']<- NA


##RH
summary(df$RH)
df[order(df$RH),'RH']
df[order(-df$RH),'RH']
#RH should be between 20%-80%(historical). It cannot be 0 as well as greater than 100%.

df[which(df$RH<20 | df$RH>80),'RH']<-NA


##WS
summary(df$WS)
df[order(df$WS),'WS']
df[order(-df$WS),'WS']
#The wind speed vcalues above 30 are considered as mistaken and are deleted since the units are in m/s.
df[which(df$WS>30),'WS']<-NA

##SR
summary(df$SR)
df[order(df$SR),'SR']
df[order(-df$SR),'SR']
#Any negative values of SR is not practical and hence removed
df[which(df$SR<0),'SR']<-NA

##BP
summary(df$BP)
df[order(df$BP),'BP']
df[order(-df$BP),'BP']
#Any negative values of SR is not practical and hence removed
df[which(df$BP<655 | df$BP>810),'BP']<-NA

## WD
summary(df$WD)
# As any angle above 360 is not possible in degrees
df[which(df$WD>360),'WD']<-NA

## Dropping Temp

colnames(df)
# Bad data, hence we sourced dta from AccuWeather.
df <- df[,-c(10)]

## Coding Events
unique(df$Events)
temp<-df
# temp$Events<-data.frame((df$Events,
#              levels=c(0,1,2,3,4,5,6,7,8,9,10,11,12),
#              labels=c("","Fog","Rain","Thunderstorm","Fog,Rain,Hail,Thunderstorm",
#                       "Fog , Rain","Rain , Thunderstorm","Fog , Thunderstorm","Fog , Rain , Thunderstorm",
#                       "Rain , Hail , Thunderstorm","Hail , Thunderstorm","Snow , Thunderstorm","Fog , Tornado")))


#df$Events<-as.numeric(df$Events)
factor.table<-data.frame(unique(as.numeric(df$Events)),unique(df$Events))
colnames(factor.table)<-c('Value', 'Event')
factor.table<-factor.table[order(factor.table$Value),]
unique(df$Events)
#df<-df$Event[,as.numeric(df$Events)]
#temp
df[which(temp$Events==1),]

##  NA
#Keeping only those rows where only at most three columns are NA
summary(df)
df[which(df$Date=='2017-01-05'),c('Station','BP')]
temp<-df[-which(is.na(df$PM2.5)),]
temp<-temp[!(rowSums(is.na(temp))>(NCOL(temp)-12)),]
df<-temp
temp$Events<-as.factor(as.numeric(temp$Events))
df<-temp

##EDA----
install.package('devtools')
library(devtools)
## Cor plot between all numeric variables and using only the complete values
M <- cor(df[,-c(1,2,14,15)],use="complete.obs")
corrplot(M)

## we see that AT is highly correlated to TempN and Humidity is highly correlated to AT. 
summary(is.na(prdtion))
install_github("ggbiplot","vqv")
library(ggbiplot)
library(VSURF)

prctd <- df$PM2.5
prdtion <- df
temppca <- na.omit(prdtion)
temppca2 <- temppca[,-c(1,2,9,14,15)]
install.package('ggfortify')
library(ggfortify)
pca <- prcomp(temppca2,center=TRUE,scale.=TRUE)
plot(pca,type="l")
summary(pca)



## Biplot
autoplot(prcomp(temppca2),data=temppca,colour="Station",loadings=TRUE,loadings.label=TRUE,loading.label.size=3)

## Variable plot
pca <- prcomp(temppca2,center=TRUE,scale.=TRUE)
plot(pca,type="l")
summary(pca)






##Scatterplot matrix
panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  if(p<0.01) txt2 <- paste("p= ", "<0.01", sep = "")
  text(0.5, 0.4, txt2)
}

pairs(df, upper.panel = panel.cor)

##Violin plot
install.packages("devtools")
library("devtools")
install_github("easyGgplot2","kassambara")
library(easyGgplot2)
library(ggplot2)


#GC
p<-ggplot(df, aes(factor(GC),PM2.5))
p+geom_violin(scale = "count",adjust = 0.8, aes(fill = GC))+
  stat_summary(fun.y = "mean", geom = "point", shape = 4, size = 3, color = "midnightblue") +
  stat_summary(fun.y = "median", geom = "point", shape = 10, size = 3, color = "red")

#Events
p<-ggplot(df, aes(Events,PM2.5))
p+geom_violin(scale = "count",adjust = 0.8,aes(fill = Events))

#Stations
p<-ggplot(df, aes(Station,PM2.5))
p+geom_violin(scale = "count",adjust = 0.8,aes(fill = Station))

rows<-sample(1:nrow(df), 0.80*nrow(df), replace=FALSE)
df.train<-df[rows,]
df.test<-df[-rows,]
rm(rows)

# linear model----

df$GC<-as.numeric(df$GC)
model1<-glm(PM2.5~WS+WD+AT+RH+SR+BP+Aerosol_Type_Land+TempN+Humid+Precip+Station+GC, data=df)
summary(model1)


#Models---------------------------------------------------

#GLM----


glm1<-glm(PM2.5~. -Date,data = df.train)
glm2<-glm(PM2.5 ~ factor(Events, exclude=c('3','5','7','8','9','11','13')) +factor(Station, exclude=c('AV','AYA','BC','CRRI','DTU','IGI','LODHI','MM','NC','NSIT','PUSA','SIRI'))+WS+BP+TempN,data=df.train)
glm3<-glm(PM2.5 ~ factor(Station, exclude=c('AV','AYA','BC','CRRI','DTU','IGI','LODHI','MM','NC','NSIT',
                                            'PUSA','SIRI'))+WS+BP+TempN,data=df.train)

#glm1.cv<-crossValidate("kfold",10,df.train,glm1,"PM2.5")
#glm2.cv<-list(crossValidate("kfold",10,df.train,glm2,"PM2.5"), crossValidate("loocv",10,df.train,glm2,"PM2.5"))
glm3.cv<-list(crossValidate("kfold",10,df.train,glm3,"PM2.5"))

glm.pred<-predict(glm3, df.test)
glm.pred[is.na(glm.pred)]<-mean(glm.pred,na.rm = T)
rmse(glm.pred, df.test$PM2.5)
glm.diag=data.frame(glm3$residuals, glm3$fitted.values)
colnames(glm.diag)<-c('resid', 'pred')
plot(x=df.test$PM2.5, y=glm.pred, xlab="Y", ylab="Y-hat", main="Y-hat vs. Y")# Y vs. Y-hat
plot(y=glm3$residuals, x=glm3$fitted.values, xlab="Fitted Values", ylab="Residuals", main="e vs. Y-hat")
qqnorm(glm3$residuals)
qqline(glm3$residuals)

##GAM----
gam1<-gam(PM2.5~. -Date, data=df.train)
summary(gam1)

form<-as.formula(paste0(resp,"~",paste0("s(",colnames(df.train[,-c(1,2,9,14,15)]),",d=4",")",collapse="+")
                        ,"+",paste0(colnames(df.train[,c(2,14,15)]),collapse="+"),collapse=""))
gam2<-gam(formula = form,data=df.train)
summary(gam2)
rm(form)

form<-as.formula(paste0(resp,"~",paste0("s(",colnames(df.train[,-c(1,2,9,10,13,14,15)]),",d=4",")",collapse="+")
                        ,"+",paste0(colnames(df.train[,c(2,14,15)]),collapse="+"),collapse=""))
gam3<-gam(formula = form, data=df.train)
summary(gam3)
rm(form)

gam1.cv=list(crossValidate('kfold',10,df.train,gam1,resp))
gam2.cv=list(crossValidate('kfold',10,df.train,gam2,resp))
gam3.cv=list(crossValidate('kfold',10,df.train,gam3,resp))

temp<-df.test[which(df.test$Events!='4'),]
rownames(temp) <- NULL
temp<-temp[complete.cases(temp),]
df.gam.test<-temp # Removing all the incomplete cases
gam.pred<-predict(gam3,temp)
gam.rmse<-rmse(gam.pred,temp$PM2.5)

##CART


#RandomForest----
library(doParallel)
library(foreach)
library(randomForest)
library(ModelMetrics)
temp <- na.omit(temp)
temp$knum<-sample(1:10,nrow(temp),replace = TRUE)
temp.train <- temp[!temp$knum==1,-c(16)]
temp.train$knum<- sample(1:10,nrow(temp.train),replace = TRUE)
rmse_kfold<-0
for (i in 1:10)
{
  df.test<-temp.train[temp.train$knum==i,]
  df.train<-temp.train[!temp.train$knum==i,]
  x <- df.train[,-c(1,9)]
  y<- df.train[,9]
  rf <- foreach(ntree=rep(250,4), .combine=combine,.packages = 'randomForest') %dopar% randomForest(x,y,ntree=ntree,na.action=na.omit)
  
  pred<-predict(rf,df.test)
  pred[is.na(pred)]<-mean(pred,na.rm = T)
  rmse_kfold<-cbind(rmse_kfold,rmse(df.test[,9],pred))
}
library(foreach)
library(randomForest)
rf
rmse_kfold

##BART

options(java.parameters = "-Xmx25g")
library('bartMachine')
library('rJava')
set_bart_machine_num_cores(20)

Y<-df$PM2.5
X<-df[,-c(1,9)]

bartModel <- bartMachine(X, Y, use_missing_data = TRUE,serialize = T)
summary(bartModel)
rmse_kfold<-k_fold_cv(X, Y, k_folds = 10, use_missing_data = TRUE)
bart_machine_cv <- bartMachineCV(X, Y,use_missing_data = TRUE,serialize = T)
investigate_var_importance(bartModel, num_replicates_for_avg = 20)
plot_y_vs_yhat(bart_machine_cv, credible_intervals = TRUE)
plot_y_vs_yhat(bart_machine_cv, prediction_intervals = TRUE)

##SVM


##NeuralNet
