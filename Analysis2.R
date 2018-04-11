# header----
save(list=ls(all=T),file='Analysis2.RData')

wd<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(wd)
load(paste0(wd, '/Analysis2.Rdata'))
rm(wd)


install.packages('gam')
library(gam)
install.packages('rJava')
library(rJava)
install.packages('bartMachine')
library(bartMachine)
install.packages('ModelMetrics')
library(ModelMetrics)
install.packages('stats')
library(stats)

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
    return (mean(rmse_kfold[,-1]))
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
    return(mean(rmse_loocv[,-1]))
  }
}

# data----

# AOT Data and processing (1)
df.aot<-read.csv('/home/shah398/Harmanik/AOT.csv')
df.aot$Date<-as.character(df.aot$Date)
df.aot$Date<-as.Date(df.aot$Date, format='%m/%d/%Y')

# Environmental variables data and processing  (2)
df.init<-read.csv('/home/shah398/Harmanik/EnvVar.csv')
df.store<-df.init[,-2]
colnames(df.store)[1]<-'Date'
colnames(df.store)[2]<-'Station'
df.store$Date<-as.character(df.store$Date)
df.store$Date<-as.Date(df.store$Date, format='%d-%m-%Y')
summary(is.na(df.store$Date))
df<-merge(df.store, df.aot, by=c('Date', 'Station'), all.x = TRUE)  # (3) -> Merging (1) & (2)
df<-df[,-c(9, 11, 12, 13, 14, 15, 16, 18, 19, 20)]

# Additional temperature data and processing  (4)
df.temp<-read.csv('//home/shah398/Harmanik/TemperatureData.csv')
df.temp$Date<-as.character(df.temp$Date)
df.temp$Date<-as.Date(df.temp$Date, format='%m/%d/%y')
df<-merge(df, df.temp, by='Date', all.x=T)  # (5) -> Merging (3) & (4)

df$GC<-0
df[which(df$Station=='AV'),'GC']<-3
df[which(df$Station=='IHBAS'),'GC']<-3
df[which(df$Station=='ITO'),'GC']<-3
df[which(df$Station=='MM'),'GC']<-3
df[which(df$Station=='NSIT'),'GC']<-0
df[which(df$Station=='PUSA'),'GC']<-4
df[which(df$Station=='RKP'),'GC']<-3
df[which(df$Station=='LODHI'),'GC']<-3
df[which(df$Station=='CRRI'),'GC']<-2
df[which(df$Station=='DTU'),'GC']<-0
df[which(df$Station=='SIRI'),'GC']<-2
df[which(df$Station=='AYA'),'GC']<-2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
df[which(df$Station=='BC'),'GC']<-3
df[which(df$Station=='EAN'),'GC']<-0
df[which(df$Station=='IGI'),'GC']<-0
df[which(df$Station=='NC'),'GC']<-4
df[which(df$Station=='PB'),'GC']<-0
df[which(df$Station=='SHAD'),'GC']<-4
df$GC<-as.factor(df$GC)
rm(df.aot,df.init,df.store,df.temp)
summary(df)

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


completeness(df)

#EDA
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


set.seed(9)
rows<-sample(1:nrow(df), 0.80*nrow(df), replace=FALSE)
df.train<-df[rows,]
df.test<-df[-rows,]
rm(rows)
# linear model----

df$GC<-as.numeric(df$GC)
model1<-gam(PM2.5~., data=df.train)
summary(model1)
