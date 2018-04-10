# header----
getwd()
setwd("/home/shah398/R project")
load("~/Google Drive/Purdue University/Academics/Sem-2/PredModel/Project/Analysis/Analysis 2/Analysis2.RData")
save(list=ls(all=T),file='Analysis2.RData')

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

# data----

# AOT Data and processing (1)
df.aot<-read.csv('/home/shah398/R project/AOT.csv')
df.aot$Date<-as.character(df.aot$Date)
df.aot$Date<-as.Date(df.aot$Date, format='%m/%d/%Y')

# Environmental variables data and processing  (2)
df.init<-read.csv('/home/shah398/R project/EnvVar.csv')
df.store<-df.init[,-2]
colnames(df.store)[1]<-'Date'
colnames(df.store)[2]<-'Station'
df.store$Date<-as.character(df.store$Date)
df.store$Date<-as.Date(df.store$Date, format='%d-%m-%Y')
summary(is.na(df.store$Date))
df<-merge(df.store, df.aot, by=c('Date', 'Station'), all.x = TRUE)  # (3) -> Merging (1) & (2)
df<-df[,-c(9, 11, 12, 13, 14, 15, 16, 18, 19, 20)]

# Additional temperature data and processing  (4)
df.temp<-read.csv('//home/shah398/R project/TemperatureData.csv')
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
# df<-df[!(rowSums(is.na(df))==(NCOL(df))-7),]

set.seed(9)
rows<-sample(1:nrow(df), 0.80*nrow(df), replace=FALSE)
df.train<-df[rows,]
df.test<-df[-rows,]
rm(rows)
# linear model----

df$GC<-as.numeric(df$GC)
model1<-gam(PM2.5~., data=df.train)
summary(model1)
model1
print('New')