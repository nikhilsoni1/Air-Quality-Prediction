# header----
setwd("~/Desktop/project/Analysis")
load("~/Desktop/project/Analysis/df.RData")
save(list=ls(all=T),file='df.RData')

# data----
df.aot<-read.csv('AOT.csv')
df.aot$Date<-as.character(df.aot$Date)
df.aot$Date<-as.Date(df.aot$Date)

df.store<-read.csv('df.csv')
df.store<-df.init[,-2]
colnames(df.store)[1]<-'Date'
colnames(df.store)[2]<-'Station'
df.store$Date<-as.character(df.store$Date)
df.store$Date<-as.Date(df.store$Date, format='%d-%m-%Y')
summary(is.na(df.store$Date))
summary(is.na(df$Date))


df<-merge(df.store , df.aot, by.x=c('Date', 'Station'), by.y=c('Date', 'Station'), all.x = TRUE)

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

set.seed(9)
rows<-sample(1:nrow(df), 0.80*nrow(df), replace=FALSE)
df.train<-df[rows,]
df.test<-df[-rows,]
rm(rows)
df<-df[,-c(9, 11, 12, 13, 14, 15, 16, 18, 19, 20)]
df<-df[which(df$PM2.5 != 'NA'),]
df[which(df[,-c(9)] == 'NA'),]
df<-df[!(rowSums(is.na(df))==(NCOL(df))-4),]