# header----
library(xlsx)
library(data.table)
library(plyr)
library(stats)
library(lubridate)
save(list=ls(all=T),file='EDA.RData')
setwd("~/Google Drive/Purdue University/Academics/Sem-2/PredModel/Project/CSV")

# data----

# import data
lst<-list.files()
length(lst)
for (i in 1:length(lst))
{
  assign(paste("df.",tolower(substring(lst[i],0,nchar(lst[i])-4)),sep=""),read.csv(lst[i],skip=12))
}

# curtaililing the individual datasets
lst.df <- ls()[1:18]
tables<- lapply(mget(lst.df),function(x)x[1:1827,])
list2env(tables,envir=.GlobalEnv)
rm(tables)

df <- rbind.fill(df.aya,df.av,df.bc,df.crri,df.dtu,df.ean,df.ihbas,df.ito,df.lodhi,df.mm,df.nc,df.nsit,df.pb,df.pusa,df.rkp,df.shad,df.siri,df.igi)

completeness<-function(dat)
{
  dat[dat==""]<-NA
  dat[dat=="."]<-NA
  a=nrow(dat)
  b=length(dat)
  c=sum(is.na(dat))/(a*b)
  return(round(100*(1-c),digits=1))
}

df.final<-df[,-c(6,7,12,13,22,23,24,25,26,28,31,32,33,35,36,37,38,39,40)]
df.final<-df.final[!(rowSums(is.na(df.final))==(NCOL(df.final))-3),]
completeness(df.final)

corr.df<-cor(df.final[,-c(1,2,3)],use="pairwise.complete.obs")
round(corr.df,2)
corrplot(corr.df)

completeness(data.frame(df.final[,-c(1,2,3)]))
summary(df.final$From.Date)
max(df.final$From.Date)
df <- df.final[order(as.Date(df.final$From.Date, format = "%d/%m/%Y")),]
