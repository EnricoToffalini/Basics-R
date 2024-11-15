
#########################################################

rm(list=ls())

#########################################################

set.seed(1)

N = 180
k = 6

schoolID = sample(1:6,N,replace=T,prob=runif(6,0.5,1))
school_rInt1 = 0.3*rnorm(k)[schoolID]
school_rInt2 = 0.1*rnorm(k)[schoolID]
school_rInt3 = 0.2*rnorm(k)[schoolID]
school_rInt4 = 0.2*rnorm(k)[schoolID]

name = replicate(N,paste0(sample(letters,5),collapse=""))
school = paste0("school",schoolID)

iq_z = rnorm(N,0,1)

wmScore = rbinom(N,20,pnorm(qnorm(.5)+school_rInt3+iq_z*.25))

visualScore = rbinom(N,20,pnorm(qnorm(.5)+school_rInt4+iq_z*.3))

mathAcc = rbinom(N,20,pnorm(qnorm(.6)+school_rInt1+iq_z*.3))
aggregate(mathAcc,by=list(schoolID),mean)
hist(mathAcc)

mathAvgTime = round(5000+rgamma(N,exp(0.5+school_rInt2-iq_z*.323),1)*5000)
aggregate(mathAvgTime,by=list(schoolID),median)
hist(mathAvgTime)

df = data.frame(school,name,mathAcc,mathAvgTime,wmScore,visualScore)
df = df[order(schoolID),]
cor(df[,3:ncol(df)])

write.csv(df,"exerData1.csv",row.names=F)

#########################################################



