outcome<-read.csv("outcome-of-care-measures.csv",colClass="character")
hospital<-read.csv("hospital-data.csv",colClass="character")
outcome.hospital<-merge(outcome,hospital,by="Provider.Number")
death<-as.numeric(outcome.hospital[,11])#Heart Attack Outcome
npatient<-as.numeric(outcome.hospital[,15])
owner<-factor(outcome.hospital$Hospital.Ownership)
library(lattice)
xyplot(death~npatient|owner,layout=c(3,3),
       panel=function(x,y,...){
          panel.xyplot(x,y,...)
          panel.lmline(npatient,death,col=2)
       }
       )