#outcome<-read.csv("outcome-of-care-measures.csv",colClass="character")
outcome[,11]<-as.numeric(outcome[,11])
table<-table(outcome$State)
outcome2<-outcome[table[outcome$State]>20,]
death<-outcome2[,11]
state<-outcome2$State
boxplot(death~state,ylab="30-day Death Rate",main="Heart Attack 30-day death Rate By State")