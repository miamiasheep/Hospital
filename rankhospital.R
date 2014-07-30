rankhospital<-function (state,outcome,num="best"){
  outcome1<-read.csv("outcome-of-care-measures.csv",colClass="character")
  state1<-outcome1$State
  if(length(state1[state1==state])==0){
    stop("invalid state")
  }
  if((outcome!="heart attack")&&(outcome!="heart failure")&&(outcome!="pneumonia")){
    stop("invalid outcome")
  }
  
  if(outcome=="heart attack"){
    index<-11
  }
  if(outcome=="heart failure"){
    index<-17
  }
  if(outcome=="pneumonia"){
    index<-23
  }
  subset<-outcome1[outcome1$State==state,]
  subset[,index]<-as.numeric(subset[,index])
  sort.outcome<-subset[order(subset[,index],subset[,2],na.last=TRUE),]# 11,17,23 for heart attack,heart failure,pneumonia 2 for hospital name
  if(num=="best"){
    rank<-1
  }
  else if(num=="worst"){
    rank<-nrow(sort.outcome)
    sort.outcome<-subset[order(subset[,index],subset[,2],na.last=FALSE),]
  }else{
    rank<-num
  }
  sort.outcome[rank,2]
}