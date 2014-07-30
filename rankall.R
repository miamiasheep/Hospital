rankall<-function ( outcome,num="best"){
  outcome1<-read.csv("outcome-of-care-measures.csv",colClass="character")
  s<-split(outcome1,outcome1$State)
  hospital<-c(1:length(s))
  state<-c(1:length(s))
  count<-1
  for(state_in in names(s)){
    state1<-outcome1$State
    if(length(state1[state1==state_in])==0){
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
    subset<-outcome1[outcome1$State==state_in,]
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
    hospital[count]<-sort.outcome[rank,2]
    state[count]<-state_in
    count<-count+1
    }
  ans<-data.frame(hospital,state)
  #rownames(ans)<-state
  ans
}