generatefolds<-function(target,sampleRate,setSeed)
{ set.seed(setSeed)
  trainlist<-list()
  cvlist<-list()
  for(i in unique(target))
  {
    temp<-which(target==i)
    
    train_fold<-sample(temp,sampleRate*length(temp),replace=FALSE)
    cv_fold<-temp[which(!temp %in% train_fold)]
    trainlist[[i+1]]<-train_fold
    cvlist[[i+1]]<-cv_fold
    #print(trainlist)
  }
  
  trainIndex<-unlist(trainlist)
  cvIndex<-unlist(cvlist)
  list(trainIndex,cvIndex)
}

generateActions<-function(x)
{
 
  #print (length(actions))
  v<-c(rep(0,length(actions)))
  names(v)<-actions
  
  temp<-count(x$action[x$action!=""])
  for (i in 1:nrow(temp))
  {
    v[as.character(temp[i,1])]<-temp[i,2]
  }
  #print (length(v))
  v[1:359]
}

generateDevices<-function(x)
{
  
  #print (length(actions))
  v<-c(rep(0,length(devices)))
  names(v)<-devices
  
  temp<-count(x$device_type[x$device_type!=""])
  for (i in 1:nrow(temp))
  {
    v[as.character(temp[i,1])]<-temp[i,2]
  }
  #print (length(v))
  v[1:length(devices)]
}

generateActionTypes<-function(x)
{
  
  #print (length(actions))
  v<-c(rep(0,length(action_types)))
  names(v)<-action_types
  
  temp<-count(x$action_type[x$action_type!=""])
  for (i in 1:nrow(temp))
  {
    v[as.character(temp[i,1])]<-temp[i,2]
  }
  #print (length(v))
  v[1:length(action_types)]
}
generatebins<-function(x)
{
  x$secs_elapsed<-x$secs_elapsed/100
  v<-discretize(x$secs_elapsed,numBins=30,r=range(0,18000))
  names(v)<-paste("bins",c(1:30),sep="_")
  #s<-sd(v)
  #v<-c(v,s)
  v<-v/length(x$secs_elapsed)
  v
}