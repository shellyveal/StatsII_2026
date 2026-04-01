# split senate delegations
splitdel=function(data,cat){
  temp=data[data$chamber=='Senate',]
  temp=temp[order(temp$cong,temp$state),]
  temp$spl=0
  for (i in 1:(nrow(temp)-1)) {
    if (temp$state[i]==temp$state[i+1]) {
      if (temp[i,cat]!=temp[i+1,cat]) {
        temp$spl[i]=1
        temp$spl[i+1]=1
      } else {next}
    } else {next}
  }
  temp=temp[temp$spl==1,]
  return(temp)
}
# ---------------------------