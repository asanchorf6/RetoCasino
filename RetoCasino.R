get_simbols<-function(){
  rueda <- c("DD","7","BBB","BB","B","C","0")
  sample(rueda,size=3,replace=TRUE,
                  prob=c(0.03,0.03,0.06,0.1,0.25,0.01,0.52))
}

score<-function(symbols){
  if(length(unique(symbols))==1){
    if(symbols[1]==0){
      cash<-0
    }else{
      if(symbols[1]=="DD"){
        cash<-100
      }else if(symbols[1]=="7"){
        cash<-80
      }else if(symbols[1]=="BBB"){
        cash<-40
      }else if(symbols[1]=="BB"){
        cash<-25
      }else{
        cash<-10
      }
    }
  }else{
    cash<-5
  }
  return(cash)
}

score2<-function(symbols){
  same<-symbols[1]==symbols[2]&symbols[1]==symbols[3]
  bars<-sum(symbols %in% c("BBB","BB","B"))
  nc<-sum(symbols=="C")
  
  if(same & symbols[1]!=0){
    payout<-c("DD"=100,"7"=80,"BBB"=40,"BB"=25,"B"=10,"C"=10)
    prize<-unname(payout[symbols[1]])
  }else if(bars==3){
    prize<-5
  }else{
    prize<-c(0,2,5)[nc+1]
  }
  
  if(prize>0){
    print(paste("cash",prize,sep=":"))
  }else{
    print("No hay premio")
  }
  return(prize)
}

play<-function(){
  symbols<-get_simbols()
  print(symbols)
  puntos<-score2(symbols)
  print(puntos)
}

play()



