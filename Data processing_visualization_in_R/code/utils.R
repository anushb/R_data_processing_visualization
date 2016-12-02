CoinFlip <- function(){
  set.seed(900000)
  var_x <- sample(0:1, 10000, repl=T)
  sum_x <- cumsum(var_x)
  avg <- sum_x/(1:10000)
  plot(avg,ylim= c(0,1),type='l')
  lines(c(0,10000), c(0.5,0.5),col='green')
}

CLT <- function(popDist="uniform",sampSize=30,numOfSamp=200) {
  x = rep(0,numOfSamp)
  
  if( tolower(popDist)  == "uniform"){
    x = rep(0,numOfSamp)  
    for(i in 1:numOfSamp){
      x[i] = mean(runif(sampSize,1,100))
    }
    hist(x,main = "Xbar plot for Unifrom Dist")
    
  }else if(tolower(popDist) == "normal"){
    for(i in 1:numOfSamp){
      x[i] = mean(rnorm(sampSize, mean=0, sd=1))
    }
    hist(x,main = "Xbar plot for Normal Dist")
    
  }else if(tolower(popDist) == "exponential"){
    for(i in 1:numOfSamp){
      x[i] = mean(rexp(sampSize, rate=1))
    }
    hist(x,main = "Xbar plot for Expo Dist")
  }
}

SLR <- function(filePath){
  mydata <- read.csv(file=filePath,header=TRUE,sep=",")
  
  #model for TV
  lmodel1<-lm(mydata$Sales~mydata$TV,data=mydata)
  summ1 <- summary(lmodel1)
  summ1
  cor(mydata$TV,mydata$Sales)
  plot(mydata$TV,mydata$Sales,main="Scatter-Plot for Linear Regression model-TV")
  abline(lmodel1)
  
  # #model for radio
  lmodel2<-lm(mydata$Sales~mydata$Radio,data=mydata)
  summ2 <- summary(lmodel2)
  summ2
  cor(mydata$Radio,mydata$Sales)
  plot(mydata$Radio,mydata$Sales,main="Scatter-Plot for Linear Regression model-Radio")
  abline(lmodel2)

  #model for newspaper
  lmodel3<-lm(mydata$Sales~mydata$Newspaper,data=mydata)
  summ3 <- summary(lmodel3)
  summ3
  cor(mydata$Newspaper,mydata$Sales)
  plot(mydata$Newspaper,mydata$Sales,main="Scatter-Plot for Linear Regression model-Newspaper")
  abline(lmodel3)
}


MLR <- function(filePath){
  mydata <- read.csv(file=filePath,header=TRUE,sep=",")
  
  #Multiple linear regression
  lmodel4<-lm(mydata$Sales~ mydata$TV + mydata$Radio +  mydata$Newspaper,data=mydata)
  summary(lmodel4)
}


LogisticRegression <- function(filePath){
  mydata <- read.table(filePath,header = TRUE)
  model<-glm(mydata$Y ~ mydata$X1 + mydata$X2 + mydata$X3 ,data=mydata)
  summary(model)
}

LogisticRegressionImproved <- function(filePath){
  mydata <- read.table(filePath,header = TRUE)
  boxplot(mydata$X1,mydata$Y)
  boxplot(mydata$X2,mydata$Y)
  boxplot(mydata$X3,mydata$Y)
  hist(mydata$X1)
  hist(mydata$X2)
  hist(mydata$X3)
  
  my_newdata<-subset(mydata,X1>-120 & X2>-70 & X3<4)
  hist(my_newdata$X1)
  hist(my_newdata$X2)
  hist(my_newdata$X3)
  
  lmodel<-glm(my_newdata$Y ~ my_newdata$X1 + my_newdata$X2 + my_newdata$X3 ,data=my_newdata)
  summary(lmodel)
  
}

