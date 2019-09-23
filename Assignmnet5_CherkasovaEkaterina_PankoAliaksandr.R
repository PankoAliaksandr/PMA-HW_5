setwd("~/Desktop/Portfolio_Management_Applications/Exercise_5")
load("BSV_CurData.Rdata")
tail(curdat)
curdat <- curdat[-c(2:12)] #delete useless data

#converting some currencies into american terms
curdat$EUR.fx.spot <- 1/curdat$EUR.fx.spot
curdat$AUD.fx.spot <- 1/curdat$AUD.fx.spot
curdat$GBP.fx.spot <- 1/curdat$GBP.fx.spot
curdat$NZD.fx.spot <- 1/curdat$NZD.fx.spot

curdat$EUR.fx.fwd1m <- 1/curdat$EUR.fx.fwd1m
curdat$AUD.fx.fwd1m <- 1/curdat$AUD.fx.fwd1m
curdat$GBP.fx.fwd1m <- 1/curdat$GBP.fx.fwd1m
curdat$NZD.fx.fwd1m <- 1/curdat$NZD.fx.fwd1m

dates <- as.character(curdat$date)

#calculating returns
n <- nrow(curdat)
m <- 11 #all currencies
N <- 10 #number of foreign currencies
returns <- curdat[1:(n-1),(2+m):(2*m+1)]/curdat[2:n,2:(m+1)]-1
returns <- returns[-c(1)] #drop usd as it is a base currency
colnames(returns) <- c("EUR.ret","CHF.ret","AUD.ret","JPY.ret","GBP.ret","CAD.ret","NZD.ret","DKK.ret","NOK.ret","SEK.ret")
rownames(returns) <- dates[-1]

#calculating forward discount
fd <- curdat[(1:n),(2+m):(2*m+1)]/curdat[(1:n),2:(m+1)] -1
fd <- fd[-c(1)]
colnames(fd) <- c("EUR.fd","CHF.fd","AUD.fd","JPY.fd","GBP.fd","CAD.fd","NZD.fd","DKK.fd","NOK.fd","SEK.fd")
rownames(fd) <- dates

#standartized forward discount
fd.mean<- rowMeans(fd )
fd.sdev <- c()
for (i in 1:n){
  fd.sdev  <- c(fd.sdev , sd(fd[i,]))
}
stanFD  <-(fd  - fd.mean)/fd.sdev 


#calculating momentum as cumulative return for last 3 months
#so we have 238 momentums in total 
nmom <- 238
mom <- matrix(nrow=nmom, ncol=10)
for (i in 1:nmom){
  for (j in 1:10){
    mom[i, j] <- (curdat[i+3,2+j]/curdat[i, 2+j]) - 1 
  }
}
    
#standartize mom
mom.mean  <- rowMeans(mom[,])
mom.sdev  <- c()
for (i in 1:nmom ){
  mom.sdev  <- c(mom.sdev, sd(mom[i,]))
}
stanMOM  <-(mom-mom.mean)/mom.sdev
colnames(stanMOM) <- c("EUR.mom","CHF.mom","AUD.mom","JPY.mom","GBP.mom","CAD.mom","NZD.mom","DKK.mom","NOK.mom","SEK.mom")
rownames(stanMOM) <- dates[-c(1:3)]
stanMOM <- as.data.frame(stanMOM)

#convert risk free rate to monthly rate
rf <- (1+curdat$USD.ibor1m/100)^(1/12)-1 

# in sample (consider full period  from 31.03.1999 to 30.11.2018)
stanFD_in <- stanFD[4:(n-1),]
stanMOM_in <- stanMOM[1:(nrow(stanMOM)-1),]
returns_in <- returns[3:nrow(returns),]
rf_in <- rf[-c(1:3,241)]


gamma <- 5
utility1<- function(theta) {
  w <- (theta[1] * stanFD_in + theta[2] * stanMOM_in)/N #weights
  rp <- c()
  for (i in 1:237) { 
    rp <- c(rp, rf_in[i] + sum(returns_in[i+1,] * w[i,]))}
  u  <-(1 + rp)^(1-gamma)/(1-gamma)
  return(-mean(u, na.rm = TRUE))}
utility1(c(1,1))

theta1 <- optim(c(1,1),utility1,method="CG",control=list(trace=TRUE))
theta1$par
#[1] 1.1186301 0.9762606
theta1$value #utility finction

#####################################
rf_out <- rf[-c(1:3)]
fd_out <- stanFD[4:241,]
mom_out <- stanMOM[1:238,]
returns_out <- returns[3:241,]
#out of sample
p <- 121 #number of periods (first 10 years)
utility2 <- function(theta) {
  w <- (theta[1] * fd_out + theta[2] * mom_out)/N 
  rp <- c()
  for (i in 1:(p-1)) { 
  rp <- c(rp, rf_out[i] + sum(returns_out[i+1,] * w[i,]))}
  u  <-(1 + rp)^(1-gamma)/(1-gamma)
  return(-mean(u, na.rm = TRUE))
  }
theta2 <- optim(c(1,1), utility2, method="CG")
theta2$par
theta2$value #utility finction
t <- c(theta2$par)


for (p in 122:238){
  theta2 <- optim(c(1,1), utility2, method="CG")
  t <- rbind(t,theta2$par)
}
colnames(t) <- c("fd.theta", "mom.theta")
save(t,file="thetaout.rda")
write.csv(t, file = "theta_out.csv")
library("readxl")

t <- as.data.frame(t)
theta_out <- t[-1,]
d <- dates[-c(1:3)]
d <- d[122:238]
rownames(theta_out) <- d
#########################
###### Weights in Sample
w_in<-(theta1$par[1]*fd_out[122:238,] + theta1$par[2] * mom_out[122:238,])/N 
###### Weights Out of Sample

w_out<-(theta_out$fd.theta*fd_out[122:238,] + theta_out$mom.theta * mom_out[122:238,])/N 

colnames(w_in)<-c("EUR","CHF","AUD","JPY","GBP","CAD","NZD","DKK","NOK","SEK")
colnames(w_out)<-c("EUR","CHF","AUD","JPY","GBP","CAD","NZD","DKK","NOK","SEK")
#w_in<-as.data.frame(w_in)
#w_out<-as.data.frame(w_out)
####

plot(w_in[,"EUR"]~curdat[125:241,1],main="EUR(red:in-sample, blue: out-of-sample)", ylim=range(w_in$EUR),xlab="Date",ylab="Weights",type="l", col="red")
lines(w_out$EUR~curdat[125:241,1], ylim=range(w_out$EUR), type="l", col="blue")


plot( w_in[,"CHF"]~curdat[125:241,1],main="CHF(red:in-sample, blue: out-of-sample)", ylim=range(w_in$CHF),xlab="Date",ylab="Weights",type="l", col="red")
lines(w_out$CHF~curdat[125:241,1], ylim=range(w_out$CHF), type="l", col="blue")


plot( w_in[,"AUD"]~curdat[125:241,1],main=" AUD(red:in-sample, blue: out-of-sample)", ylim=range(w_in$AUD),xlab="Date",ylab="Weights",type="l", col="red")
lines(w_out$AUD~curdat[125:241,1], ylim=range(w_out$AUD), type="l", col="blue")


plot( w_in[,"JPY"]~curdat[125:241,1],main="JPY(red:in-sample, blue: out-of-sample)", ylim=range(w_in$JPY),xlab="Date",ylab="Weights",type="l", col="red")
lines(w_out$JPY~curdat[125:241,1], ylim=range(w_out$JPY), type="l", col="blue")
#legend('bottomright',legend=c("in sample", "out-of-sample"), col=c("red", "blue"),lty=1:1, cex=0.3, horiz=TRUE)

plot( w_in[,"GBP"]~curdat[125:241,1],main="GBP(red:in-sample, blue: out-of-sample)", ylim=range(w_in$GBP),xlab="Date",ylab="Weights",type="l", col="red")
lines(w_out$GBP~curdat[125:241,1], ylim=range(w_out$GBP), type="l", col="blue")
#legend('topright',legend=c("in sample", "out-of-sample"), col=c("red", "blue"),lty=1:1, cex=0.5)

plot( w_in[,"CAD"]~curdat[125:241,1],main="CAD(red:in-sample, blue: out-of-sample)", ylim=range(w_in$CAD),xlab="Date",ylab="Weights",type="l", col="red")
lines(w_out$CAD~curdat[125:241,1], ylim=range(w_out$CAD), type="l", col="blue")
#legend('topright',legend=c("in sample", "out-of-sample"), col=c("red", "blue"),lty=1:1, cex=0.5)

plot( w_in[,"NZD"]~curdat[125:241,1],main=" NZD(red:in-sample, blue: out-of-sample)", ylim=range(w_in$NZD),xlab="Date",ylab="Weights",type="l", col="red")
lines(w_out$NZD~curdat[125:241,1], ylim=range(w_out$NZD), type="l", col="blue")
#legend('topright',legend=c("in sample", "out-of-sample"), col=c("red", "blue"),lty=1:1, cex=0.5)

plot( w_in[,"DKK"]~curdat[125:241,1],main=" DKK(red:in-sample, blue: out-of-sample)", ylim=range(w_in$DKK),xlab="Date",ylab="Weights",type="l", col="red")
lines(w_out$DKK~curdat[125:241,1], ylim=range(w_out$DKK), type="l", col="blue")
#legend('topright',legend=c("in sample", "out-of-sample"), col=c("red", "blue"),lty=1:1, cex=0.5)

plot( w_in[,"NOK"]~curdat[125:241,1],main="NOK(red:in-sample, blue: out-of-sample)", ylim=range(w_in$NOK),xlab="Date",ylab="Weights",type="l", col="red")
lines(w_out$NOK~curdat[125:241,1], ylim=range(w_out$NOK), type="l", col="blue")

plot( w_in[,"SEK"]~curdat[125:241,1],main="SEK(red:in-sample, blue: out-of-sample)", ylim=range(w_in$SEK),xlab="Date",ylab="Weights",type="l", col="red")
lines(w_out$SEK~curdat[125:241,1], ylim=range(w_out$SEK), type="l", col="blue")
#legend('topright',legend=c("in sample", "out-of-sample"), col=c("red", "blue"),lty=1:1, cex=0.5)


#compare theta
#theta_fd
plot(theta_out$fd.theta~curdat[125:241,1],main='Theta fd in-the-sample and out-of-sample',ylim=range(theta_out$fd.theta),xlab="Date",ylab="theta_fd",type="l", col="blue")
abline(h=theta1$par[1], col="red")
legend('topright',legend=c("in sample", "out-of-sample"), col=c("red", "blue"),lty=1:1, cex=0.5)

#theta_mom
plot(theta_out$mom.theta~curdat[125:241,1],main='Theta mom in sample and out of sample',ylim=range(theta_out$mom.theta),xlab="Date",ylab="theta_fd",type="l", col="blue")
abline(h=theta1$par[2], col="red")
legend('bottomright',legend=c("in sample", "out-of-sample"), col=c("red", "blue"),lty=1:1, cex=0.5)

#Forward
theta1$par[1]
mean(theta_out[,1])
theta1$par[1] - mean(theta_out[,1])
#Momentum
theta1$par[2]
mean(theta_out[,2])
theta1$par[2] - mean(theta_out[,2])

#in order to compare we start from 30.04.2009
#in sample calculations
theta_in <- theta1$par
ret_comp<-returns[124:240,] #from 30.04.2009
rf_comp<-as.data.frame(rf[125:241])

weights <- (theta_in[1] * fd_out[122:238,] + theta_in[2] * mom_out[122:238,])/N
ret_p <- c()
for (i in 1:116) {
  ret <- rf_comp[i,] + sum(ret_comp[i+1,] * weights[i,])
  ret_p <- rbind(ret_p, ret)}

colnames(ret_p) <- "Ret.P"
rownames(ret_p) <- row.names(weights)[-1]
#average return 
mean(ret_p) #0.002783769
#sd
sd(ret_p) #0.01279444

#utility for each month
utility_in<- ((1+ret_p)^(1-gamma)/(1-gamma)) 

#certainty equivalent return
ceq_in <- ((mean(utility_in)*(1-5))^(1/1-5)-1)
ceq_in
#0.03884116

#calculating beta
FX.Carry <- as.vector(curdat$FXCARRY.idx[126:241])
ret_p <- as.vector(ret_p)
beta.in <- cov((FX.Carry), (ret_p)) / var(FX.Carry)
beta.in #7.385766e-05

sharpe.in <- mean(ret_p, na.rm = TRUE) / sd(ret_p, na.rm = TRUE)
sharpe.in #0.2175765



#  out of sample

weights_out <- (theta_out$fd.theta * fd_out[122:238, 1] + theta_out$mom.theta * mom_out[122:238, 1])/N
for(i in 2:10){
  weights_out_i <- (theta_out$fd.theta * fd_out[122:238, i] + theta_out$mom.theta * mom_out[122:238, i])/N
  weights_out <- cbind(weights_out, weights_out_i)
}
colnames(weights_out) <-c("EUR","CHF","AUD","JPY","GBP","CAD","NZD","DKK","NOK","SEK")

ret_p_out <- c()

for (i in 1:116) {
  ret <- rf_comp[i,] + sum(ret_comp[i+1,] * weights_out[i,])
  ret_p_out <- rbind(ret_p_out, ret)
}

colnames(ret_p_out) <- "Ret.P"
rownames(ret_p_out) <- rownames(weights_out)

#average return 
mean(ret_p_out) #0.002742522
#sd
sd(ret_p_out) #0.0125237

#utility for each month
utility_out<- ((1+ret_p_out)^(1-gamma)/(1-gamma)) 

#certainty equivalent return
ceq_out <- ((mean(utility_out)*(1-5))^(1/1-5)-1)
ceq_out
#0.03842733

#calculating beta
FX.Carry <- as.vector(curdat$FXCARRY.idx[126:241])
ret_p_out <- as.vector(ret_p_out)
beta.out<- cov((FX.Carry), (ret_p_out)) / var(FX.Carry)
beta.out #8.704721e-05

sharpe.out <- mean(ret_p_out, na.rm = TRUE) / sd(ret_p_out, na.rm = TRUE)
sharpe.out #0.2189866

