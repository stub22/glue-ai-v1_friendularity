rm(list=ls(all=TRUE))
x=0;
dx=0;
u=1;
t=1;
xtraj<-c(0*1:1000)
dxtraj<-c(0*1:1000)
e<-c(0*1:1000)
de<-c(0*1:1000)
ie<-c(0*1:1000)
utr<-c(0*1:1000)
S<-c(0*1:1000)
dS<-c(0*1:1000)
sigma <- sqrt(0.4)

# parameter for the PLD control
a<-1
b<-a/2
xfinal=10;
dxfinal=0;
ddt=0.05;

T<- matrix(c(1,1,.1,0),ncol=2)
H<- matrix(c(0.1, .1), nrow = 2)
theta <- c(T =T, sigma = sigma)

# set up the forward model
## The objective function passed to 'optim'
## Create a state space representation out of the four ARMA parameters
posvelss <- function(T1,T2,T3,T4,sigma,utr) {
Tt <- matrix(c(T1,T2,T3,T4), ncol = 2)
Zt <- matrix(c(1,0,0,1), ncol = 2)
ct <- matrix(c(0,0))
#dt <- matrix(c(array(0,dim=c(1,t-1)),utr[1:t-1]), nrow = 2)
dt <- matrix(c(0,0), nrow = 2)

#GGt <- matrix(1)
H <- matrix(c(1,0,0,sigma), nrow = 2) 
HHt <- H %*% t(H) 
G <- matrix(c(1,0,0,1), nrow = 2) 
GGt <- G %*% t(G)
a0 <- c(0,0)
P0 <- matrix(1e6, nrow = 2, ncol = 2)
return(list(a0 = a0, P0 = P0, ct = ct, dt = dt, Zt = Zt, Tt = Tt, GGt = GGt,
HHt = HHt))
}

objective <- function(theta,  yt) {
sp <- posvelss(theta["T1"],theta["T2"],theta["T3"],theta["T4"], theta["sigma"],utr)
ans <- fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt,
Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt = yt)
return(-ans$logLik)
}


# a test of prediction
for (dt in (1:200)){
    tmp = rnorm(1) * sigma
    x = dx*ddt+x + rnorm(1)
    dx = u*ddt+0.95*dx + tmp    # assume that velocity reduce by friction
    S[t] = x + rnorm(1) ; # position information from sensor
    dS[t] = dx +rnorm(1); # velocity information from sensor

  # optimize the kalman filter
  #   if (dt>5)
  #  {fit <- optim(theta, objective, yt =matrix(c(S[1:t-1], dS[1:t-1]),nrow=2), hessian = TRUE)
  #      theta <- fit$par
  #    }   
    
    
    xtraj[t]= x
    dxtraj[t]= dx
    e[t]= xfinal-S[t]
    if(t>1)   {de[t] =( e[t]-e[t-1])*ddt}
    ie[t]=sum(e[1:t])*ddt
    
#PLD control
   
    u =a* e[t] +b* ie[t]
    utr[t]= u

#or u = babbler command
    t = t+1
    
#predict the next state
#sp <- posvelss(fit$par["T1"],fit$par["T2"],fit$par["T3"], fit$par["H1"], fit$par["H2"],fit$par["sigma"],utr)
#ans <- fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt,
#Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt =rbind(S[1:t-1] ))
#forecast the state in the time [t+k] and calculate the Jacobian Matrix  d(xfinal-xpredicted)^2/du    xpredicted = f^k(xt,u) f(xt,u)=linear model in kalman filter
#
# if error (E(S)-S) > some threshold, then learn

# updata PLD control
                     
#design the motion by Jacobian
    if(x>9 & x<11 & dx > -0.5 & dx <0.5) stop("arrive destination")
    
# this part could be refined using RK method
}
fit <- optim(theta, objective, yt =matrix(c(S[1:t-1], dS[1:t-1]),nrow=2), hessian = TRUE)
 

sp <- posvelss(fit$par["T1"],fit$par["T2"],fit$par["T3"],fit$par["T4"], fit$par["sigma"],utr)
ans <- fkf(a0 = sp$a0, P0 = sp$P0, dt = sp$dt, ct = sp$ct, Tt = sp$Tt,
Zt = sp$Zt, HHt = sp$HHt, GGt = sp$GGt, yt =matrix(c(S[1:t-1], dS[1:t-1]),nrow=2))
plot(ans, at.idx = 1, att.idx = 1, CI = 1)
plot(dS[1:t-1])


