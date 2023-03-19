
#importando a base de dados categorizadas. 
library(readxl)
leucc <- read.table("H:/Meu Drive/6º semestre/sobrevivência-listas em grupo/leucc.txt",header=T)

#renomeando as covariáveis
temp <- leucc$tempos
cens <- leucc$cens
idade <- leucc$idadec
zpeso <- leucc$zpesoc
zest <- leucc$zestc
pas <- leucc$pasc
vac <- leucc$vacc
risk <- leucc$riskc
r6 <- leucc$r6c
leuini <- leucc$leuinic


#Passo 1:Ajuste pelo modelo e seleção de covariáveis
require(survival)
fit1 = coxph(Surv(temp,cens) ~ (idade+zpeso+zest+pas+vac+risk+r6+leuini), data=leucc, x = T)
summary(fit1)

#retirando a covariavel risk
fit2 = coxph(Surv(temp,cens) ~ (idade+zpeso+pas+vac+leuini+r6+zest), data=leucc, x = T)
summary(fit2)

#retirando zest
fit3 = coxph(Surv(temp,cens) ~ (idade+zpeso+pas+vac+leuini+r6), data=leucc, x = T)
summary(fit3) 

#retirando r6
fit4 = coxph(Surv(temp,cens) ~ (idade+zpeso+pas+vac+leuini), data=leucc, x = T)
summary(fit4)
-2*fit4$loglik[2] #estat teste do TRV


#vemos que o modelo 4 apresenta covariáveis significativas, logo vou escolher ele. 

# ajustando o modelo 4 e vendo se ele é adequado ----------------------------------------------------

resm<-resid(fit4,type="martingale")
res<-cens - resm
ekm <- survfit(Surv(res, cens)~1)
#summary(ekm)
par(mfrow=c(1,2))
plot(ekm, mark.time=F, conf.int=F, xlab="resíduos", ylab="S(e) estimada")
res<-sort(res)
exp1<-exp(-res)
lines(res, exp1, lty=3, col="red")
legend("topright", lty=c(2,3), c("Kaplan
Meier","Exponencial(1)"), lwd=1, bty="n", cex=0.7)
st<-ekm$surv
t<-ekm$time
sexp1<-exp(-t)
plot(st, sexp1, xlab="S(e): Kaplan-Meier", ylab= "S(e):Exponencial(1)", pch=16)
abline(0,1,col="red")


par(mfrow=c(2,3))

#Analisando a principal suposição do modelo de Cox. 

## Avaliação de proporcionalidade
attach(leucc)
require(survival)

fit<-coxph(Surv(tempos[leuinic==1],cens[leuinic==1]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv, digits=5)
H0<- -log(s0)
plot(ss$time, log(H0), xlab="Tempos", ylim=range(c(-5,1)), ylab = expression(log(Lambda[0]* (t))),
     bty="n", type="s")
fit<-coxph(Surv(tempos[leuinic==0],cens[leuinic==0]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv, digits=5)
H0<- -log(s0)
lines(ss$time, log(H0), type="s", lty=4)
legend(1, -3, lty=c(4,1), c("leuini < 75","leuini > 75 "), lwd=1, bty="n", cex=0.8)
title("LEUINI")


attach(leucc)
require(survival)
fit<-coxph(Surv(tempos[idade==1],cens[idade==1]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv, digits=5)
H0<- -log(s0)
plot(ss$time, log(H0), xlab="Tempos", ylim=range(c(-5,1)), ylab = expression(log(Lambda[0]* (t))),
     bty="n", type="s")
fit<-coxph(Surv(tempos[idade==0],cens[idade==0]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv, digits=5)
H0<- -log(s0)
lines(ss$time, log(H0), type="s", lty=4)
legend(1, -3, lty=c(4,1), c("idade < 96","idade > 96 "), lwd=1, bty="n", cex=0.8)
title("IDADE")




fit<-coxph(Surv(tempos[zpeso==1],cens[zpeso==1]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv, digits=5)
H0<- -log(s0)
plot(ss$time, log(H0), xlab="Tempos", ylim=range(c(-5,1)), ylab = expression(log(Lambda[0]* (t))),
     bty="n", type="s")
fit<-coxph(Surv(tempos[zpeso==0],cens[zpeso==0]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv, digits=5)
H0<- -log(s0)
lines(ss$time, log(H0), type="s", lty=4)
legend(1, -3, lty=c(4,1), c("zpeso < -2","zpeso > -2 "), lwd=1, bty="n", cex=0.8)
title("ZPESO")


fit<-coxph(Surv(tempos[pas==1],cens[pas==1]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv, digits=5)
H0<- -log(s0)
plot(ss$time, log(H0), xlab="Tempos", ylim=range(c(-5,1)), ylab = expression(log(Lambda[0]* (t))),
     bty="n", type="s")
fit<-coxph(Surv(tempos[pas==0],cens[pas==0]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv, digits=5)
H0<- -log(s0)
lines(ss$time, log(H0), type="s", lty=4)
legend(1.4, -3, lty=c(4,1), c("pas < 5%","pas > 5% "), lwd=1, bty="n", cex=0.8)
title("PAS")


fit<-coxph(Surv(tempos[vac==1],cens[vac==1]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv, digits=5)
H0<- -log(s0)
plot(ss$time, log(H0), xlab="Tempos", ylim=range(c(-5,1)), ylab = expression(log(Lambda[0]* (t))),
     bty="n", type="s")
fit<-coxph(Surv(tempos[vac==0],cens[vac==0]) ~ 1, data=leucc, x = T, method="breslow")
ss<- survfit(fit)
s0<-round(ss$surv, digits=5)
H0<- -log(s0)
lines(ss$time, log(H0), type="s", lty=4)
legend(0.6, -3, lty=c(4,1), c("vac < 1","vac > 1 "), lwd=1, bty="n", cex=0.8)
title("VAC")


#Residuos padronizados de scaledch
resid(fit4,type='scaledsch')
cox.zph(fit4,transform='identity')
par(mfrow=c(2,3))
plot(cox.zph(fit4))



###teste de hipótese para testar a proporcionalidade dos riscos 
prop_func <- function(fit, transform = "identity", new_cox.zph = NULL) {
  sresid <- resid(fit, "schoenfeld")
  varnames <- names(fit$coefficients)
  nvar <- length(varnames)
  ndead <- length(sresid)/nvar
  if (nvar == 1) {
    times <- as.numeric(names(sresid))
  } else {
    times <- as.numeric(dimnames(sresid)[[1]])
  }
  if (is.character(transform)) {
    tname <- transform
    ttimes <- switch(transform, identity = times, rank = rank(times),
                     log = log(times), stop("Unrecognized transform"))
  }
  else {
    tname <- deparse(substitute(transform))
    if (length(tname) > 1)
      
      tname <- "user"
    ttimes <- transform(times)
  }
  xx <- ttimes - mean(ttimes)
  r2 <- sresid %*% fit$var * ndead
  test <- xx %*% r2
  corel <- c(cor(xx, r2))
  cbind(rho = c(corel,NA), new_cox.zph$table)
}
prop_func(fit4) #p-value do teste de proporcionalidade

res.cox2<-cox.zph(fit4, transform="identity", terms=FALSE)
res.cox2


##resíduos martigale 
par(mfrow=c(1,2))
rm <- res.Martingale <- residuals(fit4, type="martingale")
#res.Martingale
rd <- res.Deviance <- residuals(fit4, type="deviance")
#res.Deviance

pl <- fit4$linear.predictors

plot(pl,rm,xlab='Preditor linear',ylab='Resíduo Martingale',pch=16, main="Residuo Martingale")
abline(h=0, lty=2)

plot(pl,rd,xlab='Preditor linear',ylab='Resíduo Deviance',pch=16,main="Residuo Deviance")
abline(h=0, lty=2)


# interpretação dos coeficientes

Ht<-basehaz(fit4,centered=F)
tempos=Ht$time
H0<-Ht$hazard # Risco de base
S0<-exp(-H0)
tt = sort(tempos)
aux1 = as.matrix(tt)
n = nrow(aux1)
aux2 = as.matrix(cbind(tempos,S0))
S00 = rep(max(aux2[,2]),n)
for(i in 1:n){
  if (tt[i] > min(aux2[,1])) {
    i1 = aux2[,1] <= tt[i]
    S00[i] = min(aux2[i1,2])
  }
}
ts0 = cbind(tt,S00)
x1=1
x2=1
x3=1
x4=1
x5=1
b = fit4$coefficients
bb1 = b[1] #idade
bb2 = b[2] #zpeso
bb3 = b[3] #pas
bb4 = b[4] #vac
bb5 = b[5] #leuini


## somente variando a Idade e fixando o valor 1 para as demais covariáveis
st2<-S00^(exp(bb1*1 +bb2*x2+bb3*x3+bb4*x4+bb5*x5 )) # idade=1
st1<-S00^(exp(bb1*0 +bb2*x2+bb3*x3+bb4*x4+bb5*x5 )) # idade=0, 



plot(tt,st1, type="s",ylim=c(0,1),xlab="Tempos",ylab="S(t|x)",lty=2,col=1)
lines(tt,st2,type="s",lty=2,col=2)
legend(0,0.2,lty=c(2,2),col=c(1,2),c("<= 96 meses",">96 meses"),cex=0.5,bty="n")
title("")




