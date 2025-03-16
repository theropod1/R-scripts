set.seed(42)
rnorm(100, mean=10,sd=3)->y 
rnorm(100, mean=0.25*y,sd=0.5)->x

plot(y~x)
ci.lm(lm(y~x))
ci.lm(lm(y~x), interval="prediction", border="grey", col=NA)

###now get information necessary for manual parametric CI and PI calculation
lm(y~x)->model

cl<-0.9 ##set confidence interval width
x_<-seq(0,4,0.1)

##CI and PI
est_p<-coef(model)[1]+x_*coef(model)[2] # point estimate

t_crit_lwr<-qt((1-cl)/2,summary(model)$df[2]) # critical t value
t_crit_upr<-qt(1-(1-cl)/2,summary(model)$df[2]) # critical t value

see<-summary(model)$sigma #residual standard error (SEE)

mean(x)->xmean
var(x)->xvar
length(resid(model))->n

etc_CI<-(1/n+(x_-xmean)^2/((n-1)*xvar))^0.5
etc_PI<-(1+1/n+(x_-xmean)^2/((n-1)*xvar))^0.5

est_p+t_crit_upr*see*etc_CI->upr
est_p+t_crit_lwr*see*etc_CI->lwr

est_p+t_crit_upr*see*etc_PI->uprPI
est_p+t_crit_lwr*see*etc_PI->lwrPI

lines(upr~x_,col="red")
lines(uprPI~x_,col="red")


