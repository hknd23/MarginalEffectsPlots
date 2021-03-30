#' @export
meplot <- function(model,var1,var2,ci=.95,
                   xlab=var2,ylab=paste("Marginal Effect of",var1),
                   main="Marginal Effect Plot",
                   me_lty=1,me_lwd=3,me_col="black",
                   ci_lty=1,ci_lwd=1,ci_col="black",
                   yint_lty=2,yint_lwd=1,yint_col="black"){
  alpha <- 1-ci
  z <- qnorm(1-alpha/2)
  beta.hat <- coef(model)
  cov <- vcov(model)
  z0 <- seq(min(model$model[,var2],na.rm=T),max(model$model[,var2],na.rm=T),length.out=1000)
  dy.dx <- beta.hat[var1] + beta.hat[length(beta.hat)]*z0
  se.dy.dx <- sqrt(cov[var1,var1] + z0^2*cov[nrow(cov),ncol(cov)] + 2*z0*cov[var1,ncol(cov)])
  upr <- dy.dx + z*se.dy.dx
  lwr <- dy.dx - z*se.dy.dx
  plot(x=z0, y=dy.dx,type="n",xlim=c(min(z0),max(z0)),
       ylim=c(min(lwr),max(upr)),
       xlab = xlab,
       ylab = ylab,
       main = main)
  lines(z0, dy.dx, lwd = me_lwd, lty = me_lty, col = me_col)
  lines(z0, lwr, lwd = ci_lwd, lty = ci_lty, col = ci_col)
  lines(z0, upr, lwd = ci_lwd, lty = ci_lty, col = ci_col)
  abline(h=0,lty=yint_lty,lwd=yint_lwd,col=yint_col)
}

#' @export
meplotdum <- function(model,var1,var2,ci=.95,
                      xlab=var2,ylab=paste("Marginal Effect of",var1),
                      main="Marginal Effect Plot",
                      me_lty=1,me_lwd=3,me_col="black",
                      ci_lty=1,ci_lwd=1,ci_col="black",
                      yint_lty=2,yint_lwd=1,yint_col="black"){
  alpha <- 1-ci
  z <- qnorm(1-alpha/2)
  beta.hat <- coef(model)
  cov <- vcov(model)
  z0 <- seq(min(model$model[,var2],na.rm=T),max(model$model[,var2],na.rm=T),length.out=1000)
  dy.dx0 <- beta.hat[var1] + beta.hat[length(beta.hat)]*0
  dy.dx1 <- beta.hat[var1] + beta.hat[length(beta.hat)]*1
  
  se.dy.dx0 <- sqrt(cov[var1,var1] + 0^2*cov[nrow(cov),ncol(cov)] + 2*0*cov[var1,ncol(cov)])
  se.dy.dx1 <- sqrt(cov[var1,var1] + 1^2*cov[nrow(cov),ncol(cov)] + 2*1*cov[var1,ncol(cov)])
  
  upr1 <- dy.dx1 + z*se.dy.dx1
  upr0 <- dy.dx0 + z*se.dy.dx0
  lwr1 <- dy.dx1 - z*se.dy.dx1
  lwr0 <- dy.dx0 - z*se.dy.dx0
  
  zx<-c(0,1)
  dy.dx<-c(dy.dx0,dy.dx1)
  upr<-c(upr0,upr1)
  lwr<-c(lwr0,lwr1)
  se.dy.dx<-c(se.dy.dx0,se.dy.dx1)
  
  plot(x=zx, y=dy.dx,type="n",xlim=c(min(zx)-.5,max(zx)+.5), xaxt="none",
       ylim=c(min(lwr),max(upr)),
       xlab = xlab,
       ylab = ylab,
       main = main)
  axis(side = 1, at = c(0,1))
  axis(side = 2, at = c(1,3,7,10))
  points(zx, dy.dx, `lwd` = me_lwd,pch=16,  lty = me_lty, col = me_col)
  #points(zx, lwr, lwd = ci_lwd, lty = ci_lty, col = ci_col)
  #points(zx, upr, lwd = ci_lwd, lty = ci_lty, col = ci_col)
  segments(x0=0,x1=0,y0=lwr0,y1=upr0)
  segments(x0=1,x1=1,y0=lwr1,y1=upr1)
  abline(h=0,lty=yint_lty,lwd=yint_lwd,col=yint_col)
}
