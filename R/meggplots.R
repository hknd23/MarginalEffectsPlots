#' @export
meggplot <- function(model,var1,var2,ci=.95,
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
  ggplot2::ggplot(data=NULL,ggplot2::aes(x=z0, y=dy.dx)) + ggplot2::theme_bw()+
    ggplot2::labs(x=xlab,y=ylab,title=main) +
    ggplot2::geom_line(ggplot2::aes(z0, dy.dx),size = me_lwd, 
              linetype = me_lty, 
              color = me_col) +
    ggplot2::geom_line(ggplot2::aes(z0, lwr), size = ci_lwd, 
              linetype = ci_lty, 
              color = ci_col) +
    ggplot2::geom_line(ggplot2::aes(z0, upr), size = ci_lwd, 
              linetype = ci_lty, 
              color = ci_col) +
    ggplot2::geom_hline(yintercept=0,linetype=yint_lty,
               size=yint_lwd,
               color=yint_col)
}

#' @export
meggplotdum <- function(model,var1,var2,ci=.95,
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
  
  ggplot2::ggplot(data=NULL,ggplot2::aes(x=zx, y=dy.dx)) + ggplot2::theme_bw()+
    ggplot2::scale_x_discrete(limits = 0:1)+
    ggplot2::labs(x=xlab,y=ylab,title=main)+ 
    ggplot2::geom_point(mapping= ggplot2::aes(zx, dy.dx), color = me_col,size = me_lwd)+
    ggplot2::geom_segment(mapping=ggplot2::aes(x=0,xend=0,y=lwr0,yend =upr0))+
    ggplot2::geom_segment(mapping=ggplot2::aes(x=1,xend=1,y=lwr1,yend =upr1))+
    ggplot2::geom_hline(yintercept=0,linetype=yint_lty,
               size=yint_lwd,
               color=yint_col)}

#' @export
meggplotord <- function(model,var1,var2,ci=.95,
                        xlab=var2,ylab=paste("Marginal Effect of",var1),
                        main="Marginal Effect Plot",
                        me_lty=1,me_lwd=3,me_col="black",
                        ci_lty=1,ci_lwd=1,ci_col="black",
                        yint_lty=2,yint_lwd=1,yint_col="black"){
  alpha <- 1-ci
  z <- qnorm(1-alpha/2)
  beta.hat <- coef(model)
  cov <- vcov(model)
  z0 <- seq(min(model$model[,var2],na.rm=T),max(model$model[,var2],na.rm=T),length.out=50)
  dy.dx <- beta.hat[var1] + beta.hat[length(beta.hat)]*z0
  se.dy.dx <- sqrt(cov[var1,var1] + z0^2*cov[nrow(cov),ncol(cov)] + 2*z0*cov[var1,ncol(cov)])
  upr <- dy.dx + z*se.dy.dx
  lwr <- dy.dx - z*se.dy.dx
  ggplot2::ggplot(data=NULL,ggplot2::aes(x=z0, y=dy.dx)) + ggplot2::theme_bw()+
    ggplot2::labs(x=xlab,y=ylab,title=main) +
    ggplot2::geom_point(ggplot2::aes(z0, dy.dx),size = me_lwd, 
                        color = me_col) +
    ggplot2::geom_segment(mapping=ggplot2::aes(x=z0,xend=z0,y=lwr,yend =upr))
}
