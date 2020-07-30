fun <- function(x){0.35*dnorm(x,-temp+1,2)+0.7*dnorm(x,temp-2,5)}
par(mar=c(2,3,1,1))
temp <- 6
cn <- 3
xseq <- seq(-cn*temp,cn*temp,length.out = 1000)
yseq <- sapply(xseq,fun)
plot(xseq,yseq,type="l",lwd=5,ylab=TeX('$f(x)$'),axes=F,ylim=c(0.0025,0.08))
axis(2, at=seq(0,0.08,by=0.02), las=2)
axis(1, at=seq(-cn*temp,cn*temp,length.out = 7), las=1)

text(x=14+3,y=fun(14),TeX("$f(x)$"),cex=1.5)



# t <- 0.03
# text(x=0,y=0.005,TeX("$\\hat{f}(t_1)$"),cex=1.5)
# abline(h=t,lty=3)
# text(x=-14.5,y=t*1.07,TeX(paste("$t_1=$",t)),cex=1.2)
# 
# x <- which(yseq>=t)
# 
# polygon(c(xseq[x],rev(xseq[x])),
#         c(yseq[x],rep(-0.0004,length(x))),
#         lwd=0.001,col=alpha('dodgerblue',0.5))





t <- 0.045
text(x=0,y=0.005,TeX("$\\hat{f}(t_2)$"),cex=1.5)
abline(h=t,lty=3)
text(x=-14.5,y=t*1.05,TeX(paste("$t_1=$",t)),cex=1.2)

x <- which(yseq>=t)
brk <- which(x[2:length(x)]-x[1:(length(x)-1)] > 1)
x[1:brk]
x[(brk+1):length(x)]

polygon(c(xseq[x[1:brk]],rev(xseq[x[1:brk]])),
        c(yseq[x[1:brk]],rep(-0.0004,length(x[1:brk]))),
        lwd=0.001,col=alpha('dodgerblue',0.5))

polygon(c(xseq[x[(brk+1):length(x)]],rev(xseq[x[(brk+1):length(x)]])),
        c(yseq[x[(brk+1):length(x)]],rep(-0.0004,length(x[(brk+1):length(x)]))),
        lwd=0.001,col=alpha('dodgerblue',0.5))


# 
# t <- 0.06
# text(x=0,y=0.005,TeX("$\\hat{f}(t_3)$"),cex=1.5)
# abline(h=t,lty=3)
# text(x=-14.5,y=t*1.04,TeX(paste("$t_3=$",t)),cex=1.2)
# 
# x <- which(yseq>=t)
# 
# polygon(c(xseq[x],rev(xseq[x])),
#         c(yseq[x],rep(-0.0004,length(x))),
#         lwd=0.001,col=alpha('dodgerblue',0.5))
# 


lines(xseq,yseq,type="l",lwd=5,ylab=TeX('$f(x)$'),axes=F)

