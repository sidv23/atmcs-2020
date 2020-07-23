packages <- c("rgl","ggplot2","plot3D")
lapply(packages,require,character.only=T)
spher2cart <- function(r, theta, phi) {
  
  x <- r * sin(theta) * cos(phi)
  y <- r * sin(theta) * sin(phi)
  z <- r * cos(theta)
  
  return(list(x = x, y = y, z = z))
}

len <- 10
theta <- seq(0, pi, length = len)
phi <- seq(0, 2*pi, length = len)
M <- mesh(theta, phi)
names(M) <- c("theta", "phi")

r1 <- 1 + 1*cos(2 * M$theta)
r2 <- 1 + 3*cos(2 * M$theta)


cart1 <- spher2cart(r1, M$theta, M$phi)
cart2 <- spher2cart(r2, M$theta, M$phi)



df <- data.frame(matrix(NA,nrow=len*len,ncol=3))
colnames(df) <- c("x","y","z")

for(i in 1:len){
  for(j in 1:len){
    df$x[i+(len*(j-1))] <- cart1$x[i,j]
    df$y[i+(len*(j-1))] <- cart1$y[i,j]
    df$z[i+(len*(j-1))] <- cart1$z[i,j]
  }
}



cart11 <- carty
cart11$x <- carty$x+a*rnorm(len*len)
cart11$y <- carty$y+a*rnorm(len*len)
cart11$z <- carty$z+a*rnorm(len*len)


r3dDefaults$windowRect <- c(0,0,0,0) 
r3dDefaults$windowRect <- c(0,0,0,0) 
par(mar = c(0,0,0,0),oma = c(0,0,0,0),mfrow=c(1,1), mgp=c(-10,-10,-10), tcl=6)

scatter3D(cart11$x, cart11$y, cart11$z, border = "black",cex=1,
          colkey = FALSE, bty = "f",col="red",pch=20,
          phi = 20, theta = 35,windowRect = c(10,10,10,10))

surf3D(cart1$x, cart1$y, cart1$z, border = "black",cex=1,
       colkey = NULL, bty = "f",pch=20,col = "lightblue",
       phi = 20, theta = 35,windowRect = c(10,10,10,10),add=T)

M <- as.matrix(df)

library(TDA)
maxdimension <- 2
maxscale <- 1 
Diag <- ripsDiag(X = M, maxdimension, maxscale, library = "GUDHI", printProgress = TRUE)

p0 <-  sum((Diag[["diagram"]][,1] ==0)*1)
p1 <-  sum((Diag[["diagram"]][,1] ==1)*1)
p1 <-  sum((Diag[["diagram"]][,1] ==2)*1)
cls <- c(rep("dodgerblue",p0),rep("firebrick3",p1),rep("green",p1))


TDA::plot.diagram(Diag[["diagram"]], barcode = TRUE, axes=T,cex.main=0.8,col=cls)
title(paste("Persistence Barcode"),line = 0.5, cex.main=1)


TDA::plot.diagram(Diag[["diagram"]],cex.main=0.8,col=cls)
title(paste("Persistence Diagram"),line = 0.5, cex.main=1)


TDA::plot.diagram(Diag[['diagram']][Diag[["diagram"]][,1] ==0,],
                  cex.main=0.8,col='dodgerblue')

TDA::plot.diagram(Diag[['diagram']][Diag[["diagram"]][,1] ==1,],
                  cex.main=0.8,col='firebrick3')

TDA::plot.diagram(Diag[['diagram']][Diag[["diagram"]][,1] ==2,],
                  cex.main=0.8,col='green')

dgm <- gridDiag(X=M,FUN=distFct,maxdimension = 2,by=0.01,lim=c(-2,2,-2,2,-2,2))







##### ROBUSTNESS

set.seed(7)
X <- TDA::circleUnif(50)
x.out <- rbind(0.3*runif(2),2*runif(2))
X <- rbind(X,x.out)
par(pty="s",mar = c(0,0,0,0),oma = c(0,0,0,0),mfrow=c(1,1))
graphics::plot(X,asp=1,pch=20,col='red',xlab="",ylab="",
               xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),xaxt="n", yaxt="n")
points(x.out,pch=20,col='green')


graphics::plot(X,asp=1,pch=20,col='red',xlab="",ylab="",
               xlim=c(-1.5,1.5),ylim=c(-1.5,1.5),xaxt="n", yaxt="n")
points(x.out,pch=20,col='green')
r <- 0.35; plot.circles(X[,1],X[,2],r); plot.segments(X[,1],X[,2],2*r); 
points(X,pch=20,col='red')
points(x.out,pch=20,col='green')


maxdimension <- 1
maxscale <- 2
Diag <- ripsDiag(X=X, maxdimension, maxscale, library = "Dionysus", 
                 printProgress = TRUE,location = TRUE)
p0 <-  sum((Diag[["diagram"]][,1] ==0)*1)
p1 <-  sum((Diag[["diagram"]][,1] ==1)*1)
cls <- c(rep("dodgerblue",p0),rep("firebrick3",p1))
TDA::plot.diagram(Diag[['diagram']])





