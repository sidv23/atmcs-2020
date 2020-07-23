setwd('/Users/vishwanathgl/Box/TDA/Presentations/ATMCS/images')

packages <- c('SyNet','spatstat','tidyverse','Rfast','animation')
sapply(packages,require,character.only=T)

get.dist = function(x)
{
  sqrt(x[1]^2 + x[2]^2)
}


get.pairs = function(N,p){
  if(p==3){
    M = matrix(0, nrow = N * (N - 1) * (N-2) /6, ncol = 3)
    x = 1:N
    k = 1
    
    for (i in head(x, -2))
    {
      for (j in (i + 1):(length(x)-1))
      {
        for(l in (j+1):(length(x)))
        {
          M[k, ] = c(i, j, l)
          k = k +1
        }
      }
    }
  }
  
  if(p==2){
    
    M = matrix(0, nrow = N * (N - 1)/2, ncol = 2)
    x = 1:N
    k = 1
    
    for (i in head(x, -1))
    {
      for (j in (i + 1):(length(x)))
      {
        M[k, ] = c(i, j)
        k = k +1
      }
    }
  }
  
  return(M)
}

get.dist <- function(D,pairs){
  p <- ncol(pairs)
  n <- nrow(pairs)
  M <- matrix(0,nrow=n,ncol=1)
  
  for(i in 1:n){
    if(p==3){
      M[i] <- max(D[pairs[i,1],pairs[i,2]],D[pairs[i,1],pairs[i,3]],D[pairs[i,2],pairs[i,3]])
      
    } else if(p==2){
      M[i] <- D[pairs[i,1],pairs[i,2]]
    }
  }
  return(M)
}


draw.graph <- function(n,d=2){
  set.seed(2020)
  pp <- rpoispp(500) %>% as.matrix.ppx()
  pp <- pp[1:n,]
  par(mar=c(1,1,1,1),oma=c(1,1,1,1))
  plot(pp,pch=20,col='orange',cex=2,xlim=c(0,1),ylim=c(0,1))
  N <- nrow(pp)
  
  p2 <- get.pairs(N,p=2)
  p3 <- get.pairs(N,p=3)
  
  D <- Rfast::Dist(pp)
  D2 <- get.dist(D,p2)
  D3 <- get.dist(D,p3)
  
  rn <- (n^(-1/d))
  
  sapply(which(D3<rn),function(i){polygon(pp[p3[i,],],col = alpha('dodgerblue',0.5))})
  sapply(which(D2<rn),function(i){j <- p2[i,1];k <- p2[i,2];
  segments(pp[j,1],pp[j,2],pp[k,1],pp[k,2],lwd=2)})
  points(pp,pch=21,col='black',cex=1.5,bg='orange',lwd=2)
}

# draw.graph <- function(n,d=2){
#   set.seed(2020)
#   pp <- rpoispp(500) %>% as.matrix.ppx()
#   pp <- pp[1:n,]
#   par(mar=c(1,1,1,1),oma=c(1,1,1,1))
#   plot(pp,pch=21,col='black',cex=1.5,bg='black',lwd=2,xlim=c(0,1),ylim=c(0,1))
#   N <- nrow(pp)
#   
#   p2 <- get.pairs(N,p=2)
#   p3 <- get.pairs(N,p=3)
#   
#   D <- Rfast::Dist(pp)
#   D2 <- get.dist(D,p2)
#   D3 <- get.dist(D,p3)
#   
#   rn <- (n^(-1/d))
#   
#   sapply(which(D3<rn),function(i){polygon(pp[p3[i,],],col = alpha('dodgerblue',0.5))})
#   sapply(which(D2<rn),function(i){j <- p2[i,1];k <- p2[i,2];
#   segments(pp[j,1],pp[j,2],pp[k,1],pp[k,2],lwd=2)
#   points(pp[p2[i,],],pch=21,col='black',cex=1.5,bg='orange',lwd=2)})
#   
# }

draw.graph(500,2)


simulate.points <- function(sq,d=2){
  for(i in 1:length(sq)){
    draw.graph(n=sq[i],d=d)
  }
}

sq <- seq(5,200,5)
sq <- seq(100,500,50)
simulate.points(sq,d=2)

grain <- 1
saveGIF(simulate.points(sq,d=2),
        movie.name = "thermodynamic.gif",convert="convert",
        # ani.dev = function(...){png(res=75*grain,...)},
        ani.width = 500*grain, ani.height=500*grain)

saveGIF(simulate.points(sq,d=1.5),
        movie.name = "sparse.gif",convert="convert",
        # ani.dev = function(...){png(res=75*grain,...)},
        ani.width = 500*grain, ani.height=500*grain)

saveGIF(simulate.points(sq,d=3),
        movie.name = "dense.gif",convert="convert",
        # ani.dev = function(...){png(res=75*grain,...)},
        ani.width = 500*grain, ani.height=500*grain)
