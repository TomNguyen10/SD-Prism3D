SDprism3D <- function(data, PriCol=NULL, sdCol=NULL) {
  
  require(rgl)
  require(plot3D)
  
  # take input
  if (is.null(PriCol)) {
    PriCol <- "lightblue"
  }
  
  if (is.null(sdCol)) {
    sdCol <- "red"
  }
  
  #calculate the data Standard Deviation
  SD <- sd(data)
  
  # Create a vector for pairwise deviation
  PD <- c()
  for (i in 1:(length(data)-1)) {
    for (j in (i+1):length(data)) {
      add <- data[i] - data[j]
      PD <- append(PD, abs(add))
    }
  }
  
  PD <- sort(PD)
  
  #create vectors for all coordinates
  x <- c()
  y <- c()
  z <- c()
  curStep = 0
  thelwd = 2
  
  #calculate the coordinates of the prisms 
  for (r in 1:length(PD)) {
    len <- PD[r]
    hyp <- len/2
    
    xAdd <- c(0,len,len,0,hyp,hyp)
    x <- c(x,xAdd)
    
    yAdd <- c(curStep/length(PD), curStep/length(PD),
              (curStep+1)/length(PD), (curStep+1)/length(PD),
              curStep/length(PD), (curStep+1)/length(PD))
    y <- c(y,yAdd)
    
    zAdd <- c(0,0,0,0,hyp,hyp)
    z <- c(z,zAdd)
    
    curStep <- curStep + 1
  }
  
  #calculate the coordinates of the standard deviation
  x <- c(x, 0, sqrt(2)*SD, sqrt(2)*SD, 0, sqrt(2)*SD/2, sqrt(2)*SD/2)
  y <- c(y, 0,0,1,1,0,1)
  z <- c(z, 0,0,0,0,sqrt(2)*SD/2,sqrt(2)*SD/2)
  
  #add dimension to create the interactive plot
  xdim <- PD[length(PD)]
  x <- c(x,xdim)
  ydim <- 1
  y <- c(y,ydim)
  zdim <- xdim
  z <- c(z,zdim)
  indices <- c(1:6)
  
  #create interactive plot
  s1 <- plot3d(x,y,z)
  
  #draw the prisms 
  for (c in 1:(length(PD)+1)) {
    if (c == length(PD)+1) {
      PriCol <- sdCol
      thelwd = 4
      segments3d(c(x[4],x[1]),c(y[4],y[1]),c(z[4],z[1]), lwd = thelwd, col = PriCol)
      
    }
    segments3d(c(x[1],x[2]),c(y[1],y[2]),c(z[1],z[2]), lwd = thelwd, col = PriCol)
    segments3d(c(x[2],x[3]),c(y[2],y[3]),c(z[2],z[3]), lwd = thelwd, col = PriCol)
    segments3d(c(x[3],x[4]),c(y[3],y[4]),c(z[3],z[4]), lwd = thelwd, col = PriCol)
    segments3d(c(x[1],x[5]),c(y[1],y[5]),c(z[1],z[5]), lwd = thelwd, col = PriCol)
    segments3d(c(x[5],x[2]),c(y[5],y[2]),c(z[5],z[2]), lwd = thelwd, col = PriCol)
    segments3d(c(x[5],x[6]),c(y[5],y[6]),c(z[5],z[6]), lwd = thelwd, col = PriCol)
    segments3d(c(x[6],x[3]),c(y[6],y[3]),c(z[6],z[3]), lwd = thelwd, col = PriCol)
    segments3d(c(x[6],x[4]),c(y[6],y[4]),c(z[6],z[4]), lwd = thelwd, col = PriCol)
    
    if (c < length(PD) + 1) {
      polygon3d(c(x[1],x[5],x[6],x[4]), c(y[1],y[5],y[6],y[4]), 
                c(z[1],z[5],z[6],z[4]), col = PriCol)
      polygon3d(c(x[5],x[2],x[3],x[6]), c(y[5],y[2],y[3],y[6]), 
                c(z[5],z[2],z[3],z[6]), col = PriCol)
    }
    
    x <- x[-indices]
    y <- y[-indices]    
    z <- z[-indices]
  }
}
  


