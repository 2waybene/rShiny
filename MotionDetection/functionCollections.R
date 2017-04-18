
##  functions
aoro <- function(x1,x2,x3,y1,y2,y3){
  a <- sqrt((x2-x1)^2 + (y2-y1)^2)
  b <- sqrt((x3-x2)^2 + (y3-y2)^2)
  c <- sqrt((x3-x1)^2 + (y3-y1)^2)
  angle <- pi - acos((a^2+b^2-c^2)/(2*a*b))
  angle <- (180*angle)/pi
  m <- (y2-y1)/(x2-x1)
  b <- y1 - m*x1
  if(x2 > x1 & y2 > y1){
    if(y3 <= m*x3 + b){
      return(angle)
    }
    else {
      return(-angle)
    }
  }
  if(x2 > x1 & y2 < y1){
    if(y3 <= m*x3 + b){
      return(angle)
    }
    else {
      return(-angle)
    }
  }
  if(x2 < x1 & y2 > y1){
    if(y3 >= m*x3 + b){
      return(angle)
    }
    else {
      return(-angle)
    }
  }
  if(x2 < x1 & y2 < y1){
    if(y3 >= m*x3 + b){
      return(angle)
    }
    else {
      return(-angle)
    }
  }
  if(x2 < x1 & y2 == y1){
    if(y3 >= m*x3 + b){
      return(angle)
    }
    else {
      return(-angle)
    }
  }
  if(x2 > x1 & y2 == y1){
    if(y3 <= m*x3 + b){
      return(angle)
    }
    else {
      return(-angle)
    }
  }
  if(x2 == x1 & y2 > y1){
    if(x3 > x2){
      return(angle)
    }
    else {
      return(-angle)
    }
  }
  if(x2 == x1 & y2 < y1){
    if(x3 < x2){
      return(angle)
    }
    else {
      return(-angle)
    }
  }
}



aoflip <- function(x1,x2,x3,y1,y2,y3){
  a <- sqrt((x2-x1)^2 + (y2-y1)^2)
  b <- sqrt((x3-x2)^2 + (y3-y2)^2)
  c <- sqrt((x3-x1)^2 + (y3-y1)^2)
  angle <- pi - acos((a^2+b^2-c^2)/(2*a*b))
  if(is.na(angle) == TRUE){
    return(0)
  }
  else{
    angle <- (180*angle)/pi
    m <- (y2-y1)/(x2-x1)
    b <- y1 - m*x1
    
    # case I: moving right --> down, then up/down
    if(x2 > x1 & y2 > y1){
      if(y3 <= m*x3 + b){
        return(-angle)
      }
      else {
        return(angle)
      }
    }
    
    # case II: moving right --> up, then up/down
    if(x2 > x1 & y2 < y1){
      if(y3 <= m*x3 + b){
        return(-angle)
      }
      else {
        return(angle)
      }
    }
    
    # case III: moving left --> down, then up/down
    if(x2 < x1 & y2 > y1){
      if(y3 >= m*x3 + b){
        return(-angle)
      }
      else {
        return(angle)
      }
    }
    
    #case IV: moving left --> up, then up/down
    if(x2 < x1 & y2 < y1){
      if(y3 >= m*x3 + b){
        return(-angle)
      }
      else {
        return(angle)
      }
    }
    
    #case V: moving left --> flat, then up/down
    if(x2 < x1 & y2 == y1){
      if(y3 >= y1 ){
        #if(y3 >= m*x3 + b){
        return(-angle)
      }
      else {
        return(angle)
      }
    }
    
    #case VI: moving right --> flat, then up/down
    if(x2 > x1 & y2 == y1){
      if(y3 <= y1){
        #if(y3 <= m*x3 + b){
        return(-angle)
      }
      else {
        return(angle)
      }
    }
    
    #cast VII: moving down --> straight, then left/right
    if(x2 == x1 & y2 > y1){
      if(x3 > x2){
        return(-angle)
      }
      else {
        return(angle)
      }
    }
    
    #case VIII: moving up --> straight, then left/right 
    if(x2 == x1 & y2 < y1){
      if(x3 < x2){
        return(-angle)
      }
      else {
        return(angle)
      }
    }
  }
}





myREVaxis <-  function (x, y, xrev = FALSE, yrev = TRUE, xside = if (yrev) 3 else 1, 
                        yside = if (xrev) 4 else 2, xlab = NULL, ylab = NULL, main = NULL, bty = NULL, 
                        ...) 
{
  xname <- if (is.null(xlab)) 
    deparse(substitute(x))
  else xlab
  yname <- if (is.null(ylab)) 
    deparse(substitute(y))
  else ylab
  xlab <- if (yrev) 
    ""
  else xname
  ylab <- if (xrev) 
    ""
  else yname
  y1 <- if (yrev) 
    -y
  else y
  x1 <- if (xrev) 
    -x
  else x
  
  old.mar <- par()$mar
  on.exit(par(mar = old.mar))
  par(mar = old.mar[c(xside, yside, 4 - xside, 6 - yside)])
  
  
  xrange <- (max(x1) - min(x1))*0.1
  yrange <- (max(y1) - min(y1))*0.1
  
  plot(x1, y1, xlim = c(min(x1)- xrange, max(x1) + xrange), ylim = c(min(y1)- yrange, max(y1) + yrange),
       axes = FALSE, xlab = xlab, ylab = ylab, ...)
  
#  plot(x1, y1, main = title, axes = FALSE)
  
  if (xrev) {
    axis(xside, at = pretty(-x), labels = rev(pretty(x)))
    mtext(side = yside, line = 2, text = yname)
  }
  else axis(xside)
  
  if (yrev) {
    axis(yside, at = pretty(-y), labels = rev(pretty(y)), 
         srt = 90)
    mtext(side = xside, line = 3, text = xname)
  }
  else axis(yside)
  
  if (!is.null(bty)) 
    box(bty = bty)
  invisible()
}

computeAngles <- function (dt)
{
  angles <- c("NA", "NA")
  signs  <- c("NA", "NA")

  for (k in 1: (length(dt[,1]) -2 ))
  {
    temp.result <- aoflip(dt[k,1],dt[k+1,1],dt[k+2,1],dt[k,2],dt[k+1,2],dt[k+2,2])    
    if (temp.result >= 0) {
      temp.sign = "Pos"
    }else{
      temp.sign = "Neg"
      temp.result = -temp.result
    }
    temp.list <- list (Angle = temp.result, Direction = temp.sign)

    angles[k+2] = temp.result
    signs [k+2] = temp.sign
  }
  out.dt <- cbind (dt, as.data.frame (list (Angle = angles, Direction = signs)))
  return(out.dt)
}

