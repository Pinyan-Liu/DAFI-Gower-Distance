# Function to compute modified Gower distance between two observations
gower.dist.modify <- function(data.x, data.y = data.x, rngs = NULL, KR.corr = TRUE,
                      var.weights = NULL, robcb=NULL){
  
  ####################    
  gower.fcn <- function(x, y, rng = NULL, KR.corr = TRUE) {
    nx <- length(x)
    ny <- length(y)
    cx <- class(x)
    cy <- class(y)
    delta <- matrix(1, nx, ny)
    if (!identical(cx, cy)) 
      stop("the x and y object are of different type")
    if (is.logical(x)) {
      dd <- abs(outer(X = x, Y = y, FUN = "-"))
      delta[outer(x == FALSE, y == FALSE, FUN = "&")] <- 0
      delta[outer(is.na(x), is.na(y), FUN = "|")] <- 0
    }
    else if (is.character(x) || (is.factor(x) && !is.ordered(x))) {
      if (is.factor(x) && !identical(levels(x), levels(y))) 
        stop("x and y have different levels")
      
      # Create a unified set of levels
      unified_levels <- union(levels(factor(x)), levels(factor(y)))
      
      # Create dummy variables using the unified levels
      x_dummies <- model.matrix(~ factor(x, levels = unified_levels) - 1)
      y_dummies <- model.matrix(~ factor(y, levels = unified_levels) - 1)
      
      
      # Initialize distance matrix for dummy variables
      dd_dummies <- matrix(0, nrow(x_dummies), nrow(y_dummies))
      
      # Calculate distance for each dummy variable
      for (i in 1:ncol(x_dummies)) {
        #p <- mean(x_dummies[, i], na.rm = TRUE)
        #std_dev <- sqrt(p * (1 - p))
        scaled_x <- x_dummies[, i] 
        scaled_y <- y_dummies[, i] 
        dd_dummies <- dd_dummies + abs(outer(scaled_x, scaled_y, FUN = "-"))
      }
      
      # Scale the summed distances
      # Each variable will be x, and calculate with one of the rest y
      dd <- dd_dummies / ncol(x_dummies)
      # Apply a softening function like sigmoid
      # sigmoid <- function(x) 1 / (1 + exp(-x))
      # dd <- sigmoid(dd_dummies)
      delta[outer(is.na(x), is.na(y), FUN = "|")] <- 0
    }
    else if (is.ordered(x)) {
      if (KR.corr) {
        x <- as.numeric(x)
        y <- as.numeric(y)
        if (is.null(rng) || is.na(rng)) 
          rng <- max(x, y, na.rm = TRUE) - 1
        if(rng==0) {
          dd <- matrix(0, nx, ny)
          delta[outer(is.na(x), is.na(y), FUN = "|")] <- 0
        }    
        else{
          zx <- (x - 1)/rng
          zy <- (y - 1)/rng
          dd <- abs(outer(X = zx, Y = zy, FUN = "-"))/(max(zx, 
                                                           zy, na.rm=TRUE) - min(zx, zy, na.rm=TRUE))
          delta[outer(is.na(zx), is.na(zy), FUN = "|")] <- 0
        }
      }
      else {
        x <- as.numeric(x)
        y <- as.numeric(y)
        if (is.null(rng) || is.na(rng)) 
          rng <- max(x, y, na.rm=TRUE) - 1
        if(rng==0) dd <- matrix(0, nx, ny)
        else dd <- abs(outer(X = x, Y = y, FUN = "-"))/rng
        delta[outer(is.na(x), is.na(y), FUN = "|")] <- 0
      }
    }
    else {
      if (is.null(rng) || is.na(rng)) rng <- max(x, y, na.rm=TRUE) - min(x, y, na.rm=TRUE)
      if(!is.null(robcb)){
        if(tolower(robcb)=="iqr") {
          rng <- IQR(x=c(x,y), na.rm = TRUE)
        }
        if(tolower(robcb)=="idr") {
          rng <- c(quantile(x = c(x,y), probs=0.9, na.rm=TRUE) - 
                     quantile(x = c(x,y), probs=0.1, na.rm=TRUE))
        }
      }  
      
      if(rng==0) dd <- matrix(0, nx, ny)
      else dd <- abs(outer(X = x, Y = y, FUN = "-"))/rng
      dd[dd>1] <- 1
      
      delta[outer(is.na(x), is.na(y), FUN = "|")] <- 0
    }
    list(dist = dd, delta = delta)
  }
  
  ######## END  gower.fcn() ###################       
  
  if (is.null(dim(data.x)) && is.null(dim(data.y))) {
    out.gow <- gower.fcn(x = data.x, y = data.y, rng = rngs, 
                         KR.corr = KR.corr)
    out <- (out.gow$dist * out.gow$delta)/out.gow$delta
  }
  else if (is.null(dim(data.x)) && !is.null(dim(data.y))) {
    p <- ncol(data.y)
    if (length(data.x) != p) 
      stop("data.x should be of the same length of the no. of cols of data.y")
    num <- array(0, c(1, nrow(data.y)))
    den <- array(0, c(1, nrow(data.y)))
    if(is.null(var.weights)) var.weights <- rep(1, p)
    for (k in 1:p) {
      
      if (is.null(rngs)) 
        rng.k <- NULL
      else rng.k <- rngs[k]
      w.k <- var.weights[k]
      out.gow <- gower.fcn(x = data.x[, k], y = data.y[,k],
                           rng = rng.k, KR.corr = KR.corr)
      n <- out.gow$dist * out.gow$delta * w.k
      
      n[is.na(n)] <- 0
      num <- num + n
      d <- out.gow$delta * w.k
      d[is.na(d)] <- 0
      den <- den + d
    }
    out <- num/den
  }
  else {
    p <- ncol(data.y)
    if (ncol(data.x) != p) 
      stop("data.x and data.y must have the same no. of cols")
    num <- array(0, c(nrow(data.x), nrow(data.y)))
    den <- array(0, c(nrow(data.x), nrow(data.y)))
    if(is.null(var.weights)) var.weights <- rep(1, p)
    for (k in 1:p) {
      
      if (is.null(rngs)) 
        rng.k <- NULL
      else rng.k <- rngs[k]
      w.k <- var.weights[k]
      out.gow <- gower.fcn(x = data.x[, k], y = data.y[, k], rng = rng.k,
                           KR.corr = KR.corr)
      n <- out.gow$dist * out.gow$delta * w.k
      
      n[is.na(n)] <- 0
      num <- num + n
      d <- out.gow$delta * w.k
      d[is.na(d)] <- 0
      den <- den + d
    }
    out <- num/den
  }
  out
}    

