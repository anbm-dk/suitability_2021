# Spatial holdout samples

sp_holdout <- function(xy, n = 10, mindist = 1, repeats = 1)
{
  require(proxy)
  
  out <- list()
  
  out$holdout <- replicate(repeats, sample(nrow(xy), n), simplify = FALSE)
  
  holdout_pts <- lapply(out$holdout, function(x)
    {
    out <- xy[x, ]
    return(out)
  })
  
  dists <- lapply(holdout_pts, function(x) {
    proxy::dist(x = xy, y = x)
  })
  
  out$train <- lapply(dists, function(x1)
  {
    out2 <- apply(x1, 1, FUN = function(x2)
    {
      out <- min(x2) > mindist
      return(out)
    })
    return(out2)
  })
  out$train <- lapply(out$train, function(x)
    {
    out <- c(1:nrow(xy))[x]
    return(out)
  })
  
  return(out)
}

# Not run

set.seed(1)

x <- runif(1000)
y <- runif(1000)

xy <- cbind(x, y)

try <- sp_holdout(xy = xy, n = 30, mindist = 0.1, repeats = 6)

par(mfrow = c(2, 3), mai = c(0.6, 0.5, 0.1, 0.1))

for(i in 1:6)
{
  plot(xy[try$holdout[[i]], ], asp = 1, xlim = c(0, 1), ylim = c(0, 1))

  points(xy[try$train[[i]], ], col = 'red')
}

# END