# Spatial holdout samples

spt_holdout <- function(xy, n = 10, mindist = 1, repeats = 1, time = NULL
                       , ttype = 'each')
{
  require(proxy)
  
  out <- list()
  
  if(is.null(time))
  {
    # Normal spatial cross validation without considering time
    out$holdout <- replicate(repeats, sample(nrow(xy), n), simplify = FALSE)
  } else {
    times <- unique(time)
    counts <- unname(table(time))
    out$holdout <- list()
    if(ttype == 'each')
    {
      # Create a repetitions for each time systematically
      time_element <- rep(times, each = repeats)
      for(i in 1:length(times))
      {
        maxn <- min(n, counts[i])
        out$holdout[[i]] <- replicate(repeats
                                      , sample(c(1:nrow(xy))[time == times[i]]
                                               , maxn)
                                      , simplify = FALSE)
      }
      out$holdout <- unlist(out$holdout, recursive = FALSE)
    } else {
      # Randomly choose a time for each repetition
      j <- sample(length(times), repeats, replace = TRUE)
      time_element <- times[j]
      for(i in 1:repeats)
      {
        maxn <- min(n, counts[j[i]])
        out$holdout[[i]] <- sample(c(1:nrow(xy))[time == time_element[i]], maxn)
      }
    }
  }
  
  # Coordinates for holdout points
  holdout_pts <- lapply(out$holdout, function(x)
    {
    out <- xy[x, ]
    return(out)
  })
  
  # Distances from holdout points to training points
  dists <- lapply(holdout_pts, function(x) {
    proxy::dist(x = xy, y = x)
  })
  
  if(is.null(time))
  {
    # Unselect training points closer than mindist to the holdout points
    out$train <- lapply(dists, function(x1)
    {
      out2 <- apply(x1, 1, FUN = function(x2)
      {
        out <- min(x2) > mindist
        return(out)
      })
      return(out2)
    })
  } else {
    # As above, but also remove all training points from the same year as the 
    # holdout points for the same repetition
    out$train <- list()
    for(i in 1:length(dists))
    {
      dist_ok <- apply(dists[[i]], 1, FUN = function(x)
      {
        out <- min(x) > mindist
        return(out)
      })
      time_ok <- time != time_element[i]
      out$train[[i]] <- dist_ok & time_ok
    }
  }
  
  # Select training points
  out$train <- lapply(out$train, function(x)
    {
    out <- c(1:nrow(xy))[x]
    return(out)
  })
  
  return(out)
}

# Not run

# Example with 1000 randomly distributed points from four different time slices

set.seed(401351)

x <- runif(1000)
y <- runif(1000)

xy <- cbind(x, y)

timevec <- rep(1:4, each = 250)

tcolors <- rep(c('red', 'blue', 'black', 'purple'), each = 250)


# 1: Random time slices

try1 <- spt_holdout(xy = xy, n = 30, mindist = 0.1, repeats = 6, time = timevec
                   , ttype = 'rand')

par(mfrow = c(2, 3), mai = c(0.6, 0.5, 0.1, 0.1))

for(i in 1:6)
{
  plot(xy[try1$holdout[[i]], ], asp = 1, xlim = c(0, 1), ylim = c(0, 1)
       , col = tcolors[try1$holdout[[i]]], pch = 16)

  points(xy[try1$train[[i]], ]
         , col = tcolors[try1$train[[i]]], bg = 'white', pch = 21)
}


# 1: Systematic time slices

try2 <- spt_holdout(xy = xy, n = 30, mindist = 0.1, repeats = 2, time = timevec
                , ttype = 'each')

par(mfrow = c(2, 4), mai = c(0.6, 0.5, 0.1, 0.1))

for(i in 1:8)
{
  plot(xy[try2$holdout[[i]], ], asp = 1, xlim = c(0, 1), ylim = c(0, 1)
       , col = tcolors[try2$holdout[[i]]], pch = 16)

  points(xy[try2$train[[i]], ]
         , col = tcolors[try2$train[[i]]], bg = 'white', pch = 21)
}

# END