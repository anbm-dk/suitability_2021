library(ParallelLogger)
library(snow)
library(foreach)
library(doSNOW)

logFileName <- "log.txt"

unlink(logFileName) # Clean up log file from the previous example

clearLoggers() # Clean up the loggers from the previous example

addDefaultFileLogger(logFileName)

cluster <- makeCluster(3)

# fun <- function(x) {
#   ParallelLogger::logInfo("The value of x is ", x)
#   # Do something
#   if (x == 6)
#     ParallelLogger::logDebug("X equals 6")
#   return(NULL)
# }

fun <- function(x) {
  if(x == 5)
  {
    a <- b
  }
  x
}

# dummy <- clusterApply(cluster, 1:10, fun, progressBar = FALSE)

registerDoSNOW(cluster)

dummy <- foreach(x = 1:10) %dopar% 
    {
      fun(x)
    }
stopCluster(cluster)
registerDoSEQ()

writeLines(readChar(logFileName, file.info(logFileName)$size))
