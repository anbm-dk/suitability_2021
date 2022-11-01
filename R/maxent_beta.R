# Adjust beta for maxent

make.beta <- function(cells, fields)
{
  make_fcc <- function(n1)
  {
    out <- "L"
    if(n1 > 9)
    {
      out <- "LQ"
    }
    if(n1 > 14) {
      out <- "LQH"
    }
    if(n1 > 79) {
      out <- "LQHPT"
    }
    return(out)
  }
  fcc <- make_fcc(fields)
  
  beta_mult <- sqrt(cells/2)/sqrt(fields)
  
  args_out <- ENMeval::make.args(RMvalues = beta_mult, fc = fcc)[[1]]
  
  # # beta_threshold
  # beta_threshold1 <- NULL
  # if(fields > 79)
  #   beta_threshold1 <- 2 - fields/100
  # if(fields > 99)
  # {
  #   beta_threshold1 <- 1
  # }
  # if(!is.null(beta_threshold1))
  # {
  #   args_out <- c(args_out, paste0('beta_threshold=', beta_threshold1))
  # }
  # 
  # # beta_categorical
  # beta_categorical1 <- NULL
  # if(fields > 14)
  # {
  #   beta_categorical1 <- 0.5 - 0.25*(fields - 10)/7
  # }
  # if(fields > 16)
  # {
  #   beta_categorical1 <- 0.25
  # }
  # if(!is.null(beta_categorical1))
  # {
  #   args_out <- c(args_out, paste0('beta_categorical=', beta_categorical1))
  # }
  # 
  # # beta_lqp
  # beta_lqp1 <- 1
  # if(fields > 9)
  # {
  #   beta_lqp1 <- 0.8 - 0.3*(fields - 10)/7
  # }
  # if(fields > 17)
  # {
  #   beta_lqp1 <- 0.5 - 0.25*(fields - 17)/13
  # }
  # if(fields > 30)
  # {
  #   beta_lqp1 <- 0.25 - 0.2*(fields - 30)/70
  # }
  # if(fields > 79)
  # {
  #   beta_lqp1 <- 0.55 - 0.5*(fields - 30)/70
  # }
  # if(fields > 99)
  # {
  #   beta_lqp1 <- 0.05
  # }
  # args_out <- c(args_out, paste0('beta_lqp=', beta_lqp1))
  
  return(args_out)
}

# END