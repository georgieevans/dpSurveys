## One hot encoding of response vector 
oneHot <- function(respondent_vector, unique_combn) {
  # ----------------------------------------------
  # respondent_vector: a respondents answer vector
  # unique_combn: matrix of response combinations
  # Returns one-hot encoded vector
  # ----------------------------------------------
  
  # Initialise vector of 0s
  one_hot <- rep(0, nrow(unique_combn))
  # Find position of 1 and replace
  index <- prodlim::row.match(respondent_vector, unique_combn)
  one_hot[index] <- 1
  
  # Return one_hot vector
  return(one_hot)
}


## Device DP

# Sample n draws Uniform[0,1) using OpenSSL
customUniform <- function(n) {
  return(openssl::rand_num(n))
}

# Inversion of sequential search algorithm 
# pg. 505 of http://www.eirene.de/Devroye.pdf
customPoisson <- function(lambda) {
  X <- 0
  sum <- exp(-lambda)
  prod <- exp(-lambda)
  u <- customUniform(n = 1)
  
  while (u > sum) {
    X = X + 1
    prod = prod * lambda/X
    sum = sum + prod
  }
  return(X)
}


# GS algorithm from Ahrens and Dieter 1974
# pg. 425 of http://www.eirene.de/Devroye.pdf
# note alpha = k, beta = 1/theta
# NOTE: valid only for alpha <= 1, this is fine for our use case because alpha = 1/n
# NOTE: the GS algorithm as originally written generates samples from a Gamma(alpha, 1), which can 
#       be turned into samples from a Gamma(alpha, beta) by dividing the return 
#       by beta (as we do in this function)
customGamma <- function(alpha, beta) {
  b <- (exp(1)+alpha) / exp(1)
  c <- 1 / alpha 
  while (TRUE) {
    u <- customUniform(n = 1)
    w <- customUniform(n = 1)
    v <- b * u 
    
    if (v <= 1) {
      x <- v**c
      if (w <= exp(-x)) {
        return(x/beta)
      } 
    } else {
      x <- -log(c * (b-v))
      if (w <= x**(alpha-1)) {
        return(x/beta)
      }
    } 
  }
}


# Function to draw from a Polya distribution 
polya <- function(n, epsilon) {

  # Convert epsilon to alpha 
  alpha <- 1/(exp(epsilon))
  # Draw from gamma distribution 
  lambda <- customGamma(1/n, (1-alpha)/alpha) 
  # Return one draw from pois(lambda)
  return(customPoisson(lambda))
  
}

# Function to take K differences between two draws from a polya
deviceNoise <- function(K, n, epsilon){
  # ------------------------------------------------
  # K: Number of possible answer combinations 
  # n: Total number of respondents
  # epsilon: Privacy parameter
  # ------------------------------------------------
  
  # returns K length vector of noise 
  replicate(K, polya(n, epsilon) - polya(n,epsilon))
  
}

# Main function adds noise to each element of one hot encoding 
deviceDP <- function(one_hot, n, epsilon) {
  # ------------------------------------------------
  # one_hot: One respondents private one hot
  # n: Total number of respondents
  # epsilon: Privacy parameter
  # ------------------------------------------------
  
  # Add polya - polya noise 
  one_hot_dp <- one_hot + deviceNoise(K = length(one_hot), n = n, epsilon)
  
  # Returns DP one hot encoding 
  cbind(which(one_hot_dp != 0), one_hot_dp[which(one_hot_dp != 0)])
  
}

### Aggregate individual data into histogram on server

dpHistogram <- function(one_hot_list, unique_combn) {
  # ------------------------------------------------
  # one_hot_list: List of outputs from deviceDP
  # unique_combn: matrix of response combinations
  # --------------------------------------------- ---
  
  # Caculate private counts
  dp_df <- as.data.frame(do.call(rbind, one_hot_list))
  
  dp_count <- aggregate(dp_df$V2, by = list(position = dp_df$V1), sum)
  unique_combn$n_response <- 0
  unique_combn$n_response[as.numeric(dp_count$position)] <- dp_count$x
  
  # Return count associated with each response combination
  return(unique_combn)
}







