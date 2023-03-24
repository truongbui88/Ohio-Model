library(zoo)

#Present Value function (for constant payments)
pv = function(rate, nper, pmt) {
  PV = (1 + rate)*pmt * (1 - (1 + rate) ^ (-nper)) / rate
  return(PV)
}

#NPV function
npv = function(rate, cashflows) {
  for(i in 1:length(cashflows)){
    if(i == 1){
      NPV <- cashflows[i]/((1+rate)^(i))
    } else {
      NPV <- NPV + cashflows[i]/((1+rate)^(i))
    }
  }
  
  return(NPV)
}

#Amo payment functions
#pmt0 = basic amo payment calculation, assuming payment beginning of period 
PMT0 <- function(r, nper, pv) {
  if (r == 0) {
    a <- pv/nper
  } else {
    a <- pv*r*(1+r)^(nper-1)/((1+r)^nper-1)  
  }
  
  # if(nper == 0){
  #   a <- 0
  # }
  
  return(a)
}

#pmt = amo payment function with growth rate and timing added; t = 1 for end of period payment, 0.5 for half period, and 0 for beginning of period. 
PMT <- function(r, g = 0, nper, pv, t = 1) {
  a <- PMT0((1+r)/(1+g) - 1, nper, pv*(1+r)^t)
  return(a)
}

#Funding period function
NPER <- function(r,g,pv,t,pmt){
  PV <- pv*(1+r)^t
  R <- (1+r)/(1+g) - 1
  TempValue <- PV*R/(pmt*(1+R))
  NPER <- -log(1-TempValue,(1+R))
  if(is.infinite(NPER)) {NPER <- 100}
  if(is.nan(NPER)) {NPER <- 100}
  return(NPER)
}

# NPER(0.07, g = 0, pv = 5000, t = 1, pmt = 1000)

#Cumulative future values function
cumFV <- function(interest, cashflow){
  cumvalue <- double(length = length(cashflow))
  for (i in 2:length(cumvalue)) {
    cumvalue[i] <- cumvalue[i - 1]*(1 + interest) + cashflow[i - 1]
  }
  return(cumvalue)
}


#Geometric average return function
geo_return <- function(x, na.rm = F) {
  if (na.rm == T) {
    x = na.omit(x)
  }
  avg_return <- prod(1+x)^(1/length(x)) - 1
  return(avg_return)
}


#Estimate geometric average return from arithmetic return and standard deviation (annualized)
est_geo_return <- function(mean_return, sd_return){
  geo_return <- mean_return - sd_return^2/(2*(1+mean_return)) 
  return(geo_return)
}


#Estimate arithmetic return from geometric return and standard deviation (annualized)
root_geo_return <- function(mean_return, sd_return, geometric_return){
  est_geo_return(mean_return, sd_return) - geometric_return
}

est_arith_return <- function(geometric_return, sd_return){
  x <- uniroot(f = root_geo_return,
          interval = c(-1,1),
          geometric_return = geometric_return,
          sd_return = sd_return)
  return(x$root)
}


#Function to determine the interest crediting rate of a cash balance plan
smooth_return <- function(returns, floor, cap, upside_share) {
  x <- min(max(floor, floor + upside_share * (geo_return(returns) - floor)), cap)
  return(x)
}


expected_icr <- function(geometric_return, sd_return, smooth_period, floor, cap, upside_share, n_periods = 30, n_simulations = 10000) {
  mean_return <- est_arith_return(geometric_return, sd_return)
  simulated_returns <- matrix(rnorm(n_simulations * n_periods, mean = mean_return, sd = sd_return), nrow = n_periods, ncol = n_simulations)
  initial_returns <- matrix(floor, nrow = smooth_period - 1, ncol = n_simulations)
  returns_matrix <- rbind(initial_returns, simulated_returns)
  smooth_returns <- rollapply(returns_matrix, width = smooth_period, align = "right", FUN = smooth_return, floor = floor, cap = cap, upside_share = upside_share)
  avg_interest_rates <- apply(smooth_returns, 2, FUN = geo_return)
  exp_interest_rate <- median(avg_interest_rates)
  
  return(exp_interest_rate)
}

# expected_icr(geometric_return = 0.08, sd_return = 0.16, smooth_period = 5, floor = 0.06, cap = 0.1, upside_share = 0.7, n_periods = 30)


#Adding new entrants function
add_new_entrants <- function(g, ne_dist, wf1, wf2, ea, age, position_matrix){
  #g is the assumed population growth of the plan
  #ne_dist is a vector representing the distribution of new entrants for each entry age
  #wf1 is the population in period 1. wf2 is the wf1 population after decremented
  #ea and age are two vectors representing entry age and age for active members
  #position_matrix is the matrix that rearranges the new entrant numbers in the right positions to be added to the active workforce array
  ne <- sum(wf1)*(1 + g) - sum(wf2)
  ne_vec <- ne * ne_dist
  ne_matrix <- matrix(ne_vec, nrow = length(ea), ncol = length(age))
  ne_matrix_trans <- ne_matrix * position_matrix
  
  return(ne_matrix_trans)
}


#Recursive growing function (with lag)
recur_grow <- function(x, g) {
  if (length(x) > 1) {
    for (i in 2:length(x)) {
      x[i] <- x[i-1] * (1 + g[i - 1])
    }
  }
  return(x)
}

#Recursive growing function (no lag)
recur_grow2 <- function(x, g) {
  if (length(x) > 1) {
    for (i in 2:length(x)) {
      x[i] <- x[i-1] * (1 + g[i])
    }
  }
  return(x)
}

#Present Value of Future Benefits (PVFB) function (to be applied to a vector of "Pension Wealth") for active members
#sep_rate_vec is a vector containing separation rates. Interest is a single discount rate (ARR). value_vect is a vector containing the present values of pension benefits at separation ages.
#The purpose of this function is to calculate the PVFB at each active age (not just the entry age)
PVFB <- function(sep_rate_vec, interest, value_vec) {
  PVFB <- double(length = length(value_vec))
  for (i in 1:length(value_vec)) {
    sep_rate <- sep_rate_vec[i:length(sep_rate_vec)]
    #sep_prob in a given year is the probability that the member will survive all the previous years and get terminated exactly in the given year
    sep_prob <- cumprod(1 - lag(sep_rate, n = 2, default = 0)) * lag(sep_rate, default = 0) 
    value <- value_vec[i:length(value_vec)]
    value_adjusted <- value * sep_prob
    PVFB[i] <- npv(interest, value_adjusted[2:length(value_adjusted)])
  }
  return(PVFB)
}

#Present Value of Future Salaries (PVFS) function (to be applied to a vector of salaries)
#remaining_prob_vec is a vector containing the remaining probabilities. Interest is a si ngle discount rate (ARR). sal_vec is a vector containing the salaries.
PVFS <- function(remaining_prob_vec, interest, sal_vec) {
  PVFS <- double(length = length(sal_vec))
  for (i in 1:length(sal_vec)) {
    remaining_prob_og <- remaining_prob_vec[i:length(remaining_prob_vec)]
    remaining_prob <- remaining_prob_og / remaining_prob_og[1]
    sal <- sal_vec[i:length(sal_vec)]
    sal_adjusted <- sal * remaining_prob
    PVFS[i] <- npv(interest, sal_adjusted)
  }
  return(PVFS)
}


