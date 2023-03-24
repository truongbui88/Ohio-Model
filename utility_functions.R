library(zoo)

#Present Value function
pv <- function(rate, g = 0, nper, pmt, t = 1) {
  r <- (1 + rate)/(1 + g) - 1
  PV <- pmt/r * (1 - (1 / (1 + r)^nper)) / (1 + g) * (1 + rate)^(1 - t)
  return(PV)
}

#Rolling Present Value function. Note that the first value in the pmt_vec vector must be zero.
roll_pv <- function(rate, g = 0, nper, pmt_vec, t = 1) {
  pv_vec <- double(length(pmt_vec))
  for (i in 1:length(pv_vec)) {
    if (i == 1) {
      pv_vec[i] <- pv(rate, g, nper, pmt_vec[2], t)
    } else {
      pv_vec[i] <- pv_vec[i-1] * (1 + rate) - pmt_vec[i] * (1 + rate)^(1 - t)
    }
  }
  
  return(pv_vec)
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


#rolling NPV function
roll_npv <- function(rate, cashflows) {
  npv_vec <- double(length = length(cashflows))
  for (i in 1:(length(cashflows)-1)) {
    npv_vec[i] <- npv(rate, cashflows[(i+1):length(cashflows)])  
  }
  
  return(npv_vec)
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

#Cumulative future values function (with interest being a single value)
cumFV <- function(interest, cashflow, first_value = 0){
  cumvalue <- double(length = length(cashflow))
  cumvalue[1] <- first_value
  for (i in 2:length(cumvalue)) {
    cumvalue[i] <- cumvalue[i - 1]*(1 + interest) + cashflow[i - 1]
  }
  return(cumvalue)
}


#Cumulative future values function (with interest being a vector)
cumFV2 <- function(interest_vec, cashflow, first_value = 0){
  cumvalue <- double(length = length(cashflow))
  cumvalue[1] <- first_value
  for (i in 2:length(cumvalue)) {
    cumvalue[i] <- cumvalue[i - 1]*(1 + interest_vec[i]) + cashflow[i - 1]
  }
  return(cumvalue)
}


#Rolling mean function (a lot faster than zoo's rollmean)
#note that this roll mean function assumes a "lagged" data vector
baseR.rollmean <- function(data, window_vec) {
  n <- length(data)
  y <- double(n)
  for (i in 1:n) {
    window <- window_vec[i]
    if (i > window) {
      y[i] <- mean(data[(i-window):(i-1)]) 
    } else {
      y[i] <- NA
    }
  }
  return(y)
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

#Recursive growing function with a single base and a fixed growth rate
recur_grow3 <- function(x, g, nper) {
  x_vec <- double(length = nper)
  x_vec[1] <- x
  
  for (i in 2:length(x_vec)) {
    x_vec[i] <- x_vec[i-1] * (1 + g)
  }
  
  return(x_vec)
}


#Present Value of Future Benefits (PVFB) function (to be applied to a vector of "Pension Wealth") for active members
#sep_rate_vec is a vector containing separation rates. interest_vec is a discount rate (ARR) vector. value_vect is a vector containing the present values of pension benefits at separation ages.
#The purpose of this function is to calculate the PVFB at each active age (not just the entry age)
PVFB <- function(sep_rate_vec, interest_vec, value_vec) {
  PVFB <- double(length = length(value_vec))
  for (i in 1:length(value_vec)) {
    sep_rate <- sep_rate_vec[i:length(sep_rate_vec)]
    #sep_prob in a given year is the probability that the member will survive all the previous years and get terminated exactly in the given year
    sep_prob <- cumprod(1 - lag(sep_rate, n = 2, default = 0)) * lag(sep_rate, default = 0)
    interest <- interest_vec[i]
    value <- value_vec[i:length(value_vec)]
    value_adjusted <- value * sep_prob
    PVFB[i] <- npv(interest, value_adjusted[2:length(value_adjusted)])
  }
  return(PVFB)
}


#Present Value of Future Benefits for Cash Balance function
#We need to a separate PVFB function to deal with cash balance benefits due to the fact actual ICR may differ from assumed ICR
PVFB_CB <- function(ee_bal_vec, er_bal_vec, ee_cont_vec, er_cont_vec, icr, yos_vec, vesting_period, 
                    surv_icr_vec, annuity_acr_vec, annuity_adj_vec,
                    sep_type_vec, sep_rate_vec, interest_vec, retire_refund_ratio) {
  
  PVFB <- double(length = length(ee_bal_vec))
  
  for (i in 1:length(PVFB)) {
    
    if(i < length(PVFB)) {
      #Project the CB account balance
      CBEEBalance <- cumFV(interest = icr, cashflow = ee_cont_vec[i:length(ee_cont_vec)], first_value = ee_bal_vec[i])
      CBERBalance <- cumFV(interest = icr, cashflow = er_cont_vec[i:length(er_cont_vec)], first_value = er_bal_vec[i])
      YOS <- yos_vec[i:length(yos_vec)]
      CBBalance <- CBEEBalance + ifelse(YOS >= vesting_period, CBERBalance, 0)
      
      #Convert the account balance into PV of benefits at separation
      surv_ICR <- surv_icr_vec[i:length(surv_icr_vec)]
      AnnuityFactor_ACR <- annuity_acr_vec[i:length(annuity_acr_vec)]
      AnnFactorAdj_DR <- annuity_adj_vec[i:length(annuity_adj_vec)]
      PV_CB_Benefit <- CBBalance / surv_ICR / AnnuityFactor_ACR * AnnFactorAdj_DR
      
      #Calculate present value of future benefits (PVFB) at a given active age  
      sep_type <- sep_type_vec[i:length(sep_type_vec)]
      CBWealth = ifelse(sep_type == "retire", PV_CB_Benefit, 
                        ifelse(sep_type == "term vested", retire_refund_ratio * PV_CB_Benefit + (1 - retire_refund_ratio) * CBBalance,
                               CBBalance))
      sep_rate <- sep_rate_vec[i:length(sep_rate_vec)]
      #sep_prob in a given year is the probability that the member will survive all the previous years and get terminated exactly in the given year
      sep_prob <- cumprod(1 - lag(sep_rate, n = 2, default = 0)) * lag(sep_rate, default = 0)
      interest <- interest_vec[i]
      CBWealth_adjusted <- CBWealth * sep_prob
      PVFB[i] <- npv(interest, CBWealth_adjusted[2:length(CBWealth_adjusted)])
      
    } else {
      PVFB[i] <- 0
    }
  }
  return(PVFB)
}



#Present Value of Future Salaries (PVFS) function (to be applied to a vector of salaries)
#remaining_prob_vec is a vector containing the remaining probabilities. interest_vec is a discount rate (ARR) vector. sal_vec is a vector containing the salaries.
PVFS <- function(remaining_prob_vec, interest_vec, sal_vec) {
  PVFS <- double(length = length(sal_vec))
  for (i in 1:length(sal_vec)) {
    remaining_prob_og <- remaining_prob_vec[i:length(remaining_prob_vec)]
    remaining_prob <- remaining_prob_og / remaining_prob_og[1]
    interest <- interest_vec[i]
    sal <- sal_vec[i:length(sal_vec)]
    sal_adjusted <- sal * remaining_prob
    PVFS[i] <- npv(interest, sal_adjusted)
  }
  return(PVFS)
}


#Annuity factor for current retirees' benefits
#We need this function to calculate the annuity factors when a constant COLA is granted after the first year
annfactor <- function(surv_DR_vec, cola_vec, one_time_cola = F){
  annfactor_vec <- double(length(surv_DR_vec))
  for (i in 1:length(annfactor_vec)) {
    cola <- ifelse(one_time_cola == F, cola_vec[i], 0)
    
    if (i == length(annfactor_vec)) {
      cola_project <- 0
    } else {
      cola_project <- c(0, rep(cola, length((i+1):length(cola_vec))))  
    }
    
    cumprod_cola <- cumprod(1 + cola_project)
    annfactor_vec[i] <- sum((surv_DR_vec[i:length(surv_DR_vec)] / surv_DR_vec[i]) * cumprod_cola)
  }
  return(annfactor_vec)
}



