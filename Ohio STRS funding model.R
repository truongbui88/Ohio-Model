#################################################################
##                        Funding Model                        ##
#################################################################



project_period <- ModelPeriod
# start_projection_year <- 2023
end_projection_year <- YearStart + project_period 

#Break up the funding_data table into individual vectors
empty_matrix <- matrix(0,(end_projection_year - YearStart), 1)

for(j in 1:length(colnames(funding_data))){
  temp_matrix <- rbind(as.matrix(funding_data[,j]), empty_matrix)
  assign(as.character(colnames(funding_data)[j]), temp_matrix)
}


#Model function starts here
get_funding_data <- function(dr_current = dr_current_,
                             dr_new = dr_new_,
                             cola_current_active = COLA_current_active,
                             cola_new_active = COLA_new_active,
                             cola_current_retire = COLA_current_retire,
                             cola_current_retire_one = COLA_current_retire_one,
                             one_time_cola = one_time_cola_,
                             retire_refund_ratio = retire_refund_ratio_,
                             cal_factor = cal_factor_,
                             
                             # db_new_ratio = db_new_ratio_,
                             
                             return_scen = return_scen_,
                             model_return = model_return_,
                             funding_policy = funding_policy_,
                             amo_period_current = amo_period_current_,
                             amo_period_new = amo_period_new_,
                             amo_pay_growth = amo_pay_growth_,
                             amo_method = amo_method_) {
  
  
  liability_df <- get_liability_data(dr_current = dr_current,
                                     dr_new = dr_new,
                                     cola_current_active = cola_current_active,
                                     cola_new_active = cola_new_active,
                                     cola_current_retire = cola_current_retire,
                                     cola_current_retire_one = cola_current_retire_one,
                                     one_time_cola = one_time_cola,
                                     retire_refund_ratio = retire_refund_ratio,
                                     cal_factor = cal_factor)
  
  
  #Break up liability_df into individual vectors
  liability_df <- liability_df %>% filter(year %in% YearStart:end_projection_year)
  
  for(j in 1:length(colnames(liability_df))){
    temp_matrix <- as.matrix(liability_df[,j])
    assign(as.character(colnames(liability_df)[j]), temp_matrix)
  }
  
  
  #Model calibration 
  #payroll calibration
  payroll_DB_legacy_ratio <- lag(payroll_DB_legacy_est / payroll_est)  #use lag to align with the funding mechanism
  payroll_DB_new_ratio <- lag(payroll_DB_new_est / payroll_est)
  # payroll_CB_new_ratio <- lag(payroll_CB_new_est / payroll_est)
  
  
  #normal cost calibration/projection
  nc_rate_DB_legacy[,1] <- lag(nc_rate_DB_legacy_est * nc_cal_)
  nc_rate_DB_new[,1] <- lag(nc_rate_DB_new_est * nc_cal_)
  # nc_rate_CB_new[,1] <- lag(nc_rate_CB_new_est)
  
  #accrued liability calibration
  AAL_legacy[1,1] <- AAL_legacy_est[1,1]
  AAL[1,1] <- AAL_est[1,1]
  UAL_AVA_legacy[1,1] <- AAL_legacy[1,1] - AVA_legacy[1,1] 
  UAL_AVA[1,1] <- AAL[1,1] - AVA[1,1]
  
  
  
  #Set up amo period sequences
  amo_period_seq_current <- seq(amo_period_current, 1)
  amo_period_seq_new <- seq(amo_period_new, 1)
  n <- max(length(amo_period_seq_current), length(amo_period_seq_new))
  length(amo_period_seq_current) <- n
  length(amo_period_seq_new) <- n
  
  #Amo period table for current hires
  amo_period_table_current <- rbind(amo_period_seq_current, matrix(amo_period_seq_new, 
                                                                   nrow = project_period,
                                                                   ncol = length(amo_period_seq_current),
                                                                   byrow = T))
  
  rownames(amo_period_table_current) <- NULL         #Remove row names
  
  #Put the amo periods on diagonal rows
  for (i in 1:ncol(amo_period_table_current)) {
    amo_period_table_current[,i] <- lag(amo_period_table_current[,i], n = i - 1)
  }
  
  #Turn all NAs in the table to 0s
  amo_period_table_current[is.na(amo_period_table_current)] <- 0
  
  
  #Amo period table for future hires
  amo_period_table_new <- matrix(amo_period_seq_new,
                                 nrow = project_period + 1,
                                 ncol = length(amo_period_seq_new),
                                 byrow = T)
  
  #Put the amo periods on diagonal rows
  for (i in 1:ncol(amo_period_table_new)) {
    amo_period_table_new[,i] <- lag(amo_period_table_new[,i], n = i)
  }
  
  
  #Turn all NAs in the table to 0s
  amo_period_table_new[is.na(amo_period_table_new)] <- 0
  
  
  #Level % or level $ for debt amortization 
  if(amo_method == "level $"){
    amo_pay_growth <- 0
  }
  
  #Set up the UAAL layer and amo payment tables for current members and initialize the first UAAL layer and amo payment
  debt_layer_table_current <- matrix(0, nrow = project_period + 1, ncol = length(amo_period_seq_current) + 1)
  amo_payment_table_current <- matrix(0, nrow = project_period + 1, ncol = length(amo_period_seq_current))
  
  debt_layer_table_current[1,1] <- UAL_AVA[1]
  amo_payment_table_current[1,1] <- PMT(pv = debt_layer_table_current[1,1],
                                        r = dr_current,
                                        g = amo_pay_growth,
                                        nper = amo_period_table_current[1,1],
                                        t = 0.5)
  
  #Set up the UAAL layer and amo payment tables for new members
  debt_layer_table_new <- matrix(0, nrow = project_period + 1, ncol = length(amo_period_seq_new) + 1)
  amo_payment_table_new <- matrix(0, nrow = project_period + 1, ncol = length(amo_period_seq_new))
  
  
  #Set return values for "model" and "assumption" scenarios
  return_scenarios$model <- model_return
  return_scenarios$assumption <- dr_current
  
  
  #Return scenario
  # return_scen <- "recur_recession"
  return_scen_index <- which(colnames(return_scenarios) == return_scen)
  
  
  
  for (i in 2:length(year)) {
    fy[i] <- year[i]
    
    
    #Payroll projection
    payroll[i] <- payroll[i-1] * (1 + payroll_growth_)
    payroll_DB_legacy[i] <- payroll[i] * payroll_DB_legacy_ratio[i]
    payroll_DB_new[i] <- payroll[i] * payroll_DB_new_ratio[i]
    # payroll_CB_new[i] <- payroll[i] * payroll_CB_new_ratio[i]
    
    #Benefit payments and refunds projection
    ben_payment_legacy[i] <- retire_ben_DB_legacy_est[i] + retire_ben_current_est[i] + retire_ben_term_est[i]
    refund_legacy[i] <- refund_DB_legacy_est[i]
    ben_payment_new[i] <- retire_ben_DB_new_est[i] 
    refund_new[i] <- refund_DB_new_est[i] 
    
    #Normal cost projection
    nc_legacy[i] <- nc_rate_DB_legacy[i] * payroll_DB_legacy[i]
    nc_new[i] <- nc_rate_DB_new[i] * payroll_DB_new[i] 
    nc_rate[i] <- (nc_legacy[i] + nc_new[i]) / payroll[i]
    
    #Accrued liability projection
    liability_gain_loss_legacy[i] <- liability_gain_loss_legacy_est[i]
    liability_gain_loss_new[i] <- liability_gain_loss_new_est[i]
    liability_gain_loss[i] <- liability_gain_loss_est[i]
    # DR[i] <- ARR
    AAL_legacy[i] <- AAL_legacy[i-1] * (1 + dr_current) + (nc_legacy[i] - ben_payment_legacy[i] - refund_legacy[i]) * (1 + dr_current)^0.5 + liability_gain_loss_legacy[i]
    AAL_new[i] <- AAL_new[i-1] * (1 + dr_new) + (nc_new[i] - ben_payment_new[i] - refund_new[i]) * (1 + dr_new)^0.5 + liability_gain_loss_new[i]
    AAL[i] <- AAL_legacy[i] + AAL_new[i]
    
    #NC and employee contribution rates
    nc_rate_legacy[i] <- nc_legacy[i] / payroll_DB_legacy[i]
    
    if(payroll_DB_new[i] == 0) {
      nc_rate_new[i] <- 0
    } else {
      nc_rate_new[i] <- nc_new[i] / (payroll_DB_new[i])  
    }
    
    
    ee_nc_rate_legacy[i] <- 0.14
    ee_nc_rate_new[i] <- 0.14
    
    
    #Employer contribution rates
    er_nc_rate_legacy[i] <- nc_rate_legacy[i] - ee_nc_rate_legacy[i]
    er_nc_rate_new[i] <- nc_rate_new[i] - ee_nc_rate_new[i]
    
    # if(year[i] < 2024) {
    #   er_stat_base_rate[i] <- 0.08
    # } else {
    #   er_stat_base_rate[i] <- 0.0825
    # }
    # 
    # if(year[i] <= 2025) {
    #   public_edu_surcharge_rate[i] <- public_edu_surcharge_rate[i-1] + 0.001
    # } else {
    #   public_edu_surcharge_rate[i] <- public_edu_surcharge_rate[i-1]
    # }
    # 
    # er_stat_eff_rate[i] <- er_stat_base_rate[i] + public_edu_surcharge_rate[i] * 0.622
    
    er_stat_rate[i] <- 0.14
    
    if (funding_policy == "statutory") {
      amo_rate_legacy[i] <- er_stat_rate[i] - er_nc_rate_legacy[i]
      amo_rate_new[i] <- er_stat_rate[i] - er_nc_rate_new[i]
    } else {
      
      if (payroll_DB_new[i] == 0) {
        amo_rate_legacy[i] <- sum(amo_payment_table_current[i-1,]) / payroll_DB_legacy[i]
        amo_rate_new[i] <- 0
      } else {
        amo_rate_legacy[i] <- sum(amo_payment_table_current[i-1,]) / payroll_DB_legacy[i]
        amo_rate_new[i] <- sum(amo_payment_table_new[i-1,]) / payroll_DB_new[i] 
      }
      
    }
    
    
    admin_exp_rate[i] <- admin_exp_rate[i-1]
    
    #Employee contribution amounts
    ee_nc_cont_legacy[i] <- ee_nc_rate_legacy[i] * payroll_DB_legacy[i]
    ee_nc_cont_new[i] <- ee_nc_rate_new[i] * (payroll_DB_new[i])
    
    #Admin expense amounts
    admin_exp_legacy[i] <- admin_exp_rate[i] * payroll_DB_legacy[i]
    admin_exp_new[i] <- admin_exp_rate[i] * (payroll_DB_new[i])
    
    #Employer contribution amounts
    er_nc_cont_legacy[i] <- er_nc_rate_legacy[i] * payroll_DB_legacy[i] + admin_exp_legacy[i]
    er_nc_cont_new[i] <- er_nc_rate_new[i] * (payroll_DB_new[i]) + admin_exp_new[i]
    
    er_amo_cont_legacy[i] <- amo_rate_legacy[i] * payroll_DB_legacy[i]
    er_amo_cont_new[i] <- amo_rate_new[i] * (payroll_DB_new[i])
    
    #Simulated returns
    ROA[i] <- return_scenarios[which(return_scenarios$year == year[i]), return_scen_index][[1]]
    
    #Solvency contribution and cash flows
    cf_legacy <- ee_nc_cont_legacy[i] + er_nc_cont_legacy[i] + er_amo_cont_legacy[i] - ben_payment_legacy[i] - refund_legacy[i] - admin_exp_legacy[i]
    cf_new <- ee_nc_cont_new[i] + er_nc_cont_new[i] + er_amo_cont_new[i] - ben_payment_new[i] - refund_new[i] - admin_exp_new[i]
    cf_total <- cf_legacy + cf_new
    
    solv_cont[i] <- max(-(MVA[i-1] * (1 + ROA[i]) + cf_total * (1 + ROA[i])^0.5) / (1 + ROA[i])^0.5, 0)
    solv_cont_legacy[i] <- solv_cont[i] * AAL_legacy[i] / AAL[i]
    solv_cont_new[i] <- solv_cont[i] * AAL_new[i] / AAL[i]
    
    net_cf_legacy[i] <- cf_legacy + solv_cont_legacy[i]
    net_cf_new[i] <- cf_new + solv_cont_new[i]
    # net_cf[i] <- cf_total + solv_cont[i]
    
    #MVA projection
    MVA_legacy[i] <- MVA_legacy[i-1] * (1 + ROA[i]) + net_cf_legacy[i] * (1 + ROA[i])^0.5
    MVA_new[i] <- MVA_new[i-1] * (1 + ROA[i]) + net_cf_new[i] * (1 + ROA[i])^0.5
    MVA[i] <- MVA_new[i] + MVA_legacy[i]
    
    #AVA legacy development
    exp_income_legacy[i] <- AVA_legacy[i-1] * dr_current + net_cf_legacy[i] * dr_current/2
    act_income_MVA_legacy[i] <- MVA_legacy[i] - MVA_legacy[i-1] - net_cf_legacy[i]
    gain_loss_legacy[i] <- act_income_MVA_legacy[i] - exp_income_legacy[i]
    
    defer_year1_legacy[i] <- gain_loss_legacy[i] * 3/4
    defer_year2_legacy[i] <- defer_year1_legacy[i-1] * 2/3
    defer_year3_legacy[i] <- defer_year2_legacy[i-1] * 1/2
    total_defer_legacy[i] <- defer_year1_legacy[i] + defer_year2_legacy[i] + defer_year3_legacy[i]
    
    #AVA new development
    exp_income_new[i] <- AVA_new[i-1] * dr_new + net_cf_new[i] * dr_new/2
    act_income_MVA_new[i] <- MVA_new[i] - MVA_new[i-1] - net_cf_new[i]
    gain_loss_new[i] <- act_income_MVA_new[i] - exp_income_new[i]
    
    defer_year1_new[i] <- gain_loss_new[i] * 3/4
    defer_year2_new[i] <- defer_year1_new[i-1] * 2/3
    defer_year3_new[i] <- defer_year2_new[i-1] * 1/2
    total_defer_new[i] <- defer_year1_new[i] + defer_year2_new[i] + defer_year3_new[i]
    
    #AVA, UAl, and funded ratio projections
    AVA_legacy[i] <- MVA_legacy[i] - total_defer_legacy[i]
    AVA_new[i] <- MVA_new[i] - total_defer_new[i]
    AVA[i] <- AVA_legacy[i] + AVA_new[i]
    
    UAL_AVA_legacy[i] <- AAL_legacy[i] - AVA_legacy[i]
    UAL_AVA_new[i] <- AAL_new[i] - AVA_new[i]
    UAL_AVA[i] <- UAL_AVA_legacy[i] + UAL_AVA_new[i]
    
    UAL_MVA_legacy[i] <- AAL_legacy[i] - MVA_legacy[i]
    UAL_MVA_new[i] <- AAL_new[i] - MVA_new[i]
    UAL_MVA[i] <- UAL_MVA_legacy[i] + UAL_MVA_new[i]
    
    
    FR_AVA[i] <- AVA[i] / AAL[i]
    FR_MVA[i] <- MVA[i] / AAL[i]
    
    #Contribution analysis
    er_cont[i] <- er_nc_cont_legacy[i] + er_nc_cont_new[i] + er_amo_cont_legacy[i] + er_amo_cont_new[i] + solv_cont[i]
    er_cont_rate[i] <- er_cont[i] / payroll[i]
    tot_cont_rate[i] <- (ee_nc_cont_legacy[i] + er_nc_cont_legacy[i] + er_amo_cont_legacy[i] + ee_nc_cont_new[i] + er_nc_cont_new[i] + er_amo_cont_new[i] + solv_cont[i]) / payroll[i]
    
    #Amortization calculations
    #Amortization for legacy hires
    debt_layer_table_current[i,2:ncol(debt_layer_table_current)] <- debt_layer_table_current[i-1,1:(ncol(debt_layer_table_current)-1)] * (1 + dr_current) - amo_payment_table_current[i-1,1:ncol(amo_payment_table_current)] * (1 + dr_current)^0.5
    debt_layer_table_current[i,1] <- UAL_AVA_legacy[i] - sum(debt_layer_table_current[i,2:ncol(debt_layer_table_current)])
    
    amo_payment_table_current[i,1:ncol(amo_payment_table_current)] <- PMT(r = dr_current,
                                                                          g = amo_pay_growth,
                                                                          nper = pmax(amo_period_table_current[i,1:ncol(amo_period_table_current)], 1),
                                                                          pv = debt_layer_table_current[i,1:(ncol(debt_layer_table_current)-1)],
                                                                          t = 0.5)
    
    
    #Amortization for new hires
    debt_layer_table_new[i,2:ncol(debt_layer_table_new)] <- debt_layer_table_new[i-1,1:(ncol(debt_layer_table_new)-1)] * (1 + dr_new) - amo_payment_table_new[i-1,1:ncol(amo_payment_table_new)] * (1 + dr_new)^0.5
    debt_layer_table_new[i,1] <- UAL_AVA_new[i] - sum(debt_layer_table_new[i,2:ncol(debt_layer_table_new)])
    
    amo_payment_table_new[i,1:ncol(amo_payment_table_new)] <- PMT(r = dr_new,
                                                                  g = amo_pay_growth,
                                                                  nper = pmax(amo_period_table_new[i,1:ncol(amo_period_table_new)], 1),
                                                                  pv = debt_layer_table_new[i,1:(ncol(debt_layer_table_new)-1)],
                                                                  t = 0.5)
    
  }
  
  output <- data.frame(sapply(colnames(funding_data), get, envir = sys.frame(sys.parent(0))))
  
  return(output)
  
}  
# write.csv(output, "output.csv")  
#   return(output)
#   
# }