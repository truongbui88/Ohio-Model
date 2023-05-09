#################################################################
##                       Liability Model                       ##
#################################################################

get_liability_data <- function(
    dr_current = dr_current_,
    dr_new = dr_new_,
    cola_current_active = COLA_current_active,
    cola_new_active = COLA_new_active,
    cola_current_retire = COLA_current_retire,
    cola_current_retire_one = COLA_current_retire_one,
    one_time_cola = one_time_cola_,
    retire_refund_ratio = retire_refund_ratio_,
    cal_factor = cal_factor_,
    
    db_new_ratio = db_new_ratio_
    
) {
  
  
  
  benefit_data <- get_benefit_data(dr_current = dr_current,
                                   dr_new = dr_new,
                                   cola_current_active = cola_current_active,
                                   cola_new_active = cola_new_active,
                                   cola_current_retire = cola_current_retire,
                                   cola_current_retire_one = cola_current_retire_one,
                                   one_time_cola = one_time_cola,
                                   retire_refund_ratio = retire_refund_ratio,
                                   cal_factor = cal_factor)
  
  #Plan design ratio for new hires:
  hybrid_new_ratio <- 1 - db_new_ratio
  hybrid_legacy_ratio_ <- 1 - db_legacy_ratio_
  
  #Join wf active table with FinalData table to calculate the overall payroll, normal costs, PVFB, and PVFS each year
  wf_active_df_final <- wf_data$wf_active_df %>% 
    filter(year <= YearStart + ModelPeriod) %>% 
    mutate(entry_year = year - (age - ea)) %>% 
    left_join(benefit_data$final_tab, by = c("ea" = "entry_age", "age" = "Age", "year" = "Years", "entry_year" = "EntryYear")) %>% 
    select(ea, age, year, entry_year, n_active, normal_cost_DB, normal_cost_Hybrid, Salary, PVFB_DB, PVFB_Hybrid, PVFNC_DB, PVFNC_Hybrid, PVFS) %>% 
    replace(is.na(.), 0) %>% 
    # filter(n_active > 0) %>% 
    #allocate members to plan designs based on entry year
    #2001 was the year the Combined Plan was introduced
    mutate(n_active_DB_legacy = ifelse(entry_year <= 2001, n_active,
                                       ifelse(entry_year <= YearStart, n_active * db_legacy_ratio_, 0)),
           n_active_DB_new = ifelse(entry_year <= YearStart, 0, n_active * db_new_ratio),
           n_active_Hybrid_legacy = ifelse(entry_year <= 2001, 0,
                                           ifelse(entry_year <= YearStart, n_active * hybrid_legacy_ratio_, 0)),
           n_active_Hybrid_new = ifelse(entry_year <= YearStart, 0, n_active * hybrid_new_ratio)) %>% 
    group_by(year) %>% 
    summarise(
      #Payroll
      payroll_DB_legacy_est = sum(Salary * n_active_DB_legacy),
      payroll_DB_new_est = sum(Salary * n_active_DB_new),
      payroll_Hybrid_legacy_est = sum(Salary * n_active_Hybrid_legacy),
      payroll_Hybrid_new_est = sum(Salary * n_active_Hybrid_new),
      payroll_est = sum(Salary * n_active),
      #Normal cost rates
      nc_rate_DB_legacy_est = ifelse(payroll_DB_legacy_est == 0, 0, sum(normal_cost_DB * Salary * n_active_DB_legacy) / sum(Salary * n_active_DB_legacy)),
      nc_rate_DB_new_est = ifelse(payroll_DB_new_est == 0, 0, sum(normal_cost_DB * Salary * n_active_DB_new) / sum(Salary * n_active_DB_new)),
      nc_rate_Hybrid_legacy_est = ifelse(payroll_Hybrid_legacy_est == 0, 0, sum(normal_cost_Hybrid * Salary * n_active_Hybrid_legacy) / sum(Salary * n_active_Hybrid_legacy)),
      nc_rate_Hybrid_new_est = ifelse(payroll_Hybrid_new_est == 0, 0, sum(normal_cost_Hybrid * Salary * n_active_Hybrid_new) / sum(Salary * n_active_Hybrid_new)),
      #Present value of future benefits
      PVFB_DB_legacy_est = sum(PVFB_DB * n_active_DB_legacy),
      PVFB_DB_new_est = sum(PVFB_DB * n_active_DB_new),
      PVFB_Hybrid_legacy_est = sum(PVFB_Hybrid * n_active_Hybrid_legacy),
      PVFB_Hybrid_new_est = sum(PVFB_Hybrid * n_active_Hybrid_new),
      #Present value of future normal costs
      PVFNC_DB_legacy_est = sum(PVFNC_DB * n_active_DB_legacy),
      PVFNC_DB_new_est = sum(PVFNC_DB * n_active_DB_new),
      PVFNC_Hybrid_legacy_est = sum(PVFNC_Hybrid * n_active_Hybrid_legacy),
      PVFNC_Hybrid_new_est = sum(PVFNC_Hybrid * n_active_Hybrid_new),
      #Count of active members
      n_active = sum(n_active),
      n_active_DB_legacy = sum(n_active_DB_legacy),
      n_active_Hybrid_legacy = sum(n_active_Hybrid_legacy)
    ) %>% 
    ungroup() %>% 
    mutate(nc_rate_est = (nc_rate_DB_legacy_est * payroll_DB_legacy_est + nc_rate_DB_new_est * payroll_DB_new_est + nc_rate_Hybrid_legacy_est * payroll_Hybrid_legacy_est + nc_rate_Hybrid_new_est * payroll_Hybrid_new_est) / payroll_est,
           AAL_active_DB_legacy_est = PVFB_DB_legacy_est - PVFNC_DB_legacy_est,
           AAL_active_DB_new_est = PVFB_DB_new_est - PVFNC_DB_new_est,
           AAL_active_Hybrid_legacy_est = PVFB_Hybrid_legacy_est - PVFNC_Hybrid_legacy_est,
           AAL_active_Hybrid_new_est = PVFB_Hybrid_new_est - PVFNC_Hybrid_new_est) %>% 
    replace(is.na(.), 0) 
  
  
  #Term table
  wf_term_df_final <- wf_data$wf_term_df %>% 
    filter(year <= YearStart + ModelPeriod,
           n_term > 0) %>% 
    mutate(entry_year = year - (age - ea)) %>% 
    #join FinalData to get PV_DB_Benefit (the present value of benefits at termination)
    left_join(benefit_data$final_tab, by = c("ea" = "entry_age", "term_year" = "Years", "entry_year" = "EntryYear")) %>% 
    select(ea, age, year, term_year, entry_year, RetirementAge, n_term, PV_DB_Benefit, PV_Hybrid_Benefit) %>% 
    #join BenefitsTable to get the surv_DR at current age
    left_join(benefit_data$ben_tab %>% select(-PV_DB_Benefit, -PV_Hybrid_Benefit), by = c("ea" = "entry_age", "age" = "RetirementAge", "year" = "RetYear", "term_year", "entry_year" = "EntryYear")) %>% 
    select(ea, age, year, term_year, entry_year, RetirementAge, n_term, PV_DB_Benefit, PV_Hybrid_Benefit, surv_DR_DB) %>% 
    #rename to clarify variables' meanings
    rename(surv_DR_current = surv_DR_DB) %>% 
    
    mutate(
      #PVFB_term_DB = First DB benefit * annuity factor at retirement * surv_DR at retirement / surv_DR at current time
      #Note that PV_DB_Benefit (PV at termination) = First DB benefit * annuity factor at retirement * surv_DR at retirement
      PVFB_term_DB = PV_DB_Benefit / surv_DR_current,
      PVFB_term_Hybrid = PV_Hybrid_Benefit / surv_DR_current,
      
      n_term_DB_legacy = ifelse(entry_year <= 2001, n_term,
                                ifelse(entry_year <= YearStart, n_term * db_legacy_ratio_, 0)),
      n_term_DB_new = ifelse(entry_year <= YearStart, 0, n_term * db_new_ratio),
      
      n_term_Hybrid_legacy = ifelse(entry_year <= 2001, 0,
                                      ifelse(entry_year <= YearStart, n_term * hybrid_legacy_ratio_, 0)),
      n_term_Hybrid_new = ifelse(entry_year <= YearStart, 0, n_term * hybrid_new_ratio)
    ) %>% 
    group_by(year) %>% 
    summarise(AAL_term_DB_legacy_est = sum(PVFB_term_DB * n_term_DB_legacy),
              AAL_term_DB_new_est = sum(PVFB_term_DB * n_term_DB_new),
              AAL_term_Hybrid_legacy_est = sum(PVFB_term_Hybrid * n_term_Hybrid_legacy),
              AAL_term_Hybrid_new_est = sum(PVFB_term_Hybrid * n_term_Hybrid_new)
    ) %>% 
    ungroup()
  
  
  
  #Join wf refund table with benefit table to calculate the overall refunds each year
  wf_refund_df_final <- wf_data$wf_refund_df %>% 
    filter(year <= YearStart + ModelPeriod,
           n_refund > 0) %>% 
    mutate(entry_year = year - (age - ea)) %>% 
    left_join(benefit_data$ben_tab, by = c("ea" = "entry_age", "age" = "RetirementAge", "year" = "RetYear", "term_year", "entry_year" = "EntryYear")) %>% 
    select(ea, age, year, term_year, entry_year, n_refund, DBEEBalance, Hybrid_EEBalance) %>% 
    #allocate members to plan designs based on entry year
    mutate(n_refund_DB_legacy = ifelse(entry_year <= 2001, n_refund,
                                       ifelse(entry_year <= YearStart, n_refund * db_legacy_ratio_, 0)),
           n_refund_DB_new = ifelse(entry_year <= YearStart, 0, n_refund * db_new_ratio),
           
           n_refund_Hybrid_legacy = ifelse(entry_year <= 2001, 0,
                                         ifelse(entry_year <= YearStart, n_refund * hybrid_legacy_ratio_, 0)),
           n_refund_Hybrid_new = ifelse(entry_year <= YearStart, 0, n_refund * hybrid_new_ratio)
    ) %>% 
    group_by(year) %>% 
    summarise(refund_DB_legacy_est = sum(DBEEBalance * n_refund_DB_legacy),
              refund_DB_new_est = sum(DBEEBalance * n_refund_DB_new),
              refund_Hybrid_legacy_est = sum(Hybrid_EEBalance * n_refund_Hybrid_legacy),
              refund_Hybrid_new_est = sum(Hybrid_EEBalance * n_refund_Hybrid_new)
    ) %>% 
    ungroup()
  
  
  
  #Join wf retire table with benefit table to calculate the overall retirement benefits each year
  wf_retire_df_final <- wf_data$wf_retire_df %>% 
    filter(year <= YearStart + ModelPeriod) %>% 
    mutate(entry_year = year - (age - ea)) %>%    
    left_join(benefit_data$ben_tab, by = c("ea" = "entry_age", "entry_year" = "EntryYear", "term_year", "retire_year" = "RetYear")) %>% 
    select(ea, age, year, term_year, retire_year, entry_year, n_retire, DB_Benefit, Hybrid_Benefit, COLA) %>% 
    left_join(benefit_data$ann_factor_tab %>% select(-COLA), by = c("ea" = "entry_age", "entry_year" = "EntryYear", "term_year", "year" = "RetYear")) %>% 
    select(ea, age, year, term_year, retire_year, entry_year, n_retire, DB_Benefit, Hybrid_Benefit, COLA, AnnuityFactor_DR_DB) %>% 
    rename(base_DB_benefit = DB_Benefit,
           base_Hybrid_benefit = Hybrid_Benefit) %>% 
    #Adjust the benefit based on COLA and allocate members to plan designs based on entry year
    mutate(
      DB_benefit_final = base_DB_benefit * (1 + COLA)^(year - retire_year),
      Hybrid_benefit_final = base_Hybrid_benefit * (1 + COLA)^(year - retire_year),
      
      n_retire_DB_legacy = ifelse(entry_year <= 2001, n_retire,
                                  ifelse(entry_year <= YearStart, n_retire * db_legacy_ratio_, 0)),
      n_retire_DB_new = ifelse(entry_year <= YearStart, 0, n_retire * db_new_ratio),
      
      n_retire_Hybrid_legacy = ifelse(entry_year <= 2001, 0,
                                      ifelse(entry_year <= YearStart, n_retire * hybrid_legacy_ratio_, 0)),
      n_retire_Hybrid_new = ifelse(entry_year <= YearStart, 0, n_retire * hybrid_new_ratio),
      #We use "AnnuityFactor_DR - 1" below because the PVFB for retirees excludes the first payment (i.e. the first payment has already been delivered when the PVFB is calculated)
      PVFB_retire_DB = DB_benefit_final * (AnnuityFactor_DR_DB - 1),
      PVFB_retire_Hybrid = Hybrid_benefit_final * (AnnuityFactor_DR_DB - 1)
    ) %>% 
    group_by(year) %>% 
    summarise(retire_ben_DB_legacy_est = sum(DB_benefit_final * n_retire_DB_legacy),
              retire_ben_DB_new_est = sum(DB_benefit_final * n_retire_DB_new),
              retire_ben_Hybrid_legacy_est = sum(Hybrid_benefit_final * n_retire_Hybrid_legacy),
              retire_ben_Hybrid_new_est = sum(Hybrid_benefit_final * n_retire_Hybrid_new),
              
              AAL_retire_DB_legacy_est = sum(PVFB_retire_DB * n_retire_DB_legacy),
              AAL_retire_DB_new_est = sum(PVFB_retire_DB * n_retire_DB_new),
              AAL_retire_Hybrid_legacy_est = sum(PVFB_retire_Hybrid * n_retire_Hybrid_legacy),
              AAL_retire_Hybrid_new_est = sum(PVFB_retire_Hybrid * n_retire_Hybrid_new)
    ) %>% 
    ungroup()
  
  
  
  #Project benefit payments for current retirees
  retire_current_int <- RetireeDistribution %>% 
    select(age, n_retire_ratio:total_ben_ratio) %>% 
    mutate(n_retire_current = n_retire_ratio * retiree_pop_current,
           total_ben_current = total_ben_ratio * ben_payment_current,
           avg_ben_current = total_ben_current / n_retire_current,
           Years = YearStart)
  
  
  wf_retire_current <- benefit_data$ann_factor_retire_tab %>% 
    filter(Years <= YearStart + ModelPeriod) %>% 
    left_join(retire_current_int, by = c("Age" = "age", "Years")) %>% 
    select(base_age:AnnuityFactor_DR_retire, n_retire_current, avg_ben_current, total_ben_current) %>% 
    group_by(base_age) %>% 
    mutate(n_retire_current = recur_grow(n_retire_current, -mort_DB),
           avg_ben_current = recur_grow2(avg_ben_current, cola),
           total_ben_current = n_retire_current * avg_ben_current,
           #We use "AnnuityFactor_DR - 1" below because the PVFB for retirees excludes the first payment (i.e. the first payment has already been delivered when the PVFB is calculated)
           PVFB_retire_current = avg_ben_current * (AnnuityFactor_DR_retire - 1)
    ) %>% 
    filter(!is.na(n_retire_current)) %>% 
    ungroup()
  
  wf_retire_current_final <- wf_retire_current %>% 
    group_by(Years) %>% 
    summarise(retire_ben_current_est = sum(total_ben_current),
              AAL_retire_current_est = sum(n_retire_current * PVFB_retire_current)
    ) %>% 
    ungroup() %>% 
    rename(year = Years)
  
  
  #Project benefit payments for current term vested members
  #Note that we use the original "dr_current_" in calculating the benefit payments so that any discount rate adjustment can work
  retire_ben_term <- PMT(r = dr_current_, nper = amo_period_term, pv = PVFB_term_current, g = amo_term_growth)
  
  year <- YearStart:(YearStart + ModelPeriod)
  amo_years_term <- (YearStart + 1):(YearStart + amo_period_term)
  retire_ben_term_est <- double(length = length(year))
  retire_ben_term_est[which(year %in% amo_years_term)] <- recur_grow3(retire_ben_term, amo_term_growth, amo_period_term)
  
  wf_term_current <- data.frame(year, retire_ben_term_est) %>% 
    mutate(AAL_term_current_est = roll_pv(rate = dr_current, g = amo_term_growth, nper = amo_period_term, pmt_vec = retire_ben_term_est))
  
  
  #####Funding model - liability side
  funding_df <- wf_active_df_final %>% 
    left_join(wf_term_df_final) %>% 
    left_join(wf_refund_df_final) %>% 
    left_join(wf_retire_df_final) %>%
    left_join(wf_retire_current_final) %>% 
    left_join(wf_term_current) %>%
    replace(is.na(.), 0) %>% 
    mutate(
      AAL_legacy_est = AAL_active_DB_legacy_est + AAL_active_Hybrid_legacy_est + AAL_term_DB_legacy_est + AAL_term_Hybrid_legacy_est + AAL_retire_DB_legacy_est + AAL_retire_Hybrid_legacy_est + AAL_retire_current_est + AAL_term_current_est,
      AAL_new_est = AAL_active_DB_new_est + AAL_active_Hybrid_new_est + AAL_term_DB_new_est + AAL_term_Hybrid_new_est + AAL_retire_DB_new_est + AAL_retire_Hybrid_new_est,
      AAL_est = AAL_legacy_est + AAL_new_est,
      tot_ben_refund_legacy_est = refund_DB_legacy_est + refund_Hybrid_legacy_est + retire_ben_DB_legacy_est + retire_ben_Hybrid_legacy_est + retire_ben_current_est + retire_ben_term_est,
      tot_ben_refund_new_est = refund_DB_new_est + refund_Hybrid_new_est + retire_ben_DB_new_est + retire_ben_Hybrid_new_est,
      tot_ben_refund_est = tot_ben_refund_legacy_est + tot_ben_refund_new_est
    )
  
  #Calculate liability gain/loss if any and project AAL using the roll forward method
  funding_df$liability_gain_loss_legacy_est <- 0
  funding_df$liability_gain_loss_new_est <- 0
  funding_df$liability_gain_loss_est <- 0
  
  funding_df$AAL_legacy_roll <- 0
  funding_df$AAL_new_roll <- 0
  funding_df$AAL_roll <- 0
  
  for (i in 1:nrow(funding_df)) {
    if (i == 1) {
      funding_df$liability_gain_loss_legacy_est[i] <- 0
      funding_df$liability_gain_loss_new_est[i] <- 0
      
      funding_df$AAL_legacy_roll[i] <- funding_df$AAL_legacy_est[i]
      funding_df$AAL_new_roll[i] <- funding_df$AAL_new_est[i]
      
    } else {
      
      funding_df$liability_gain_loss_legacy_est[i] <- round(funding_df$AAL_legacy_est[i] - (funding_df$AAL_legacy_est[i-1] * (1 + dr_current) + funding_df$payroll_DB_legacy_est[i-1] * funding_df$nc_rate_DB_legacy_est[i-1] + funding_df$payroll_Hybrid_legacy_est[i-1] * funding_df$nc_rate_Hybrid_legacy_est[i-1] - funding_df$tot_ben_refund_legacy_est[i]), digits = 1)
      funding_df$liability_gain_loss_new_est[i] <- round(funding_df$AAL_new_est[i] - (funding_df$AAL_new_est[i-1] * (1 + dr_new) + funding_df$payroll_DB_new_est[i-1] * funding_df$nc_rate_DB_new_est[i-1] + funding_df$payroll_Hybrid_new_est[i-1] * funding_df$nc_rate_Hybrid_new_est[i-1] - funding_df$tot_ben_refund_new_est[i]), digits = 1)
      
      funding_df$AAL_legacy_roll[i] <- funding_df$AAL_legacy_roll[i-1] * (1 + dr_current) + funding_df$payroll_DB_legacy_est[i-1] * funding_df$nc_rate_DB_legacy_est[i-1] + funding_df$payroll_Hybrid_legacy_est[i-1] * funding_df$nc_rate_Hybrid_legacy_est[i-1] - funding_df$tot_ben_refund_legacy_est[i] + funding_df$liability_gain_loss_legacy_est[i]
      funding_df$AAL_new_roll[i] <- funding_df$AAL_new_roll[i-1] * (1 + dr_new) + funding_df$payroll_DB_new_est[i-1] * funding_df$nc_rate_DB_new_est[i-1] + funding_df$payroll_Hybrid_new_est[i-1] * funding_df$nc_rate_Hybrid_new_est[i-1] - funding_df$tot_ben_refund_new_est[i] + funding_df$liability_gain_loss_new_est[i]
      
    }
  }
  
  funding_df$liability_gain_loss_est <- funding_df$liability_gain_loss_legacy_est + funding_df$liability_gain_loss_new_est
  funding_df$AAL_roll <- funding_df$AAL_legacy_roll + funding_df$AAL_new_roll
  
  #Check liability gain/loss
  #If the liability gain/loss isn't 0 under the perfect condition (experience = assumption), something must be wrong.
  
  return(funding_df)
  
}

# write.csv(funding_df, "funding_df_OhioSTRS.csv")
