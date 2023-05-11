##################################################################
##                           Analysis                           ##
##################################################################

source("Ohio STRS master.R")

#Target columns
target_col <- c("fy", "ROA", "AAL", "MVA", "AVA", "FR_MVA", "UAL_MVA_real", "er_cont_rate", "er_cont_real", "all_in_cost_real")


#Baseline return and recurring recession scenarios under statutory funding policy 
baseline_stat <- get_funding_data() %>% select(any_of(target_col))

two_recession_stat <- get_funding_data(return_scen = "recur_recession") %>% select(any_of(target_col))


#Baseline return and recurring recession scenarios under ADC funding policy
#Note: default amo period is 30 years
baseline_ADC <- get_funding_data(funding_policy = "ADC") %>% select(any_of(target_col))

two_recession_ADC <- get_funding_data(funding_policy = "ADC", return_scen = "recur_recession") %>% select(any_of(target_col))


#Export outputs
output_list <- list(baseline_stat = baseline_stat,
                    baseline_ADC = baseline_ADC,
                    two_recession_stat = two_recession_stat,
                    two_recession_ADC = two_recession_ADC)


write.xlsx(output_list, "analysis_output.xlsx", overwrite = T)
