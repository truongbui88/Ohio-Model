######Analysis##############

source("Ohio STRS master.R")

#1. Baseline scenario
baseline_liability <- get_liability_data()
baseline_funding <- get_funding_data()

baseline_funding_short <- baseline_funding %>% select(fy, nc_rate, AAL, AVA, MVA, UAL_AVA, UAL_MVA,
                                                      FR_AVA, FR_MVA, er_cont, er_cont_rate)

#2. Anthony's scenario: 
#add a 0.7% compound COLA to everyone, including current actives, current reirees, and new hires
#switch to ADC
#amortize current UAL over 50 years
#amortize new UAL over 15 years
#lower the discount rate for new hires to 6%
#baseline returns = 7%

anthony_liability <- get_liability_data(cola_current_active = 0.007,
                                        cola_new_active = 0.007,
                                        cola_current_retire = 0.007,
                                        dr_new = 0.06)

anthony_funding <- get_funding_data(cola_current_active = 0.007,
                                    cola_new_active = 0.007,
                                    cola_current_retire = 0.007,
                                    dr_new = 0.06,
                                    funding_policy = "ADC",
                                    amo_period_current = 50,
                                    amo_period_new = 15)

anthony_funding_short <- anthony_funding %>% select(fy, nc_rate, AAL, AVA, MVA, UAL_AVA, UAL_MVA,
                                                    FR_AVA, FR_MVA, er_cont, er_cont_rate)


scen_list <- list(Baseline = baseline_funding_short,
                  Anthony = anthony_funding_short)

write.xlsx(scen_list, "Ohio STRS Analysis.xlsx")



