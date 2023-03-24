


# EntryYear <- 1980:2052
# Years <- 1980:2154    #(why 2152? Because 120 - 18 + 2052 = 2154)
# YearStart <- 2022
# Age <- 18:120
# YOS <- 0:70
# #RetirementAge <- 20:120
# RetYear <- 2005:2154



#Assigning individual  Variables
model_inputs <- read_excel(FileName, sheet = 'Main')

for(i in 1:nrow(model_inputs)){
  if(!is.na(model_inputs[i,2])){
    assign(as.character(model_inputs[i,2]),as.double(model_inputs[i,3]))
  }
}


#Joining headcount data, salary data, and salary growth data
SalaryMatrix_long <- SalaryMatrix %>% 
  pivot_longer(cols = -1, names_to = "YOS", values_to = "Salary")
HeadCountMatrix_long <- HeadCountMatrix %>% 
  pivot_longer(cols = -1, names_to = "YOS", values_to = "Count")

#Calculate salary cumulative growth 
SalaryGrowth <- SalaryGrowth %>% 
  mutate(sal_cum_growth = cumprod(1 + lag(salary_increase, default = 0)))

#Joining salary data, headcount data, and estimate entry salaries for existing employees
SalaryHeadCountData <- SalaryMatrix_long %>%
  left_join(HeadCountMatrix_long) %>% 
  # replace(is.na(.), 0) %>% 
  mutate(YOS = as.numeric(YOS),
         CurrentYear = YearStart,
         entry_age = Age - YOS,
         EntryYear = CurrentYear - YOS) %>% 
  filter(Salary > 0, entry_age >= 18) %>% 
  left_join(SalaryGrowth) %>% 
  #estimate the salart at entry-age
  mutate(entry_salary = Salary / sal_cum_growth) %>% 
  select(EntryYear, entry_age, Age, YOS, Count, entry_salary)

#Calculate entrant count distribution
#Salary entry is the starting salary data 
SalaryEntry <- SalaryEntry_ %>% 
  mutate(entrant_dist = Count/sum(Count)) %>% 
  select(-Count)


##############################################################################################################################

#DB Retirement Conditions
IsRetirementEligible_DB_Regular <- function(Age, YOS, RetYear){
  Check = ifelse((YOS >= 5 & Age >= 65), TRUE,
                 ifelse(RetYear <= 2015 & YOS >= 30, TRUE,
                        ifelse(RetYear <= 2017 & YOS >= 31, TRUE,
                               ifelse(RetYear <= 2019 & YOS >= 32, TRUE,
                                      ifelse(RetYear <= 2021 & YOS >= 33, TRUE,
                                             ifelse(RetYear <= 2023 & YOS >= 34, TRUE,
                                                    ifelse(RetYear <= 2026 & YOS >= 35, TRUE,
                                                           ifelse(RetYear > 2026 & Age >= 60 & YOS >= 35, TRUE, FALSE))))))))
  return(Check)
}

IsRetirementEligible_DB_Early <- function(Age, YOS, RetYear){
  Check = ifelse((YOS >= 5 & Age >= 60), TRUE, 
                 ifelse(RetYear > 2015 & YOS >= 30, TRUE,
                        ifelse(RetYear <= 2015 & YOS >= 25 & Age >= 55, TRUE,
                               ifelse(RetYear <= 2017 & YOS >= 26 & Age >= 55, TRUE,
                                      ifelse(RetYear <= 2019 & YOS >= 27 & Age >= 55, TRUE,
                                             ifelse(RetYear <= 2021 & YOS >= 28 & Age >= 55, TRUE,
                                                    ifelse(RetYear <= 2023 & YOS >= 29 & Age >= 55, TRUE,
                                                           ifelse(RetYear > 2023 & YOS >= 30 & Age >= 25, TRUE, FALSE))))))))
  return(Check)
}

IsRetirementEligible_DB <- function(Age, YOS, RetYear){
  Check = ifelse(IsRetirementEligible_DB_Regular(Age, YOS, RetYear) | IsRetirementEligible_DB_Early(Age, YOS, RetYear), TRUE, FALSE)
  
  return(Check)
}

RetirementType_DB <- function(Age, YOS, RetYear){
  
  Check = ifelse(IsRetirementEligible_DB_Regular(Age, YOS, RetYear), 'Regular',
                 ifelse(IsRetirementEligible_DB_Early(Age, YOS, RetYear), 'Early','None'))
  
  return(Check)
}


SeparationType_DB <- function(Age, YOS, RetYear){
  Check = ifelse(IsRetirementEligible_DB(Age, YOS, RetYear) == T, 'Retirement',
                 ifelse(YOS > 5, 'Termination Vested', 'Termination Non-Vested'))
  
  return(Check)
}


#Hybrid Retirement
IsRetirementEligible_Hybrid_Regular <- function(Age, YOS, RetYear){
  Check = ifelse(Age >= 60 & YOS >= 5, TRUE, FALSE)
  
  return(Check)
}

IsRetirementEligible_Hybrid_Early <- function(Age, YOS, RetYear){
  Check = ifelse(Age >= 50 & Age < 60 & YOS >= 5, TRUE, FALSE)
  
  return(Check)
}

IsRetirementEligible_Hybrid <- function(Age, YOS, RetYear){
  Check = ifelse(IsRetirementEligible_Hybrid_Regular(Age, YOS, RetYear) | IsRetirementEligible_Hybrid_Early(Age, YOS, RetYear), TRUE, FALSE)
  
  return(Check)
}

RetirementType_Hybrid <- function(Age, YOS, RetYear){
  
  Check = ifelse(IsRetirementEligible_Hybrid_Regular(Age, YOS, RetYear), 'Regular',
                 ifelse(IsRetirementEligible_Hybrid_Early(Age, YOS, RetYear), 'Early','None'))
  
  return(Check)
}


SeparationType_Hybrid <- function(Age, YOS, RetYear){
  Check = ifelse(IsRetirementEligible_Hybrid(Age, YOS, RetYear) == T, 'Retirement',
                 ifelse(YOS > 5, 'Termination Vested', 'Termination Non-Vested'))
  
  return(Check)
}

##############################################################################################################################

#Custom function to calculate cumulative future values
# cumFV <- function(interest, cashflow){
#   cumvalue <- double(length = length(cashflow))
#   for (i in 2:length(cumvalue)) {
#     cumvalue[i] <- cumvalue[i - 1]*(1 + interest) + cashflow[i - 1]
#   }
#   return(cumvalue)
# }

LinearInterpolation <- function(Data,AgeStart,AgeEnd,Columns,Increment){
  TempMatrix <- matrix(0,nrow= (AgeEnd - AgeStart + 1),Columns)
  TempMatrix[,1] <- AgeStart:AgeEnd
  colnames(TempMatrix) <- colnames(Data)
  TempMatrix <- as.data.frame(TempMatrix)
  
  for(i in 1:nrow(Data)){
    Index <- which(TempMatrix[,1] == as.double(Data[i,1]))
    for(j in 2:Columns){
      TempMatrix[Index,j] <- Data[i,j]
    }
  }
  
  for(i in 1:(nrow(Data)-1)){
    for(k in 2:Columns){
      for(j in 1:(Increment - 1)){
        BaseRowIndex <- (i-1)*Increment + 1
        Addition <- (Data[(i+1),k] - Data[i,k]) / (Data[(i+1),1] - Data[i,1])
        TempMatrix[(BaseRowIndex+j),k] <- Data[i,k] + (Addition*j)
      }
    }
  }
  
  return(TempMatrix)
}

CompoundSalaryIncrease <- function(Data){
  #Data[,1] <- 1
  for(i in 2:nrow(Data)){
    for(j in 2:ncol(Data)){
      #Column 1 is the Age label so we want to avoid computing j-1 at j=2
      if(j > 2){
        Data[i,j] <- Data[i,j]*Data[i-1,j-1]
      }
    }
  }
  
  return(Data)
}

##############################################################################################################################

#Clean Data
TerminationRateVested <- LinearInterpolation(TerminationRateVested,20,60,3,5)
#SalaryGrowth <- LinearInterpolation(SalaryGrowth,20,120,2,5)
SurvivalRates[is.na(SurvivalRates)] <- 0
MaleMP[is.na(MaleMP)] <- 0
FemaleMP[is.na(FemaleMP)] <- 0
# AgeNames <- as.character(52:70)
EarlyRetirement_After2015 <- pivot_longer(EarlyRetirement_After2015, cols = -YOS, names_to = "Age", values_to = "EarlyRetPct_After2015")
# EarlyRetirement_After2015 <- pivot_longer(EarlyRetirement_After2015,cols = AgeNames, names_to = 'Age',values_to = 'EarlyRetPct_After2015')
EarlyRetirement_After2015$Age <- as.double(EarlyRetirement_After2015$Age)

#Use the Age 20 salary increase for age 18 and 19
#Do it in R over excel since the linear interpolation works in increments of 5
# TerminationRateVested <- rbind(c(18,0.06,0.06),c(19,0.06,0.06),TerminationRateVested)

#Joining headcount data, salary data and salary increase data
#YOSNames is the midpoint for the Age/YOS ranges matrix
# YOSNames <- c("2","7","12","17","22","27","32")
# YOSNames <- as.character(seq(2,32,by = 5))
# SalaryMatrix_LongForm <- pivot_longer(SalaryMatrix, cols = YOSNames, names_to = 'YOS',values_to = 'Salary')
# HeadCountMatrix_LongForm <- pivot_longer(HeadCountMatrix, cols = YOSNames, names_to = 'YOS',values_to = 'HeadCount')
# SalaryHeadCountData <- left_join(SalaryMatrix_LongForm,HeadCountMatrix_LongForm, by = c("Age", "YOS")) %>%
#   arrange(YOS) %>% replace(is.na(.), 0) %>%
#   
#   #reconvert YOS to numeric since it was string for the pivor longer function
#   replace(is.na(.), 0) %>% mutate(YOS = as.numeric(YOS),
#                                   CurYear = YearStart,
#                                   #Use the calculated entry_age so that we can join with NC calcs later
#                                   entry_age = Age - YOS,
#                                   EntryYear = CurYear - YOS) %>%
#   filter(Salary > 0, entry_age >= 18) %>%
#   ungroup()
# 
# #Salary entry is the starting salary data only used for the salary table. All other places use the total Salary/HeadCount Data
# SalaryEntry <- SalaryHeadCountData %>% filter(YOS == 2) %>% select(-EntryYear, -Age, -YOS) %>% rename(start_sal = Salary)

##############################################################################################################################

#These rates dont change so they're outside the function
#Transform base mortality rates and mortality improvement rates
MaleMP <- MaleMP %>% 
  pivot_longer(-Age, names_to = "Years", values_to = "MP_male") %>% 
  mutate(Years = as.numeric(Years))

MaleMP_ultimate <- MaleMP %>%       #ultimate rates = rates for the last year in the MP table
  filter(Years == max(Years)) %>% 
  rename(MP_ultimate_male = MP_male) %>% 
  select(-Years)

#Starts with 1951 for MP improvement scales
MaleMP_final <- expand_grid(Age, Years = 1951:max(Years)) %>% 
  left_join(MaleMP, by = c("Age", "Years")) %>% 
  left_join(MaleMP_ultimate, by = "Age") %>% 
  mutate(MP_final_male = ifelse(Years > max(MaleMP$Years), MP_ultimate_male, MP_male)) %>% 
  group_by(Age) %>% 
  
  mutate(MPcumprod_male_raw = cumprod(1 - MP_final_male),
         #The adjusted value is the ratio of the original value and the anchor point
         #in this case 2014 to help us find MP values before 2014
         MPcumprod_male_adj = MPcumprod_male_raw / MPcumprod_male_raw[Years == 2010]) %>%
  ungroup()


FemaleMP <- FemaleMP %>% 
  pivot_longer(-Age, names_to = "Years", values_to = "MP_female") %>% 
  mutate(Years = as.numeric(Years))

FemaleMP_ultimate <- FemaleMP %>% 
  filter(Years == max(Years)) %>% 
  rename(MP_ultimate_female = MP_female) %>% 
  select(-Years)

FemaleMP_final <- expand_grid(Age, Years = 1951:max(Years)) %>% 
  left_join(FemaleMP, by = c("Age", "Years")) %>% 
  left_join(FemaleMP_ultimate, by = "Age") %>% 
  mutate(MP_final_female = ifelse(Years > max(FemaleMP$Years), MP_ultimate_female, MP_female)) %>%
  group_by(Age) %>% 
  mutate(MPcumprod_female_raw = cumprod(1 - MP_final_female),
         MPcumprod_female_adj = MPcumprod_female_raw / MPcumprod_female_raw[Years == 2010]) %>% 
  ungroup()


##Mortality calculations
#Expand grid for ages 20-120 and years 2010 to 2121 (why 2121? Because 120 - 20 + 2021 = 2121)
MortalityTable_int <- expand_grid(EntryYear, entry_age = SalaryEntry$entry_age, Age, YOS) %>% 
  mutate(term_year = EntryYear + YOS,
         Years = EntryYear + Age - entry_age,
         #Years is the same as retirement years
         RetYear = Years)  %>% 
  filter(term_year <= Years) %>% 
  arrange(EntryYear, entry_age, YOS, Age)

#Join base mortality table with mortality improvement table and calculate the final mortality rates
MortalityTable <- MortalityTable_int %>% 
  left_join(SurvivalRates, by = "Age") %>% 
  left_join(MaleMP_final, by = c("Age", "Years")) %>% 
  left_join(FemaleMP_final, by = c("Age", "Years")) %>% 
  
  mutate(RetirementCond_DB = IsRetirementEligible_DB(Age, YOS, RetYear),
         RetirementCond_Hybrid = IsRetirementEligible_Hybrid(Age, YOS, RetYear),
         #ScaleMultiple = ifelse(Age <= 69, 0.5,
         #                      ifelse(Age <= 79, 0.7,
         #                              ifelse(Age <= 84, 0.9, 1))),
         #ScaleMultipleFemaleAct
         
         #pg 60 of 2022 val report. it's adjusted 1.1 for post retirement male
         #adjusted 0.95 for pre retirement female
         #
         #DB Plan
         mort_male_DB = ifelse(RetirementCond_DB == F, Pub2010_employee_male, #Adding adj. facctors
                               Pub2010_healthy_retiree_male * 1.1) * MPcumprod_male_adj,
         mort_female_DB = ifelse(RetirementCond_DB == F, Pub2010_employee_female * 0.95,
                                 Pub2010_healthy_retiree_female) * MPcumprod_female_adj,
         mort_DB = (mort_male_DB + mort_female_DB)/2,
         #
         #Hybrid Plan
         mort_male_Hybrid = ifelse(RetirementCond_Hybrid == F, Pub2010_employee_male, #Adding adj. facctors
                                   Pub2010_healthy_retiree_male * 1.1) * MPcumprod_male_adj,
         mort_female_Hybrid = ifelse(RetirementCond_Hybrid == F, Pub2010_employee_female * 0.95,
                                     Pub2010_healthy_retiree_female) * MPcumprod_female_adj,
         mort_Hybrid = (mort_male_Hybrid + mort_female_Hybrid)/2)

#filter out the necessary variables
MortalityTable <- MortalityTable %>% select(EntryYear, term_year, RetYear, entry_age, Age, YOS, mort_DB, mort_Hybrid)
#arrange(RetYear, entry_age) 

#Create a second mortality table for current retirees
MortalityTable_retire <- expand_grid(Age = Age[Age >= 40], Years = Years[Years >= YearStart]) %>% 
  left_join(SurvivalRates, by = "Age") %>% 
  left_join(MaleMP_final, by = c("Age", "Years")) %>% 
  left_join(FemaleMP_final, by = c("Age", "Years")) %>% 
  mutate(base_age = Age - (Years - YearStart),
         mort_male_DB = Pub2010_healthy_retiree_male * MPcumprod_male_adj * 1.1,
         mort_female_DB = Pub2010_healthy_retiree_female * MPcumprod_female_adj,
         mort_DB = (mort_male_DB + mort_female_DB)/2,
         mort_Hybrid = mort_DB) %>% 
  select(base_age, Age, Years, mort_DB, mort_Hybrid) %>% 
  filter(base_age >= 40) %>% 
  arrange(base_age)

##############################################################################################################################

#Separation Rates
SeparationRates <- expand_grid(EntryYear, Age, YOS) 
SeparationRates <- SeparationRates %>%
  mutate(entry_age = Age - YOS,
         RetYear = EntryYear + YOS) %>% 
  filter(entry_age %in% SalaryHeadCountData$entry_age) %>% 
  arrange(EntryYear, entry_age, Age) %>% 
  left_join(TerminationRateVested, by = "Age") %>%
  left_join(TerminationRateNonVested, by = "YOS") %>%
  left_join(RetirementRates, by = "Age") %>%
  ### Additions ###
  #mutate_all(as.numeric) %>% 
  replace(is.na(.), 0)

#If you're retirement eligible, use the retirement rates, then checks YOS < 5 and use the regular termination rates
SeparationRates <- SeparationRates %>% 
  mutate(retirement_type_DB = RetirementType_DB(Age,YOS, RetYear),
         SepRateMale_DB = ifelse(retirement_type_DB == "Regular", DBMale_Ret_Unred, 
                                 ifelse(retirement_type_DB == "Early", DBMale_Ret_Red,
                                        ifelse(YOS < 5, NonTermVest_Male, TermVest_Male))),
         
         SepRateFemale_DB = ifelse(retirement_type_DB == "Regular", DBFemale_Ret_Unred, 
                                 ifelse(retirement_type_DB == "Early", DBFemale_Ret_Red,
                                        ifelse(YOS < 5, NonTermVest_Female, TermVest_Female))),
         
         SepRate_DB = (SepRateMale_DB + SepRateFemale_DB)/2,
         SepRate_Hybrid = (HybridMale_Ret + HybridFemale_Ret)/2) %>%
  
  group_by(EntryYear, entry_age) %>% 
  
  mutate(RemainingProb_DB = cumprod(1 - lag(SepRate_DB, default = 0)),
         SepProb_DB = lag(RemainingProb_DB, default = 1) - RemainingProb_DB,
         
         RemainingProb_Hybrid = cumprod(1 - lag(SepRate_Hybrid, default = 0)),
         SepProb_Hybrid = lag(RemainingProb_Hybrid, default = 1) - RemainingProb_Hybrid) %>% 
  ungroup()

#Filter out unecessary values
SeparationRates <- SeparationRates %>% select(EntryYear, entry_age, RetYear, Age, YOS, 
                                              RemainingProb_DB, SepProb_DB, SepRate_DB, RemainingProb_Hybrid, SepProb_Hybrid, SepRate_Hybrid)

##############################################################################################################################
##################################################################
##                    Benefit Model Function                    ##
##################################################################

get_benefit_data <- function(
    dr_current = dr_current_,
    dr_new = dr_new_,
    cola_current_active = COLA_current_active,
    cola_new_active = COLA_new_active,
    cola_current_retire = COLA_current_retire,
    cola_current_retire_one = COLA_current_retire_one,
    one_time_cola = one_time_cola_,
    retire_refund_ratio = retire_refund_ratio_,
    cal_factor = cal_factor_
) {
  
  
  #Create a long-form table of Age and YOS and merge with salary data
  SalaryData <- expand_grid(EntryYear, entry_age = SalaryEntry$entry_age, YOS) %>%  
    mutate(Age = entry_age + YOS,
           Years = EntryYear + YOS) %>%
    filter(Age <= MaxAge) %>%
    arrange(EntryYear, entry_age, YOS) %>% 
    left_join(SalaryEntry, by = c("entry_age")) %>% 
    left_join(SalaryGrowth, by = c("YOS")) %>% 
    left_join(SalaryHeadCountData %>% select(EntryYear, entry_age, entry_salary), by = c("EntryYear", "entry_age")) %>% 
    group_by(EntryYear, entry_age) %>%
    mutate(Salary = ifelse(EntryYear <= max(SalaryHeadCountData$EntryYear), entry_salary * sal_cum_growth,
                           start_sal * sal_cum_growth * (1 + payroll_growth_)^(EntryYear - YearStart)),
           #Salary = start_sal*salary_increase_compound*(1 + payroll_growth)^(Years - YOS - YearStart),
           FinalAvgSalary_3YR = rollmean(lag(Salary), k = 3, fill = NA, align = "right"),
           FinalAvgSalary_5YR = rollmean(lag(Salary), k = 5, fill = NA, align = "right"),
           #DB_EEContrib = (DB_EE_cont + DC_DB_EE_cont)*Salary,
           DB_EEContrib = DB_EE_cont*Salary,
           DBEEBalance = cumFV(0.02, DB_EEContrib),
           # DBEEBalance = ifelse(YOS < 3, cumFV(0.02, DB_EEContrib),
           #                      ifelse(YOS >=3 & YOS < 5, cumFV(0.03, DB_EEContrib),
           #                             cumFV(0.03, 1.5*DB_EEContrib))),
           
           DC_EEContrib = DC_EE_cont*Salary,
           Hybrid_EEContrib = DC_DB_EE_cont*Salary,
           Hybrid_EEBalance = ifelse(YOS < 3, cumFV(0.02, (DC_EEContrib + Hybrid_EEContrib)),
                                     ifelse(YOS >=3 & YOS < 5, cumFV(0.03, (DC_EEContrib + Hybrid_EEContrib)),
                                            cumFV(0.03, 1.5*(DC_EEContrib + Hybrid_EEContrib)))),
           
           
           # CBEEContAmount = CB_EE_paycredit * Salary,
           # CBERContAmount = CB_ER_paycredit * Salary,
           # CBEEBalance = cumFV(ICR, CBEEContAmount),
           # CBERBalance = cumFV(ICR, CBERContAmount),
           # CBBalance = CBEEBalance + ifelse(YOS >= CB_vesting, CBERBalance, 0),
           # CumulativeWage = cumFV(ARR, Salary)
    ) %>% 
    ungroup() %>% 
    filter(!is.na(Salary))
  
  
  #Survival Probability and Annuity Factor for active members
  AnnFactorData <- MortalityTable %>% 
    semi_join(SalaryData, by = c("EntryYear", "entry_age")) %>% 
    group_by(EntryYear, entry_age, YOS) %>% 
    mutate(
      DR = ifelse(EntryYear <= YearStart, dr_current, dr_new),
      cum_DR = cumprod(1 + lag(DR, default = 0)),
      surv_DB = cumprod(1 - lag(mort_DB, default = 0)),
      surv_Hybrid = cumprod(1 - lag(mort_Hybrid, default = 0)),
      
      surv_DR_DB = surv_DB/cum_DR,
      # surv_ICR = surv_DB/(1 + ICR)^(Age - min(Age)),
      # surv_ACR = surv_DB/(1 + ACR)^(Age - min(Age)),
      
      COLA = ifelse(EntryYear <= YearStart, cola_current_active, cola_new_active),
      surv_DR_COLA_DB = surv_DR_DB * (1 + COLA)^(Age - min(Age)),
      
      # surv_ACR_COLA = surv_ACR * (1 + COLA)^(Age - min(Age)),
      AnnuityFactor_DR_DB = rev(cumsum(rev(surv_DR_COLA_DB)))/surv_DR_COLA_DB,
      # AnnuityFactor_ACR = rev(cumsum(rev(surv_ACR_COLA)))/surv_ACR_COLA,
      
      surv_DR_Hybrid = surv_Hybrid/cum_DR,
      surv_DR_COLA_Hybrid = surv_DR_Hybrid * (1 + COLA)^(Age - min(Age)),
      AnnuityFactor_DR_Hybrid = rev(cumsum(rev(surv_DR_COLA_Hybrid)))/surv_DR_COLA_Hybrid) %>% 
    ungroup()
  
  #Survival Probability and Annuity Factor for retirees
  AnnFactorData_retire <- MortalityTable_retire %>% 
    group_by(base_age) %>% 
    mutate(surv_DB = cumprod(1 - lag(mort_DB, default = 0)),
           DR = dr_current,
           cum_DR = cumprod(1 + lag(DR, default = 0)),
           surv_DR_DB = surv_DB/cum_DR,
           cola_type = ifelse(one_time_cola == T, "one_time", "normal"),
           cola = ifelse(cola_type == "one_time", ifelse(Years == YearStart + 2, cola_current_retire_one, 0), 
                         cola_current_retire),
           
           surv_DR_COLA_DB = surv_DR_DB * (1 + COLA)^(Age - min(Age)),
           AnnuityFactor_DR_retire = annfactor(surv_DR_vec = surv_DR_DB, cola_vec = cola, one_time_cola = one_time_cola)) %>% 
    ungroup()
  
  ##############################################################################################################################
  
  ReducedFactor <- expand_grid(EntryYear, entry_age = SalaryEntry$entry_age, Age, YOS) %>% 
    semi_join(SalaryData, by = c("EntryYear", "entry_age")) %>% 
    filter(entry_age + YOS <= Age) %>% 
    mutate(Years = EntryYear + Age - entry_age,
           #Years is the same as retirement years
           RetYear = Years) %>% 
    left_join(EarlyRetirement_Before2015, by = c("Age", "YOS")) %>%
    left_join(EarlyRetirement_After2015, by = c("Age", "YOS")) %>%
    replace(is.na(.), 0) %>% 
    #group_by(RetYear, Age, YOS) %>% 
    mutate(RetType = RetirementType_DB(Age, YOS, RetYear),
           RF_Before2015 = ifelse(RetType == "Early", EarlyRetPct_Before2015,
                                  ifelse(RetType == "None", 0, 1)),
           RF_After2015 = ifelse(RetType == "Early", EarlyRetPct_After2015/100/(YOS*BenMult_DB),
                                 ifelse(RetType == "None", 0, 1))) %>% 
    rename(RetirementAge = Age) %>% 
    ungroup() %>% 
    select(-Years, -RetYear)
  
  
  BenefitsTable <- AnnFactorData %>%
    rename(RetirementAge = Age) %>%
    mutate(term_age = entry_age + YOS) %>%
    left_join(SalaryData, by = c("term_age" = "Age", "YOS", "term_year" = "Years", "entry_age", "EntryYear")) %>% 
    left_join(RetirementMultipliers, by = c("YOS")) %>%
    left_join(ReducedFactor, by = c("EntryYear", "entry_age", "RetirementAge", "YOS")) %>%
    
    # YOS is in the benefit section because of graded multipliers
    mutate(BaseBenefit1 = ifelse(YOS >= 35, Cumuative_Mult_2015*FinalAvgSalary_3YR, 0.022*FinalAvgSalary_3YR*YOS),
           BaseBenefit2 = 86*YOS,
           RF = ifelse(RetYear < 2015, RF_Before2015, RF_After2015),
           DBBenefit = ifelse(RetYear < 2015, pmax(BaseBenefit1,BaseBenefit2), 
                              BenMult_DB*FinalAvgSalary_5YR*YOS),
           #cal_factor is a calibration factor added to match the normal cost from the val report.
           DBBenefit = DBBenefit * cal_factor,
           
           AnnFactorAdj_DB = AnnuityFactor_DR_DB * surv_DR_DB,
           DB_Benefit = RF * DBBenefit,
           PV_DB_Benefit = DB_Benefit*AnnFactorAdj_DB,
           
           AnnFactorAdj_Hybrid = AnnuityFactor_DR_Hybrid * surv_DR_Hybrid,
           HybridBenefit_5YR = ifelse(RetirementAge >= 60 & YOS >= 5, BenMult_Hybrid*FinalAvgSalary_5YR*YOS, 0),
           HybridBenefit_3YR = ifelse(RetirementAge >= 60 & YOS >= 5, BenMult_Hybrid*FinalAvgSalary_3YR*YOS, 0),
           #HybridBenefit = ifelse(RetYear >= 2015, HybridBenefit_5YR, HybridBenefit_3YR),
           PV_Hybrid_Benefit = ifelse(RetYear >= 2015, HybridBenefit_5YR, HybridBenefit_3YR),
           
           # CBBalance_final = CBBalance / surv_ICR,                                                       #project the cash balance forward to the annuitization day 
           # CB_Benefit = CBBalance_final / AnnuityFactor_ACR,                                             #annuitize the final cash balance
           # PV_CB_Benefit = ifelse(YOS >= CB_vesting, CB_Benefit * AnnFactorAdj_DB, CBBalance),
           
           #Get retirement eligibility 
           can_retire = IsRetirementEligible_DB(Age = RetirementAge, YOS, RetYear))
  # replace(is.na(.), 0)
  
  
  #For a given combination of entry age and termination age, the member is assumed to choose the retirement age that maximizes the PV of future retirement benefits. That value is the "optimum benefit". 
  # OptimumBenefit <- BenefitsTable %>% 
  #   group_by(EntryYear, entry_age, term_age) %>% 
  #   summarise(MaxBenefit = max(PV_DB_Benefit)) %>%
  #   mutate(MaxBenefit = ifelse(is.na(MaxBenefit), 0, MaxBenefit)) %>% 
  #   ungroup()
  
  
  #Employees are assumed to retire at the earliest age of retirement eligibility
  retire_age_tab <- BenefitsTable %>%
    group_by(EntryYear, entry_age, term_age) %>%
    summarise(RetirementAge = n() - sum(can_retire) + min(RetirementAge)) %>% 
    ungroup() %>% 
    mutate(RetirementAge = ifelse(RetirementAge == 121, term_age, RetirementAge))
  
  
  BenefitsTable_retire <- BenefitsTable %>% 
    semi_join(retire_age_tab) %>% 
    select(EntryYear, entry_age, term_age, RetirementAge, PV_DB_Benefit, PV_Hybrid_Benefit, AnnFactorAdj_DB) %>% 
    mutate(PV_DB_Benefit = ifelse(is.na(PV_DB_Benefit), 0, PV_DB_Benefit))
  
  
  #For a given combination of entry year, entry age and termination age, the DB member is assumed to choose the retirement age that maximizes the PV of future retirement benefits. That value is the "optimum benefit". 
  # OptimumBenefit_DB <- BenefitsTable %>% 
  #   group_by(EntryYear, entry_age, term_age) %>% 
  #   summarise(Max_PV_DB = max(PV_DB_Benefit)) %>%
  #   mutate(Max_PV_DB = ifelse(is.na(Max_PV_DB), 0, Max_PV_DB)) %>%
  #   # filter(entry_age %in% SalaryEntry$entry_age, term_age >= 20) %>%
  #   ungroup() %>% 
  #   #join the BenefitsTable to get the "optimal" retirement age
  #   left_join(BenefitsTable %>% filter(PV_DB_Benefit > 0), by = c("EntryYear", "entry_age", "term_age", "Max_PV_DB" = "PV_DB_Benefit")) %>%
  #   select(EntryYear, entry_age, term_age, RetirementAge, Max_PV_DB) %>% 
  #   mutate(RetirementAge = ifelse(is.na(RetirementAge), term_age, RetirementAge)) %>%
  #   ungroup()#Assume retire age = term age for non-vested members
  # 
  # OptimumBenefit_Hybrid <- BenefitsTable %>% 
  #   group_by(EntryYear, entry_age, term_age) %>% 
  #   summarise(Max_PV_Hybrid = max(PV_Hybrid_Benefit)) %>%
  #   mutate(Max_PV_Hybrid = ifelse(is.na(Max_PV_Hybrid), 0, Max_PV_Hybrid)) %>%
  #   # filter(entry_age %in% SalaryEntry$entry_age, term_age >= 20) %>%
  #   ungroup()
  
  #Using the DB "optimal retirement ages" as retirement ages for the CB plan members
  # OptimumBenefit_CB <- BenefitsTable %>% 
  #   semi_join(OptimumBenefit_DB) %>% 
  #   select(EntryYear, entry_age, term_age, RetirementAge, PV_CB_Benefit)
  
  ##################################################################################################
  
  ####### Benefit Accrual & Normal Cost #######
  #### Real Pension Wealth = Pension Wealth adjusted for inflation
  #### Actuarial PV of Pension Wealth = Pension Wealth 
  #Combine optimal benefit with employee balance and calculate the PV of future benefits and salaries 
  #####################################
  
  #PVFB(sep_rate_vec = FinalData$SepRate_DB, interest = ARR, value_vec = FinalData$DBWealth)
  
  # source("utility_functions.R")
  FinalData <- SalaryData %>% 
    # left_join(OptimumBenefit_DB, by = c("EntryYear", "entry_age", "Age" = "term_age")) %>%
    # left_join(OptimumBenefit_CB, by = c("EntryYear", "entry_age", "Age" = "term_age", "RetirementAge")) %>%
    # left_join(OptimumBenefit_Hybrid, by = c("EntryYear", "entry_age", "Age" = "term_age")) %>% 
    left_join(BenefitsTable_retire, by = c("EntryYear", "entry_age", "Age" = "term_age")) %>% 
    left_join(SeparationRates, by = c("EntryYear", "Age", "YOS", "entry_age", "Years" = "RetYear")) %>%
    group_by(EntryYear, entry_age) %>%
    mutate(
      DR = ifelse(EntryYear <= YearStart, dr_current, dr_new),
      SepType = SeparationType_DB(Age,YOS, Years),
      # LumpSumPct = 0.2,
      #Term vested DB members are assumed to choose between a refund and deferred benefits based on a retire/refund ratio. This ratio can change for calibration purposes.
      DBWealth = ifelse(SepType == 'Retirement', PV_DB_Benefit, 
                        ifelse(SepType == 'Termination Vested', retire_refund_ratio * PV_DB_Benefit + (1 - retire_refund_ratio) * DBEEBalance, DBEEBalance)),
      
      ben_decision = ifelse(YOS == 0, NA, ifelse(SepType == "Retirement", "retire",
                                                 ifelse(SepType == "Termination Vested", "mix", "refund"))),
      #HybridWealth = ifelse(SepType == 'Retirement', pmax(Hybrid_EEBalance,Max_PV_Hybrid), 
      #                   ifelse(SepType == 'Termination Vested', LumpSumPct*Hybrid_EEBalance + (1-LumpSumPct)*Max_PV_Hybrid, Hybrid_EEBalance)),
      LumpSumPct_Hybrid = 0.3,
      HybridWealth = LumpSumPct_Hybrid*Hybrid_EEBalance + (1-LumpSumPct_Hybrid)*PV_Hybrid_Benefit,
      # CBWealth = ifelse(DBWealth == DBEEBalance, CBBalance, PV_CB_Benefit),   #mimic DB members' behavior. This is to simplify the workforce projection done later.
      
      RealPenWealth = DBWealth/(1 + assum_infl)^YOS,
      Real_DBWealth = DBWealth/(1 + assum_infl)^YOS,
      # Real_CBWealth = CBWealth/(1 + assum_infl)^YOS,
      Real_HybridWealth = HybridWealth/(1 + assum_infl)^YOS,
      
      
      #Calculate present value of future benefits (PVFB) 
      PVFB_DB = PVFB(sep_rate_vec = SepRate_DB, interest_vec = DR, value_vec = DBWealth),
      # PVFB_CB = PVFB(sep_rate_vec = SepRate_DB, interest = ARR, value_vec = CBWealth),
      PVFB_Hybrid = PVFB(sep_rate_vec = SepRate_Hybrid, interest_vec = DR, value_vec = HybridWealth),
      
      #Calculate present value of future salaries (PVFS)
      PVFS = PVFS(remaining_prob_vec = RemainingProb_DB, interest_vec = DR, sal_vec = Salary),
      normal_cost_DB = PVFB_DB[YOS == 0] / PVFS[YOS == 0],
      # normal_cost_CB = PVFB_CB[YOS == 0] / PVFS[YOS == 0],
      normal_cost_Hybrid = PVFB_Hybrid[YOS == 0] / PVFS[YOS == 0],
      
      #Calculate present value of future normal costs (PVFNC)
      PVFNC_DB = PVFS * normal_cost_DB,
      PVFNC_Hybrid = PVFS * normal_cost_Hybrid
      # PVFNC_CB = PVFS * normal_cost_CB
    ) %>%
    # replace(is.na(.), 0) %>%
    ungroup()
  
  
  #Calculate normal cost rate for each entry age in each entry year
  NormalCost <- FinalData %>% 
    filter(YOS == 0) %>% 
    select(EntryYear, entry_age, normal_cost_DB, normal_cost_Hybrid)
  
  #Calculate the aggregate normal cost for current year (for testing purposes)
  NC_aggregate <- NormalCost %>% 
    left_join(SalaryHeadCountData, by = c("EntryYear", "entry_age")) %>%
    left_join(SalaryData %>% select(EntryYear, entry_age, Age, Salary), by = c("EntryYear", "entry_age", "Age")) %>% 
    filter(!is.na(Count)) %>% 
    summarise(normal_cost_aggregate_DB = sum(normal_cost_DB * Salary * Count) / sum(Salary * Count),
              normal_cost_aggregate_Hybrid = sum(normal_cost_Hybrid * Salary * Count) / sum(Salary * Count)
              # normal_cost_aggregate_CB = sum(normal_cost_CB * Salary * HeadCount) / sum(Salary * HeadCount)
    )
  
  output <- list(ann_factor_tab = AnnFactorData,
                 ann_factor_retire_tab = AnnFactorData_retire,
                 reduced_factor = ReducedFactor,
                 ben_tab = BenefitsTable,
                 ben_retire_tab = BenefitsTable_retire,
                 final_tab = FinalData,
                 nc_tab = NormalCost,
                 nc_agg = NC_aggregate)
  
  return(output)
  
}
#Calculate the aggregate normal cost
#NC_aggregate  
################################


# ####### DC Account Balance 
# SalaryData2 <- SalaryData %>% 
#   filter(entry_age == HiringAge) %>% 
#   select(Age, YOS, start_sal, entry_age, HireType, salary_increase, Salary, RemainingProb) %>% 
#   mutate(DC_EEContrib = Salary * DC_EE_cont,
#          DC_ERContrib = Salary * DC_ER_cont,
#          DC_Contrib = DC_EEContrib + DC_ERContrib,
#          DC_balance = cumFV(DC_return, DC_Contrib),
#          RealDC_balance = DC_balance/(1 + assum_infl)^YOS) %>% 
#   left_join(SalaryData %>% select(Age, YOS, RealPenWealth), by = c("Age", "YOS")) %>% 
#   mutate(RealHybridWealth = RealDC_balance + RealPenWealth)

## Test benefit function
# 
# NC <- benefit_cal()
# NC2 <- benefit_cal(DB_ARR = 0.06, DB_mult = 0.01)
# DB_4 <- benefit_cal(output = "DB", DB_ARR = 0.04, ea = 22)
# DB_7 <- benefit_cal(output = "DB", DB_ARR = 0.07, ea = 22)
# DC <- benefit_cal(output = "DC", DCreturn = 0.06, ea = 22)
# attri <- benefit_cal(output = "attrition", ea = 22)
# 
# 
# test <- DB_4 %>%
#   left_join(DB_7, by = "Age") %>%
#   left_join(DC, by = "Age") %>%
#   pivot_longer(cols = 2:4,
#                names_to = "type",
#                values_to = "wealth")
# 
# ggplot(test, aes(x = Age, y = wealth, col = type)) +
#   geom_line()
# 
# ggplot(attri, aes(x = Age, y = RemainingProb)) +
#   geom_line()
##################################
