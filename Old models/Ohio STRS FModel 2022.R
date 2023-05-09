library("readxl")
library(ggplot2)
library(tidyverse)
setwd(getwd())
rm(list = ls())
#
#User can change these values
StartYear <- 2020
StartProjectionYear <- 2023
EndProjectionYear <- 2052
FileName <- 'Ohio STRS FModel Inputs 2022.xlsx'
#
#Reading Input File
user_inputs_numeric <- read_excel(FileName, sheet = 'Numeric Inputs')
user_inputs_character <- read_excel(FileName, sheet = 'Character Inputs')
Historical_Data <- read_excel(FileName, sheet = 'Historical Data')
Scenario_Data <- read_excel(FileName, sheet = 'Inv_Returns')
BenefitPayments <- read_excel(FileName, sheet = 'Benefit Payments')

source('Ohio STRS DB BModel.R')
#
##################################################################################################################################################################
#
#Functions for later use
#Function for Present Value for Amortization
PresentValue = function(rate, nper, pmt) {
  PV = pmt * (1 - (1 + rate) ^ (-nper)) / rate * (1 + rate)
  return(PV)
}

NPV = function(rate, cashflows) {
  for(i in 1:length(cashflows)){
    if(i == 1){
      NPV <- cashflows[i]/((1+rate)^(i))
    } else {
      NPV <- NPV + cashflows[i]/((1+rate)^(i))
    }
  }
  
  return(NPV)
}

#Function for calculating amo payments
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

#pmt = amo payment function with growth rate and timing added; t = 1 for end of period payment, 0.5 for half period. 
PMT <- function(r, g = 0, nper, pv, t = 1) {
  a <- PMT0((1+r)/(1+g) - 1, nper, pv*(1+r)^t)
  return(a)
}
#
##################################################################################################################################################################
#
#Reading Values from Input input file and assigning values
#Assigning numeric inputs
for(i in 1:nrow(user_inputs_numeric)){
  if(!is.na(user_inputs_numeric[i,2])){
    assign(as.character(user_inputs_numeric[i,2]),as.double(user_inputs_numeric[i,3]))
  }
}
#
#Assigning character inputs
for(i in 1:nrow(user_inputs_character)){
  if(!is.na(user_inputs_character[i,2])){
    assign(as.character(user_inputs_character[i,2]),as.character(user_inputs_character[i,3]))
  }
}

#Create an empty Matrix for the Projection Years
EmptyMatrix <- matrix(0,(EndProjectionYear - StartProjectionYear + 1), 1)
for(j in 1:length(colnames(Historical_Data))){
  TempMatrix <- rbind(as.matrix(Historical_Data[,j]), EmptyMatrix)
  assign(as.character(colnames(Historical_Data)[j]), TempMatrix)
}
#Assign values for Projection Years
FYE <- as.matrix(StartYear:EndProjectionYear)
colnames(FYE) <- "FYE"
#Get Start Index, since historical data has 3 rows, we want to start at 4
StartIndex <- StartProjectionYear - StartYear + 1
HistoricalIndex <- StartProjectionYear - StartYear

#Initialize Amortization and Outstnading Base
RowColCount <- (EndProjectionYear - StartProjectionYear + 1)

##Amo period tables
#currentlayer <- seq(NoYearsADC_CurrentHire, 1)
currentlayer <- seq(24, 1)
futurelayer_currenthire <- seq(NoYearsADC_CurrentHire, 1)
futurelayer_futurehire <- seq(NoYearsADC_NewHire, 1)
n <- max(length(currentlayer), length(futurelayer_currenthire))
length(currentlayer) <- n
length(futurelayer_currenthire) <- n

#Amo period table for current hires plan
OffsetYears_CurrentHires <- rbind(currentlayer, matrix(futurelayer_currenthire, 
                                                       nrow = RowColCount,
                                                       ncol = length(currentlayer),
                                                       byrow = T))

rownames(OffsetYears_CurrentHires) <- NULL         #Remove row names

for (i in 1:ncol(OffsetYears_CurrentHires)) {      #Put the amo periods on diagonal rows
  OffsetYears_CurrentHires[,i] <- lag(OffsetYears_CurrentHires[,i], n = i - 1)
}
OffsetYears_CurrentHires[is.na(OffsetYears_CurrentHires)] <- 0    #Turn all NAs in the table to 0s

#Amo period table for future hires plan
OffsetYears_NewHires <- matrix(futurelayer_futurehire, 
                               nrow = RowColCount + 1,
                               ncol = length(futurelayer_futurehire),
                               byrow = T)

for (i in 1:ncol(OffsetYears_NewHires)) {      #Put the amo periods on diagonal rows
  OffsetYears_NewHires[,i] <- lag(OffsetYears_NewHires[,i], n = i - 1)
}
OffsetYears_NewHires[is.na(OffsetYears_NewHires)] <- 0    #Turn all NAs in the table to 0s


#Amo base & payment - current hires initial setup
OutstandingBase_CurrentHires <- matrix(0, RowColCount + 1, length(currentlayer) + 1)
Amortization_CurrentHires <- matrix(0, RowColCount + 1, length(currentlayer))
#Initialize the first UAAL layer and amo payment (current hires)
OutstandingBase_CurrentHires[1,1] <- UAL_AVA_CurrentHires[HistoricalIndex]
Amortization_CurrentHires[1,1] <- PMT(pv = OutstandingBase_CurrentHires[1,1],
                                      r = NewDR_CurrentHires[HistoricalIndex],
                                      g = AmoBaseInc_CurrentHire, 
                                      nper = OffsetYears_CurrentHires[1,1], 
                                      t = 0.5)

#Amo base & payment - future hires initial setup
OutstandingBase_NewHires <- matrix(0,RowColCount + 1, length(futurelayer_futurehire) + 1)
Amortization_NewHires <- matrix(0,RowColCount + 1, length(futurelayer_futurehire))


##################################################################################################################################################################

RunModel <- function(DR_CurrentHires = dis_r_proj_currentHires,
                     DR_NewHires = dis_r_proj_newHires,
                     ReturnType = AnalysisType,
                     DeSimType = ScenType,
                     FundingPolicy = ER_Policy,
                     CostSharing_AmoNew = CostSharing_Amo_NewHire,
                     CostSharing_NCNew = CostSharing_NC_NewHire,
                     CostSharing_AmoCurrent = CostSharing_Amo_CurrentHire,
                     CostSharing_NCCurrent = CostSharing_NC_CurrentHire,
                     #CurrentDebt_period = NoYearsADC_CurrentDebt,
                     #NewDebtCurrentHire_period = NoYearsADC_NewDebtCurrentHire,
                     #NewDebtNewHire_period = NoYearsADC_NewDebtNewHire,
                     #AmoMethod_current = AmoMethod_CurrentHire,
                     #AmoMethod_new = AmoMethod_NewHire,
                     #OneTimeInfusion = CashInfusion,
                     DC_choice = DC_Enrolled_Pct,
                     Combined_choice = CombinedPlan_Enrolled_Pct){ 
  
  #Scenario Index for referencing later based on investment return data
  ScenarioIndex <- which(colnames(Scenario_Data) == as.character(DeSimType))
  #intialize this value at 0 for Total ER Contributions
  Total_ER[StartIndex-1] <- 0
  
  #Use this ratio for the current hire DB/DC/Combined Plan split
  DBCurrentRatio <- DBPayroll_CurrentHires[HistoricalIndex] / TotalPayroll[HistoricalIndex]
  DCCurrentRatio <- DCPayroll_CurrentHires[HistoricalIndex] / TotalPayroll[HistoricalIndex]
  CombinedCurrentRatio <- CombinedPlanPayroll_CurrentHires[HistoricalIndex] / TotalPayroll[HistoricalIndex]
  ARPCurrentRatio <- ARPPayroll_CurrentHires[HistoricalIndex] / TotalPayroll[HistoricalIndex]
  
  for(i in StartIndex:length(FYE)){
    #Payroll
    TotalPayroll[i] <- TotalPayroll[i-1]*(1 + Payroll_growth)
    PayrollIncrease <- TotalPayroll[i-1]*(Payroll_growth)
    CurrentHirePct[i] <- CurrentHirePct[i-1]*0.95
    CurrentHirePayroll[i] <- TotalPayroll[i]*CurrentHirePct[i]
    NewHirePayroll[i] <- TotalPayroll[i]*(1 - CurrentHirePct[i])
    #
    #Current Hire and New Hire split
    DBPayroll_CurrentHires[i] <- CurrentHirePayroll[i]*DBCurrentRatio
    DCPayroll_CurrentHires[i] <- CurrentHirePayroll[i]*DCCurrentRatio
    CombinedPlanPayroll_CurrentHires[i] <- CurrentHirePayroll[i]*CombinedCurrentRatio
    ARPPayroll_CurrentHires[i] <- CurrentHirePayroll[i]*ARPCurrentRatio
    
    DBPayroll_NewHires[i] <- NewHirePayroll[i]*(1 - (Combined_choice + DC_choice))
    DCPayroll_NewHires[i] <- NewHirePayroll[i]*DC_choice
    CombinedPlanPayroll_NewHires[i] <- NewHirePayroll[i]*Combined_choice
    ARPPayroll_NewHires[i] <- NewHirePayroll[i] - (DBPayroll_NewHires[i] + CombinedPlanPayroll_NewHires[i] + DCPayroll_NewHires[i])
    #
    #
    #Discount Rate
    OriginalDR_CurrentHires[i] <- dis_r_currentHires
    NewDR_CurrentHires[i] <- DR_CurrentHires
    OriginalDR_NewHires[i] <- dis_r_newHires
    NewDR_NewHires[i] <- DR_NewHires
    #
    #Benefit Payments, Admin Expenses, Refunds, Transfer
    BenPayments_Total[i] <- BenPayments_Total[i-1]*(1+BenGrowthMax)
    BenPayments_NewHires[i] <- BenPayments_Total[i]*(1 - CurrentHirePct[i])
    BenPayments_CurrentHires[i] <- BenPayments_Total[i] - BenPayments_NewHires[i]
    AdminExp[i] <- -1*Admin_Exp_Pct*TotalPayroll[i]
    AdminExp_CurrentHires[i] <- -1*Admin_Exp_Pct*CurrentHirePayroll[i]
    AdminExp_NewHires[i] <- -1*Admin_Exp_Pct*NewHirePayroll[i]
    #
    #Accrued Liability, MOY NC - Original DR
    MOYNCExistOrigDR[i] <- NC_DB_Pct_CurrentHire*DBPayroll_CurrentHires[i] + NC_Combined_Pct_CurrentHire*CombinedPlanPayroll_CurrentHires[i]
    MOYNCNewHiresOrigDR[i] <- NC_DB_Pct_NewHire*DBPayroll_NewHires[i] + NC_Combined_Pct_NewHire*CombinedPlanPayroll_NewHires[i]
    AccrLiabOrigDR_CurrentHires[i] <- AccrLiabOrigDR_CurrentHires[i-1]*(1+OriginalDR_CurrentHires[i]) + (MOYNCExistOrigDR[i] + BenPayments_CurrentHires[i])*(1+OriginalDR_CurrentHires[i])^0.5
    AccrLiabOrigDR_NewHires[i] <- AccrLiabOrigDR_NewHires[i-1]*(1+OriginalDR_CurrentHires[i]) + (MOYNCNewHiresOrigDR[i] + BenPayments_NewHires[i])*(1+OriginalDR_NewHires[i])^0.5
    AccrLiabOrigDR_Total[i] <- AccrLiabOrigDR_CurrentHires[i] + AccrLiabOrigDR_NewHires[i]
    # #
    #Accrued Liability, MOY NC - New DR
    DRDifference_CurrentHires <- 100*(OriginalDR_CurrentHires[i] - NewDR_CurrentHires[i])
    DRDifference_NewHires <- 100*(OriginalDR_NewHires[i] - NewDR_NewHires[i])
    MOYNCExistNewDR[i] <- MOYNCExistOrigDR[i]*((1+(NCSensDR/100))^(DRDifference_CurrentHires))
    MOYNCNewHiresNewDR[i] <- MOYNCNewHiresOrigDR[i]*((1+(NCSensDR/100))^(DRDifference_NewHires))
    AccrLiabNewDR_CurrentHires[i] <- AccrLiabOrigDR_CurrentHires[i]*((1+(LiabSensDR/100))^(DRDifference_CurrentHires))*((1+(Convexity/100))^((DRDifference_CurrentHires)^2/2))
    AccrLiabNewDR_NewHires[i] <- AccrLiabOrigDR_NewHires[i]*((1+(LiabSensDR/100))^(DRDifference_NewHires))*((1+(Convexity/100))^((DRDifference_NewHires)^2/2))
    AccrLiabNewDR_Total[i] <- AccrLiabNewDR_CurrentHires[i] + AccrLiabNewDR_NewHires[i]
    
    #ProjectionCount is used because amortization and return scenarios do not start at the same time as start index
    #Because start index includes historical data and thus might be 3 by the time ProjectionCount is 1
    ProjectionCount <- i - StartIndex + 1
    
    #EE NC, Contrib and Cost Sharing and Amo Policy
    #Hard coded #s for now
    Total_NC_CurrentHires_Pct[i] <- 0.1061
    Total_NC_NewHires_Pct[i] <- 0.1061
    Total_NC_DB_CurrentHires_Pct[i] <- 0.1086
    Total_NC_Combined_CurrentHires_Pct[i] <- 0.0458
    Total_NC_DB_NewHires_Pct[i] <- 0.1086
    Total_NC_Combined_NewHires_Pct[i] <- 0.0458
    
    ER_Contrib_DB_CurrentHires_Pct[i] <- ERContribDB_Stat_CurrentHire
    ER_Contrib_Combined_CurrentHires_Pct[i] <- ERContribCombined_Stat_CurrentHire
    ER_Contrib_DB_NewHires_Pct[i] <- ERContribDB_Stat_NewHire
    ER_Contrib_Combined_NewHires_Pct[i] <- ERContribCombined_Stat_NewHire
    
    if(CostSharing_NCCurrent == 'Yes'){
      EE_NC_DB_CurrentHires_Pct[i] <- Total_NC_DB_CurrentHires_Pct[i]/2
      EE_NC_Combined_CurrentHires_Pct[i] <- Total_NC_Combined_CurrentHires_Pct[i]/2
    } else {
      EE_NC_DB_CurrentHires_Pct[i] <- EEContribDB_CurrentHire
      EE_NC_Combined_CurrentHires_Pct[i] <- EEContribCombined_DB_Transfer_CurrentHire
    }
    
    if(CostSharing_NCNew == 'Yes'){
      EE_NC_DB_NewHires_Pct[i] <- Total_NC_DB_NewHires_Pct[i]/2
      EE_NC_Combined_NewHires_Pct[i] <- Total_NC_Combined_NewHires_Pct[i]/2  
    } else {
      EE_NC_DB_NewHires_Pct[i] <- EEContribDB_NewHire
      EE_NC_Combined_NewHires_Pct[i] <- EEContribCombined_DB_Transfer_NewHire
    }
    
    ER_NC_DB_CurrentHires_Pct[i] <- Total_NC_DB_CurrentHires_Pct[i] - EE_NC_DB_CurrentHires_Pct[i]
    ER_NC_Combined_CurrentHires_Pct[i] <- Total_NC_Combined_CurrentHires_Pct[i] - EE_NC_Combined_CurrentHires_Pct[i]
    ER_NC_DB_NewHires_Pct[i] <- Total_NC_DB_NewHires_Pct[i] - EE_NC_DB_NewHires_Pct[i]
    ER_NC_Combined_NewHires_Pct[i] <- Total_NC_Combined_NewHires_Pct[i] - EE_NC_Combined_NewHires_Pct[i]
    
    #Amo Policy
    if(FundingPolicy == 'Statutory'){
      AmoRate_Stat_DB_CurrentHires[i] <- ER_Contrib_DB_CurrentHires_Pct[i] - ER_NC_DB_CurrentHires_Pct[i]
      AmoRate_Stat_Combined_CurrentHires[i] <- ER_Contrib_Combined_CurrentHires_Pct[i] - ER_NC_Combined_CurrentHires_Pct[i]
      AmoRate_Stat_DB_NewHires[i] <- ER_Contrib_DB_NewHires_Pct[i] - ER_NC_DB_NewHires_Pct[i]
      AmoRate_Stat_Combined_NewHires[i] <- ER_Contrib_Combined_NewHires_Pct[i] - ER_NC_Combined_NewHires_Pct[i]
      
      AmoRate_CurrentHires[i] <- (AmoRate_Stat_DB_CurrentHires[i]*DBPayroll_CurrentHires[i] +
                                    AmoRate_Stat_Combined_CurrentHires[i]*CombinedPlanPayroll_CurrentHires[i] +
                                    ERContrib_DC_DB_UAL_CurrentHire*DCPayroll_CurrentHires[i] +
                                    ERContrib_DC_ARP_UAL_CurrentHire*ARPPayroll_CurrentHires[i]) / CurrentHirePayroll[i]
      
      AmoRate_NewHires[i] <- (AmoRate_Stat_DB_NewHires[i]*DBPayroll_NewHires[i] +
                                AmoRate_Stat_Combined_NewHires[i]*CombinedPlanPayroll_NewHires[i] +
                                ERContrib_DC_DB_UAL_NewHire*DCPayroll_NewHires[i] +
                                ERContrib_DC_ARP_UAL_NewHire*ARPPayroll_NewHires[i]) / NewHirePayroll[i]
      
    } else if(FundingPolicy == 'ADEC'){
      AmoRate_CurrentHires[i] <- sum(Amortization_CurrentHires[ProjectionCount,]) / TotalPayroll[i]
      AmoRate_NewHires[i] <- sum(Amortization_NewHires[ProjectionCount,]) / NewHirePayroll[i]
    }
    
    if(CostSharing_AmoCurrent == 'Yes'){
      EEAmoRate_CurrentHires[i] <- AmoRate_CurrentHires[i]/2
    } else {
      EEAmoRate_CurrentHires[i] <- 0
    }
    
    if(CostSharing_AmoNew == 'Yes'){
      EEAmoRate_NewHires[i] <- AmoRate_NewHires[i]/2
    } else {
      EEAmoRate_NewHires[i] <- 0
    }
    
    #
    #EE, ER and Amo Cashflows
    EE_NC_CurrentHires[i] <- EE_NC_DB_CurrentHires_Pct[i]*DBPayroll_CurrentHires[i] +  EE_NC_Combined_CurrentHires_Pct[i]*CombinedPlanPayroll_CurrentHires[i]
    EE_NC_NewHires[i] <- EE_NC_DB_NewHires_Pct[i]*DBPayroll_NewHires[i] + EE_NC_Combined_NewHires_Pct[i]*CombinedPlanPayroll_NewHires[i]
    ER_NC_CurrentHires[i] <- ER_NC_DB_CurrentHires_Pct[i]*DBPayroll_CurrentHires[i] +  ER_NC_Combined_CurrentHires_Pct[i]*CombinedPlanPayroll_CurrentHires[i]
    ER_NC_NewHires[i] <- ER_NC_DB_NewHires_Pct[i]*DBPayroll_NewHires[i] + ER_NC_Combined_NewHires_Pct[i]*CombinedPlanPayroll_NewHires[i]
    
    EE_Amo_CurrentHires[i] <- EEAmoRate_CurrentHires[i]*(DBPayroll_CurrentHires[i] + CombinedPlanPayroll_CurrentHires[i])
    EE_Amo_NewHires[i] <- EEAmoRate_NewHires[i]*(DBPayroll_NewHires[i] + CombinedPlanPayroll_NewHires[i])
    ER_Amo_CurrentHires[i] <- max(AmoRate_CurrentHires[i]*TotalPayroll[i] - EE_Amo_CurrentHires[i], -ER_NC_CurrentHires[i])
    ER_Amo_NewHires[i] <- max(AmoRate_NewHires[i]*NewHirePayroll[i] - EE_Amo_NewHires[i], -ER_NC_NewHires[i])
    #
    #Return based on Deterministic or Stochastic
    if(ReturnType == 'Stochastic'){
      ROA_MVA[i] <- rnorm(1,Sim_Return,Sim_Volatility)
    } else if(ReturnType == 'Deterministic'){
      ROA_MVA[i] <- as.double(Scenario_Data[i,ScenarioIndex]) 
    }
    #
    #Cash Flows and Solvency Contribution
    #Add EE Amo evenutally (Cost sharing)
    #Add supplementary Contribution
    Cashflows_Current[i] <- BenPayments_CurrentHires[i] + AdminExp_CurrentHires[i] + EE_NC_CurrentHires[i] + ER_NC_CurrentHires[i] + EE_Amo_CurrentHires[i] + ER_Amo_CurrentHires[i]
    Cashflows_New[i] <- BenPayments_NewHires[i] + AdminExp_NewHires[i] + EE_NC_NewHires[i] + ER_NC_NewHires[i] + EE_Amo_NewHires[i] + ER_Amo_NewHires[i]
    Cashflows_Total[i] <- Cashflows_Current[i] + Cashflows_New[i]
    
    Solv_Contrib_Total[i] <- as.double(max(-(MVA[i-1]*(1+ROA_MVA[i]) + Cashflows_Total[i]*(1+ROA_MVA[i])^0.5) / (1+ROA_MVA[i])^0.5,0))
    Solv_Contrib_CurrentHires[i] <- Solv_Contrib_Total[i]*(AccrLiabNewDR_CurrentHires[i] / AccrLiabOrigDR_Total[i])
    Solv_Contrib_NewHires[i] <- Solv_Contrib_Total[i]*(AccrLiabNewDR_NewHires[i]/AccrLiabOrigDR_Total[i])
    #
    #Net CF, Expected MVA, Solvency Contribution
    NetCF_CurrentHires[i] <- Cashflows_Current[i] + Solv_Contrib_NewHires[i]
    ExpInvInc_CurrentHires[i] <- (MVA_CurrentHires[i-1]*NewDR_CurrentHires[i-1]) + (NetCF_CurrentHires[i]*NewDR_CurrentHires[i-1]*0.5)
    MVA_CurrentHires[i] <- MVA_CurrentHires[i-1]*(1+ROA_MVA[i]) + NetCF_CurrentHires[i]*(1+ROA_MVA[i])^0.5
    ActualReturnMVA_CurrentHires[i] <- MVA_CurrentHires[i] - MVA_CurrentHires[i-1] - NetCF_CurrentHires[i]
    
    NetCF_NewHires[i] <- Cashflows_New[i] + Solv_Contrib_CurrentHires[i]
    ExpInvInc_NewHires[i] <- (MVA_NewHires[i-1]*NewDR_NewHires[i-1]) + (NetCF_NewHires[i]*NewDR_NewHires[i-1]*0.5)
    MVA_NewHires[i] <- MVA_NewHires[i-1]*(1+ROA_MVA[i]) + NetCF_NewHires[i]*(1+ROA_MVA[i])^0.5
    ActualReturnMVA_NewHires[i] <- MVA_NewHires[i] - MVA_NewHires[i-1] - NetCF_NewHires[i]
    #
    #Gain Loss, Defered Losses
    GainLoss_CurrentHires[i] <- ActualReturnMVA_CurrentHires[i] - ExpInvInc_CurrentHires[i] 
    DeferedCurYear_CurrentHires[i] <- GainLoss_CurrentHires[i]*(0.75/1)
    Year1DeferedLosses_CurrentHires[i] <- DeferedCurYear_CurrentHires[i-1]*(0.5/0.75)
    Year2DeferedLosses_CurrentHires[i] <- Year1DeferedLosses_CurrentHires[i-1]*(0.25/0.5)
    Year3DeferedLosses_CurrentHires[i] <- Year2DeferedLosses_CurrentHires[i-1]*(0/0.25)
    TotalDeferedLosses_CurrentHires[i] <- Year1DeferedLosses_CurrentHires[i] + Year2DeferedLosses_CurrentHires[i] + Year3DeferedLosses_CurrentHires[i] + DeferedCurYear_CurrentHires[i]
    
    GainLoss_NewHires[i] <- ActualReturnMVA_NewHires[i] - ExpInvInc_NewHires[i] 
    DeferedCurYear_NewHires[i] <- GainLoss_NewHires[i]*(0.75/1)
    Year1DeferedLosses_NewHires[i] <- DeferedCurYear_NewHires[i-1]*(0.5/0.75)
    Year2DeferedLosses_NewHires[i] <- Year1DeferedLosses_NewHires[i-1]*(0.25/0.5)
    Year3DeferedLosses_NewHires[i] <- Year2DeferedLosses_NewHires[i-1]*(0/0.25)
    TotalDeferedLosses_NewHires[i] <- Year1DeferedLosses_NewHires[i] + Year2DeferedLosses_NewHires[i] + Year3DeferedLosses_NewHires[i] + DeferedCurYear_NewHires[i]
    #
    #AVA, MVA, UAL, FR
    AVA_CurrentHires[i] <- MVA_CurrentHires[i] - TotalDeferedLosses_CurrentHires[i]
    AVA_CurrentHires[i] <- max(AVA_CurrentHires[i],AVA_lowerbound*MVA_CurrentHires[i])
    AVA_CurrentHires[i] <- min(AVA_CurrentHires[i],AVA_upperbound*MVA_CurrentHires[i])
    UAL_AVA_CurrentHires[i] <- AccrLiabNewDR_CurrentHires[i] - AVA_CurrentHires[i]
    UAL_MVA_CurrentHires[i] <- AccrLiabNewDR_CurrentHires[i] - MVA_CurrentHires[i]
    
    AVA_NewHires[i] <- MVA_NewHires[i] - TotalDeferedLosses_NewHires[i]
    AVA_NewHires[i] <- max(AVA_NewHires[i],AVA_lowerbound*MVA_NewHires[i])
    AVA_NewHires[i] <- min(AVA_NewHires[i],AVA_upperbound*MVA_NewHires[i])
    UAL_AVA_NewHires[i] <- AccrLiabNewDR_NewHires[i] - AVA_NewHires[i]
    UAL_MVA_NewHires[i] <- AccrLiabNewDR_NewHires[i] - MVA_NewHires[i]
    
    AVA[i] <- AVA_CurrentHires[i] + AVA_NewHires[i]
    MVA[i] <- MVA_CurrentHires[i] + MVA_NewHires[i]
    FR_AVA[i] <- AVA[i] / AccrLiabNewDR_Total[i]
    FR_MVA[i] <- MVA[i] / AccrLiabNewDR_Total[i]
    UAL_AVA[i] <- UAL_AVA_CurrentHires[i] + UAL_AVA_NewHires[i]
    UAL_MVA[i] <- UAL_MVA_CurrentHires[i] + UAL_MVA_NewHires[i]
    UAL_AVA_InflAdj[i] <- UAL_AVA[i] / ((1 + asum_infl)^(FYE[i] - 2021))
    UAL_MVA_InflAdj[i] <- UAL_MVA[i] / ((1 + asum_infl)^(FYE[i] - 2021))
    #
    #Total Contrib
    Total_ERContrib[i] <- ER_NC_CurrentHires[i] + ER_NC_NewHires[i] + ER_Amo_CurrentHires[i] + ER_Amo_NewHires[i] + Solv_Contrib_Total[i]
    ER_Percentage[i] <- Total_ERContrib[i] /  TotalPayroll[i]
    Total_ER[i] <- Total_ER[i-1] + ER_InflAdj[i]
    ER_InflAdj <- Total_ERContrib[i] / ((1 + asum_infl)^(FYE[i] - 2021))
    AllInCost <- Total_ER[i] + UAL_MVA_InflAdj[i]
    #
    ##Amortization
    #Current Hires
    if(ProjectionCount < nrow(Amortization_CurrentHires)){
      OutstandingBase_CurrentHires[ProjectionCount+1,2:ncol(OutstandingBase_CurrentHires)] <- OutstandingBase_CurrentHires[ProjectionCount,1:(ncol(OutstandingBase_CurrentHires)-1)]*(1 + NewDR_CurrentHires[i]) - (Amortization_CurrentHires[ProjectionCount,1:ncol(Amortization_CurrentHires)]*(1 + NewDR_CurrentHires[i])^0.5)
      OutstandingBase_CurrentHires[ProjectionCount+1,1] <- UAL_AVA_CurrentHires[i] - sum(OutstandingBase_CurrentHires[ProjectionCount+1,2:ncol(OutstandingBase_CurrentHires)])
      
      #Amo Layers
      Amortization_CurrentHires[ProjectionCount+1,1:ncol(Amortization_CurrentHires)] <- PMT(pv = OutstandingBase_CurrentHires[ProjectionCount+1,1:(ncol(OutstandingBase_CurrentHires)-1)], 
                                                                                            r = NewDR_CurrentHires[i], 
                                                                                            g = AmoBaseInc_CurrentHire, 
                                                                                            t = 0.5,
                                                                                            nper = pmax(OffsetYears_CurrentHires[ProjectionCount+1,1:ncol(OffsetYears_CurrentHires)],1))
    }
    
    #New Hires
    if(ProjectionCount < nrow(Amortization_NewHires)){
      OutstandingBase_NewHires[ProjectionCount+1,2:ncol(OutstandingBase_NewHires)] <- OutstandingBase_NewHires[ProjectionCount,1:(ncol(OutstandingBase_NewHires)-1)]*(1 + NewDR_NewHires[i]) - (Amortization_NewHires[ProjectionCount,1:ncol(Amortization_NewHires)]*(1 + NewDR_NewHires[i])^0.5)
      OutstandingBase_NewHires[ProjectionCount+1,1] <- UAL_AVA_NewHires[i] - sum(OutstandingBase_NewHires[ProjectionCount+1,2:ncol(OutstandingBase_NewHires)])
      
      #Amo Layers
      Amortization_NewHires[ProjectionCount+1,1:ncol(Amortization_NewHires)] <- PMT(pv = OutstandingBase_NewHires[ProjectionCount+1,1:(ncol(OutstandingBase_NewHires)-1)], 
                                                                                    r = NewDR_NewHires[i], 
                                                                                    g = AmoBaseInc_NewHire, 
                                                                                    t = 0.5,
                                                                                    nper = pmax(OffsetYears_NewHires[ProjectionCount+1,1:ncol(OffsetYears_NewHires)],1))
    }
  }
  
  Output <- FYE
  for(i in 2:length(Historical_Data)){
    Output <- cbind(Output, get(colnames(Historical_Data)[i]))
  }

  return(as.data.frame(Output))
}


##################################################################################################################################################################



#Test
# Test_baseline <- RunModel()
# Test_baseline_DB <- RunModel(NewHire_Plan = "DB", NewHireDC_choice = 0.5)
# Test_baseline_DCchoice <- RunModel(NewHire_Plan = "DB-DC Choice", NewHireDC_choice = 0.75, DC_ContRate = 0.028)
# Test_baseline_hybridlower <- RunModel(NewHire_Plan = "Hybrid", BenMultNew = 0.01, DC_ContRate = 0.033)


# Test5 <- as.data.frame(RunModel(ReturnType = "Stochastic"))
# write.csv(Test5, "Test5.csv")
# 
# ADC_scenario <- as.data.frame(RunModel(FundingPolicy = "ADC"))
# write.csv(ADC_scenario, "ADC_scenario.csv")


#Scenarios
# Scenario_Returns <- as.data.frame(FYE)
# Scenario_UAL <- as.data.frame(FYE)
# Scenario_FR <- as.data.frame(FYE)
# Scenario_ER_Percentage <- as.data.frame(FYE)
# Scenario_ER_InflAdj <- as.data.frame(FYE)
# Scenario_Total_ER <- as.data.frame(FYE)
# Scenario_AllIn_ER <- as.data.frame(FYE)
# 
# #There are 3 types of scenarios here - Recessions, Supplemental and Lv$%
# #We are trying to run all of them outside of a function because we need the data for UAL, FR, etc.
# #If we run them in a function, we can only generate one output
# ScenarioRuns <- 'Return Scenarios'
# #Initialize Max Length, this will be used at the end
# MaxLength <- 0
# if(ScenarioRuns == 'Return Scenarios'){
#   Scenarios <- c('Assumption','Model','Recession','Recurring Recession')
#   for (i in 1:length(Scenarios)){
#     NewData <- as.data.frame(RunModel('Deterministic',SimReturn, SimVolatility, 'Statutory', Scenarios[i], 'Lv%'))
#     Scenario_Returns <- cbind(Scenario_Returns,NewData$ROA_MVA)
#     Scenario_UAL <- cbind(Scenario_UAL,NewData$UAL_MVA_InflAdj)
#     Scenario_FR <- cbind(Scenario_FR,NewData$FR_MVA)
#     Scenario_ER_Percentage <- cbind(Scenario_ER_Percentage,NewData$ER_Percentage)
#     Scenario_ER_InflAdj <- cbind(Scenario_ER_InflAdj,NewData$ER_InflAdj)
#     Scenario_Total_ER <- cbind(Scenario_Total_ER,NewData$Total_ER)
#     Scenario_AllIn_ER <- cbind(Scenario_AllIn_ER,NewData$AllInCost)
#   }
#   #Scenario names should be the same as Scenarios but for some cases like supplemental, lv$, etc. it will be different
#   ScenarioNames <- Scenarios
#   
# } else if(ScenarioRuns == 'Lv$ vs %'){
#   Scenarios <- c('Assumption','Lv%','Assumption','Lv$','Recurring Recession','Lv%','Recurring Recession','Lv$')
#   MaxLength <- length(Scenarios)/2
#   for (i in 1:MaxLength){
#     NewData <- as.data.frame(RunModel('Deterministic',SimReturn, SimVolatility, 'ADC', Scenarios[i*2 - 1], Scenarios[i*2]))
#     Scenario_Returns <- cbind(Scenario_Returns,NewData$ROA_MVA)
#     Scenario_UAL <- cbind(Scenario_UAL,NewData$UAL_MVA_InflAdj)
#     Scenario_FR <- cbind(Scenario_FR,NewData$FR_MVA)
#     Scenario_ER_Percentage <- cbind(Scenario_ER_Percentage,NewData$ER_Percentage)
#     Scenario_ER_InflAdj <- cbind(Scenario_ER_InflAdj,NewData$ER_InflAdj)
#     Scenario_Total_ER <- cbind(Scenario_Total_ER,NewData$Total_ER)
#     Scenario_AllIn_ER <- cbind(Scenario_AllIn_ER,NewData$AllInCost)
#   }
#   #Scenario names should be the same as Scenarios but for some cases like supplemental, lv$, etc. it will be different
#   ScenarioNames <- c('Assumption Lv%', 'Assumption Lv$', 'Recurring Recession Lv%', 'Recurring Recession Lv$')
# }
# 
# #MaxLength should in theory be the lenght of the scenarios but because of Lv$%, it may not be
# #Hence we have to do the max function
# if(ScenarioRuns != 'Lv$ vs %'){
#   MaxLength <- length(Scenarios)
# }
# 
# for(i in 1:MaxLength){
#   #Start from StartIndex because thats when the projection is
#   #Total ER is already inflation adjusted
#   TotalERScenario <- sum(Scenario_Total_ER[nrow(Scenario_Total_ER),i+1])/1000
#   #inflation adjusted UAL
#   EndingUAL <- Scenario_UAL[nrow(Scenario_UAL),i+1]/1000
#   AllInER <- Scenario_AllIn_ER[nrow(Scenario_AllIn_ER),i+1]/1000
#   
#   if(i == 1){
#     ERCostTable <- c(TotalERScenario,EndingUAL, AllInER)
#   } else {
#     ERCostTable <- rbind(ERCostTable, c(TotalERScenario,EndingUAL, AllInER))
#   }
# }
# colnames(ERCostTable) <- c('Total ER Contributions','Ending UAL','All in ER Cost')
# rownames(ERCostTable) <- ScenarioNames
# 
# colnames(Scenario_Returns) <- c('FYE',ScenarioNames)
# colnames(Scenario_UAL) <- c('FYE',ScenarioNames)
# colnames(Scenario_FR) <- c('FYE',ScenarioNames)
# colnames(Scenario_ER_Percentage) <- c('FYE',ScenarioNames)
# colnames(Scenario_ER_InflAdj) <- c('FYE',ScenarioNames)
# 
# #library(ggplot2)
#
# Data <- as.data.frame(cbind(FYE, RunModel(AnnualCashInfusion_Check = 'Yes', AnnualCashInfusion = 100)$FR_MVA, RunModel(AnnualCashInfusion_Check = 'No', OneTimeInfusion = 100)$FR_MVA))
# ScenarioPlot <- function(Data, YAxisLabel){
#   ggplot(Data, aes(x = FYE)) +
#     geom_line(aes(y = Data[,2]), color = "#FF6633", size = 2) +
#     geom_line(aes(y = Data[,3]), color = "#FFCC33", size = 2) +
#     # geom_line(aes(y = Data[,4]), color = "#0066CC", size = 2) +
#     # geom_line(aes(y = Data[,5]), color = "#CC0000", size = 2) +
#     labs(y = YAxisLabel, x = 'Year') + ggtitle(YAxisLabel)
#   #scale_linetype_manual(labels = '')
# }
# ScenarioPlot(Data, 'Funded Ratio (MVA)')
# #
# # ##################################################################################################################################################################
# #
# #Simulations
# start_time <- Sys.time()
# #Set seed insures a consistency when simulations are run multiple times
# set.seed((1234))
# NumberofSimulations <- 1000
# #initialize the return simulations based on years and # of simulations
# Returns_Sims <- matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)
# UAL_Sims <- matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)
# FR_Sims <- matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)
# ER_Sims <- matrix(1:length(FYE),nrow = length(FYE), ncol = NumberofSimulations + 1)
# 
# #Run the simulations
# for (i in 1:NumberofSimulations){
#   NewData <- as.data.frame(RunModel('Stochastic', SimReturnAssumed, SimVolatility, 'ADC', '', 'Lv%'))
#   Returns_Sims[,i+1] <- NewData$ROA_MVA
#   UAL_Sims[,i+1] <- NewData$UAL_MVA_InflAdj
#   FR_Sims[,i+1] <- NewData$FR_MVA
#   ER_Sims[,i+1] <- NewData$ER_Percentage
# }
# 
# Simulations_Returns <- cbind(FYE,FYE,FYE)
# Simulations_UAL <- cbind(FYE,FYE,FYE)
# Simulations_FR <- cbind(FYE,FYE,FYE)
# Simulations_ER <- cbind(FYE,FYE,FYE)
# 
# #Get the 25th, 50th, 75th percentile
# for(i in 1:length(FYE)){
#   Simulations_Returns[i,] <- t(quantile(Returns_Sims[i,2:ncol(Returns_Sims)],c(0.25,0.5,0.75)))
#   Simulations_UAL[i,] <- t(quantile(UAL_Sims[i,2:ncol(UAL_Sims)],c(0.25,0.5,0.75)))
#   Simulations_FR[i,] <- t(as.data.frame(quantile(FR_Sims[i,2:ncol(FR_Sims)],c(0.25,0.5,0.75))))
#   Simulations_ER[i,] <- t(quantile(ER_Sims[i,2:ncol(ER_Sims)],c(0.25,0.5,0.75)))
# }
# 
# #plot the graphs
# SimulationPlot <- function(Data, FYE){
#   Data <- (as.data.frame(Data))
#   Data <- cbind(FYE, Data)
#   colnames(Data) <- c('FYE','25th Percentile', '50th Percentile', '75th Percentile')
#   ggplot(Data, aes(x = Data[,1])) +
#     geom_line(aes(y = Data[,2]), color = "#FF6633", size = 2) +
#     geom_line(aes(y = Data[,3]), color = "#FFCC33", size = 2) +
#     geom_line(aes(y = Data[,4]), color = "#0066CC", size = 2)
# }
# SimulationPlot(Simulations_FR, FYE)
# 
# end_time <- Sys.time()
# print(end_time - start_time)
