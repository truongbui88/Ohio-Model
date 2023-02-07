source("Ohio STRS DB BModel.R")
library(data.table)
library(zoo)
library(profvis)
#Initialize empty workforce projection arrays
ea <- SalaryEntry$entry_age
age <- Age
year <- YearStart:2130   #test now, fix this later
term_year <- year
retire_year <- year


active_dim <- c(length(ea), length(age), length(year))
active_dim_names <- list(ea, age, year)

term_dim <- c(length(ea), length(age), length(year), length(term_year))
term_dim_names <- list(ea, age, year, term_year)

retire_dim <- c(length(ea), length(age), length(year), length(term_year), length(retire_year))
retire_dim_names <- list(ea, age, year, term_year, retire_year)

wf_active <- array(0, dim = active_dim, dimnames = active_dim_names)
wf_term <- array(0, dim = term_dim, dimnames = term_dim_names)
wf_refund <- wf_term
wf_retire <- array(0, dim = retire_dim, dimnames = retire_dim_names)


#Initial active population
active_int_df <- expand_grid(ea, age) %>%
  left_join(SalaryHeadCountData, by = c("ea" = "entry_age", "age" = "Age")) %>%
  replace(is.na(.), 0) %>%
  select(ea, age, Count)

active_int_matrix <- xtabs(Count ~ ea + age, active_int_df)

wf_active[,,1] <- active_int_matrix

# active_int_pop <- c(2000, 8000, 8000, 7000, 8000, 9000, 8000, 7000, 6000, 5000)
# 
# active_int_ea <- data.frame(ea = SalaryEntry$entry_age, age = SalaryEntry$entry_age, n_active = active_int_pop)
# 
# active_int <- expand_grid(ea, age) %>% 
#   left_join(active_int_ea) %>% 
#   replace(is.na(.), 0) %>% 
#   pivot_wider(names_from = age, values_from = n_active) %>% 
#   select(-1)
# 
# wf_active[,,1] <- as.matrix(active_int)

#Position matrix to add new hires
position_matrix <- expand_grid(ea, age) %>% 
  mutate(new = ifelse(ea == age, 1, 0)) 

position_matrix <- xtabs(new ~ ea + age, position_matrix)

##Create probability array

#Mortality probability array (4 dimensions)
mort_df_term <- expand_grid(ea, age, year, term_year) %>% 
  left_join(MortalityTable, by = c("ea" = "entry_age", "age" = "Age", "year" = "Years", "term_year")) %>% 
  mutate(mort = ifelse(is.na(mort), 0, mort))

mort_array_term <- xtabs(mort ~ ea + age + year + term_year, mort_df_term)

#Separation probability array (3 dimensions): 
sep_df <- expand_grid(ea, age, year) %>% 
  mutate(ey = year - (age - ea)) %>% 
  left_join(SeparationRates, by = c("ea" = "entry_age", "age" = "Age", "ey" = "EntryYear")) %>% 
  select(ea, age, year, SepRate) %>% 
  mutate(SepRate = ifelse(is.na(SepRate), 0, SepRate))

sep_array <- xtabs(SepRate ~ ea + age + year, sep_df)

#Refund and retirement probability arrays
#Determine the optimal retirement age
optimal_retire <- FinalData %>% 
  rename(term_age = Age) %>% 
  select(EntryYear, entry_age, term_age, YOS, retire_age, Max_PV_DB, DBEEBalance) %>% 
  mutate(ben_decision = ifelse(YOS == 0, NA, ifelse(DBEEBalance > Max_PV_DB, "refund", "retire")),
         refund = case_when(ben_decision == "refund" ~ 1,     #use case_when instead of ifselse to handle NA values better
                            TRUE ~ 0),
         retire = case_when(ben_decision == "retire" ~ 1,
                            TRUE ~ 0),
         refund_age = term_age)

#Retire probability array (4 dimensions)
retire_df <- expand_grid(ea, age, year, term_year) %>% 
  mutate(entry_year = year - (age - ea),
         term_age = age - (year - term_year),
         YOS = term_age - ea) %>% 
  filter(year - term_year >= 0, YOS >= 0) %>% 
  left_join(optimal_retire, by = c("ea" = "entry_age",
                                   "age" = "retire_age",
                                   "entry_year" = "EntryYear",
                                   "term_age",
                                   "YOS")) %>% 
  mutate(retire = ifelse(is.na(retire), 0, retire))

retire_array <- xtabs(retire ~ ea + age + year + term_year, retire_df) 

#Refund probability array (4 dimensions). Note that employees get refunds in the same year they get terminated. 
refund_df <- expand_grid(ea, age, year, term_year) %>% 
  mutate(entry_year = year - (age - ea),
         term_age = age - (year - term_year),
         YOS = term_age - ea) %>% 
  filter(year - term_year >= 0, YOS >= 0) %>% 
  left_join(optimal_retire, by = c("ea" = "entry_age",
                                   "age" = "refund_age",
                                   "entry_year" = "EntryYear",
                                   "term_age",
                                   "YOS")) %>% 
  mutate(refund = ifelse(is.na(refund), 0, refund))

refund_array <- xtabs(refund ~ ea + age + year + term_year, refund_df)

#Transition matrix to shift the population to the right by 1 age after 1 year
TM <-  diag(length(age) + 1)[-1, -(length(age) + 1)] 


#Workforce projection
for (i in 2:length(year)) {
  active2term <- wf_active[,,i-1] * sep_array[,,i-1]   #calculate the # of newly terminated actives. 2-dimensional array
  
  wf_active[,,i] <- (wf_active[,,i-1] - active2term) %*% TM  #deduct terminated members from the active workforce and shift the wf_active matrix to the right by one year
  
  new_entrants <- add_new_entrants(g = pop_growth, ne_dist = SalaryEntry$entrant_dist, wf1 = wf_active[,,i-1],
                                   wf2 = wf_active[,,i], ea = ea, age = age, position_matrix = position_matrix)  #new entrants matrix to be added to the active workforce

  wf_active[,,i] = wf_active[,,i] + new_entrants  #add new entrants
  
  term2death <- wf_term[,,i-1,] * mort_array_term[,,i-1,] #3-dimensional array
  
  wf_term[,,i,] <- apply(wf_term[,,i-1,] - term2death, 3, function(x) x %*% TM) %>% array(term_dim[-3]) 
  
  wf_term[,,i,i] <- active2term %*% TM   #add newly terminated members the term population
  
  term2refund <- wf_term[,,i,i] * refund_array[,,i,i]  #calculate the # of newly refunded members. 2-dimensional array
  
  wf_term[,,i,i] <- wf_term[,,i,i] - term2refund
  
  wf_refund[,,i,i] <- term2refund
  
  term2retire <- wf_term[,,i,] * retire_array[,,i,]  #calculate the # of newly retired members. 3-dimensional array
  
  wf_term[,,i,] <- wf_term[,,i,] - term2retire
  
  retire2death <- apply(wf_retire[,,i-1,,], 4, function(x) x * mort_array_term[,,i-1,]) %>% array(retire_dim[-3])   #4-dimensional array
  
  wf_retire[,,i,,] <- apply(wf_retire[,,i-1,,] - retire2death, c(3,4), function(x) x %*% TM) %>% array(retire_dim[-3])
  
  wf_retire[,,i,,i] <- term2retire
  
}


#####Convert the multidimensional arrays into data frames 
wf_active_df <- data.frame(expand.grid(ea = ea, 
                                       age = age, 
                                       year = year), 
                           n_active = as.vector(wf_active)) %>% filter(age >= ea)

wf_term_df <- data.frame(expand.grid(ea = ea, age = age, year = year, term_year = term_year), n_term = as.vector(wf_term)) %>% 
  filter(age >= ea, year >= term_year)

wf_refund_df <- data.frame(expand.grid(ea = ea, age = age, year = year, term_year = term_year), n_refund = as.vector(wf_refund)) %>% 
  filter(age >= ea, year >= term_year)

#Since the wf_retire array is too big to handle using the above method, we need to split it into smaller parts for processing
wf_retire_list <- list()  #empty list to save retire workforce data in the for loop

for (i in seq_along(SalaryEntry$entry_age)) {
  wf_retire_name <- paste0("wf_retire_", SalaryEntry$entry_age[i])
  assign(wf_retire_name, wf_retire[i,,,,])
  wf_retire_i <- data.table(CJ(retire_year, term_year, year, age), n_retire = as.vector(get(wf_retire_name)))[n_retire > 0,] %>% 
    mutate(ea = SalaryEntry$entry_age[i])
  assign(wf_retire_name, wf_retire_i)   #do this to save memory space
  wf_retire_list <- append(wf_retire_list, list(get(wf_retire_name)))
}

#Combine all retire data frames from the retire list into one retire data frame 
wf_retire_df <- rbindlist(wf_retire_list) %>% 
  select(ea, age, year, term_year, retire_year, n_retire)


#Plan design ratio for new hires:
DB_member_ratio <- 1
CB_member_ratio <- 0


#Join wf active table with FinalData table to calculate the overall payroll, normal costs, PVFB, and PVFS each year
wf_active_df_final <- wf_active_df %>% 
  mutate(entry_year = year - (age - ea)) %>% 
  left_join(FinalData, by = c("ea" = "entry_age", "age" = "Age", "year" = "Years", "entry_year" = "EntryYear")) %>% 
  select(ea, age, year, entry_year, n_active, normal_cost_DB, normal_cost_CB, Salary, PVFB_DB, PVFB_CB, PVFNC_DB, PVFNC_CB, PVFS) %>% 
  replace(is.na(.), 0) %>% 
  # filter(n_active > 0) %>% 
  #allocate members to plan designs based on entry year
  mutate(n_active_DB_Before2001 = ifelse(entry_year < 2001, n_active, 0),
         n_active_DB_After2001 = ifelse(entry_year >= 2001, n_active*DB_member_ratio_after2001, 0),
         n_active_DB_Legacy = n_active_DB_Before2001 + n_active_DB_After2001,
      
         n_active_DB_Future = ifelse(entry_year >= YearStart, n_active*DB_member_ratio_future, 0),
         n_active_CB_Legacy = ifelse(entry_year <= YearStart & entry_year >= 2001, n_active*(1-DB_member_ratio_after2001), 0),
         n_active_CB_Future = ifelse(entry_year > YearStart, n_active*(1-DB_member_ratio_future), 0)) %>% 
  group_by(year) %>% 
  summarise(payroll_DB_Legacy = sum(Salary * n_active_DB_Legacy),
            payroll_DB_Future = sum(Salary * n_active_DB_Future),
            payroll_CB_Legacy = sum(Salary * n_active_CB_Legacy),
            payroll_CB_Future = sum(Salary * n_active_CB_Future),
            payroll = sum(Salary * n_active),
            nc_rate_DB = ifelse(payroll_DB == 0, 0, sum(normal_cost_DB * Salary * n_active_DB) / sum(Salary * n_active_DB)),
            nc_rate_CB = ifelse(payroll_CB == 0, 0, sum(normal_cost_CB * Salary * n_active_CB) / sum(Salary * n_active_CB)),
            
            PVFB_DB_Legacy = sum(PVFB_DB * n_active_DB_Legacy),
            PVFB_DB_Future = sum(PVFB_DB * n_active_DB_Future),
            PVFB_CB_Legacy = sum(PVFB_DB * n_active_DB_Legacy),
            PVFB_CB_Future = sum(PVFB_DB * n_active_CB_Future),
            
            PVFNC_DB_Legacy = sum(PVFNC_CB * n_active_DB_Legacy),
            PVFNC_DB_Future = sum(PVFNC_CB * n_active_DB_Future),
            PVFNC_CB_Legacy = sum(PVFNC_CB * n_active_CB_Legacy),
            PVFNC_CB_Future = sum(PVFNC_CB * n_active_CB_Future),
            # PVFNC_CB_test = sum(PVFS * normal_cost_CB * n_active_CB),
            
            PVFS_DB = sum(PVFS * n_active_DB),
            PVFS_CB = sum(PVFS * n_active_CB),
            n_active = sum(n_active)) %>% 
  ungroup() %>% 
  mutate(nc_rate = (nc_rate_DB * payroll_DB + nc_rate_CB * payroll_CB) / payroll,
         AAL_active_DB = PVFB_DB - PVFNC_DB,
         
         AAL_active_CB = PVFB_CB - PVFNC_CB) %>% 
  replace(is.na(.), 0) 
  
  
#Term table
wf_term_df_final <- wf_term_df %>% 
  filter(n_term > 0) %>% 
  mutate(entry_year = year - (age - ea)) %>% 
  left_join(FinalData, by = c("ea" = "entry_age", "term_year" = "Years", "entry_year" = "EntryYear")) %>% 
  select(ea, age, year, term_year, entry_year, n_term, DBWealth, CBWealth) %>% 
  left_join(AnnFactorData, by = c("ea" = "entry_age", "age" = "Age", "year" = "Years", "term_year", "entry_year" = "EntryYear")) %>% 
  select(ea, age, year, term_year, entry_year, n_term, DBWealth, CBWealth, surv_DR) %>% 
  mutate(PVFB_term_DB = DBWealth / surv_DR,
         PVFB_term_CB = CBWealth / surv_DR,
         n_term_DB = ifelse(entry_year < YearStart, n_term, n_term * DB_member_ratio),
         n_term_CB = ifelse(entry_year < YearStart, 0, n_term * CB_member_ratio)) %>% 
  group_by(year) %>% 
  summarise(PVFB_term_DB = sum(PVFB_term_DB * n_term_DB),
            PVFB_term_CB = sum(PVFB_term_CB * n_term_CB))



#Join wf refund table with benefit table to calculate the overall refunds each year
wf_refund_df_final <- wf_refund_df %>% 
  filter(n_refund > 0) %>% 
  mutate(entry_year = year - (age - ea)) %>% 
  left_join(BenefitsTable, by = c("ea" = "entry_age", "age" = "retire_age", "year" = "Years", "term_year", "entry_year" = "EntryYear")) %>% 
  select(ea, age, year, term_year, entry_year, n_refund, DBEEBalance, CBBalance) %>% 
  #allocate members to plan designs based on entry year
  mutate(n_refund_DB = ifelse(entry_year < YearStart, n_refund, n_refund * DB_member_ratio),
         n_refund_CB = ifelse(entry_year < YearStart, 0, n_refund * CB_member_ratio)) %>% 
  group_by(year) %>% 
  summarise(refund_DB = sum(DBEEBalance * n_refund_DB),
            refund_CB = sum(CBBalance * n_refund_CB)) %>% 
  ungroup()



#Join wf retire table with benefit table to calculate the overall retirement benefits each year
wf_retire_df_final <- wf_retire_df %>% 
  mutate(entry_year = year - (age - ea)) %>%    
  left_join(BenefitsTable, by = c("ea" = "entry_age", "entry_year" = "EntryYear", "term_year", "retire_year" = "Years")) %>% 
  select(ea, age, year, term_year, retire_year, entry_year, n_retire, DB_Benefit, CB_Benefit) %>% 
  left_join(AnnFactorData, by = c("ea" = "entry_age", "entry_year" = "EntryYear", "term_year", "year" = "Years")) %>% 
  select(ea, age, year, term_year, retire_year, entry_year, n_retire, DB_Benefit, CB_Benefit, AnnuityFactor_DR) %>% 
  rename(base_DB_benefit = DB_Benefit,
         base_CB_benefit = CB_Benefit) %>% 
  #let's ignore COLA for now 
  #allocate members to plan designs based on entry year
  mutate(n_retire_DB = ifelse(entry_year < YearStart, n_retire, n_retire * DB_member_ratio),
         n_retire_CB = ifelse(entry_year < YearStart, 0, n_retire * CB_member_ratio),
         PVFB_retire_DB = base_DB_benefit * AnnuityFactor_DR,
         PVFB_retire_CB = base_CB_benefit * AnnuityFactor_DR) %>% 
  group_by(year) %>% 
  summarise(retire_ben_DB = sum(base_DB_benefit * n_retire_DB),
            retire_ben_CB = sum(base_CB_benefit * n_retire_CB),
            PVFB_retire_DB = sum(PVFB_retire_DB * n_retire_DB),
            PVFB_retire_CB = sum(PVFB_retire_CB * n_retire_CB)) %>% 
  ungroup()



#Project benefit payments for current retirees
retire_current_int <- RetireeDistribution %>% 
  select(age, n_retire_ratio:avg_ben_ratio) %>% 
  mutate(n_retire_current = n_retire_ratio * retiree_pop_current,
         total_ben_current = total_ben_ratio * ben_payment_current,
         avg_ben_current = total_ben_current / n_retire_current,
         Years = YearStart)


wf_retire_current <- AnnFactorData_retire %>% 
  left_join(retire_current_int, by = c("Age" = "age", "Years")) %>% 
  select(base_age:AnnuityFactor_DR, n_retire_current, avg_ben_current, total_ben_current) %>% 
  mutate(cola = COLA) %>% 
  group_by(base_age) %>% 
  mutate(n_retire_current = recur_grow(n_retire_current, -mort),
         avg_ben_current = recur_grow2(avg_ben_current, cola),
         total_ben_current = n_retire_current * avg_ben_current) %>% 
  filter(!is.na(n_retire_current)) %>% 
  ungroup()
  
wf_retire_current_final <- wf_retire_current %>% 
  group_by(Years) %>% 
  summarise(retire_ben_current = sum(total_ben_current))



#Mini funding model
funding_df <- wf_active_df_final %>% 
  left_join(wf_term_df_final) %>% 
  left_join(wf_refund_df_final) %>% 
  left_join(wf_retire_df_final) %>% 
  replace(is.na(.), 0)


write.csv(funding_df, "funding_df_TexsTRS_CB.csv")


###################################################  TESTING   ###########################################


# wf_active_test <- wf_active_df %>% 
#   group_by(year, ea) %>% 
#   summarise(n_active = sum(n_active))
# 
# 
# ggplot(wf_active_test, aes(x = ea, y = n_active, col = year, group = year)) +
#   geom_line() + 
#   facet_wrap(~year)


# 
# wf_retire_test <- wf_retire_df %>% 
#   mutate(entry_year = year - (age - ea)) %>%    
#   left_join(BenefitsTable, by = c("ea" = "entry_age", "entry_year" = "EntryYear", "term_year", "retire_year" = "Years")) %>% 
#   select(ea, age, year, term_year, retire_year, entry_year, n_retire, DB_Benefit, CB_Benefit) %>% 
#   rename(base_DB_benefit = DB_Benefit,
#          base_CB_benefit = CB_Benefit) %>% #let's ignore COLA for now 
#   #allocate members to plan designs based on entry year
#   mutate(n_retire_DB = ifelse(entry_year < YearStart, n_retire, n_retire * DB_member_ratio),
#          n_retire_CB = ifelse(entry_year < YearStart, 0, n_retire * CB_member_ratio)) %>% 
#   group_by(year, age) %>% 
#   summarise(n_retire = sum(n_retire),
#             total_ben = sum(base_DB_benefit * n_retire_DB),
#             avg_ben = sum(base_DB_benefit * n_retire_DB)/sum(n_retire_DB)) %>% 
#   ungroup() %>% 
#   group_by(year) %>% 
#   mutate(n_retire_ratio = n_retire / sum(n_retire),
#          total_ben_ratio = total_ben / sum(total_ben),
#          avg_ben_ratio = avg_ben / mean(avg_ben)) %>% 
#   ungroup()
#   
# ggplot(wf_retire_test %>% filter(year > 2100), aes(x = age, y = n_retire_ratio, col = year, group = year)) +
#   geom_line() +
#   facet_wrap(~year)
# 
# ggplot(wf_retire_test, aes(x = age, y = avg_ben, col = year, group = year)) +
#   geom_line() +
#   facet_wrap(~year)
# 
# ggplot(wf_retire_test, aes(x = age, y = avg_ben_ratio, col = year, group = year)) +
#   geom_line() +
#   facet_wrap(~year)
# 
# ggplot(wf_retire_test %>% filter(year > 2100), aes(x = age, y = total_ben_ratio, col = year, group = year)) +
#   geom_line() +
#   facet_wrap(~year)
# 
# wf_retire_test2 <- wf_retire_test %>% 
#   filter(year == YearStart + 100)
# 
# write.csv(wf_retire_test2, "wf_retire_test2.csv")

