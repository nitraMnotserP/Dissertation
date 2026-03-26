setwd("~/Documents/Dissertation_Analyses/Dissertation_Analyses/SEM/Enrollment Outcome")

##read in the data
imps_long <- read.csv("CA_Enrolled_Imputed_Long_SEM.csv")

weights <- read.csv("BRR_weights.csv", header = TRUE) %>% 
  select(stu_id:w4w1w2w3stu200)

##look at mean SES
imps_long %>% 
  group_by(.imp) %>% 
  summarise(meanSES = mean(SES),
            meanMath9 = mean(Math_9))%>% 
  print(n = 100)

##create stable SES groups across imputations for Mplus
imps_long <- imps_long %>% 
  group_by(stu_id) %>% 
  summarise(meanSES = mean(SES),
            meanMath9 = mean(Math_9)) %>% 
  mutate(
    SES_Group = ntile(meanSES, 3),
    Math_Group = ntile(meanMath9, 3)
  ) %>% 
  select(-meanSES, -meanMath9) %>% 
  left_join(imps_long, by = "stu_id")


##split imputed data into list
imp_list <- imps_long %>%
  split(.$.imp) 

old_rep_names <- paste0("w4w1w2w3stu", sprintf("%03d", 1:200))
new_rep_names <- paste0("rep_", 1:200)


##merge in the BRR weights an recode for mplus
imp_list_recode <- lapply(imp_list, function(df){
  
  df %>% 
    left_join(weights, by = "stu_id") %>% 
    rename_with(~ new_rep_names, all_of(old_rep_names)) %>% 
    mutate(
      city = if_else(Locale == "CITY", 1, 0, missing = NA_real_),
      rural = if_else(Locale == "RURAL", 1, 0, missing = NA_real_),
      town = if_else(Locale == "TOWN", 1, 0 , missing = NA_real_),
      private = if_else(School_type == "CATHOLIC OR OTHER PRIVATE", 1, 0, missing = NA_real_),
      high_math_course = case_when(
        high_math_course %in% c("MISSING", "UNIT NON-RESPONSE") ~ NA_real_,
        high_math_course %in% c("NO MATH", "BASIC MATH", "OTHER MATH", "PRE-ALGEBRA", "ALGEBRA I", "GEOMETRY",
                                "ALGEBRA II", "TRIGONOMETRY", "OTHER ADVANCED MATH", "PROBABILITY AND STATISTICS",
                                "OTHER AP/IB MATH") ~ 0,
        high_math_course %in% c("AP/IB CALCULUS", "CALCULUS", "PRECALCULUS") ~ 1
      ),
      Black = if_else(Race %in% c("BLACK/AFRICAN-AMERICAN, NON-HISPANIC"), 1, 0),
      Hispanic = if_else(Race %in% c("HISPANIC, NO RACE SPECIFIED", "HISPANIC, RACE SPECIFIED"), 1, 0),
      Asian = if_else(Race %in% c("ASIAN, NON-HISPANIC"), 1, 0),
      White = if_else(Race %in% c("WHITE, NON-HISPANIC"), 1, 0),
      Othrace = if_else(Race %in% c("AMER. INDIAN/ALASKA NATIVE, NON-HISPANIC", "MORE THAN ONE RACE, NON-HISPANIC", "NATIVE HAWAIIAN/PACIFIC ISLANDER, NON-HISPANIC"), 1, 0),
      male = if_else(Gender %in% c("MALE"), 1, 0, missing = NA_real_),
      ##set as facotr
      SES_Group = as.factor(SES_Group),
      Math_Group = as.factor(Math_Group),
      ##scale continuous vairabels
      Math_9 = as.numeric(scale(Math_9)),
      Math_11 = as.numeric(scale(Math_11)),
      SES = as.numeric(scale(SES)),
      Peer_SC = as.numeric(scale(Peer_SC)),
      ##create interactions 
      achXpeers = Math_9 * Peer_SC,
      exp_9xPeers = STU_EXPECT_9 * Peer_SC,
      ##set as numeric
      achXpeers = as.numeric(scale(achXpeers)),
      exp_9xPeers = as.numeric(scale(exp_9xPeers)),
      mathTach_expect = as.numeric(scale(mathTach_expect)),
      PerSchDisorder = as.numeric(scale(PerSchDisorder)), 
      PerBarEffTch = as.numeric(scale(PerBarEffTch)),
      ) %>% 
    rename(
      EXP_9 = STU_EXPECT_9, 
      EXP_11 = STU_EXPECT_11,
      pEXP_9 = PAR_EXPECT_9, 
      pEXP_11 = PAR_EXPECT_11, 
      stu_wgt = w4w1w2w3stu
    ) %>% 
    mutate(
      stu_wgt_sub = if_else(hs_grad == 1 & Math_Group == 1, stu_wgt, 0)
    ) %>% 
    select(stu_id, Enrolled, hs_grad, SES, SES_Group, Math_Group, EXP_9:Math_11, Peer_SC, Peer_Course_Taking:s1frndtalkm, 
           city:male, high_math_course, mathTach_expect, PerSchDisorder, PerBarEffTch, stu_wgt_sub, stu_wgt,rep_1:rep_200)
    
})

##########################################################################################################################
##########################################Descriptive Statistics#####################################################################
##########################################################################################################################



design.list <- lapply(imp_list_recode, function(df){
  
  df %>%
    as_survey_rep(
      type = "BRR", 
      repweights = sprintf("rep_%d", 1:200),
      combined_weights = TRUE,
      weight = stu_wgt) %>% 
    filter(
      hs_grad == 1 #& Math_Group == 1
    )
  
})

corr.list <- lapply(design.list, function(design){
  cov.mat <- svyvar(~Math_9 + Math_11 + EXP_9 + EXP_11 +
                      pEXP_9 + pEXP_11 + Peer_SC,
                    design)
  cov2cor(as.matrix(cov.mat))
})

corr.list.extract <- lapply(corr.list, function(corr){
  attr(corr, "var") <- NULL
  attr(corr, "means") <- NULL
  attr(corr, "statistic") <- NULL
  corr
})

corr.pooled <- Reduce("+", corr.list.extract) / length(corr.list.extract)



                  



                                                   
##########################################################################################################################
#####################################Write files as .dat for Mlus##########################################################################
##########################################################################################################################

##Write as individual .csv files for use in mplus
out_dir <- '/Users/prestonmartin/Documents/Dissertation_Analyses/Dissertation_Analyses/SEM/Enrollment Outcome/Enroll For Mplus/dat'

for (i in seq_along(imp_list_recode)) {
  write.table(
    imp_list_recode[[i]],
    file = file.path(out_dir, paste0("Enroll_SEM_imp_", i, ".dat")),
    sep = " ",
    row.names = FALSE,
    col.names = FALSE
  )
}

###write .csv containing just the column names
imp_1 <- imp_list_recode[[1]]
colnames <- data.frame(colnames(imp_1))
write_csv(colnames, "full_sample_colnames_for_mplus.csv")


