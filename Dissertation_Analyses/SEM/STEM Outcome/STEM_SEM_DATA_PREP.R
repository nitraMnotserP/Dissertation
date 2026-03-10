setwd("~/Documents/Dissertation_Analyses/Dissertation_Analyses/SEM/STEM Outcome")

##read in the data
imps_long <- read.csv("CA_STEM_Imputed_Long_SEM.csv")

weights <- read.csv("BRR_weights.csv", header = TRUE) %>% 
  select(stu_id:w4w1w2w3stu200)


##split imputed data into list
imp_list <- imps_long %>%
  split(.$.imp)

old_rep_names <- paste0("w4w1w2w3stu", sprintf("%03d", 1:200))
new_rep_names <- paste0("rep_", 1:200)


##merge in the BRR weights
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
      STEM_Major = case_when(
        STEM_Major == "STEM (IF FIRST OR SECOND/DOUBLE MAJOR IS STEM)" ~ 1,
        STEM_Major == "NOT STEM" ~ 0,
        STEM_Major %in% c("MISSING", "UNIT NON-RESPONSE") ~ NA),
      Black = if_else(Race %in% c("BLACK/AFRICAN-AMERICAN, NON-HISPANIC"), 1, 0),
      Hispanic = if_else(Race %in% c("HISPANIC, NO RACE SPECIFIED", "HISPANIC, RACE SPECIFIED"), 1, 0),
      Asian = if_else(Race %in% c("ASIAN, NON-HISPANIC"), 1, 0),
      White = if_else(Race %in% c("WHITE, NON-HISPANIC"), 1, 0),
      Othrace = if_else(Race %in% c("AMER. INDIAN/ALASKA NATIVE, NON-HISPANIC", "MORE THAN ONE RACE, NON-HISPANIC", "NATIVE HAWAIIAN/PACIFIC ISLANDER, NON-HISPANIC"), 1, 0),
      male = if_else(Gender %in% c("MALE"), 1, 0, missing = NA_real_),
      
      ## Math groups
      Low_math  = if_else(Math_9 < 49.2, 1, 0, missing = NA_real_),
      Mid_math  = if_else(Math_9 >= 49.2 & Math_9 <= 57.6, 1, 0, missing = NA_real_),
      High_math = if_else(Math_9 > 57.6, 1, 0, missing = NA_real_),
      
      Math_Group = case_when(
        Low_math == 1 ~ 1,
        Mid_math == 1 ~ 2,
        High_math == 1 ~ 3
      ),
      
      Math_Group = as.factor(Math_Group),
      
      ## SES groups
      Low_SES  = if_else(SES < -0.292, 1, 0, missing = NA_real_),
      Mid_SES  = if_else(SES >= -0.292 & SES <= 0.407, 1, 0, missing = NA_real_),
      High_SES = if_else(SES > 0.407, 1, 0, missing = NA_real_),
      
      SES_Group = case_when(
        Low_SES == 1 ~ 1,
        Mid_SES == 1 ~ 2,
        High_SES == 1 ~ 3
      ),
      
      SES_Group = as.factor(SES_Group),
      
      ## Combinations
      lowS_lowM   = if_else(Low_math == 1 & Low_SES == 1, 1, 0, missing = NA_real_),
      lowS_midM   = if_else(Low_math == 1 & Mid_SES == 1, 1, 0, missing = NA_real_),
      lowS_highM  = if_else(Low_math == 1 & High_SES == 1, 1, 0, missing = NA_real_),
      
      midS_lowM   = if_else(Mid_math == 1 & Low_SES == 1, 1, 0, missing = NA_real_),
      midS_midM   = if_else(Mid_math == 1 & Mid_SES == 1, 1, 0, missing = NA_real_),
      midS_highM  = if_else(Mid_math == 1 & High_SES == 1, 1, 0, missing = NA_real_),
      
      highS_lowM  = if_else(High_math == 1 & Low_SES == 1, 1, 0, missing = NA_real_),
      highS_midM  = if_else(High_math == 1 & Mid_SES == 1, 1, 0, missing = NA_real_),
      highS_highM = if_else(High_math == 1 & High_SES == 1, 1, 0, missing = NA_real_),
      
      Math_9 = as.numeric(scale(Math_9)),
      Math_11 = as.numeric(scale(Math_11)),
      SES = as.numeric(scale(SES)),
      Peer_SC = as.numeric(scale(Peer_SC)),
      
      achXpeers = Math_9 * Peer_SC,
      exp_9xPeers = STU_EXPECT_9 * Peer_SC,
      
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
    select(stu_id, STEM_Major, Math_Group, SES_Group, EXP_9:Math_11, Peer_SC, Peer_Course_Taking:s1frndtalkm, 
           city:male, high_math_course, mathTach_expect, PerSchDisorder, PerBarEffTch, stu_wgt,rep_1:rep_200)
})


setwd("/Users/prestonmartin/Documents/Dissertation_Analyses/Dissertation_Analyses/SEM/STEM Outcome/Mplus Imps/dat")
##Write as individual .csv files for use in mplus
for (i in seq_along(imp_list_recode)) {
  write.table(
    imp_list_recode[[i]],
    file = paste0("STEM_SEM_imp_", i, ".dat"),
    sep = " ",
    row.names = FALSE,
    col.names = FALSE
  )
}

###write .csv containing jsu teh conames
imp_1 <- imp_list_recode[[1]]
colnames <- data.frame(colnames(imp_1))
write_csv(colnames, "colnames_for_mplus.csv")

