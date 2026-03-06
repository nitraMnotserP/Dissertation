######################################################################################
################################Survey workflow#######################################
######################################################################################

setwd("~/Documents/Dissertation Analyses/Dissertation_Analyses/CA-Enrollment Models")

##read in the data and BRR weights
imps_long <- read.csv("CA_Enrollment_Imputed_Long.csv")
##BRR weights
weights <- read.csv("BRR_weights.csv") %>% 
  select(-X)

##split the data an put it into a list
imp_list <- imps_long %>%
  split(.$.imp)

##merge in the BRR weights
imp_list <- lapply(imp_list, function(df){
  
  df %>% 
    left_join(weights, by = "stu_id") %>%
    select(-X)
    
    
})


##Test recodes here:
imp_1 <- imp_list$`1`

test <- imp_1 %>% 
  as_survey_rep(
    type = "BRR",
    repweights = sprintf("w4w1w2w3stu%03d", 1:200),
    combined_weights = FALSE,
    weight = w4w1w2w3stu
  ) 

test %>%
  group_by(SES_group, Math_Group) %>% 
  survey_count()


test %>%
  summarize(elec_bill = survey_quantile(SES,
                                        quantiles = c(0.33, 0.66)
  ))


########################
######START HERE########
########################


imp_list <- lapply(imp_list, function(df){
  
  df %>% 
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
      enrolled = case_when(
        enrolled == "YES" ~ 1,
        enrolled == "NO" ~ 0,
        enrolled %in% c("MISSING", "UNIT NON-RESPONSE") ~ NA),
      Black = if_else(Race %in% c("BLACK/AFRICAN-AMERICAN, NON-HISPANIC"), 1, 0),
      Hispanic = if_else(Race %in% c("HISPANIC, NO RACE SPECIFIED", "HISPANIC, RACE SPECIFIED"), 1, 0),
      Asian = if_else(Race %in% c("ASIAN, NON-HISPANIC"), 1, 0),
      White = if_else(Race %in% c("WHITE, NON-HISPANIC"), 1, 0),
      Othrace = if_else(Race %in% c("AMER. INDIAN/ALASKA NATIVE, NON-HISPANIC", "MORE THAN ONE RACE, NON-HISPANIC", "NATIVE HAWAIIAN/PACIFIC ISLANDER, NON-HISPANIC"), 1, 0),
      male = if_else(Gender %in% c("MALE"), 1, 0, missing = NA_real_),

      ## Math groups
      Low_math  = if_else(Math_Ach < 48.45357, 1, 0, missing = NA_real_),
      Mid_math  = if_else(Math_Ach >= 48.45357 & Math_Ach <= 56.55437, 1, 0, missing = NA_real_),
      High_math = if_else(Math_Ach > 56.55437, 1, 0, missing = NA_real_),
      
      Math_Group = case_when(
        Low_math == 1 ~ "Low Math",
        Mid_math == 1 ~ "Mid Math",
        High_math == 1 ~ "High Math"
      ),
      
      ## SES groups
      Low_SES  = if_else(SES < -0.2769667, 1, 0, missing = NA_real_),
      Mid_SES  = if_else(SES >= -0.2769667 & SES <= 0.4240000, 1, 0, missing = NA_real_),
      High_SES = if_else(SES > 0.4240000, 1, 0, missing = NA_real_),
      
      SES_group = case_when(
        Low_SES == 1 ~ "Low SES",
        Mid_SES == 1 ~ "Mid SES",
        High_SES == 1 ~ "high SES"
      ),
      ## Combinations
      lowS_lowM   = if_else(Low_math == 1 & Low_SES == 1, 1, 0, missing = NA_real_),
      lowS_midM   = if_else(Low_math == 1 & Mid_SES == 1, 1, 0, missing = NA_real_),
      lowS_highM  = if_else(Low_math == 1 & High_SES == 1, 1, 0, missing = NA_real_),
      
      midS_lowM   = if_else(Mid_math == 1 & Low_SES == 1, 1, 0, missing = NA_real_),
      midS_midM   = if_else(Mid_math == 1 & Mid_SES == 1, 1, 0, missing = NA_real_),
      midS_highM  = if_else(Mid_math == 1 & High_SES == 1, 1, 0, missing = NA_real_),
      
      highS_lowM  = if_else(High_math == 1 & Low_SES == 1, 1, 0, missing = NA_real_),
      highS_midM  = if_else(High_math == 1 & Mid_SES == 1, 1, 0, missing = NA_real_),
      highS_highM = if_else(High_math == 1 & High_SES == 1, 1, 0, missing = NA_real_)
      
      
    )
}) 

##Load the needed packages for analyses
library(survey)
library(mitools)
library(RSQLite)
##create the imputation list to pass as data argument in the following survey design call
impdata <- imputationList(imp_list,
                          dbtype = "SQLite", 
                          ###You will need to change the line below this comment to a folder on your machine
                          dbname = "/Documents/Dissertation Analyses/Dissertation_Analyses/CA-Enrollment Models/imp_Surv_DB.db")

##create BRR survey deign object with imputation list data
hsls.svy <-  svrepdesign(weights = ~w4w1w2w3stu, repweights = "w4w1w2w3stu[001-200]+", 
                         type = "BRR", data = impdata, combined.weights = TRUE) 

##check the survey design (should be BRR with 30 imputations)
hsls.svy
hsls.svy$designs$`1`

mod1 <- with(hsls.svy,
             svyglm(enrolled ~ Black + Hispanic + Asian + Othrace + male + 
                      high_math_course + rural + city + town + private + lowS_midM + 
                      lowS_highM + midS_midM + midS_highM + highS_lowM + highS_midM + 
                      highS_highM + school_climate + mathTach_expect + 
                      PerSchDisorder + PerBarEffTch, family = quasibinomial("logit"), multicore = TRUE))


mod1.tab <- summary(MIcombine(mod1), digits = 3)
mod1.tab$t <- mod1.tab$results / mod1.tab$se
mod1.tab$OR <- exp(mod1.tab$results)
mod1.tab

##avg_slopes(): average (marginal) estimates.
mod1.ame <- lapply(mod1, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_midM", "lowS_highM", "midS_midM", 
                                            "midS_highM", "highS_lowM", "highS_midM",  
                                            "highS_highM"),
                              type = "response")   # puts it on probability scale
  
})

mod1.pooled.ame <- MIcombine(mod1.ame) ##13 & 14: high SES, Low Math (1; 0)
mod1.pooled.ame <- summary(mod1.pooled.ame)

##(Reference = low SES/mid math & mid SES/mid math)
mod2 <- with(hsls.svy,
             svyglm(enrolled ~ Black + Hispanic + Asian + Othrace + male + 
                      high_math_course + rural + city + town + private + lowS_lowM + 
                      lowS_highM + midS_lowM + midS_highM + highS_lowM + highS_midM + 
                      highS_highM + school_climate + mathTach_expect + 
                      PerSchDisorder + PerBarEffTch,
                      family = quasibinomial("logit"), multicore = TRUE))

mod2.tab <- summary(MIcombine(mod2), digits = 3)
mod2.tab$t <- mod2.tab$results / mod2.tab$se
mod2.tab$OR <- exp(mod2.tab$results)
mod2.tab


##avg_slopes(): average (marginal) estimates.
mod2.ame <- lapply(mod2, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_lowM", "lowS_highM", "midS_lowM",
                                            "midS_highM", "highS_lowM", "highS_midM", 
                                            "highS_highM"),
                              type = "response")   # puts it on probability scale
  
})

mod2.pooled.ame <- MIcombine(mod2.ame) ##13 & 14: high SES, Low Math (1; 0)
mod2.pooled.ame <- summary(mod2.pooled.ame)

##(Reference = low SES/high math & mid SES/high math)
mod3 <- with(hsls.svy,
             svyglm(enrolled ~ Black + Hispanic + Asian + Othrace + male + 
                      high_math_course + rural + city + town + private + lowS_lowM + 
                      lowS_midM + midS_lowM + midS_midM + highS_lowM + highS_midM + 
                      highS_highM + school_climate + mathTach_expect + 
                      PerSchDisorder + PerBarEffTch, 
                    family = quasibinomial("logit"), multicore = TRUE))

mod3.tab <- summary(MIcombine(mod3), digits = 3)
mod3.tab$t <- mod3.tab$results / mod3.tab$se
mod3.tab$OR <- exp(mod3.tab$results)
mod3.tab

##avg_slopes(): average (marginal) estimates.
mod3.ame <- lapply(mod3, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_lowM", "lowS_midM", "midS_lowM", 
                                            "midS_midM", "highS_lowM", "highS_midM", 
                                            "highS_highM"),
                              type = "response")   # puts it on probability scale
  
})

mod3.pooled.ame <- MIcombine(mod3.ame) ##13 & 14: high SES, Low Math (1; 0)
mod3.pooled.ame <- summary(mod3.pooled.ame)

##(Reference = high SES/high math & high SES/high math)
mod4 <- with(hsls.svy,
             svyglm(enrolled ~ Black + Hispanic + Asian + Othrace + male + 
                      high_math_course + rural + city + town + private + lowS_lowM + 
                      lowS_midM + lowS_highM + midS_lowM + midS_midM + midS_highM + highS_lowM + 
                      school_climate + mathTach_expect + PerSchDisorder + PerBarEffTch, 
                      family = quasibinomial("logit"), multicore = TRUE))

mod4.tab <- summary(MIcombine(mod4), digits = 3)
mod4.tab$t <- mod4.tab$results / mod4.tab$se
mod4.tab$OR <- exp(mod4.tab$results)
mod4.tab

##avg_slopes(): average (marginal) estimates.
mod4.ame <- lapply(mod4, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_lowM", "lowS_midM", "lowS_highM", 
                                            "midS_lowM", "midS_midM", "midS_highM", "highS_lowM"),
                              type = "response")   # puts it on probability scale
  
})

mod4.pooled.ame <- MIcombine(mod4.ame) ##13 & 14: high SES, Low Math (1; 0)
mod4.pooled.ame <- summary(mod4.pooled.ame)


################################################################################################
##############################White Students Subgroup Models####################################
################################################################################################

##subset the survey design object:
hsls.svy.white <- subset(hsls.svy, White == 1)

##(Reference = low SES/low math & mid SES/low math)
mod1.white <- with(hsls.svy.white,
                 svyglm(enrolled ~  male + high_math_course + rural + city + town + private + 
                          lowS_midM + lowS_highM + midS_midM + midS_highM + highS_lowM + highS_midM + 
                          highS_highM + school_climate + mathTach_expect + 
                          PerSchDisorder + PerBarEffTch, 
                        family = quasibinomial("logit"), multicore = TRUE))

mod1.tab.white <- summary(MIcombine(mod1.white), digits = 3)
mod1.tab.white$t <- mod1.tab.white$results / mod1.tab.white$se
mod1.tab.white$OR <- exp(mod1.tab.white$results)
mod1.tab.white


##(Reference = low SES/mid math & mid SES/mid math)
mod2.white <- with(hsls.svy.white,
             svyglm(enrolled ~ male + high_math_course + rural + city + town + private + lowS_lowM + 
                      lowS_highM + midS_lowM + midS_highM + highS_lowM + highS_midM + 
                      highS_highM + school_climate + mathTach_expect + 
                      PerSchDisorder + PerBarEffTch,
                    family = quasibinomial("logit"), multicore = TRUE))

mod2.tab.white <- summary(MIcombine(mod2.white), digits = 3)
mod2.tab.white$t <- mod2.tab.white$results / mod2.tab.white$se
mod2.tab.white$OR <- exp(mod2.tab.white$results)
mod2.tab.white


##(Reference = low SES/high math & mid SES/high math)
mod3.white <- with(hsls.svy.white,
             svyglm(enrolled ~ male + high_math_course + rural + city + town + private + lowS_lowM + 
                      lowS_midM + midS_lowM + midS_midM + highS_lowM + highS_midM + 
                      highS_highM + school_climate + mathTach_expect + 
                      PerSchDisorder + PerBarEffTch, 
                    family = quasibinomial("logit"), multicore = TRUE))

mod3.tab.white <- summary(MIcombine(mod3.white), digits = 3)
mod3.tab.white$t <- mod3.tab.white$results / mod3.tab.white$se
mod3.tab.white$OR <- exp(mod3.tab.white$results)
mod3.tab.white


##(Reference = high SES/high math & high SES/high math)
mod4.white <- with(hsls.svy.white,
             svyglm(enrolled ~ male + high_math_course + rural + city + town + private + lowS_lowM + 
                      lowS_midM + lowS_highM + midS_lowM + midS_midM + midS_highM + highS_lowM + 
                      school_climate + mathTach_expect + 
                      PerSchDisorder + PerBarEffTch, 
                    family = quasibinomial("logit"), multicore = TRUE))

mod4.tab.white <- summary(MIcombine(mod4.white), digits = 3)
mod4.tab.white$t <- mod4.tab.white$results / mod4.tab.white$se
mod4.tab.white$OR <- exp(mod4.tab.white$results)
mod4.tab.white

####Calculate marginal effects for white students

##avg_slopes(): average (marginal) estimates.
mod1.ame.white <- lapply(mod1.white, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_midM", "lowS_highM", "midS_midM", 
                                            "midS_highM", "highS_lowM", "highS_midM",  
                                            "highS_highM"),
                              type = "response")   # puts it on probability scale
  
})

mod1.pooled.ame.white <- MIcombine(mod1.ame.white) ##13 & 14: high SES, Low Math (1; 0)
mod1.pooled.ame.white <- summary(mod1.pooled.ame.white)


##avg_slopes(): average (marginal) estimates.
mod2.ame.white <- lapply(mod2.white, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_lowM", "lowS_highM", "midS_lowM",
                                            "midS_highM", "highS_lowM", "highS_midM", 
                                            "highS_highM"),
                              type = "response")   # puts it on probability scale
  
})

mod2.pooled.ame.white <- MIcombine(mod2.ame.white) ##13 & 14: high SES, Low Math (1; 0)
mod2.pooled.ame.white <- summary(mod2.pooled.ame.white)

##avg_slopes(): average (marginal) estimates.
mod3.ame.white <- lapply(mod3.white, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_lowM", "lowS_midM", "midS_lowM", 
                                            "midS_midM", "highS_lowM", "highS_midM", 
                                            "highS_highM"),
                              type = "response")   # puts it on probability scale
  
})

mod3.pooled.ame.white <- MIcombine(mod3.ame.white) ##13 & 14: high SES, Low Math (1; 0)
mod3.pooled.ame.white <- summary(mod3.pooled.ame.white)

##avg_slopes(): average (marginal) estimates.
mod4.ame.white <- lapply(mod4.white, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_lowM", "lowS_midM", "lowS_highM", 
                                            "midS_lowM", "midS_midM", "midS_highM", "highS_lowM"),
                              type = "response")   # puts it on probability scale
  
})

mod4.pooled.ame.white <- MIcombine(mod4.ame.white) ##13 & 14: high SES, Low Math (1; 0)
mod4.pooled.ame.white <- summary(mod4.pooled.ame.white)
write.csv(mod4.pooled.ame.white, "mod4.pooled.ame.white.csv")

################################################################################################
##############################Black Students Subgroup Models####################################
################################################################################################
##subset the survey design object:
hsls.svy.black <- subset(hsls.svy, Black == 1)

##(Reference = low SES/low math & mid SES/low math)
mod1.black <- with(hsls.svy.black,
                   svyglm(enrolled ~  male + high_math_course + rural + city + town + private + 
                            lowS_midM + lowS_highM + midS_midM + midS_highM + highS_lowM + highS_midM + 
                            highS_highM + school_climate + mathTach_expect + 
                            PerSchDisorder + PerBarEffTch, 
                          family = quasibinomial("logit"), multicore = TRUE))

mod1.tab.black <- summary(MIcombine(mod1.black), digits = 3)
mod1.tab.black$t <- mod1.tab.black$results / mod1.tab.black$se
mod1.tab.black$OR <- exp(mod1.tab.black$results)
mod1.tab.black


##(Reference = low SES/mid math & mid SES/mid math)
mod2.black <- with(hsls.svy.black,
                   svyglm(enrolled ~ male + high_math_course + rural + city + town + private + lowS_lowM + 
                            lowS_highM + midS_lowM + midS_highM + highS_lowM + highS_midM + 
                            highS_highM + school_climate + mathTach_expect + 
                            PerSchDisorder + PerBarEffTch,
                          family = quasibinomial("logit"), multicore = TRUE))

mod2.tab.black <- summary(MIcombine(mod2.black), digits = 3)
mod2.tab.black$t <- mod2.tab.black$results / mod2.tab.black$se
mod2.tab.black$OR <- exp(mod2.tab.black$results)
mod2.tab.black

##(Reference = low SES/high math & mid SES/high math)
mod3.black <- with(hsls.svy.black,
                   svyglm(enrolled ~ male + high_math_course + rural + city + town + private + lowS_lowM + 
                            lowS_midM + midS_lowM + midS_midM + highS_lowM + highS_midM + 
                            highS_highM + school_climate + mathTach_expect + 
                            PerSchDisorder + PerBarEffTch, 
                          family = quasibinomial("logit"), multicore = TRUE))

mod3.tab.black <- summary(MIcombine(mod3.black), digits = 3)
mod3.tab.black$t <- mod3.tab.black$results / mod3.tab.black$se
mod3.tab.black$OR <- exp(mod3.tab.black$results)
mod3.tab.black

##(Reference = high SES/high math & high SES/high math)
mod4.black <- with(hsls.svy.black,
                   svyglm(enrolled ~ male + high_math_course + rural + city + town + private + lowS_lowM + 
                            lowS_midM + lowS_highM + midS_lowM + midS_midM + midS_highM + highS_lowM + 
                            school_climate + mathTach_expect + PerSchDisorder + PerBarEffTch, 
                          family = quasibinomial("logit"), multicore = TRUE))

mod4.tab.black <- summary(MIcombine(mod4.black), digits = 3)
mod4.tab.black$t <- mod4.tab.black$results / mod4.tab.black$se
mod4.tab.black$OR <- exp(mod4.tab.black$results)
mod4.tab.black

####Calculate marginal effects for black students

##avg_slopes(): average (marginal) estimates.
mod1.ame.black <- lapply(mod1.black, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_midM", "lowS_highM", "midS_midM", 
                                            "midS_highM", "highS_lowM", "highS_midM",  
                                            "highS_highM"),
                              type = "response")   # puts it on probability scale
  
})

mod1.pooled.ame.black <- MIcombine(mod1.ame.black) ##13 & 14: high SES, Low Math (1; 0)
mod1.pooled.ame.black <- summary(mod1.pooled.ame.black)


##avg_slopes(): average (marginal) estimates.
mod2.ame.black <- lapply(mod2.black, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_lowM", "lowS_highM", "midS_lowM",
                                            "midS_highM", "highS_lowM", "highS_midM", 
                                            "highS_highM"),
                              type = "response")   # puts it on probability scale
  
})

mod2.pooled.ame.black <- MIcombine(mod2.ame.black) ##13 & 14: high SES, Low Math (1; 0)
mod2.pooled.ame.black <- summary(mod2.pooled.ame.black)

##avg_slopes(): average (marginal) estimates.
mod3.ame.black <- lapply(mod3.black, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_lowM", "lowS_midM", "midS_lowM", 
                                            "midS_midM", "highS_lowM", "highS_midM", 
                                            "highS_highM"),
                              type = "response")   # puts it on probability scale
  
})

mod3.pooled.ame.black <- MIcombine(mod3.ame.black) ##13 & 14: high SES, Low Math (1; 0)
mod3.pooled.ame.black <- summary(mod3.pooled.ame.black)

##avg_slopes(): average (marginal) estimates.
mod4.ame.black <- lapply(mod4.black, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_lowM", "lowS_midM", "lowS_highM", 
                                            "midS_lowM", "midS_midM", "midS_highM", "highS_lowM"),
                              type = "response")   # puts it on probability scale
  
})

mod4.pooled.ame.black <- MIcombine(mod4.ame.black) ##13 & 14: high SES, Low Math (1; 0)
mod4.pooled.ame.black <- summary(mod4.pooled.ame.black)

################################################################################################
##############################Hispanic Students Subgroup Models#################################
################################################################################################
##subset the survey design object:
hsls.svy.hispanic <- subset(hsls.svy, Hispanic == 1)

##(Reference = low SES/low math & mid SES/low math)
mod1.hispanic <- with(hsls.svy.hispanic,
                      svyglm(enrolled ~  male + high_math_course + rural + city + town + private + 
                               lowS_midM + lowS_highM + midS_midM + midS_highM + highS_lowM + highS_midM + 
                               highS_highM + school_climate + mathTach_expect + 
                               PerSchDisorder + PerBarEffTch, 
                             family = quasibinomial("logit"), multicore = TRUE))

mod1.tab.hispanic <- summary(MIcombine(mod1.hispanic), digits = 3)
mod1.tab.hispanic$t <- mod1.tab.hispanic$results / mod1.tab.hispanic$se
mod1.tab.hispanic$OR <- exp(mod1.tab.hispanic$results)
mod1.tab.hispanic


##(Reference = low SES/mid math & mid SES/mid math)
mod2.hispanic <- with(hsls.svy.hispanic,
                      svyglm(enrolled ~ male + high_math_course + rural + city + town + private + lowS_lowM + 
                               lowS_highM + midS_lowM + midS_highM + highS_lowM + highS_midM + 
                               highS_highM + school_climate + mathTach_expect + 
                               PerSchDisorder + PerBarEffTch,
                             family = quasibinomial("logit"), multicore = TRUE))

mod2.tab.hispanic <- summary(MIcombine(mod2.hispanic), digits = 3)
mod2.tab.hispanic$t <- mod2.tab.hispanic$results / mod2.tab.hispanic$se
mod2.tab.hispanic$OR <- exp(mod2.tab.hispanic$results)
mod2.tab.hispanic

##(Reference = low SES/high math & mid SES/high math)
mod3.hispanic <- with(hsls.svy.hispanic,
                      svyglm(enrolled ~ male + high_math_course + rural + city + town + private + lowS_lowM + 
                               lowS_midM + midS_lowM + midS_midM + highS_lowM + highS_midM + 
                               highS_highM + school_climate + mathTach_expect + 
                               PerSchDisorder + PerBarEffTch, 
                             family = quasibinomial("logit"), multicore = TRUE))

mod3.tab.hispanic <- summary(MIcombine(mod3.hispanic), digits = 3)
mod3.tab.hispanic$t <- mod3.tab.hispanic$results / mod3.tab.hispanic$se
mod3.tab.hispanic$OR <- exp(mod3.tab.hispanic$results)
mod3.tab.hispanic

##(Reference = high SES/high math & high SES/high math)
mod4.hispanic <- with(hsls.svy.hispanic,
                      svyglm(enrolled ~ male + high_math_course + rural + city + town + private + lowS_lowM + 
                               lowS_midM + lowS_highM + midS_lowM + midS_midM + midS_highM + highS_lowM + 
                               school_climate + mathTach_expect + PerSchDisorder + PerBarEffTch, 
                             family = quasibinomial("logit"), multicore = TRUE))

mod4.tab.hispanic <- summary(MIcombine(mod4.hispanic), digits = 3)
mod4.tab.hispanic$t <- mod4.tab.hispanic$results / mod4.tab.hispanic$se
mod4.tab.hispanic$OR <- exp(mod4.tab.hispanic$results)
mod4.tab.hispanic

####Calculate marginal effects for hispanic students

##avg_slopes(): average (marginal) estimates.
mod1.ame.hispanic <- lapply(mod1.hispanic, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_midM", "lowS_highM", "midS_midM", 
                                            "midS_highM", "highS_lowM", "highS_midM",  
                                            "highS_highM"),
                              type = "response")   # puts it on probability scale
  
})

mod1.pooled.ame.hispanic <- MIcombine(mod1.ame.hispanic) ##13 & 14: high SES, Low Math (1; 0)
mod1.pooled.ame.hispanic <- summary(mod1.pooled.ame.hispanic)


##avg_slopes(): average (marginal) estimates.
mod2.ame.hispanic <- lapply(mod2.hispanic, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_lowM", "lowS_highM", "midS_lowM",
                                            "midS_highM", "highS_lowM", "highS_midM", 
                                            "highS_highM"),
                              type = "response")   # puts it on probability scale
  
})

mod2.pooled.ame.hispanic <- MIcombine(mod2.ame.hispanic) ##13 & 14: high SES, Low Math (1; 0)
mod2.pooled.ame.hispanic <- summary(mod2.pooled.ame.hispanic)

##avg_slopes(): average (marginal) estimates.
mod3.ame.hispanic <- lapply(mod3.hispanic, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_lowM", "lowS_midM", "midS_lowM", 
                                            "midS_midM", "highS_lowM", "highS_midM", 
                                            "highS_highM"),
                              type = "response")   # puts it on probability scale
  
})

mod3.pooled.ame.hispanic <- MIcombine(mod3.ame.hispanic) ##13 & 14: high SES, Low Math (1; 0)
mod3.pooled.ame.hispanic <- summary(mod3.pooled.ame.hispanic)

##avg_slopes(): average (marginal) estimates.
mod4.ame.hispanic <- lapply(mod4.hispanic, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_lowM", "lowS_midM", "lowS_highM", 
                                            "midS_lowM", "midS_midM", "midS_highM", "highS_lowM"),
                              type = "response")   # puts it on probability scale
  
})

mod4.pooled.ame.hispanic <- MIcombine(mod4.ame.hispanic) ##13 & 14: high SES, Low Math (1; 0)
mod4.pooled.ame.hispanic <- summary(mod4.pooled.ame.hispanic)


################################################################################################
##############################Male Students Subgroup Models#####################################
################################################################################################

##subset the survey design object:
hsls.svy.male <- subset(hsls.svy, male == 1)

##(Reference = low SES/low math & mid SES/low math)
mod1.male <- with(hsls.svy.male,
                  svyglm(enrolled ~  Black + Hispanic + Asian + Othrace +
                           high_math_course + rural + city + town + private + lowS_midM + 
                           lowS_highM + midS_midM + midS_highM + highS_lowM + highS_midM + 
                           highS_highM + school_climate + mathTach_expect + 
                           PerSchDisorder + PerBarEffTch, 
                         family = quasibinomial("logit"), multicore = TRUE))

mod1.tab.male <- summary(MIcombine(mod1.male), digits = 3)
mod1.tab.male$t <- mod1.tab.male$results / mod1.tab.male$se
mod1.tab.male$OR <- exp(mod1.tab.male$results)
mod1.tab.male


##(Reference = low SES/mid math & mid SES/mid math)
mod2.male <- with(hsls.svy.male,
                  svyglm(enrolled ~ Black + Hispanic + Asian + Othrace +
                           high_math_course + rural + city + town + private + lowS_lowM + 
                           lowS_highM + midS_lowM + midS_highM + highS_lowM + highS_midM + 
                           highS_highM + school_climate + mathTach_expect + 
                           PerSchDisorder + PerBarEffTch,
                         family = quasibinomial("logit"), multicore = TRUE))

mod2.tab.male <- summary(MIcombine(mod2.male), digits = 3)
mod2.tab.male$t <- mod2.tab.male$results / mod2.tab.male$se
mod2.tab.male$OR <- exp(mod2.tab.male$results)
mod2.tab.male

##(Reference = low SES/high math & mid SES/high math)
mod3.male <- with(hsls.svy.male,
                  svyglm(enrolled ~ Black + Hispanic + Asian + Othrace +
                           high_math_course + rural + city + town + private + lowS_lowM + 
                           lowS_midM + midS_lowM + midS_midM + highS_lowM + highS_midM + 
                           highS_highM + school_climate + mathTach_expect + 
                           PerSchDisorder + PerBarEffTch, 
                         family = quasibinomial("logit"), multicore = TRUE))

mod3.tab.male <- summary(MIcombine(mod3.male), digits = 3)
mod3.tab.male$t <- mod3.tab.male$results / mod3.tab.male$se
mod3.tab.male$OR <- exp(mod3.tab.male$results)
mod3.tab.male

##(Reference = high SES/high math & high SES/high math)
mod4.male <- with(hsls.svy.male,
                  svyglm(enrolled ~ Black + Hispanic + Asian + Othrace +
                           high_math_course + rural + city + town + private + lowS_lowM + 
                           lowS_midM + lowS_highM + midS_lowM + midS_midM + midS_highM + highS_lowM + 
                           school_climate + mathTach_expect + PerSchDisorder + PerBarEffTch, 
                         family = quasibinomial("logit"), multicore = TRUE))

mod4.tab.male <- summary(MIcombine(mod4.male), digits = 3)
mod4.tab.male$t <- mod4.tab.male$results / mod4.tab.male$se
mod4.tab.male$OR <- exp(mod4.tab.male$results)
mod4.tab.male

####Calculate marginal effects for male students

##avg_slopes(): average (marginal) estimates.
mod1.ame.male <- lapply(mod1.male, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_midM", "lowS_highM", "midS_midM", 
                                            "midS_highM", "highS_lowM", "highS_midM",  
                                            "highS_highM"),
                              type = "response")   # puts it on probability scale
  
})

mod1.pooled.ame.male <- MIcombine(mod1.ame.male) ##13 & 14: high SES, Low Math (1; 0)
mod1.pooled.ame.male <- summary(mod1.pooled.ame.male)


##avg_slopes(): average (marginal) estimates.
mod2.ame.male <- lapply(mod2.male, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_lowM", "lowS_highM", "midS_lowM",
                                            "midS_highM", "highS_lowM", "highS_midM", 
                                            "highS_highM"),
                              type = "response")   # puts it on probability scale
  
})

mod2.pooled.ame.male <- MIcombine(mod2.ame.male) ##13 & 14: high SES, Low Math (1; 0)
mod2.pooled.ame.male <- summary(mod2.pooled.ame.male)

##avg_slopes(): average (marginal) estimates.
mod3.ame.male <- lapply(mod3.male, function(df){
  
  marginaleffects::avg_comparisons(df,
                              variables = c("lowS_lowM", "lowS_midM", "midS_lowM", 
                                            "midS_midM", "highS_lowM", "highS_midM", 
                                            "highS_highM"),
                              type = "response")   # puts it on probability scale
  
})

mod3.pooled.ame.male <- MIcombine(mod3.ame.male) ##13 & 14: high SES, Low Math (1; 0)
mod3.pooled.ame.male <- summary(mod3.pooled.ame.male)

##avg_slopes(): average (marginal) estimates.
mod4.ame.male <- lapply(mod4.male, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_lowM", "lowS_midM", "lowS_highM", 
                                            "midS_lowM", "midS_midM", "midS_highM", 
                                            "highS_lowM"),
                              type = "response")   # puts it on probability scale
  
})

mod4.pooled.ame.male <- MIcombine(mod4.ame.male) ##13 & 14: high SES, Low Math (1; 0)
mod4.pooled.ame.male <- summary(mod4.pooled.ame.male)


################################################################################################
##############################Female Students Subgroup Models###################################
################################################################################################

##subset the survey design object:
hsls.svy.female <- subset(hsls.svy, male == 0)

##(Reference = low SES/low math & mid SES/low math)
mod1.female <- with(hsls.svy.female,
                    svyglm(enrolled ~  Black + Hispanic + Asian + Othrace +
                             high_math_course + rural + city + town + private + lowS_midM + 
                             lowS_highM + midS_midM + midS_highM + highS_lowM + highS_midM + 
                             highS_highM + school_climate + mathTach_expect + 
                             PerSchDisorder + PerBarEffTch, 
                           family = quasibinomial("logit"), multicore = TRUE))

mod1.tab.female <- summary(MIcombine(mod1.female), digits = 3)
mod1.tab.female$t <- mod1.tab.female$results / mod1.tab.female$se
mod1.tab.female$OR <- exp(mod1.tab.female$results)
mod1.tab.female


##(Reference = low SES/mid math & mid SES/mid math)
mod2.female <- with(hsls.svy.female,
                    svyglm(enrolled ~ Black + Hispanic + Asian + Othrace +
                             high_math_course + rural + city + town + private + lowS_lowM + 
                             lowS_highM + midS_lowM + midS_highM + highS_lowM + highS_midM + 
                             highS_highM + school_climate + mathTach_expect + 
                             PerSchDisorder + PerBarEffTch,
                           family = quasibinomial("logit"), multicore = TRUE))

mod2.tab.female <- summary(MIcombine(mod2.female), digits = 3)
mod2.tab.female$t <- mod2.tab.female$results / mod2.tab.female$se
mod2.tab.female$OR <- exp(mod2.tab.female$results)
mod2.tab.female

##(Reference = low SES/high math & mid SES/high math)
mod3.female <- with(hsls.svy.female,
                    svyglm(enrolled ~ Black + Hispanic + Asian + Othrace +
                             high_math_course + rural + city + town + private + lowS_lowM + 
                             lowS_midM + midS_lowM + midS_midM + highS_lowM + highS_midM + 
                             highS_highM + school_climate + mathTach_expect + 
                             PerSchDisorder + PerBarEffTch, 
                           family = quasibinomial("logit"), multicore = TRUE))

mod3.tab.female <- summary(MIcombine(mod3.female), digits = 3)
mod3.tab.female$t <- mod3.tab.female$results / mod3.tab.female$se
mod3.tab.female$OR <- exp(mod3.tab.female$results)
mod3.tab.female

##(Reference = high SES/high math & high SES/high math)
mod4.female <- with(hsls.svy.female,
                    svyglm(enrolled ~ Black + Hispanic + Asian + Othrace +
                             high_math_course + rural + city + town + private + lowS_lowM + 
                             lowS_midM + lowS_highM + midS_lowM + midS_midM + midS_highM + highS_lowM + 
                             school_climate + mathTach_expect + PerSchDisorder + PerBarEffTch, 
                           family = quasibinomial("logit"), multicore = TRUE))

mod4.tab.female <- summary(MIcombine(mod4.female), digits = 3)
mod4.tab.female$t <- mod4.tab.female$results / mod4.tab.female$se
mod4.tab.female$OR <- exp(mod4.tab.female$results)
mod4.tab.female

####Calculate marginal effects for female students

##avg_slopes(): average (marginal) estimates.
mod1.ame.female <- lapply(mod1.female, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_midM", "lowS_highM", "midS_midM", 
                                            "midS_highM", "highS_lowM", "highS_midM",  
                                            "highS_highM"),
                              type = "response")   # puts it on probability scale
  
})

mod1.pooled.ame.female <- MIcombine(mod1.ame.female) ##13 & 14: high SES, Low Math (1; 0)
mod1.pooled.ame.female <- summary(mod1.pooled.ame.female)


##avg_slopes(): average (marginal) estimates.
mod2.ame.female <- lapply(mod2.female, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_lowM", "lowS_highM", "midS_lowM",
                                            "midS_highM", "highS_lowM", "highS_midM", 
                                            "highS_highM"),
                              type = "response")   # puts it on probability scale
  
})

mod2.pooled.ame.female <- MIcombine(mod2.ame.female) ##13 & 14: high SES, Low Math (1; 0)
mod2.pooled.ame.female <- summary(mod2.pooled.ame.female)

##avg_slopes(): average (marginal) estimates.
mod3.ame.female <- lapply(mod3.female, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_lowM", "lowS_midM", "midS_lowM", 
                                            "midS_midM", "highS_lowM", "highS_midM", 
                                            "highS_highM"),
                              type = "response")   # puts it on probability scale
  
})

mod3.pooled.ame.female <- MIcombine(mod3.ame.female) ##13 & 14: high SES, Low Math (1; 0)
mod3.pooled.ame.female <- summary(mod3.pooled.ame.female)

##avg_slopes(): average (marginal) estimates.
mod4.ame.female <- lapply(mod4.female, function(df){
  
  marginaleffects::avg_slopes(df,
                              variables = c("lowS_lowM", "lowS_midM", "lowS_highM", 
                                            "midS_lowM", "midS_midM", "midS_highM", "highS_lowM"),
                              type = "response")   # puts it on probability scale
  
})

mod4.pooled.ame.female <- MIcombine(mod4.ame.female) ##13 & 14: high SES, Low Math (1; 0)
mod4.pooled.ame.female <- summary(mod4.pooled.ame.female)

################################################################################################
#########################Creating Result Tables#################################################
################################################################################################

##Full model results

mod1.tab$Mod_Num <- 1
mod2.tab$Mod_Num <- 2
mod3.tab$Mod_Num <- 3
mod4.tab$Mod_Num <- 4

Full.sample.tab <- rbind(mod1.tab, mod2.tab, mod3.tab, mod4.tab)

##white sub-group model
mod1.tab.white$Mod_Num <- 1
mod2.tab.white$Mod_Num <- 2
mod3.tab.white$Mod_Num <- 3
mod4.tab.white$Mod_Num <- 4

White.sample.tab <- rbind(mod1.tab.white, mod2.tab.white, mod3.tab.white, mod4.tab.white)

##Black model results

mod1.tab.black$Mod_Num <- 1
mod2.tab.black$Mod_Num <- 2
mod3.tab.black$Mod_Num <- 3
mod4.tab.black$Mod_Num <- 4

Black.sample.tab <- rbind(mod1.tab.black, mod2.tab.black, mod3.tab.black, mod4.tab.black)

##Hispanic model results

mod1.tab.hispanic$Mod_Num <- 1
mod2.tab.hispanic$Mod_Num <- 2
mod3.tab.hispanic$Mod_Num <- 3
mod4.tab.hispanic$Mod_Num <- 4

Hispanic.sample.tab <- rbind(mod1.tab.hispanic, mod2.tab.hispanic, mod3.tab.hispanic, mod4.tab.hispanic)

##Male model results

mod1.tab.male$Mod_Num <- 1
mod2.tab.male$Mod_Num <- 2
mod3.tab.male$Mod_Num <- 3
mod4.tab.male$Mod_Num <- 4

Male.sample.tab <- rbind(mod1.tab.male, mod2.tab.male, mod3.tab.male, mod4.tab.male)

##Female model results

mod1.tab.female$Mod_Num <- 1
mod2.tab.female$Mod_Num <- 2
mod3.tab.female$Mod_Num <- 3
mod4.tab.female$Mod_Num <- 4

Female.sample.tab <- rbind(mod1.tab.female, mod2.tab.female, mod3.tab.female, mod4.tab.female)





Female.sample.tab %>% 
  select(results, se, OR, Mod_Num) %>% 
  


write.csv(Full.sample.tab, "Enrollment_Full.csv")
write.csv(White.sample.tab, "Enrollment_White.csv")
write.csv(Black.sample.tab, "Enrollment_Black.csv")
write.csv(Hispanic.sample.tab, "Enrollment_Hispanic.csv")
write.csv(Male.sample.tab, "Enrollment_Male.csv")
write.csv(Female.sample.tab, "Enrollment_Female.csv")

##Full sample AME
mod1.pooled.ame$Mod_Num <- 1
mod2.pooled.ame$Mod_Num <- 2
mod3.pooled.ame$Mod_Num <- 3
mod4.pooled.ame$Mod_Num <- 4
full.sample.pooled.ame <- rbind(mod1.pooled.ame, mod2.pooled.ame, mod3.pooled.ame, mod4.pooled.ame)

##White sample AME
mod1.pooled.ame.white$Mod_Num <- 1
mod2.pooled.ame.white$Mod_Num <- 2
mod3.pooled.ame.white$Mod_Num <- 3
mod4.pooled.ame.white$Mod_Num <- 4
white.sample.pooled.ame <- rbind(mod1.pooled.ame.white, mod2.pooled.ame.white, mod3.pooled.ame.white, mod4.pooled.ame.white)

##Black sample AME
mod1.pooled.ame.black$Mod_Num <- 1
mod2.pooled.ame.black$Mod_Num <- 2
mod3.pooled.ame.black$Mod_Num <- 3
mod4.pooled.ame.black$Mod_Num <- 4
black.sample.pooled.ame <- rbind(mod1.pooled.ame.black, mod2.pooled.ame.black, mod3.pooled.ame.black, mod4.pooled.ame.black)

##Hispanic sample AME
mod1.pooled.ame.hispanic$Mod_Num <- 1
mod2.pooled.ame.hispanic$Mod_Num <- 2
mod3.pooled.ame.hispanic$Mod_Num <- 3
mod4.pooled.ame.hispanic$Mod_Num <- 4
hispanic.sample.pooled.ame <- rbind(mod1.pooled.ame.hispanic, mod2.pooled.ame.hispanic, mod3.pooled.ame.hispanic, mod4.pooled.ame.hispanic)

##Male sample AME
mod1.pooled.ame.male$Mod_Num <- 1
mod2.pooled.ame.male$Mod_Num <- 2
mod3.pooled.ame.male$Mod_Num <- 3
mod4.pooled.ame.male$Mod_Num <- 4
male.sample.pooled.ame <- rbind(mod1.pooled.ame.male, mod2.pooled.ame.male, mod3.pooled.ame.male, mod4.pooled.ame.male)

##Female sample AME
mod1.pooled.ame.female$Mod_Num <- 1
mod2.pooled.ame.female$Mod_Num <- 2
mod3.pooled.ame.female$Mod_Num <- 3
mod4.pooled.ame.female$Mod_Num <- 4
female.sample.pooled.ame <- rbind(mod1.pooled.ame.female, mod2.pooled.ame.female, mod3.pooled.ame.female, mod4.pooled.ame.female)



write.csv(full.sample.pooled.ame, "Enrollment_Full_AME.csv")
write.csv(white.sample.pooled.ame, "Enrollment_White_AME.csv")
write.csv(black.sample.pooled.ame, "Enrollment_Black_AME.csv")
write.csv(hispanic.sample.pooled.ame, "Enrollment_Hispanic_AME.csv")
write.csv(male.sample.pooled.ame, "Enrollment_Male_AME.csv")
write.csv(female.sample.pooled.ame, "Enrollment_Female_AME.csv")
################################################################################################
#########################Diagnostic Testing for Subgroup Models#################################
################################################################################################

library(tidyverse)

female.sample.pooled.ame
# 2. Clean and Format
final_table <- female.sample.pooled.ame %>%
  # Separate highS_highM into "SES" and "Achievement"
  # We remove the trailing numbers (1, 2, 3) from the string first
  mutate(clean_id = str_remove_all(Mod_Num, "[0-9]")) %>%
  separate(clean_id, into = c("SES", "Achievement"), sep = "_") %>%
  # Relabel for readability
  mutate(
    SES = case_when(
      SES == "highS" ~ "High",
      SES == "midS"  ~ "Middle",
      SES == "lowS"  ~ "Low"
    ),
    Achievement = case_when(
      Achievement == "highM" ~ "High",
      Achievement == "midM"  ~ "Middle",
      Achievement == "lowM"  ~ "Low"
    )
  ) %>%
  # Select and Rename columns to match your desired output
  select(Mod_Num, SES, Achievement, AME = results, SE = se, `Lower 95% CI` = `(lower`, `Upper 95% CI` = `upper)`) %>%
  # Sort by Model then SES (High -> Mid -> Low)
  mutate(SES = factor(SES, levels = c("High", "Middle", "Low")),
         Achievement = factor(Achievement, levels = c("High", "Middle", "Low"))) %>%
  arrange(Mod_Num, SES, Achievement)

# 3. View the result
print(final_table)

