#downloadHSLS(root="/Users/prestonmartin/Documents/Dissertation Analyses/HSLS")

hsls.1 <- readHSLS(path = "/Users/prestonmartin/Documents/Dissertation Analyses/HSLS/HSLS/2009", ##where you saved the data
                   filename = "hsls_17_student_pets_sr_v1_0.sav") ##the files name



setwd("~/Documents/Dissertation Analyses/Dissertation_Analyses/Quantile Regressions")

##bank of weights used: 
##"w4w1w2w3stu"
##"w2w1stu"
# Generate the w1student001 to w1student200 variable names
w1_vars <- sprintf("w1student%03d", 1:200)

##extract same variables as above from the hsls.1 object for data cleaning and analysis
gddat <- data.frame(getData(data = hsls.1, varnames = c("stu_id",
                                                        "x1txmtscor", ##x1 mathematics standardized theta score
                                                        "x1stuedexpct", ##student educational expectations
                                                        "x2stuedexpct", ##student educational expectations
                                                        "x1paredexpct", ##parent educational expectations
                                                        "x1race", ##race/ethnicity composite
                                                        "x1sex", ##sex.gender
                                                        "x1ses",
                                                        "x2ses",
                                                        "w1student",
                                                        "p1repeatgrd",
                                                        "s4evratndclg",
                                                        "x3thimath",
                                                        "x1locale",
                                                        "x1control",
                                                        "x1txmquint",
                                                        "x1sesq5",
                                                        "x1famincome", ##income
                                                        "x1paredu", ##parent education 
                                                        "w1student",
                                                        w1_vars),
                            dropOmittedLevels = FALSE)) ##keep this as FALSE in the event that missing values are meaningful



gddat.imp <- gddat %>% 
  mutate(
    Locale = x1locale,
    city = if_else(x1locale == "CITY", 1, 0, missing = NA_real_),
    rural = if_else(x1locale == "RURAL", 1, 0, missing = NA_real_),
    town = if_else(x1locale == "TOWN", 1, 0 , missing = NA_real_),
    private = if_else(x1control == "CATHOLIC OR OTHER PRIVATE", 1, 0, missing = NA_real_),
    high_math_course = case_when(
      x3thimath %in% c("MISSING", "UNIT NON-RESPONSE") ~ NA_real_,
      x3thimath %in% c("NO MATH", "BASIC MATH", "OTHER MATH", "PRE-ALGEBRA", "ALGEBRA I", "GEOMETRY",
                       "ALGEBRA II", "TRIGONOMETRY", "OTHER ADVANCED MATH", "PROBABILITY AND STATISTICS",
                       "OTHER AP/IB MATH") ~ 0,
      x3thimath %in% c("AP/IB CALCULUS", "CALCULUS", "PRECALCULUS") ~ 1
    ),
    enrolled = case_when(
      s4evratndclg == "YES" ~ 1,
      s4evratndclg == "NO" ~ 0,
      s4evratndclg %in% c("MISSING", "UNIT NON-RESPONSE") ~ NA),
    Race = x1race,
    Race = na_if(as.character(x1race), "MISSING"),
    Race = as.factor(Race),
    Black = as.integer(x1race == 3),
    Hispanic = as.integer(x1race %in% c(4, 5)),
    Asian = as.integer(x1race == 2),
    White = as.integer(x1race == 8),  # reference group
    Othrace = as.integer(!(x1race %in% c(2, 3, 4, 5, 8))),
    male = if_else(x1sex == 1, 1, 0, missing = NA_real_),
    EXPECTED_EDUCATION_9 = case_when(
      x1stuedexpct == "UNIT NON-RESPONSE" ~ NA_real_,
      x1stuedexpct == "DON'T KNOW" ~ 1,
      x1stuedexpct == "LESS THAN HIGH SCHOOL" ~ 2,
      x1stuedexpct == "HIGH SCHOOL DIPLOMA OR GED" ~ 3,
      x1stuedexpct == "START AN ASSOCIATE'S DEGREE" ~ 4,
      x1stuedexpct == "COMPLETE AN ASSOCIATE'S DEGREE" ~ 5,
      x1stuedexpct == "START A BACHELOR'S DEGREE" ~ 6,
      x1stuedexpct == "COMPLETE A BACHELOR'S DEGREE" ~ 7,
      x1stuedexpct == "START A MASTER'S DEGREE" ~ 8,
      x1stuedexpct == "COMPLETE A MASTER'S DEGREE" ~ 9,
      x1stuedexpct == "START PH.D/M.D/LAW/OTHER PROF DEGREE" ~ 10,
      x1stuedexpct == "COMPLETE PH.D/M.D/LAW/OTHER PROF DEGREE" ~ 11),
    PAR_EXPECT_9 = case_when(
      x1paredexpct == "UNIT NON-RESPONSE" ~ NA_real_,
      x1paredexpct == "DON'T KNOW" ~ 1,
      x1paredexpct == "LESS THAN HIGH SCHOOL" ~ 2,
      x1paredexpct == "HIGH SCHOOL DIPLOMA OR GED" ~ 3,
      x1paredexpct == "START AN ASSOCIATE'S DEGREE" ~ 4,
      x1paredexpct == "COMPLETE AN ASSOCIATE'S DEGREE" ~ 5,
      x1paredexpct == "START A BACHELOR'S DEGREE" ~ 6,
      x1paredexpct == "COMPLETE A BACHELOR'S DEGREE" ~ 7,
      x1paredexpct == "START A MASTER'S DEGREE" ~ 8,
      x1paredexpct == "COMPLETE A MASTER'S DEGREE" ~ 9,
      x1paredexpct == "START PH.D/M.D/LAW/OTHER PROF DEGREE" ~ 10,
      x1paredexpct == "COMPLETE PH.D/M.D/LAW/OTHER PROF DEGREE" ~ 11),
    SES = x1ses,
    Math_Ach = x1txmtscor,
    Math_Ach = if_else(Math_Ach == -8, NA, Math_Ach),
    SES = if_else(SES == -8, NA, SES),
    SES = scale(SES),
    Math_Ach = scale(Math_Ach),
    Income = case_when(
      x1famincome == "MISSING" ~ NA_real_,
      x1famincome == "UNIT NON-RESPONSE" ~ NA_real_,
      x1famincome == "FAMILY INCOME LESS THAN OR EQUAL TO $15,000" ~ 1,
      x1famincome == "FAMILY INCOME > $15,000 AND <= $35,000" ~ 2,
      x1famincome == "FAMILY INCOME > $35,000 AND <= $55,000" ~ 3,
      x1famincome == "FAMILY INCOME > $55,000 AND <= $75,000" ~ 4,
      x1famincome == "FAMILY INCOME > $75,000 AND <= $95,000" ~ 5,
      x1famincome == "FAMILY INCOME > $95,000 AND <= $115,000" ~ 6,
      x1famincome == "FAMILY INCOME > $115,000 AND <= $135,000" ~ 7,
      x1famincome == "FAMILY INCOME > $135,000 AND <= $155,000" ~ 8,
      x1famincome == "FAMILY INCOME > $155,000 AND <=$175,000" ~ 9,
      x1famincome == "FAMILY INCOME > $175,000 AND <= $195,000" ~ 10,
      x1famincome == "FAMILY INCOME > $195,000 AND <= $215,00" ~ 11,
      x1famincome == "FAMILY INCOME > $215,000 AND <= $235,000" ~ 12,
      x1famincome == "FAMILY INCOME > $235,000" ~ 12),
    Log_Income = log(Income),
    Parent_Edu = case_when(
      x1paredu == "MISSING" ~ NA_real_,
      x1paredu == "UNIT NON-RESPONSE" ~ NA_real_,
      x1paredu == "LESS THAN HIGH SCHOOL" ~ 1,
      x1paredu == "HIGH SCHOOL DIPLOMA OR GED" ~ 1,
      x1paredu == "ASSOCIATE'S DEGREE" ~ 3,
      x1paredu == "BACHELOR'S DEGREE" ~ 4,
      x1paredu == "MASTER'S DEGREE" ~ 5,
      x1paredu == "PH.D/M.D/LAW/OTHER HIGH LVL PROF DEGREE" ~ 6),
    Parent_Edu_std = scale(Parent_Edu),
    Income_std = scale(Income)
  ) %>% 
  mutate() %>% 
  select(stu_id, Locale:Income_std, w1student, w1_vars)


gddat.imp <- rebindAttributes(gddat.imp, hsls.1)

###########################################################################################
################################Full Sample################################################
###########################################################################################

###Unristicted quantile regression for full smaple:
rq.9.null <- rq.sdf(formula = Math_Ach ~ SES, data = gddat.imp, tau = 0.9, weightVar = "w1student")
rq.8.null <- rq.sdf(formula = Math_Ach ~ SES, data = gddat.imp, tau = 0.8, weightVar = "w1student")
rq.7.null <- rq.sdf(formula = Math_Ach ~ SES, data = gddat.imp, tau = 0.7, weightVar = "w1student")
rq.6.null <- rq.sdf(formula = Math_Ach ~ SES, data = gddat.imp, tau = 0.6, weightVar = "w1student")
rq.5.null <- rq.sdf(formula = Math_Ach ~ SES, data = gddat.imp, tau = 0.5, weightVar = "w1student")
rq.4.null <- rq.sdf(formula = Math_Ach ~ SES, data = gddat.imp, tau = 0.4, weightVar = "w1student")
rq.3.null <- rq.sdf(formula = Math_Ach ~ SES, data = gddat.imp, tau = 0.3, weightVar = "w1student")
rq.2.null <- rq.sdf(formula = Math_Ach ~ SES, data = gddat.imp, tau = 0.2, weightVar = "w1student")
rq.1.null <- rq.sdf(formula = Math_Ach ~ SES, data = gddat.imp, tau = 0.1, weightVar = "w1student")
summary(rq.9.null)
summary(rq.8.null)
summary(rq.7.null)
summary(rq.6.null)
summary(rq.5.null)
summary(rq.4.null)
summary(rq.3.null)
summary(rq.2.null)
summary(rq.1.null)


null_model_list <- list(rq.1.null, rq.2.null, rq.3.null, rq.4.null, rq.5.null, 
                        rq.6.null, rq.7.null, rq.8.null, rq.9.null)
taus <- seq(0.1, 0.9, by = 0.1)

coef_df_null <- lapply(1:9, function(i) {
  model <- null_model_list[[i]]
  # Extract all coefficients and SEs into a temporary data frame
  data.frame(
    tau = taus[i],
    term = names(model$coef),
    estimate = as.numeric(model$coef),
    se = as.numeric(model$se)
  )
}) %>% 
  bind_rows() %>%
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se
  )

write.csv(coef_df_null, "Null_Model_Results.csv")
######################
########Full Model##############
######################

##conduct quantile regression at a given tau value (by default, tau is set to be 0.5) 
rq.9.full <- rq.sdf(formula = Math_Ach ~ SES * male + Black + Asian + Hispanic + Othrace + town + rural + city + private, data = gddat.imp, tau = 0.9, weightVar = "w1student")
rq.8.full <- rq.sdf(formula = Math_Ach ~ SES * male + Black + Asian + Hispanic + Othrace + town + rural + city + private, data = gddat.imp, tau = 0.8, weightVar = "w1student")
rq.7.full <- rq.sdf(formula = Math_Ach ~ SES * male + Black + Asian + Hispanic + Othrace + town + rural + city + private, data = gddat.imp, tau = 0.7, weightVar = "w1student")
rq.6.full <- rq.sdf(formula = Math_Ach ~ SES * male + Black + Asian + Hispanic + Othrace + town + rural + city + private, data = gddat.imp, tau = 0.6, weightVar = "w1student")
rq.5.full <- rq.sdf(formula = Math_Ach ~ SES * male + Black + Asian + Hispanic + Othrace + town + rural + city + private, data = gddat.imp, tau = 0.5, weightVar = "w1student")
rq.4.full <- rq.sdf(formula = Math_Ach ~ SES * male + Black + Asian + Hispanic + Othrace + town + rural + city + private, data = gddat.imp, tau = 0.4, weightVar = "w1student")
rq.3.full <- rq.sdf(formula = Math_Ach ~ SES * male + Black + Asian + Hispanic + Othrace + town + rural + city + private, data = gddat.imp, tau = 0.3, weightVar = "w1student")
rq.2.full <- rq.sdf(formula = Math_Ach ~ SES * male + Black + Asian + Hispanic + Othrace + town + rural + city + private, data = gddat.imp, tau = 0.2, weightVar = "w1student")
rq.1.full <- rq.sdf(formula = Math_Ach ~ SES * male + Black + Asian + Hispanic + Othrace + town + rural + city + private, data = gddat.imp, tau = 0.1, weightVar = "w1student")

summary(rq.9.full)
summary(rq.8.full)
summary(rq.7.full)
summary(rq.6.full)
summary(rq.5.full)
summary(rq.4.full)
summary(rq.3.full)
summary(rq.2.full)
summary(rq.1.full)

##Combine results into a data frame
coef_df_full <- data.frame(
  tau = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
  coef.ses = c(rq.1.full[["coef"]][["SES"]], rq.2.full[["coef"]][["SES"]], rq.3.full[["coef"]][["SES"]], rq.4.full[["coef"]][["SES"]], rq.5.full[["coef"]][["SES"]], 
               rq.6.full[["coef"]][["SES"]], rq.7.full[["coef"]][["SES"]], rq.8.full[["coef"]][["SES"]], rq.9.full[["coef"]][["SES"]]),
  se = c(rq.1.full[["se"]][["SES"]], rq.2.full[["se"]][["SES"]], rq.3.full[["se"]][["SES"]], rq.4.full[["se"]][["SES"]], rq.5.full[["se"]][["SES"]], 
         rq.6.full[["se"]][["SES"]], rq.7.full[["se"]][["SES"]], rq.8.full[["se"]][["SES"]], rq.9.full[["se"]][["SES"]])) %>% 
  mutate(
    lower = coef.ses - 1.96 * se,
    upper = coef.ses + 1.96 * se
  )

coef_df_interaction_full <- data.frame(
  tau = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
  coef.ses = c(rq.1.full[["coef"]][["SES:male"]], rq.2.full[["coef"]][["SES:male"]], rq.3.full[["coef"]][["SES:male"]], rq.4.full[["coef"]][["SES:male"]], rq.5.full[["coef"]][["SES:male"]], 
               rq.6.full[["coef"]][["SES:male"]], rq.7.full[["coef"]][["SES:male"]], rq.8.full[["coef"]][["SES:male"]], rq.9.full[["coef"]][["SES:male"]]),
  se = c(rq.1.full[["se"]][["SES:male"]], rq.2.full[["se"]][["SES:male"]], rq.3.full[["se"]][["SES:male"]], rq.4.full[["se"]][["SES:male"]], rq.5.full[["se"]][["SES:male"]], 
         rq.6.full[["se"]][["SES:male"]], rq.7.full[["se"]][["SES:male"]], rq.8.full[["se"]][["SES:male"]], rq.9.full[["se"]][["SES:male"]])) %>% 
  mutate(
    lower = coef.ses - 1.96 * se,
    upper = coef.ses + 1.96 * se
  )

model_list <- list(rq.1.full, rq.2.full, rq.3.full, rq.4.full, rq.5.full, rq.6.full, rq.7.full, rq.8.full, rq.9.full)
taus <- seq(0.1, 0.9, by = 0.1)

coef_df_full <- lapply(1:9, function(i) {
  model <- model_list[[i]]
  # Extract all coefficients and SEs into a temporary data frame
  data.frame(
    tau = taus[i],
    term = names(model$coef),
    estimate = as.numeric(model$coef),
    se = as.numeric(model$se)
  )
}) %>% 
  bind_rows() %>%
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se
  )

write.csv(coef_df_full, "Full_Model_Results.csv")


#################################################################################
##############################White Sub-group Analysis###########################
#################################################################################

gddat.imp.white <- subset(gddat.imp, subset = White == 1)

# conduct quantile regression at a given tau value (by default, tau is set to be 0.5) 
rq.9.white <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.white, tau = 0.9, weightVar = "w1student")
rq.8.white <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.white, tau = 0.8, weightVar = "w1student")
rq.7.white <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.white, tau = 0.7, weightVar = "w1student")
rq.6.white <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.white, tau = 0.6, weightVar = "w1student")
rq.5.white <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.white, tau = 0.5, weightVar = "w1student")
rq.4.white <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.white, tau = 0.4, weightVar = "w1student")
rq.3.white <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.white, tau = 0.3, weightVar = "w1student")
rq.2.white <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.white, tau = 0.2, weightVar = "w1student")
rq.1.white <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.white, tau = 0.1, weightVar = "w1student")
summary(rq.9.white)
summary(rq.8.white)
summary(rq.7.white)
summary(rq.6.white)
summary(rq.5.white)
summary(rq.4.white)
summary(rq.3.white)
summary(rq.2.white)
summary(rq.1.white)


# Combine results into a data frame
coef_df_white <- data.frame(
  tau = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
  coef.ses = c(rq.1.white[["coef"]][["SES"]], rq.2.white[["coef"]][["SES"]], rq.3.white[["coef"]][["SES"]], rq.4.white[["coef"]][["SES"]], rq.5.white[["coef"]][["SES"]], 
               rq.6.white[["coef"]][["SES"]], rq.7.white[["coef"]][["SES"]], rq.8.white[["coef"]][["SES"]], rq.9.white[["coef"]][["SES"]]),
  se = c(rq.1.white[["se"]][["SES"]], rq.2.white[["se"]][["SES"]], rq.3.white[["se"]][["SES"]], rq.4.white[["se"]][["SES"]], rq.5.white[["se"]][["SES"]], 
         rq.6.white[["se"]][["SES"]], rq.7.white[["se"]][["SES"]], rq.8.white[["se"]][["SES"]], rq.9.white[["se"]][["SES"]]))%>%
  mutate(
    lower = coef.ses - 1.96 * se,
    upper = coef.ses + 1.96 * se
  )

coef_df_interaction_white <- data.frame(
  tau = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
  coef.ses = c(rq.1.white[["coef"]][["SES:male"]], rq.2.white[["coef"]][["SES:male"]], rq.3.white[["coef"]][["SES:male"]], rq.4.white[["coef"]][["SES:male"]], rq.5.white[["coef"]][["SES:male"]], 
               rq.6.white[["coef"]][["SES:male"]], rq.7.white[["coef"]][["SES:male"]], rq.8.white[["coef"]][["SES:male"]], rq.9.white[["coef"]][["SES:male"]]),
  se = c(rq.1.white[["se"]][["SES:male"]], rq.2.white[["se"]][["SES:male"]], rq.3.white[["se"]][["SES:male"]], rq.4.white[["se"]][["SES:male"]], rq.5.white[["se"]][["SES:male"]], 
         rq.6.white[["se"]][["SES:male"]], rq.7.white[["se"]][["SES:male"]], rq.8.white[["se"]][["SES:male"]], rq.9.white[["se"]][["SES:male"]]))%>%
  mutate(
    lower = coef.ses - 1.96 * se,
    upper = coef.ses + 1.96 * se
  )


white_model_list <- list(rq.1.white, rq.2.white, rq.3.white, rq.4.white, rq.5.white, 
                        rq.6.white, rq.7.white, rq.8.white, rq.9.white)
taus <- seq(0.1, 0.9, by = 0.1)

coef_df_w <- lapply(1:9, function(i) {
  model <- white_model_list[[i]]
  # Extract all coefficients and SEs into a temporary data frame
  data.frame(
    tau = taus[i],
    term = names(model$coef),
    estimate = as.numeric(model$coef),
    se = as.numeric(model$se)
  )
}) %>% 
  bind_rows() %>%
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se
  )

write.csv(coef_df_w, "White_Model_Results.csv")



#################################################################################
##############################Hispanic Sub-group Analysis###########################
#################################################################################
gddat.imp.hispanic <- subset(gddat.imp, subset = Hispanic == 1)


# conduct quantile regression at a given tau value (by default, tau is set to be 0.5) 
rq.9.hispanic <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.hispanic, tau = 0.9, weightVar = "w1student")
rq.8.hispanic <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.hispanic, tau = 0.8, weightVar = "w1student")
rq.7.hispanic <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.hispanic, tau = 0.7, weightVar = "w1student")
rq.6.hispanic <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.hispanic, tau = 0.6, weightVar = "w1student")
rq.5.hispanic <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.hispanic, tau = 0.5, weightVar = "w1student")
rq.4.hispanic <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.hispanic, tau = 0.4, weightVar = "w1student")
rq.3.hispanic <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.hispanic, tau = 0.3, weightVar = "w1student")
rq.2.hispanic <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.hispanic, tau = 0.2, weightVar = "w1student")
rq.1.hispanic <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.hispanic, tau = 0.1, weightVar = "w1student")
summary(rq.9.hispanic)
summary(rq.8.hispanic)
summary(rq.7.hispanic)
summary(rq.6.hispanic)
summary(rq.5.hispanic)
summary(rq.4.hispanic)
summary(rq.3.hispanic)
summary(rq.2.hispanic)
summary(rq.1.hispanic)

# Combine results into a data frame
coef_df_hispanic <- data.frame(
  tau = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
  coef.ses = c(rq.1.hispanic[["coef"]][["SES"]], rq.2.hispanic[["coef"]][["SES"]], rq.3.hispanic[["coef"]][["SES"]], 
               rq.4.hispanic[["coef"]][["SES"]], rq.5.hispanic[["coef"]][["SES"]], 
               rq.6.hispanic[["coef"]][["SES"]], rq.7.hispanic[["coef"]][["SES"]], 
               rq.8.hispanic[["coef"]][["SES"]], rq.9.hispanic[["coef"]][["SES"]]),
  
  se = c(rq.1.hispanic[["se"]][["SES"]], rq.2.hispanic[["se"]][["SES"]], rq.3.hispanic[["se"]][["SES"]], 
         rq.4.hispanic[["se"]][["SES"]], rq.5.hispanic[["se"]][["SES"]], 
         rq.6.hispanic[["se"]][["SES"]], rq.7.hispanic[["se"]][["SES"]], 
         rq.8.hispanic[["se"]][["SES"]], rq.9.hispanic[["se"]][["SES"]]))%>%
  mutate(
    lower = coef.ses - 1.96 * se,
    upper = coef.ses + 1.96 * se
  )


coef_df_interaction_hispanic <- data.frame(
  tau = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
  coef.ses = c(rq.1.hispanic[["coef"]][["SES:male"]], rq.2.hispanic[["coef"]][["SES:male"]], 
               rq.3.hispanic[["coef"]][["SES:male"]], rq.4.hispanic[["coef"]][["SES:male"]], 
               rq.5.hispanic[["coef"]][["SES:male"]], rq.6.hispanic[["coef"]][["SES:male"]], 
               rq.7.hispanic[["coef"]][["SES:male"]], rq.8.hispanic[["coef"]][["SES:male"]], 
               rq.9.hispanic[["coef"]][["SES:male"]]),
  
  se = c(rq.1.hispanic[["se"]][["SES:male"]], rq.2.hispanic[["se"]][["SES:male"]], 
         rq.3.hispanic[["se"]][["SES:male"]], rq.4.hispanic[["se"]][["SES:male"]], 
         rq.5.hispanic[["se"]][["SES:male"]], rq.6.hispanic[["se"]][["SES:male"]], 
         rq.7.hispanic[["se"]][["SES:male"]], rq.8.hispanic[["se"]][["SES:male"]], 
         rq.9.hispanic[["se"]][["SES:male"]])) %>%
  mutate(
    lower = coef.ses - 1.96 * se,
    upper = coef.ses + 1.96 * se
  )


hispanic_model_list <- list(rq.1.hispanic, rq.2.hispanic, rq.3.hispanic, rq.4.hispanic, rq.5.hispanic, 
                         rq.6.hispanic, rq.7.hispanic, rq.8.hispanic, rq.9.hispanic)
taus <- seq(0.1, 0.9, by = 0.1)

coef_df_h <- lapply(1:9, function(i) {
  model <- hispanic_model_list[[i]]
  # Extract all coefficients and SEs into a temporary data frame
  data.frame(
    tau = taus[i],
    term = names(model$coef),
    estimate = as.numeric(model$coef),
    se = as.numeric(model$se)
  )
}) %>% 
  bind_rows() %>%
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se
  )

write.csv(coef_df_h, "Hispanic_Model_Results.csv")

#################################################################################
##############################Black Sub-group Analysis###########################
#################################################################################
gddat.imp.black <- subset(gddat.imp, subset = Black == 1)


# conduct quantile regression at a given tau value (by default, tau is set to be 0.5) 
rq.9.black <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.black, tau = 0.9, weightVar = "w1student")
rq.8.black <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.black, tau = 0.8, weightVar = "w1student")
rq.7.black <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.black, tau = 0.7, weightVar = "w1student")
rq.6.black <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.black, tau = 0.6, weightVar = "w1student")
rq.5.black <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.black, tau = 0.5, weightVar = "w1student")
rq.4.black <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.black, tau = 0.4, weightVar = "w1student")
rq.3.black <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.black, tau = 0.3, weightVar = "w1student")
rq.2.black <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.black, tau = 0.2, weightVar = "w1student")
rq.1.black <- rq.sdf(formula = Math_Ach ~ SES * male + town + rural + city + private, data = gddat.imp.black, tau = 0.1, weightVar = "w1student")
summary(rq.9.black)
summary(rq.8.black)
summary(rq.7.black)
summary(rq.6.black)
summary(rq.5.black)
summary(rq.4.black)
summary(rq.3.black)
summary(rq.2.black)
summary(rq.1.black)

# Combine results into a data frame
coef_df_black <- data.frame(
  tau = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
  coef.ses = c(rq.1.black[["coef"]][["SES"]], rq.2.black[["coef"]][["SES"]], 
               rq.3.black[["coef"]][["SES"]], rq.4.black[["coef"]][["SES"]], 
               rq.5.black[["coef"]][["SES"]], rq.6.black[["coef"]][["SES"]], 
               rq.7.black[["coef"]][["SES"]], rq.8.black[["coef"]][["SES"]], 
               rq.9.black[["coef"]][["SES"]]),
  
  se = c(rq.1.black[["se"]][["SES"]], rq.2.black[["se"]][["SES"]], rq.3.black[["se"]][["SES"]], 
         rq.4.black[["se"]][["SES"]], rq.5.black[["se"]][["SES"]], 
         rq.6.black[["se"]][["SES"]], rq.7.black[["se"]][["SES"]], 
         rq.8.black[["se"]][["SES"]], rq.9.black[["se"]][["SES"]]))%>% 
  mutate(
    lower = coef.ses - 1.96 * se,
    upper = coef.ses + 1.96 * se
  )

coef_df_interaction_black <- data.frame(
  tau = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
  coef.ses = c(rq.1.black[["coef"]][["SES:male"]], rq.2.black[["coef"]][["SES:male"]], 
               rq.3.black[["coef"]][["SES:male"]], rq.4.black[["coef"]][["SES:male"]], 
               rq.5.black[["coef"]][["SES:male"]], rq.6.black[["coef"]][["SES:male"]], 
               rq.7.black[["coef"]][["SES:male"]], rq.8.black[["coef"]][["SES:male"]], 
               rq.9.black[["coef"]][["SES:male"]]),
  
  se = c(rq.1.black[["se"]][["SES:male"]], rq.2.black[["se"]][["SES:male"]], 
         rq.3.black[["se"]][["SES:male"]], rq.4.black[["se"]][["SES:male"]], 
         rq.5.black[["se"]][["SES:male"]], rq.6.black[["se"]][["SES:male"]], 
         rq.7.black[["se"]][["SES:male"]], rq.8.black[["se"]][["SES:male"]], 
         rq.9.black[["se"]][["SES:male"]])) %>% 
  mutate(
    lower = coef.ses - 1.96 * se,
    upper = coef.ses + 1.96 * se
  )



black_model_list <- list(rq.1.black, rq.2.black, rq.3.black, rq.4.black, rq.5.black, 
                            rq.6.black, rq.7.black, rq.8.black, rq.9.black)
taus <- seq(0.1, 0.9, by = 0.1)

coef_df_b <- lapply(1:9, function(i) {
  model <- black_model_list[[i]]
  # Extract all coefficients and SEs into a temporary data frame
  data.frame(
    tau = taus[i],
    term = names(model$coef),
    estimate = as.numeric(model$coef),
    se = as.numeric(model$se)
  )
}) %>% 
  bind_rows() %>%
  mutate(
    lower = estimate - 1.96 * se,
    upper = estimate + 1.96 * se
  )

write.csv(coef_df_b, "Black_Model_Results.csv")

