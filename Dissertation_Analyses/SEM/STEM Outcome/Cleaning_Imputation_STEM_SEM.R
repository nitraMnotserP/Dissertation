hsls.1 <- EdSurvey::readHSLS(path = "/Users/prestonmartin/Documents/Dissertation_Analyses/HSLS/HSLS/2009", ##where you saved the data
                   filename = "hsls_17_student_pets_sr_v1_0.sav") ##the files name



setwd("~/Documents/Dissertation Analyses/Dissertation_Analyses/CA-STEM Models")

##bank of weights used: 
##"w4w1w2w3stu"
##"w2w1stu"
# Generate the w1student001 to w1student200 variable names
w1_vars <- sprintf("w4w1w2w3stu%03d", 1:200)

##extract same variables as above from the hsls.1 object for data cleaning and analysis
gddat <- data.frame(EdSurvey::getData(data = hsls.1, varnames = c("stu_id",
                                                        "x1txmtscor", ##x1 mathematics standardized theta score
                                                        "x2txmtscor", ##x1 mathematics standardized theta score
                                                        "x1stuedexpct", ##student educational expectations
                                                        "x2stuedexpct", ##student educational expectations
                                                        "x1paredexpct", ##parent educational expectations
                                                        "x2paredexpct", ##parent educational expectations
                                                        
                                                        ##social capital variables
                                                        "s1frndgrades",
                                                        "s1frndschool",
                                                        "s1frndclass",
                                                        "s1frndclg",
                                                        "s1frndtlkjob",
                                                        "s1frndtlkprb",
                                                        "s1frndtlkclg",
                                                        "s1frndtlkoth",
                                                        "s1frndtalks",
                                                        "s1frndtalkm",
                                                        
                                                        "x1race", ##race/ethnicity composite
                                                        "x1sex", ##sex.gender
                                                        "x1ses",
                                                        "s4evratndclg", ##subset
                                                        "x4rfdgmjstem", ##dependent
                                                        
                                                        "x3thimath",
                                                        "x1locale",
                                                        "x1control",
                                                        ##school variables
                                                        "x1schoolcli",
                                                        "m1facilities", 
                                                        "m1ratio", 
                                                        "m1resources", 
                                                        "m1profdev",
                                                        "m1dropout",
                                                        "m1morale",
                                                        "x1tsexp", 
                                                        "x1tmexp", 
                                                        "x1coupercou", 
                                                        "x1schoolcli",
                                                        "m1tardy",
                                                        "m1stuabsent",
                                                        "m1cut",
                                                        "m1tchrabsent",
                                                        "m1dropout",
                                                        "m1apathy",
                                                        "m1involvemnt",
                                                        "m1unprepprob",
                                                        "m1health",
                                                        "m1resources",
                                                        "m1ablrange",
                                                        "m1sesrange",
                                                        "m1langrange",
                                                        "m1specneed",
                                                        "m1uninterest",
                                                        "m1morale",
                                                        "m1disrupt",
                                                        "m1profdev",
                                                        "m1admsupport",
                                                        "m1computer",
                                                        "m1techsupprt",
                                                        "m1books",
                                                        "m1stuequip",
                                                        "m1demoequip",
                                                        "m1facilities",
                                                        "m1ratio",
                                                        "m1planning",
                                                        "m1autonomy",
                                                        "m1famsupport",
                                                        "w4w1w2w3stu",
                                                        w1_vars),
                            dropOmittedLevels = FALSE)) ##keep this as FALSE in the event that missing values are meaningful

weights <- gddat %>% 
  select(stu_id, w1_vars)

table(gddat$x1txmtscor)
table(gddat$x2txmtscor)

##select sample of only those with high school diploma or equivalent credential

##helper to convert HSLS missing codes in character fields
to_na_missing <- function(x) {
  x_chr <- as.character(x)
  x_chr[x_chr %in% c("MISSING", "UNIT NON-RESPONSE")] <- NA_character_
  x_chr
}

gddat.imp <- gddat %>%
  mutate(
    ##Simple factors
    Locale      = as.factor(x1locale),
    School_type = as.factor(x1control),
    
    ##High math course: clean strings -> factor
    high_math_course = to_na_missing(x3thimath),
    high_math_course = as.factor(high_math_course),
    
    STEM_Major = x4rfdgmjstem,   # <-- likely needs a different source column
    STEM_Major = as.character(x4rfdgmjstem),
    STEM_Major = if_else(STEM_Major %in% c("MISSING","UNIT NON-RESPONSE", "DON'T KNOW"), NA_character_, STEM_Major),
    
    # Race / Gender: convert "MISSING" to NA then factor
    x1race = na_if(as.character(x1race), "MISSING"),
    Race   = as.factor(x1race),
    
    x1sex  = na_if(as.character(x1sex), "MISSING"),
    Gender = as.factor(x1sex),
    
    # Expectations: treat "UNIT NON-RESPONSE" as NA (character-safe), then factor
    STU_EXPECT_9 = case_when(
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
    STU_EXPECT_11 = case_when(
      x2stuedexpct == "UNIT NON-RESPONSE" ~ NA_real_,
      x2stuedexpct == "DON'T KNOW" ~ 1,
      x2stuedexpct == "LESS THAN HIGH SCHOOL COMPLETION" ~ 2,
      x2stuedexpct == "COMPLETE HS DIPLOMA/GED/ALTERNATIVE HS CREDENTIAL" ~ 3,
      x2stuedexpct == "START, BUT NOT COMPLETE CERTIFICATE/DIPLOMA FROM SCHOOL PROVIDING OCC TRAINING" ~ 4,
      x2stuedexpct == "COMPLETE CERTIFICATE/DIPLOMA FROM SCHOOL PROVIDING OCCUPATIONAL TRAINING" ~ 5,
      x2stuedexpct == "START, BUT NOT COMPLETE ASSOCIATE'S DEGREE" ~ 4,
      x2stuedexpct == "COMPLETE ASSOCIATE'S DEGREE" ~ 5,
      x2stuedexpct == "START, BUT NOT COMPLETE BACHELOR'S DEGREE" ~ 6,
      x2stuedexpct == "COMPLETE BACHELOR'S DEGREE" ~ 7,
      x2stuedexpct == "START, BUT NOT COMPLETE MASTER'S DEGREE" ~ 8,
      x2stuedexpct == "COMPLETE MASTER'S DEGREE" ~ 9,
      x2stuedexpct == "START, BUT NOT COMPLETE PH.D./M.D./LAW DEGREE/HIGH LEVEL PROFESSIONAL DEGREE" ~ 10,
      x2stuedexpct == "COMPLETE PH.D./M.D./LAW DEGREE/OTHER HIGH LEVEL PROFESSIONAL DEGREE" ~ 11),
    PAR_EXPECT_9 = case_when(
      x1paredexpct == "MISSING" ~ NA_real_,
      x1paredexpct == "UNIT NON-RESPONSE" ~ NA_real_,
      x1paredexpct == "DON'T KNOW" ~ 1,
      x1paredexpct == "LESS THAN HIGH SCHOOL" ~ 2,
      x1paredexpct == "HIGH SCHOOL DIPLOMA OR GED" ~ 2,
      x1paredexpct == "START AN ASSOCIATE'S DEGREE" ~ 3,
      x1paredexpct == "COMPLETE AN ASSOCIATE'S DEGREE" ~ 3,
      x1paredexpct == "START A BACHELOR'S DEGREE" ~ 4,
      x1paredexpct == "COMPLETE A BACHELOR'S DEGREE" ~ 4,
      x1paredexpct == "START A MASTER'S DEGREE" ~ 5,
      x1paredexpct == "COMPLETE A MASTER'S DEGREE" ~ 5,
      x1paredexpct == "START PH.D/M.D/LAW/OTHER PROF DEGREE" ~ 6,
      x1paredexpct == "COMPLETE PH.D/M.D/LAW/OTHER PROF DEGREE" ~ 6),
    PAR_EXPECT_11 = case_when(
      x2paredexpct == "UNIT NON-RESPONSE" ~ NA_real_,
      x2paredexpct == "DON'T KNOW" ~ 1,
      x2paredexpct == "LESS THAN HIGH SCHOOL COMPLETION" ~ 2,
      x2paredexpct == "COMPLETE HS DIPLOMA/GED/ALTERNATIVE HS CREDENTIAL" ~ 2,
      x2paredexpct == "START, BUT NOT COMPLETE CERTIFICATE/DIPLOMA FROM SCHOOL PROVIDING OCC TRAINING" ~ 3,
      x2paredexpct == "COMPLETE CERTIFICATE/DIPLOMA FROM SCHOOL PROVIDING OCCUPATIONAL TRAINING" ~ 3,
      x2paredexpct == "START, BUT NOT COMPLETE ASSOCIATE'S DEGREE" ~ 3,
      x2paredexpct == "COMPLETE ASSOCIATE'S DEGREE" ~ 3,
      x2paredexpct == "START, BUT NOT COMPLETE BACHELOR'S DEGREE" ~ 4,
      x2paredexpct == "COMPLETE BACHELOR'S DEGREE" ~ 4,
      x2paredexpct == "START, BUT NOT COMPLETE MASTER'S DEGREE" ~ 5,
      x2paredexpct == "COMPLETE MASTER'S DEGREE" ~ 5,
      x2paredexpct == "START, BUT NOT COMPLETE PH.D./M.D./LAW DEGREE/HIGH LEVEL PROFESSIONAL DEGREE" ~ 6,
      x2paredexpct == "COMPLETE PH.D./M.D./LAW DEGREE/OTHER HIGH LEVEL PROFESSIONAL DEGREE" ~ 6),
    
    ##first factor
    s1frndschool = case_when(
      s1frndschool == "MISSING" ~ NA_real_,
      s1frndschool == "UNIT NON-RESPONSE" ~ NA_real_,
      s1frndschool == "TRUE" ~ 1,
      s1frndschool == "FALSE" ~ 0,
      TRUE ~ NA_real_),
    s1frndgrades = case_when( 
      s1frndgrades == "MISSING" ~ NA_real_,
      s1frndgrades == "UNIT NON-RESPONSE" ~ NA_real_,
      s1frndgrades == "TRUE" ~ 1,
      s1frndgrades == "FALSE" ~ 0,
      TRUE ~ NA_real_),
    s1frndclass = case_when(
      s1frndclass == "MISSING" ~ NA_real_,
      s1frndclass == "UNIT NON-RESPONSE" ~ NA_real_,
      s1frndclass == "TRUE" ~ 1,
      s1frndclass == "FALSE" ~ 0,
      TRUE ~ NA_real_),
    s1frndclg = case_when(
      s1frndclg == "MISSING" ~ NA_real_,
      s1frndclg == "UNIT NON-RESPONSE" ~ NA_real_,
      s1frndclg == "TRUE" ~ 1,
      s1frndclg == "FALSE" ~ 0,
      TRUE ~ NA_real_),
    
    ##second factor
    
    s1frndtlkjob = case_when(
      s1frndtlkjob == "MISSING" ~ NA_real_,
      s1frndtlkjob == "UNIT NON-RESPONSE" ~ NA_real_,
      s1frndtlkjob == "YES" ~ 1,
      s1frndtlkjob == "NO" ~ 0,
      TRUE ~ NA_real_),
    
    s1frndtlkprb = case_when(
      s1frndtlkprb == "MISSING" ~ NA_real_,
      s1frndtlkprb == "UNIT NON-RESPONSE" ~ NA_real_,
      s1frndtlkprb == "YES" ~ 1,
      s1frndtlkprb == "NO" ~ 0,
      TRUE ~ NA_real_),  
    
    s1frndtlkclg = case_when(
      s1frndtlkclg == "MISSING" ~ NA_real_,
      s1frndtlkclg == "UNIT NON-RESPONSE" ~ NA_real_,
      s1frndtlkclg == "YES" ~ 1,
      s1frndtlkclg == "NO" ~ 0,
      TRUE ~ NA_real_),   
    
    s1frndtlkoth = case_when(
      s1frndtlkoth == "MISSING" ~ NA_real_,
      s1frndtlkoth == "UNIT NON-RESPONSE" ~ NA_real_,
      s1frndtlkoth == "YES" ~ 1,
      s1frndtlkoth == "NO" ~ 0,
      TRUE ~ NA_real_), 
    
    s1frndtalks = case_when(
      s1frndtalks == "MISSING" ~ NA_real_,
      s1frndtalks == "UNIT NON-RESPONSE" ~ NA_real_,
      s1frndtalks == "YES" ~ 1,
      s1frndtalks == "NO" ~ 0,
      TRUE ~ NA_real_), 
    
    s1frndtalkm = case_when(
      s1frndtalkm == "MISSING" ~ NA_real_,
      s1frndtalkm == "UNIT NON-RESPONSE" ~ NA_real_,
      s1frndtalkm == "YES" ~ 1,
      s1frndtalkm == "NO" ~ 0,
      TRUE ~ NA_real_),
    
    ###Math aand SES
    SES = x1ses,
    SES = if_else(SES == -8, NA_real_, SES),
    Math_9 = x1txmtscor,
    Math_9 = if_else(Math_9 == -8, NA_real_, Math_9),
    Math_11 = x2txmtscor,
    Math_11 = if_else(Math_11 == -8, NA_real_, Math_11),
    
    ##continuous measure of school Climate
    school_climate = if_else(x1schoolcli == -9, NA_real_, x1schoolcli),
    counselor_expect = if_else(x1coupercou == -9 | x1coupercou == -8, NA, x1coupercou),
    sciTch_expect = if_else(x1tsexp == -9 | x1tsexp == -8 | x1tsexp == -7, NA, x1tsexp),
    mathTach_expect = if_else(x1tmexp == -9 | x1tmexp == -8 | x1tmexp == -7, NA, x1tmexp),
    
    ##Likert item of school climate
    m1facilities.2 = case_when(
      m1facilities == "MISSING" ~ NA_real_,
      m1facilities == "UNIT NON-RESPONSE" ~ NA_real_,
      m1facilities == "NOT APPLICABLE" ~ NA_real_,
      m1facilities == "NOT AT ALL" ~ 4,
      m1facilities == "A LITTLE" ~ 3,
      m1facilities == "SOME" ~ 2,
      m1facilities == "A LOT" ~ 1,
    ),
    m1resources.2 = case_when(
      m1resources == "MISSING" ~ NA_real_,
      m1resources == "UNIT NON-RESPONSE" ~ NA_real_,
      m1resources == "NOT APPLICABLE" ~ NA_real_,
      m1resources == "NOT A PROBLEM" ~ 4,
      m1resources == "MINOR PROBLEM" ~ 3,
      m1resources == "MODERATE PROBLEM" ~ 2,
      m1resources == "SERIOUS PROBLEM" ~ 1,
    ),
    m1ratio.2 = case_when(
      m1ratio == "MISSING" ~ NA_real_,
      m1ratio == "UNIT NON-RESPONSE" ~ NA_real_,
      m1ratio == "NOT APPLICABLE" ~ NA_real_,
      m1ratio == "NOT AT ALL" ~ 4,
      m1ratio == "A LITTLE" ~ 3,
      m1ratio == "SOME" ~ 2,
      m1ratio == "A LOT" ~ 1,
    ),
    m1profdev.2 = case_when(
      m1profdev == "MISSING" ~ NA_real_,
      m1profdev == "UNIT NON-RESPONSE" ~ NA_real_,
      m1profdev == "NOT APPLICABLE" ~ NA_real_,
      m1profdev == "NOT AT ALL" ~ 4,
      m1profdev == "A LITTLE" ~ 3,
      m1profdev == "SOME" ~ 2,
      m1profdev == "A LOT" ~ 1
    ),
    
    ##Perceived School Disorder
    m1tardy.2 = case_when(
      m1tardy == "MISSING" ~ NA_real_,
      m1tardy == "UNIT NON-RESPONSE" ~ NA_real_,
      m1morale == "NOT APPLICABLE" ~ NA_real_,
      m1tardy == "NOT A PROBLEM" ~ 4,
      m1tardy == "MINOR PROBLEM" ~ 3,
      m1tardy == "MODERATE PROBLEM" ~ 2,
      m1tardy == "SERIOUS PROBLEM" ~ 1
    ),
    m1stuabsent.2 = case_when(
      m1stuabsent == "MISSING" ~ NA_real_,
      m1stuabsent == "UNIT NON-RESPONSE" ~ NA_real_,
      m1stuabsent == "NOT APPLICABLE" ~ NA_real_,
      m1stuabsent == "NOT A PROBLEM" ~ 4,
      m1stuabsent == "MINOR PROBLEM" ~ 3,
      m1stuabsent == "MODERATE PROBLEM" ~ 2,
      m1stuabsent == "SERIOUS PROBLEM" ~ 1
    ),
    m1cut.2 = case_when(
      m1cut == "MISSING" ~ NA_real_,
      m1cut == "UNIT NON-RESPONSE" ~ NA_real_,
      m1cut == "NOT APPLICABLE" ~ NA_real_,
      m1cut == "NOT A PROBLEM" ~ 4,
      m1cut == "MINOR PROBLEM" ~ 3,
      m1cut == "MODERATE PROBLEM" ~ 2,
      m1cut == "SERIOUS PROBLEM" ~ 1
    ),
    m1tchrabsent.2 = case_when(
      m1tchrabsent == "MISSING" ~ NA_real_,
      m1tchrabsent == "UNIT NON-RESPONSE" ~ NA_real_,
      m1tchrabsent == "NOT APPLICABLE" ~ NA_real_,
      m1tchrabsent == "NOT A PROBLEM" ~ 3,
      m1tchrabsent == "MINOR PROBLEM" ~ 2,
      m1tchrabsent == "MODERATE TO SERIOUS PROBLEM" ~ 1,
    ),
    m1dropout.2 = case_when(
      m1dropout == "MISSING" ~ NA_real_,
      m1dropout == "UNIT NON-RESPONSE" ~ NA_real_,
      m1dropout == "NOT A PROBLEM" ~ 4,
      m1dropout == "MINOR PROBLEM" ~ 3,
      m1dropout == "MODERATE PROBLEM" ~ 2,
      m1dropout == "SERIOUS PROBLEM" ~ 1
    ),
    m1apathy.2 = case_when(
      m1apathy == "MISSING" ~ NA_real_,
      m1apathy == "UNIT NON-RESPONSE" ~ NA_real_,
      m1apathy == "NOT A PROBLEM" ~ 4,
      m1apathy == "MINOR PROBLEM" ~ 3,
      m1apathy == "MODERATE PROBLEM" ~ 2,
      m1apathy == "SERIOUS PROBLEM" ~ 1
    ),
    m1involvemnt.2 = case_when(
      m1involvemnt == "MISSING" ~ NA_real_,
      m1involvemnt == "UNIT NON-RESPONSE" ~ NA_real_,
      m1involvemnt == "NOT A PROBLEM" ~ 4,
      m1involvemnt == "MINOR PROBLEM" ~ 3,
      m1involvemnt == "MODERATE PROBLEM" ~ 2,
      m1involvemnt == "SERIOUS PROBLEM" ~ 1
    ),
    m1unprepprob.2 = case_when(
      m1unprepprob == "MISSING" ~ NA_real_,
      m1unprepprob == "UNIT NON-RESPONSE" ~ NA_real_,
      m1unprepprob == "NOT A PROBLEM" ~ 4,
      m1unprepprob == "MINOR PROBLEM" ~ 3,
      m1unprepprob == "MODERATE PROBLEM" ~ 2,
      m1unprepprob == "SERIOUS PROBLEM" ~ 1
    ),
    ## Perceived Barriers to Effective Teaching
    
    m1techsupprt.2 = case_when(
      as.character(m1techsupprt) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1techsupprt) == "NOT AT ALL" ~ 1,
      as.character(m1techsupprt) == "A LITTLE"   ~ 2,
      as.character(m1techsupprt) == "SOME"       ~ 3,
      as.character(m1techsupprt) == "A LOT"      ~ 4
    ),
    
    m1health.2 = case_when(
      as.character(m1health) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1health) == "NOT A PROBLEM" ~ 1,
      as.character(m1health) == "MINOR PROBLEM"   ~ 2,
      as.character(m1health) == "MODERATE PROBLEM"       ~ 3,
      as.character(m1health) == "SERIOUS PROBLEM"      ~ 4
    ),
    
    m1resources.2 = case_when(
      as.character(m1resources) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1resources) == "NOT AT ALL" ~ 1,
      as.character(m1resources) == "MINOR PROBLEM"   ~ 2,
      as.character(m1resources) == "MODERATE PROBLEM"       ~ 3,
      as.character(m1resources) == "SERIOUS PROBLEM"      ~ 4
    ),
    
    m1ablrange.2 = case_when(
      as.character(m1ablrange) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1ablrange) == "NOT AT ALL" ~ 1,
      as.character(m1ablrange) == "A LITTLE"   ~ 2,
      as.character(m1ablrange) == "SOME"       ~ 3,
      as.character(m1ablrange) == "A LOT"      ~ 4
    ),
    
    m1sesrange.2 = case_when(
      as.character(m1sesrange) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1sesrange) == "NOT AT ALL" ~ 1,
      as.character(m1sesrange) == "A LITTLE"   ~ 2,
      as.character(m1sesrange) == "SOME"       ~ 3,
      as.character(m1sesrange) == "A LOT"      ~ 4
    ),
    
    m1langrange.2 = case_when(
      as.character(m1langrange) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1langrange) == "NOT AT ALL" ~ 1,
      as.character(m1langrange) == "A LITTLE"   ~ 2,
      as.character(m1langrange) == "SOME"       ~ 3,
      as.character(m1langrange) == "A LOT"      ~ 4
    ),
    
    m1specneed.2 = case_when(
      as.character(m1specneed) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1specneed) == "NOT AT ALL" ~ 1,
      as.character(m1specneed) == "A LITTLE"   ~ 2,
      as.character(m1specneed) == "SOME"       ~ 3,
      as.character(m1specneed) == "A LOT"      ~ 4
    ),
    
    m1uninterest.2 = case_when(
      as.character(m1uninterest) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1uninterest) == "NOT AT ALL" ~ 1,
      as.character(m1uninterest) == "A LITTLE"   ~ 2,
      as.character(m1uninterest) == "SOME"       ~ 3,
      as.character(m1uninterest) == "A LOT"      ~ 4
    ),
    
    m1morale.2 = case_when(
      as.character(m1morale) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1morale) == "NOT AT ALL" ~ 1,
      as.character(m1morale) == "A LITTLE"   ~ 2,
      as.character(m1morale) == "SOME"       ~ 3,
      as.character(m1morale) == "A LOT"      ~ 4
    ),
    
    m1disrupt.2 = case_when(
      as.character(m1disrupt) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1disrupt) == "NOT AT ALL" ~ 1,
      as.character(m1disrupt) == "A LITTLE"   ~ 2,
      as.character(m1disrupt) == "SOME"       ~ 3,
      as.character(m1disrupt) == "A LOT"      ~ 4
    ),
    
    m1profdev.2 = case_when(
      as.character(m1profdev) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1profdev) == "NOT AT ALL" ~ 1,
      as.character(m1profdev) == "A LITTLE"   ~ 2,
      as.character(m1profdev) == "SOME"       ~ 3,
      as.character(m1profdev) == "A LOT"      ~ 4
    ),
    
    m1admsupport.2 = case_when(
      as.character(m1admsupport) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1admsupport) == "NOT AT ALL" ~ 1,
      as.character(m1admsupport) == "A LITTLE"   ~ 2,
      as.character(m1admsupport) == "SOME"       ~ 3,
      as.character(m1admsupport) == "A LOT"      ~ 4
    ),
    
    m1computer.2 = case_when(
      as.character(m1computer) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1computer) == "NOT AT ALL" ~ 1,
      as.character(m1computer) == "A LITTLE"   ~ 2,
      as.character(m1computer) == "SOME"       ~ 3,
      as.character(m1computer) == "A LOT"      ~ 4
    ),
    
    m1books.2 = case_when(
      as.character(m1books) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1books) == "NOT AT ALL" ~ 1,
      as.character(m1books) == "A LITTLE"   ~ 2,
      as.character(m1books) == "SOME"       ~ 3,
      as.character(m1books) == "A LOT"      ~ 4
    ),
    
    m1stuequip.2 = case_when(
      as.character(m1stuequip) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1stuequip) == "NOT AT ALL" ~ 1,
      as.character(m1stuequip) == "A LITTLE"   ~ 2,
      as.character(m1stuequip) == "SOME"       ~ 3,
      as.character(m1stuequip) == "A LOT"      ~ 4
    ),
    
    m1demoequip.2 = case_when(
      as.character(m1demoequip) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1demoequip) == "NOT AT ALL" ~ 1,
      as.character(m1demoequip) == "A LITTLE"   ~ 2,
      as.character(m1demoequip) == "SOME"       ~ 3,
      as.character(m1demoequip) == "A LOT"      ~ 4
    ),
    
    m1facilities.2 = case_when(
      as.character(m1facilities) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1facilities) == "NOT AT ALL" ~ 1,
      as.character(m1facilities) == "A LITTLE"   ~ 2,
      as.character(m1facilities) == "SOME"       ~ 3,
      as.character(m1facilities) == "A LOT"      ~ 4
    ),
    
    m1ratio.2 = case_when(
      as.character(m1ratio) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1ratio) == "NOT AT ALL" ~ 1,
      as.character(m1ratio) == "A LITTLE"   ~ 2,
      as.character(m1ratio) == "SOME"       ~ 3,
      as.character(m1ratio) == "A LOT"      ~ 4
    ),
    
    m1planning.2 = case_when(
      as.character(m1planning) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1planning) == "NOT AT ALL" ~ 1,
      as.character(m1planning) == "A LITTLE"   ~ 2,
      as.character(m1planning) == "SOME"       ~ 3,
      as.character(m1planning) == "A LOT"      ~ 4
    ),
    
    m1autonomy.2 = case_when(
      as.character(m1autonomy) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1autonomy) == "NOT AT ALL" ~ 1,
      as.character(m1autonomy) == "A LITTLE"   ~ 2,
      as.character(m1autonomy) == "SOME"       ~ 3,
      as.character(m1autonomy) == "A LOT"      ~ 4
    ),
    
    m1famsupport.2 = case_when(
      as.character(m1famsupport) %in% c("MISSING", "UNIT NON-RESPONSE", "NOT APPLICABLE") ~ NA_real_,
      as.character(m1famsupport) == "NOT AT ALL" ~ 1,
      as.character(m1famsupport) == "A LITTLE"   ~ 2,
      as.character(m1famsupport) == "SOME"       ~ 3,
      as.character(m1famsupport) == "A LOT"      ~ 4
    )
    
  ) %>%
  
  # Keep if either credit metric > 0; treat NA as 0 (FALSE)
  filter(
    s4evratndclg %in% c("YES") & STEM_Major != "ITEM LEGITIMATE SKIP/NA"
  ) %>%
  
  # If w1_vars is a character vector of columns to keep, use all_of()
  select(
    stu_id, Locale:m1famsupport.2, s1frndgrades, s1frndschool:s1frndtalkm, w4w1w2w3stu
  )


##########################################################################################################################
##################################Derive Factor Scores for School Quality Variables#######################################
##########################################################################################################################

## Perceived Barriers to Effective Teaching
##Chronbachs Alpha = 0.87
gddat.imp %>% 
  select(m1techsupprt.2:m1famsupport.2) %>% 
  psych::alpha()

###KMO: Overall MSA =  0.88
gddat.imp %>% 
  select(m1techsupprt.2:m1famsupport.2) %>% 
  psych::KMO()

##Bartlett's test of sphericicty: p < .001
gddat.imp %>% 
  select(m1techsupprt.2:m1famsupport.2) %>% 
  cortest.bartlett()

fa_model.1 <- gddat.imp %>% 
  select(m1techsupprt.2:m1famsupport.2) %>% 
  fa(nfactors = 1, scores = "regression", fm = "ml")

PBET_factor_scores <-  fa_model.1$scores


####Perceived School Disorder
##Chronbachs Alpha = 0.89
gddat.imp %>% 
  select(m1tardy.2:m1unprepprob.2) %>% 
  psych::alpha()

###KMO: Overall MSA =  0.91
gddat.imp %>% 
  select(m1tardy.2:m1unprepprob.2) %>% 
  psych::KMO()

##Bartlett's test of sphericicty: p < .001
gddat.imp %>% 
  select(m1tardy.2:m1unprepprob.2) %>% 
  cortest.bartlett()

fa_model.2 <- gddat.imp %>% 
  select(m1tardy.2:m1unprepprob.2) %>% 
  fa(nfactors = 1, scores = "regression", fm = "ml")

PSD_factor_scores <- fa_model.2$scores

##Extract factor scores from each FA
PBET_factor_scores <- as.data.frame(fa_model.1$scores)
PSD_factor_scores <- as.data.frame(fa_model.2$scores)

##rename the columns
PBET_factor_scores <- PBET_factor_scores %>% 
  rename(PerBarEffTch = ML1)

PSD_factor_scores <- PSD_factor_scores %>% 
  rename(PerSchDisorder = ML1)

##########################################################################################################################
##################################Derive Factor Scores for Peer SC Variables##############################################
##########################################################################################################################

##Peer talk about course taking = s1frndtlkoth + s1frndtalks + s1frndtalkm 
##Peer Talk about Future = s1frndtlkjob + s1frndtlkprb + s1frndtlkclg 
##Peer Academic Orientation = s1frndgrades + s1frndschool + s1frndclass + s1frndclg 


######Chronbach's alpha:

##Peer talk about course taking-Chronbach's alpha = 0.83
gddat.imp %>% 
  select(s1frndtlkoth, s1frndtalks, s1frndtalkm) %>% 
  psych::alpha()

##Peer Talk about Future--Chronbach's alpha = 0.70
gddat.imp %>% 
  select(s1frndtlkjob, s1frndtlkprb, s1frndtlkclg) %>% 
  psych::alpha()


##Chronbach's alpha = 0.57
gddat.imp %>% 
  select(s1frndgrades, s1frndschool, s1frndclass, s1frndclg) %>% 
  psych::alpha()


##########Calcualte three factor solution for all peer SC varibales
fa_model.3  <- gddat.imp %>% 
  select(s1frndgrades:s1frndtalkm) %>% 
  fa(nfactors = 3,
     cor = "tet", ##for binary items
     scores = "regression", 
     fm = "ml")

fa_model.3

Peer_SC_factor_scores <- as.data.frame(fa_model.3$scores)

Peer_SC_factor_scores <- Peer_SC_factor_scores %>% 
  rename(
    Peer_Course_Taking = ML1,
    Peer_Academic_Orient = ML2,
    Peer_Talk_Future = ML3)

###Bind the scores to your data set
dat.new <- cbind(gddat.imp, PBET_factor_scores, PSD_factor_scores, Peer_SC_factor_scores) %>% 
  select(stu_id:mathTach_expect, PerSchDisorder, PerBarEffTch, 
         Peer_Course_Taking:Peer_Talk_Future, s1frndschool:s1frndtalkm, w4w1w2w3stu)

##create scree plot
dat.new %>% 
  select(Peer_Course_Taking:Peer_Talk_Future) %>% 
  psych::scree()

##calculate PCA for single observed peer social capital variable
pca.sc <- dat.new %>% 
  select(Peer_Course_Taking:Peer_Talk_Future) %>% 
  psych::principal(nfactors = 1, scores = TRUE)


peer.sc <- data.frame(pca.sc$scores) %>% 
  rename(Peer_SC = PC1)

dat.new <- cbind(dat.new, peer.sc) 


imputation <- mice::futuremice(data = dat.new, m = 30, maxit = 10, parallelseed = 9204857, print = TRUE)

imps_long <- mice::complete(imputation, include = FALSE, action = "long")

write.csv(imps_long, "CA_STEM_Imputed_Long_SEM.csv")

##Move to STEM_SEM_DATA_PREP.R 
