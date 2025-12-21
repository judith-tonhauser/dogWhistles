# dogwhistle project
# preprocessing.R

# set language to English
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load relevant packages
library(tidyverse)

# load the data
ndwlib = read_csv(file="../data/ndwlib.csv") 
nrow(ndwlib) #35
names(ndwlib)
ndwcons = read_csv(file="../data/ndwcons.csv") 
nrow(ndwcons) #35
names(ndwcons)
dwlib = read_csv(file="../data/dwlib.csv") 
nrow(dwlib) #35
dwcons = read_csv(file="../data/dwcons.csv") 
nrow(dwcons) #35

# add information about the condition to the files
ndwlib = ndwlib %>%
  mutate(dw = "no") %>%
  mutate(preregistered = "liberal") %>%
  mutate("criticalQuestion" = `Some people want to increase spending for new prisons to lock up domestic abusers. Others would rather spend this money on education programs for women, in order to help them identify early warning signs of abusive partnerships and to prevent domestic abuse.\n\nWhat about you? If you had to choose, would you rather see this money spent on building new prisons or education programs?`) %>%
  select(-c(`Some people want to increase spending for new prisons to lock up domestic abusers. Others would rather spend this money on education programs for women, in order to help them identify early warning signs of abusive partnerships and to prevent domestic abuse.\n\nWhat about you? If you had to choose, would you rather see this money spent on building new prisons or education programs?`))

ndwcons = ndwcons %>%
  mutate(dw = "no") %>%
  mutate(preregistered = "cons") %>%
  mutate("criticalQuestion" = `Some people want to increase spending for new prisons to lock up domestic abusers. Others would rather spend this money on education programs for women, in order to help them identify early warning signs of abusive partnerships and to prevent domestic abuse.\n\nWhat about you? If you had to choose, would you rather see this money spent on building new prisons or education programs?`) %>%
  select(-c(`Some people want to increase spending for new prisons to lock up domestic abusers. Others would rather spend this money on education programs for women, in order to help them identify early warning signs of abusive partnerships and to prevent domestic abuse.\n\nWhat about you? If you had to choose, would you rather see this money spent on building new prisons or education programs?`))

dwlib = dwlib %>%
  mutate(dw = "yes") %>%
  mutate(preregistered = "liberal") %>%
  mutate("criticalQuestion" = `Some people want to increase spending for new prisons to lock up domestic abusers.  Others would rather spend this money on education programs for adult human females, in order to help them identify early warning signs of abusive partnerships and to prevent domestic abuse.\n\nWhat about you? If you had to choose, would you rather see this money spent on building new prisons or education programs?`) %>%
  select(-c(`Some people want to increase spending for new prisons to lock up domestic abusers.  Others would rather spend this money on education programs for adult human females, in order to help them identify early warning signs of abusive partnerships and to prevent domestic abuse.\n\nWhat about you? If you had to choose, would you rather see this money spent on building new prisons or education programs?`))

dwcons = dwcons %>%
  mutate(dw = "yes") %>%
  mutate(preregistered = "cons") %>%
  mutate("criticalQuestion" = `Some people want to increase spending for new prisons to lock up domestic abusers.  Others would rather spend this money on education programs for adult human females, in order to help them identify early warning signs of abusive partnerships and to prevent domestic abuse.\n\nWhat about you? If you had to choose, would you rather see this money spent on building new prisons or education programs?`) %>%
  select(-c(`Some people want to increase spending for new prisons to lock up domestic abusers.  Others would rather spend this money on education programs for adult human females, in order to help them identify early warning signs of abusive partnerships and to prevent domestic abuse.\n\nWhat about you? If you had to choose, would you rather see this money spent on building new prisons or education programs?`))

# bind the data
d = rbind(ndwlib,ndwcons,dwlib,dwcons)
nrow(d) #140

# replace timestamp with participant ID
colnames(d)[1] <- "participantID" # rename the column
d$participantID <- match(d$participantID, unique(d$participantID)) # assign unique IDs
table(d$participantID)

#view(d)

# rename the columns
colnames(d)[2] <- "feelStrongly"
colnames(d)[3] <- "transConfused"
colnames(d)[4] <- "transMentallyIll"
colnames(d)[5] <- "transDangerous"
colnames(d)[6] <- "transFrauds"
colnames(d)[7] <- "transUnnatural"
colnames(d)[8] <- "cisConfused"
colnames(d)[9] <- "cisMentallyIll"
colnames(d)[10] <- "cisDangerous"
colnames(d)[11] <- "cisFrauds"
colnames(d)[12] <- "cisUnnatural"
colnames(d)[13] <- "genderFairnessApartment"
colnames(d)[14] <- "genderFairnessSports"
colnames(d)[15] <- "genderFairnessStreetHarassment"
colnames(d)[16] <- "genderFairnessBullying"
colnames(d)[17] <- "generalFairnessEducation"
colnames(d)[18] <- "generalFairnessHealthcare"
colnames(d)[19] <- "fearNumberTrans"
colnames(d)[20] <- "fearComparisonProblems"
colnames(d)[21] <- "equalityEqualChance"
colnames(d)[22] <- "equalityShouldntWorry"
colnames(d)[23] <- "participantAge"
colnames(d)[24] <- "participantSexualOrientation"
colnames(d)[25] <- "participantGender"
colnames(d)[26] <- "participantCisOrTrans"

# column 28: preregistered as conservative or liberal

# dependent variable ----

# recode, following H&P, the response to the critical question as a four-point scale
# put together prison vs. education program question and feel strongly question as one single question
# On a punitiveness scale, 4 represents people with the most punitive attitude
# 1 represents people with the least punitive attitude
d <- d %>%
  mutate(targetResponse = case_when(
    criticalQuestion == "Building new prisons" & feelStrongly == "I feel very strongly about this." ~ 4,
    criticalQuestion == "Building new prisons" & feelStrongly == "I feel not very strongly about this." ~ 3,
    criticalQuestion == "Education programs" & feelStrongly == "I feel not very strongly about this." ~ 2,
    criticalQuestion == "Education programs" & feelStrongly == "I feel very strongly about this." ~ 1,
    TRUE ~ 666 # Assign NA for any unmatched cases
  ))
table(d$targetResponse)
#1  2  3  4 
#67 42 15 16

# how many participants in each of the four target responses?
d %>% 
  select(participantID,targetResponse) %>% 
  unique() %>% 
  group_by(targetResponse) %>% 
  summarize(count=n())

# targetResponse count
# 1              1    67
# 2              2    42
# 3              3    15
# 4              4    16
# the majority of the participants are towards less punitive (1 and 2)

# predictor variable transStereotypeIndex ----

# calculate each participant's trans stereotype index (following H&P's black stereotype index from 5 to 35) 
# 5 means the person doesn't accept the negative stereotypes about trans people at all
# 35 means the person accepts the negative stereotypes about trans people

d <- d %>%
  mutate(transStereotypeIndex = rowSums(select(., 
                                            transConfused,
                                            transMentallyIll,
                                            transDangerous,
                                            transFrauds,
                                            transUnnatural)))
table(d$transStereotypeIndex) #5 to 35

#view(d)
# predictor variable genderFairnessIndex ----

# calculate each participant's gender fairness index (following H&P's racial fairness index from 4 to 23)
# by summing up gender fairness apartment, sports, street harassment, and bullying after changing them to the appropriate values
# 4 means the person believes that transgender people are treated very unfairly
# 23 means the person believes that transgender people are treated very fairly
str(d$genderFairnessApartment) #chr change yes/no to 1/2
d <- d %>%
  mutate(genderFairnessApartmentNum = case_when(
    genderFairnessApartment == "Yes" ~ 1, # Yes, transgender people are treated less fairly than cisgender people
    genderFairnessApartment == "No" ~ 2, # No
    TRUE ~ 666))

# genderFairnessSports: Transgender experience discrimination in sports more than cisgender
# how serious is this problem?
# numbers from 1 (not a problem) to 7 (serious problem)
str(d$genderFairnessSports) 
table(d$genderFairnessSports)
# reverse the coding so that 1 (serious problem) to 7 (not a problem)
d$genderFairnessSports = 8-d$genderFairnessSports
table(d$genderFairnessSports)

# genderFairnessStreetHarassment: Transgender experience street harassment more than cisgender
# how serious is this problem?
# numbers from 1 (not a problem) to 7 (serious problem)
str(d$genderFairnessStreetHarassment) 
# reverse the coding so that 1 (serious problem) to 7 (not a problem)
d$genderFairnessStreetHarassment = 8-d$genderFairnessStreetHarassment

# genderFairnessBullying: Transgender youth experience more bullying than cisgender youth
str(d$genderFairnessBullying) 
# reverse the coding so that 1 (serious problem) to 7 (not a problem)
d$genderFairnessBullying = 8-d$genderFairnessBullying

d <- d %>%
  mutate(genderFairnessIndex = rowSums(select(., 
                                               genderFairnessApartmentNum,
                                               genderFairnessSports,
                                               genderFairnessStreetHarassment,
                                               genderFairnessBullying)))

# control variables ----

#### cisStereotypeIndex ----

# calculate each participant's cis stereotype index (following H&P's white stereotype index from 5 to 35) 
# 5 means the person doesn't accept the negative stereotypes about cis people at all
# 35 means the person accepts the negative stereotypes about cis people
# can be used as a control score to see the difference between cis and trans
# the difference between the cis and trans stereotypes score shows how differently the person sees transgender people compared to cisgender people
d <- d %>%
  mutate(cisStereotypeIndex = rowSums(select(., 
                                             cisConfused,
                                             cisMentallyIll,
                                             cisDangerous,
                                             cisFrauds,
                                             cisUnnatural)))

#### generalFairnessIndex ----

# calculate general fairness index (following H&P's general fairness score from 2 to 8)
# by summing up general fairness education and healthcare
# 2 means the person believes that people are treated very unfairly and unequally
# 8 means the person believes that people are treated very fairly and equally
str(d$generalFairnessEducation)
d <- d %>%
  mutate(generalFairnessEducationNum = case_when(
    generalFairnessEducation == "Strongly agree" ~ 4,
    generalFairnessEducation == "Somewhat agree" ~ 3,
    generalFairnessEducation == "Somewhat disagree" ~ 2,
    generalFairnessEducation == "Strongly disagreee" ~ 1,
    TRUE ~ 666))
table(d$generalFairnessEducation)
table(d$generalFairnessEducationNum)

str(d$generalFairnessHealthcare)
d <- d %>%
  mutate(generalFairnessHealthcareNum = case_when(
    generalFairnessHealthcare == "Strongly agree" ~ 4,
    generalFairnessHealthcare == "Somewhat agree" ~ 3,
    generalFairnessHealthcare == "Somewhat disagree" ~ 2,
    generalFairnessHealthcare == "Strongly disagreee" ~ 1,
    TRUE ~ 666))
table(d$generalFairnessHealthcare)
table(d$generalFairnessHealthcareNum)

d <- d %>%
  mutate(generalFairnessIndex = rowSums(select(., 
                                              generalFairnessEducationNum,
                                              generalFairnessHealthcareNum)))
table(d$generalFairnessIndex)

#### moreTransPeopleIndex ----

# the data under fearNumberTrans and fearComparisonProblems were originally collected to calculate
# a fearOfTransPeopleIndex, following H&P's fear of crime score from 2 to 6

# however, the impression that the number of trans people increased doesn't mean that one also fears trans people
# and the impression that discrimination of transgender people is less important than other problems doesn't 
# mean the participants are unaware of the problem that trans people face

# so we decided to not sum up fearNumberTrans and fearComparisonProblems to calculate a fearOfTransPeopleIndex
# but are using these two numbers separately

# moreTransPeopleIndex
# 1 means the person is highly aware of trans people and issues
# 3 means the person is highly unaware of trans people and issues
# the higher the score, the less aware the person is of trans people and issues
str(d$fearNumberTrans) 
d <- d %>%
  mutate(moreTransPeopleIndex = case_when(
    fearNumberTrans == "The number of trans people increased" ~ 1,
    fearNumberTrans == "The number of trans people stayed about the same" ~ 2,
    fearNumberTrans == "The number of trans people decreased" ~ 3,
    TRUE ~ 666))
table(d$moreTransPeopleIndex)
# 1   2   3 
# 111  27   2
# most participants believe that there are now more trans people

#### fearComparisonProblemsIndex ----

# 1 means the person is highly aware of trans people and issues
# 3 means the person is highly unaware of trans people and issues
# the higher the score, the more unaware the person is of trans people and issues

str(d$fearComparisonProblems)
d <- d %>%
  mutate(fearComparisonProblemsIndex = case_when(
    fearComparisonProblems == "It is the most important problem" ~ 1,
    fearComparisonProblems == "It is no more important than other problems" ~ 2,
    fearComparisonProblems == "It is less important than other problems" ~ 3,
    TRUE ~ 666))
table(d$fearComparisonProblems)
table(d$fearComparisonProblemsIndex)
#  1  2  3 
#  6 75 59
# most participants assume that the issues trans people face are no more important than other problems
# a lot assume that the issues trans people face are less important than other problems

#### equalityIndex ----

# calculate equality index (following H&P's equality index from 2 to 8)
# by summing up qualityEqualChance and equalityShouldntWorry
# 2 means the person cares about fairness and equality very much
# 8 means the person doesn't care about fairness and equality at all
# the higher this index, the more the person cares about fairness and equality
# equalityEqualChance is ambiguous: strongly disagreeing can either mean the person doesn't think it's a big problem 
# or the person doesn't think inequality exists

str(d$equalityEqualChance) #chr change to num 4-1
d <- d %>%
  mutate(equalityEqualChanceNum = case_when(
    equalityEqualChance == "Strongly agree" ~ 1,
    equalityEqualChance == "Somewhat agree" ~ 2,
    equalityEqualChance == "Somewhat disagree" ~ 3,
    equalityEqualChance == "Strongly disagree" ~ 4,
    TRUE ~ 666))
table(d$equalityEqualChance)
table(d$equalityEqualChanceNum)

str(d$equalityShouldntWorry) #chr change to num 1-4
d <- d %>%
  mutate(equalityShouldntWorryNum = case_when(
    equalityShouldntWorry == "Strongly agree" ~ 4,
    equalityShouldntWorry == "Somewhat agree" ~ 3,
    equalityShouldntWorry == "Somewhat disagree" ~ 2,
    equalityShouldntWorry == "Strongly disagree" ~ 1,
    TRUE ~ 666))
table(d$equalityShouldntWorry)
table(d$equalityShouldntWorryNum)

d <- d %>%
  mutate(equalityIndex = rowSums(select(., 
                                        equalityEqualChanceNum,
                                        equalityShouldntWorryNum)))
table(d$equalityIndex)
# 2  3  4  5  6  7  8 
# 44 20 22 19 11 13 11

#### additional control variables ----

# H&P: ideology 
# We know whether participants are liberal or conservative
table(d$preregistered)
# cons liberal 
# 70      70

# H&P: party identification 
# not collected

# H&P: education
# not collected

# H&P: gender
table(d$participantGender)
# Man Woman 
# 71    69

str(d$participantGender)
d = d %>%
  mutate(participantGenderNum = case_when(
    participantGender == "Woman" ~ 0,
    participantGender == "Man" ~ 1,
    TRUE ~ 666))
table(d$participantGenderNum)

# H&P: age
table(d$participantAge)
str(d$participantAge)

# H&P: income
# not collected

# H&P: south
# not collected (region is not justifiable)

# SexualOrientation
table(d$participantSexualOrientation)

# Bisexual 
# 18 
# Gay or Lesbian 
# 6 
# Other 
# 2 
# Queer 
# 1 
# Straight or heterosexual 
# 113
# most are straight or hetero

# CisOrTrans
table(d$participantCisOrTrans) # all are cisgender

# save the preprocessed data
write_csv(d, file="../data/d.csv")

# information about participants ----

length(unique(d$participantID)) #140 participants

#gender distribution
d %>%
  group_by(`participantGender`) %>%
  summarise(count = n()) # 71 men, 69 women

# age range
table(d$participantAge) #21-83

table(d$participantCisOrTrans) # all are cis

# SexualOrientation
table(d$participantSexualOrientation)


