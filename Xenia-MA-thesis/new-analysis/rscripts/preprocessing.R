# dogwhistle project
# preprocessing.R

# set language to English
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load relevant packages and set background color
library(tidyverse)
library(dplyr)

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

# optional view
# view(ndwlib)

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


# recode, following H&P, the response to the critical question as a four-point scale
# put together prison vs. education program question and feel strongly question as one single question
# On a punitiveness scale, 1 represents people with the most punitive attitude
# 4 represents people with the least punitive attitude
d <- d %>%
  mutate(targetResponse = case_when(
    criticalQuestion == "Building new prisons" & feelStrongly == "I feel very strongly about this." ~ 1,
    criticalQuestion == "Building new prisons" & feelStrongly == "I feel not very strongly about this." ~ 2,
    criticalQuestion == "Education programs" & feelStrongly == "I feel not very strongly about this." ~ 3,
    criticalQuestion == "Education programs" & feelStrongly == "I feel very strongly about this." ~ 4,
    TRUE ~ 666 # Assign NA for any unmatched cases
  ))

d %>% 
  select(participantID,criticalQuestion) %>% 
  unique() %>% 
  group_by(criticalQuestion) %>% 
  summarize(count=n())

# criticalQuestion     count
# 1 Building new prisons    31
# 2 Education programs     109

d %>% 
  select(participantID,targetResponse) %>% 
  unique() %>% 
  group_by(targetResponse) %>% 
  summarize(count=n())

# targetResponse count
# 1              1    16
# 2              2    15
# 3              3    42
# 4              4    67

str(d)

# calculate each participant's trans stereotype index (following H&P's black stereotype index from 5 to 35) 
# 5 means the person doesn't accept the stereotypes at all
# 35 means the person accepts the stereotypes
# the higher the score, the more the person accepts Xenia's proposed stereotypes
# a shortcoming that must be considered is that the stereotypes are not checked against real data 
# (are they even stereotypes?)
d <- d %>%
  mutate(transStereotypeIndex = rowSums(select(., 
                                            transConfused,
                                            transMentallyIll,
                                            transDangerous,
                                            transFrauds,
                                            transUnnatural)))

# calculate each participant's cis stereotype index (following H&P's white stereotype index from 5 to 35) 
# doesn't really allow for insights but can be used as a control score to see the difference between cis and trans
# the difference between the cis and trans stereotypes score shows how differently the person sees transgender people compared to cisgender people
d <- d %>%
  mutate(cisStereotypeIndex = rowSums(select(., 
                                               cisConfused,
                                               cisMentallyIll,
                                               cisDangerous,
                                               cisFrauds,
                                               cisUnnatural)))

# calculate each participant's gender fairness index (following H&P's racial fairness index from 4 to 23)
# by summing up the relevant columns after changing them to the appropriate values
# 4 means the person believes that transgender people are treated very fairly
# 23 means the person believes that transgender people are treated very unfairly
# the higher the score, the more aware the person is of the unfairness towards transgender people
# lack of knowledge/awareness about these topics can also lead to low score
str(d$genderFairnessApartment) #chr change yes/no to 1/2
d <- d %>%
  mutate(genderFairnessApartmentNum = case_when(
    genderFairnessApartment == "Yes" ~ 1,
    genderFairnessApartment == "No" ~ 2,
    TRUE ~ 666))

str(d$genderFairnessSports) #num change 7-1 to 1-7
table(d$genderFairnessSports)
d$genderFairnessSports = 8-d$genderFairnessSports
table(d$genderFairnessSports)

str(d$genderFairnessStreetHarassment) #num change 7-1 to 1-7
d$genderFairnessStreetHarassment = 8-d$genderFairnessStreetHarassment

str(d$genderFairnessBullying) #num change 7-1 to 1-7
d$genderFairnessBullying = 8-d$genderFairnessBullying

d <- d %>%
  mutate(genderFairnessIndex = rowSums(select(., 
                                               genderFairnessApartmentNum,
                                               genderFairnessSports,
                                               genderFairnessStreetHarassment,
                                               genderFairnessBullying)))

# calculate general fairness index (following H&P's general fairness score from 2 to 8)
# 2 means the person believes that people are treated very fairly and equally
# 8 means the person believes that people are treated very unfairly and unequally
# the higher the score, the more aware the person is of the unfairness in society
str(d$generalFairnessEducation)
d <- d %>%
  mutate(generalFairnessEducationNum = case_when(
    generalFairnessEducation == "Strongly agree" ~ 1,
    generalFairnessEducation == "Somewhat agree" ~ 2,
    generalFairnessEducation == "Somewhat disagree" ~ 3,
    generalFairnessEducation == "Strongly disagreee" ~ 4,
    TRUE ~ 666))
table(d$generalFairnessEducation)
table(d$generalFairnessEducationNum)

str(d$generalFairnessHealthcare)
d <- d %>%
  mutate(generalFairnessHealthcareNum = case_when(
    generalFairnessHealthcare == "Strongly agree" ~ 1,
    generalFairnessHealthcare == "Somewhat agree" ~ 2,
    generalFairnessHealthcare == "Somewhat disagree" ~ 3,
    generalFairnessHealthcare == "Strongly disagreee" ~ 4,
    TRUE ~ 666))
table(d$generalFairnessHealthcare)
table(d$generalFairnessHealthcareNum)

d <- d %>%
  mutate(generalFairnessIndex = rowSums(select(., 
                                              generalFairnessEducationNum,
                                              generalFairnessHealthcareNum)))
table(d$generalFairnessIndex)

# calculate fear of trans people score (following H&P's fear of crime score from 2 to 6)
# here there's a difference to H&P
# the impression that the number of trans people increased doesn't mean that one also fears trans people
# whereas H&P's impression that crime has increased means that the participant fears crime
# so it might be better to not call this score the "fear of trans people" score, but rather
# "awareness of trans people and issues"
# there is no clear correlation between the observed increase/decrease of trans people and the considered importance of transgender discrimination
# therefore it is not possible to create a meaningful score out of the two questions
# the scales diverge in opposite directions (fearNumberTrans = 3 -> very aware, fearComparisonProblems = 3 -> very unaware)
# maybe we should change the order of fearNumberTrans: increased = 1, decreased = 3


# A perceived increase of the number of trans people indicates more awareness
# 3 means the person is highly aware of trans people and issues
# 1 means the person is highly unaware of trans people and issues
# the higher the score, the more aware the person is of trans people and issues
str(d$fearNumberTrans) 
d <- d %>%
  mutate(fearNumberTransNum = case_when(
    fearNumberTrans == "The number of trans people increased" ~ 3,
    fearNumberTrans == "The number of trans people stayed about the same" ~ 2,
    fearNumberTrans == "The number of trans people decreased" ~ 1,
    TRUE ~ 666))
table(d$fearNumberTrans)
table(d$fearNumberTransNum)
# 1   2   3 
# 2  27 111 

# considering discrimination of transgender people as an important problem indicates more awareness
# 1 means the person is highly aware of trans people and issues
# 3 means the person is highly unaware of trans people and issues
# the higher the score, the more unaware the person is of trans people and issues
# But compared to the other problems mentioned (education, taxes and the environment), 
# saying discrimination of transgender people is less important than other problems doesn't mean they are unaware of the problem!

str(d$fearComparisonProblems)
d <- d %>%
  mutate(fearComparisonProblemsNum = case_when(
    fearComparisonProblems == "It is the most important problem" ~ 1,
    fearComparisonProblems == "It is no more important than other problems" ~ 2,
    fearComparisonProblems == "It is less important than other problems" ~ 3,
    TRUE ~ 666))
table(d$fearComparisonProblems)
table(d$fearComparisonProblemsNum)
#  1  2  3 
#  6 75 59

d <- d %>%
  mutate(fearOfTransPeopleIndex = rowSums(select(., 
                                            fearNumberTransNum,
                                            fearComparisonProblemsNum)))
table(d$fearOfTransPeopleIndex)
# 3  4  5  6 
# 2 26 60 52

d$awarenessOfTransPeopleAndIssues = d$fearOfTransPeopleIndex

# calculate equality index (following H&P's equality index from 2 to 8)
# 2 means the person doesn't care about fairness and equality at all
# 8 means the person cares about fairness and equality very much
# the higher this index, the more the person cares about fairness and equality
# equalityEqualChance is ambiguous: strongly disagreeing can either mean the person doesn't think it's a big problem 
# or the person doesn't think inequality exists

str(d$equalityEqualChance) #chr change to num 4-1
d <- d %>%
  mutate(equalityEqualChanceNum = case_when(
    equalityEqualChance == "Strongly agree" ~ 4,
    equalityEqualChance == "Somewhat agree" ~ 3,
    equalityEqualChance == "Somewhat disagree" ~ 2,
    equalityEqualChance == "Strongly disagree" ~ 1,
    TRUE ~ 666))
table(d$equalityEqualChance)
table(d$equalityEqualChanceNum)
# 1  2  3  4 
# 25 23 38 54 

str(d$equalityShouldntWorry) #chr change to num 1-4
d <- d %>%
  mutate(equalityShouldntWorryNum = case_when(
    equalityShouldntWorry == "Strongly agree" ~ 1,
    equalityShouldntWorry == "Somewhat agree" ~ 2,
    equalityShouldntWorry == "Somewhat disagree" ~ 3,
    equalityShouldntWorry == "Strongly disagree" ~ 4,
    TRUE ~ 666))
table(d$equalityShouldntWorry)
table(d$equalityShouldntWorryNum)
# 1  2  3  4 
# 18 23 37 62 

d <- d %>%
  mutate(equalityIndex = rowSums(select(., 
                                        equalityEqualChanceNum,
                                        equalityShouldntWorryNum)))
table(d$equalityIndex)
#  2  3  4  5  6  7  8 
# 11 13 11 19 22 20 44 

# calculate transAttitudeIndex (not in H&P, from 11 to 64)
# the transAttitudeIndex is the sum of transStereotypesIndex, genderFairnessIndex, and fearOfTransPeopleIndex
# 11 means the person is not aware at all
# 64 means the person is very aware
# the higher this index, the more aware the person is about challenges that transgender people face
# Because of the problems with the previous indices, we need to treat this index with caution as well
d <- d %>%
  mutate(transAttitudeIndex = rowSums(select(., 
                                        transStereotypeIndex,
                                        genderFairnessIndex,
                                        fearOfTransPeopleIndex)))
table(d$transAttitudeIndex)

# calculate attitudeControlIndex (not in H&P, from 4 to 16)
# the attitudeControlIndex is the sum of generalFairnessIndex and equalityIndex
# 4 means the person is not aware of inequality and doesn't care about general fairness
# 16 means the person is aware of inequality and cares about general fairness
# the higher the score, the more the person is aware of inequality and cares about general fairness
d <- d %>%
  mutate(attitudeControlIndex = rowSums(select(., 
                                             generalFairnessIndex,
                                             equalityIndex)))
table(d$attitudeControlIndex)

write_csv(d, file="../data/d.csv")

# the following metrics are to be discussed ----


# View the first 20 rows to check the values in person-gender column
head(d[, 26], 20)

#count how many men and women participants we have
# Count the number of men and women in the dataset
d %>%
  group_by(`participantGender`) %>%
  summarise(count = n()) # 17 men, 18 women

#assigning numeric value to gender: Man = 0, Woman = 1
temporary_data1 <- temporary_data1 %>%
  mutate(Man0Woman1 = ifelse(`person-gender`== "Man", 0, 1))

# Calculate the point-biserial correlation between Gender and TargetStimulus
correlation_results_gender <- cor.test(
  temporary_data1$Prison0Education1,
  temporary_data1$Man0Woman1,
  method = "pearson"
)

