# dogwhistle project
# preprocessing.R

# set language to English
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load relevant packages and set background color
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

# # convert all columns to character so that we can pivot long
# d <- d %>%
#   mutate(across(1:29, as.character)) 
# 
# # reorder the columns in preparation for pivoting
# d = d[,c(1,29,2:ncol(d))]
# names(d)
#            
# # pivot the data
# d <- d %>%
#   pivot_longer(
#     cols = 4:23,
#     names_to = "question",
#     values_to = "response"
#   )


# recode, following H&P, the response to the critical question as a four-point scale
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

# # Convert trans-stereotypes columns to numeric
# data1 <- data1 %>%
#   mutate(across(c(`trans-stereotypes-confused`, 
#                   `trans-stereotypes-mentally-ill`, 
#                   `trans-stereotypes-dangerous`, 
#                   `trans-stereotypes-frauds`, 
#                   `trans-stereotypes-unnatural`), 
#                 as.numeric))


# calculate each participant's trans stereotype index (following H&P's black stereotype index from 5 to 35) 
d <- d %>%
  mutate(transStereotypeIndex = rowSums(select(., 
                                            transConfused,
                                            transMentallyIll,
                                            transDangerous,
                                            transFrauds,
                                            transUnnatural)))

# calculate each participant's cis stereotype index (following H&P's white stereotype index from 5 to 35) 
d <- d %>%
  mutate(cisStereotypeIndex = rowSums(select(., 
                                               cisConfused,
                                               cisMentallyIll,
                                               cisDangerous,
                                               cisFrauds,
                                               cisUnnatural)))




# # Apply scoring and sum the responses into a new column
# data1 <- data1 %>%
#   mutate(across(`cis-stereotypes-confused`:`cis-stereotypes-unnatural`, as.numeric)) %>%
#   mutate(cisStereotypesControl = rowSums(select(., 
#                                                 `cis-stereotypes-confused`, 
#                                                 `cis-stereotypes-mentally-ill`, 
#                                                 `cis-stereotypes-dangerous`, 
#                                                 `cis-stereotypes-frauds`, 
#                                                 `cis-stereotypes-unnatural`), na.rm = TRUE))
# # Rename the column trans_stereotypes to transStereotypesScore
# data1 <- data1 %>%
#   rename(transStereotypesScore = trans_stereotypes)

# calculate each participant's gender fairness index (following H&P's racial fairness index from 4 to 23)
# by summing up the relevant columns after changing them to the appropriate values
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
# the higher this index, the more they care about fairness and equality

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
d <- d %>%
  mutate(transAttitudeIndex = rowSums(select(., 
                                        transStereotypeIndex,
                                        genderFairnessIndex,
                                        fearOfTransPeopleIndex)))
table(d$transAttitudeIndex)

# calculate attitudeControlIndex (not in H&P, from 4 to 16)
d <- d %>%
  mutate(attitudeControlIndex = rowSums(select(., 
                                             generalFairnessIndex,
                                             equalityIndex)))
table(d$attitudeControlIndex)

write_csv(d, file="../data/d.csv")

# this is as far as we went through Xenia's script ----

proportion = d %>%
  group_by(transStereotypeIndex) %>%
  summarize(n = n()) %>%
  mutate(prop = n / nrow(d))


  mutate(short_trigger = fct_rev(fct_reorder(as.factor(short_trigger),Mean_proj)))
proj.means

mtcars %>%
  group_by(am, gear) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))


# here's some sample code for a graph

ggplot(d, aes(transStereotypeIndex)) +
  geom_histogram()

ggplot(d, aes(x=targetResponse, fill=dw)) +
  geom_histogram( color='#e9ecef', alpha=0.6, position='identity')

ggplot(data=d, aes(x=transStereotypeIndex, y=targetResponse)) +
  geom_point(aes(colour = factor(dw) )) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  ylab("Target response") +
  xlab("Trans Stereotype Index") 

ggplot(data=d, aes(x=genderFairnessIndex, y=targetResponse)) +
  geom_violin() +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  ylab("Target response") +
  xlab("Gender Fairness")


# really end code here

# Combine and code the 'fearOfTransPeopleScore' column
data1 <- data1 %>%
  mutate(
    # Assign points to 'fear-number-of-trans-people'
    fear_number_of_trans_people_score = case_when(
      `fear-number-of-trans-people` == "The number of trans people increased" ~ 3,
      `fear-number-of-trans-people` == "The number of trans people stayed about the same" ~ 2,
      `fear-number-of-trans-people` == "The number of trans people decreased" ~ 1,
      TRUE ~ NA_real_
    ),
    
# Assign points to 'fear-all-problems'
fear_all_problems_score = case_when(
      `fear-all-problems` == "It is no more important than other problems" ~ 2,
      `fear-all-problems` == "It is the most important problem" ~ 1,
      `fear-all-problems` == "It is less important than other problems" ~ 3,
      TRUE ~ NA_real_
    ),
    
# Combine the two scores into 'fearOfTransPeopleScore'
fearOfTransPeopleScore = fear_number_of_trans_people_score + fear_all_problems_score
  )


# Combine and code the 'EqualityScore' column
data1 <- data1 %>%
  mutate(
    # Assign points to 'equality-equal-chance'
    equality_equal_chance_score = case_when(
      `equality-equal-chance` == "Strongly agree" ~ 4,
      `equality-equal-chance` == "Somewhat agree" ~ 3,
      `equality-equal-chance` == "Somewhat disagree" ~ 2,
      `equality-equal-chance` == "Strongly disagree" ~ 1,
      TRUE ~ NA_real_
    ),
    
# Assign points to 'equality-shouldnt-worry'
equality_shouldnt_worry_score = case_when(
      `equality-shouldnt-worry` == "Strongly agree" ~ 1,
      `equality-shouldnt-worry` == "Somewhat agree" ~ 2,
      `equality-shouldnt-worry` == "Somewhat disagree" ~ 3,
      `equality-shouldnt-worry` == "Strongly disagree" ~ 4,
      TRUE ~ NA_real_
    ),
    
# Combine the two scores into 'EqualityScore'
EqualityScore = equality_equal_chance_score + equality_shouldnt_worry_score
  )


# View the first 20 rows to check the values in person-gender column
head(data1[, 26], 20)

#count how many men and women participants we have
library(dplyr)

# Count the number of men and women in the dataset
data1 %>%
  group_by(`person-gender`) %>%
  summarise(count = n()) # 17 men, 18 women

# Removing unwanted columns from the dataset
data1 <- data1 %>%
  select(-fear_number_of_trans_people_score,
         -fear_all_problems_score,
         -equality_equal_chance_score,
         -equality_shouldnt_worry_score)


# Create the new column 'transAttitudesScore' by summing 'genderFairnessScore' and 'transStereotypesScore'
data1 <- data1 %>%
  mutate(transAttitudesScore = genderFairnessScore + transStereotypesScore + fearOfTransPeopleScore)
# the higher the score, the more transphobic the person is!

# Make a new column for the score of all controls together
data1 <- data1 %>%
  mutate(
    ControlsScore = generalFairnessScore + EqualityScore)
# the higher the score, the more they care about fairness and equality

# Create a new column to assign participants to groups based on targetStimuli
data1 <- data1%>%
  mutate(
    TargetStimuliGroup = case_when(
      targetStimuli %in% c(1, 2) ~ "Prison",
      targetStimuli %in% c(3, 4) ~ "Education",
      TRUE ~ NA_character_ # Handles any unexpected values
    )
  )

# View the updated dataset
head(data1)


# Filter data for trans attitudes and controls of the group who chose prison
prison_group_scores <- data1 %>%
  filter(TargetStimuliGroup == "Prison") %>%
  select(participantID, transAttitudesScore, ControlsScore)

# View the table of scores for the TargetStimuli_Prison group
print(prison_group_scores)


# Filter data for tans attitudes and controls of the group who chose education programs
education_group_scores <- data1 %>%
  filter(TargetStimuliGroup == "Education") %>%
  select(participantID, transAttitudesScore, ControlsScore)

# View the table of scores for the TargetStimuli_Education group
print(education_group_scores)


# Average scores for both groups for transAttitudesScore and for ControlsScore
# Filter for TargetStimuli_Prison group
prison_group <- data1 %>% filter(TargetStimuliGroup == "Prison")

# Calculate averages for Prison group
avg_prison <- prison_group %>%
  summarise(
    avg_transAttitudesScore = mean(transAttitudesScore, na.rm = TRUE),
    avg_ControlsScore = mean(ControlsScore, na.rm = TRUE)
  )

cat(
  "Average Trans Attitudes Score (Prison Group):", avg_prison$avg_transAttitudesScore, "\n",
  "Average Controls Score (Prison Group):", avg_prison$avg_ControlsScore, "\n"
) # average transAttitudesScore in Prison group: 31.3 average ControlsScore: 13


# Filter for Education group
education_group <- data1 %>% filter(TargetStimuliGroup == "Education")

# Calculate averages for Education group
avg_education <- education_group %>%
  summarise(
    avg_transAttitudesScore = mean(transAttitudesScore, na.rm = TRUE),
    avg_ControlsScore = mean(ControlsScore, na.rm = TRUE)
  )

cat(
  "Average Trans Attitudes Score (Education Group):", avg_education$avg_transAttitudesScore, "\n",
  "Average Controls Score (Education Group):", avg_education$avg_ControlsScore, "\n"
) #transAttitudesScore average: 22.7, ControlsScoreAverage: 13.2

# Create a new column for the difference between transStereotypesScore and cisStereotypesScore
data1 <- data1 %>%
  mutate(transVSCisDifference = transStereotypesScore - cisStereotypesControl)

# View the updated dataset
head(data1)

summary(data1$transStereotypesScore)
summary(data1$cisStereotypesControl)
nrow(prison_group)
data1 <- data1 %>%
  mutate(
    transVSCisDifference = ifelse(is.na(transStereotypesScore) | is.na(cisStereotypesControl), 
                                  NA, transStereotypesScore - cisStereotypesControl)
  )

# Recalculate average for transVSCisDifference
prison_group <- data1 %>% filter(TargetStimuliGroup == "Prison")
education_group <- data1 %>% filter(TargetStimuliGroup == "Education")

avg_prison <- prison_group %>%
  summarise(avg_transVSCisDifference = mean(transVSCisDifference, na.rm = TRUE))

avg_education <- education_group %>%
  summarise(avg_transVSCisDifference = mean(transVSCisDifference, na.rm = TRUE))

cat(
  "Average transVSCisDifference (Prison Group):", avg_prison$avg_transVSCisDifference, "\n",
  "Average transVSCisDifference (Education Group):", avg_education$avg_transVSCisDifference, "\n"
) # Prison group average transVSCisDifference: 5.7
  # Education group average transVSCisDifference: 1.6


# Create a new dataset with only the selected columns
selected_columns <- c(
  "participantID",
  "TargetStimuliGroup",
  "targetStimuli",
  "transStereotypesScore",
  "cisStereotypesControl",
  "transAttitudesScore",
  "transVSCisDifference",
  "ControlsScore",
  "person-age",
  "person-sexual-orientation",
  "person-gender",
  "person-cis-trans"
)

# Select the columns and create a new dataset
temporary_data1 <- data1[, selected_columns]

# View the first few rows to check the data
head(temporary_data1)

# Create a new column transAttitudes based on the conditions
temporary_data1 <- temporary_data1 %>%
mutate(transAttitudes = if_else(transAttitudesScore <= 36, "positive", "negative"))

# Create a new column ControlsAttitudes based on ControlsScore
temporary_data1 <- temporary_data1 %>%
  mutate(ControlsAttitudes = ifelse(ControlsScore <= 9, "negative", "positive"))
# Add the PoliticalOrientation column and set it to "Liberal" for all rows
temporary_data1 <- temporary_data1 %>%
  mutate(person_PoliticalOrientation = "Liberal")


# Rearranging columns in the specified order
temporary_data1 <- temporary_data1 %>%
  select(
    "participantID",
    "TargetStimuliGroup", 
    "targetStimuli",
    "transStereotypesScore",
    "transAttitudesScore",
    "transAttitudes",
    "cisStereotypesControl",
    "transVSCisDifference",
    "ControlsScore",
    "ControlsAttitudes",
    "person-age",
    "person_PoliticalOrientation",
    "person-sexual-orientation",
    "person-gender",
    "person-cis-trans"
  )

# Filter participants with "positive" in transAttitudes
positive_trans_attitudes <- temporary_data1 %>%
  filter(transAttitudes == "positive") %>%
  select(participantID, TargetStimuliGroup, transAttitudes)

# View the result
print(positive_trans_attitudes, n = Inf)


# Filter participants with "negative" in transAttitudes
negative_trans_attitudes <- temporary_data1 %>%
  filter(transAttitudes == "negative") %>%
  select(participantID, TargetStimuliGroup, transAttitudes)

# View the result
print(negative_trans_attitudes)

# Filter for participants with "negative" ControlsAttitudes who chose "Prison"
negative_controls_prison <- temporary_data1 %>%
  filter(ControlsAttitudes == "negative", TargetStimuliGroup == "Prison") %>%
  select(participantID, ControlsAttitudes, TargetStimuliGroup)

# View the results
print(negative_controls_prison)

# Filter for participants with "negative" ControlsAttitudes who chose "Education"
negative_controls_education <- temporary_data1 %>%
  filter(ControlsAttitudes == "negative", TargetStimuliGroup == "Education") %>%
  select(participantID, ControlsAttitudes, TargetStimuliGroup)

# View the results
print(negative_controls_education, Inf)
# 3 participants (7,16,21)

# Filter for participants with "negative" ControlsAttitudes and "negative" transAttitudes
negative_controls_transAttitudes <- temporary_data1 %>%
  filter(ControlsAttitudes == "negative", transAttitudes == "negative") %>%
  select(participantID, ControlsAttitudes, transAttitudes)

# View the results
print(negative_controls_transAttitudes, Inf)
# 0 participants 

# Filter for participants with "negative" and "positive" transAttitudes
negative_controls_positive_transAttitudes <- temporary_data1 %>%
  filter(ControlsAttitudes == "negative", transAttitudes == "positive") %>%
  select(participantID, ControlsAttitudes, transAttitudes)

# View the results
print(negative_controls_positive_transAttitudes, Inf)
# 3 participant (7,16,21)

# Filter for participants who chose "Prison" and view their transAttitudes
prison_trans_attitudes <- temporary_data1 %>%
  filter(TargetStimuliGroup == "Prison") %>%
  select(participantID, TargetStimuliGroup, transAttitudes)

# View the results
print(prison_trans_attitudes)

# Correlation between "Prison" in TargetStimuliGroup and "negative" in transAttitudes
#assigning numeric value to the target stimulus
temporary_data1 <- temporary_data1 %>%
  mutate(Prison0Education1 = ifelse(TargetStimuliGroup == "Prison", 0, 1))

#assigning numeric value to transAttitudes
temporary_data1 <- temporary_data1 %>%
  mutate(transAttitudes0_1 = ifelse(transAttitudes == "negative", 0, 1))

# Calculate the point-biserial correlation between trans attitudes and target stimulus in liberal group, baseline condition
correlation_result <- cor.test(
  temporary_data1$Prison0Education1,
  temporary_data1$transAttitudes0_1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result$estimate, "\n")
cat("P-value:", correlation_result$p.value, "\n")
# Correlation Coefficient: 0.5315958. Strong positive correlation 
# P-value: 0.001015247.statistically significant


#assigning numeric value to gender: Man = 0, Woman = 1
temporary_data1 <- temporary_data1 %>%
  mutate(Man0Woman1 = ifelse(`person-gender`== "Man", 0, 1))

# Calculate the point-biserial correlation between Gender and TargetStimulus
correlation_results_gender <- cor.test(
  temporary_data1$Prison0Education1,
  temporary_data1$Man0Woman1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_results_gender$estimate, "\n")
cat("P-value:", correlation_results_gender$p.value, "\n")
# Correlation Coefficient: -0.2975595 
# P-value: 0.08256008 

# Correlation between TargetStimuliGroup and Controls
#assigning numeric value to the controls
temporary_data1 <- temporary_data1 %>%
  mutate(Controls0_1 = ifelse(ControlsAttitudes == "negative", 0, 1))

# Calculate the point-biserial correlation
correlation_result_controls <- cor.test(
  temporary_data1$Prison0Education1,
  temporary_data1$Controls0_1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result_controls$estimate, "\n")
cat("P-value:", correlation_result_controls$p.value, "\n")
# Correlation Coefficient: -0,09375
# P-value: 0.5921853

# Correlation between Trans attitudes and Controls
# Calculate the point-biserial correlation
correlation_result_transVScontrols <- cor.test(
  temporary_data1$transAttitudes0_1,
  temporary_data1$Controls0_1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result_transVScontrols$estimate, "\n")
cat("P-value:", correlation_result_transVScontrols$p.value, "\n")
# Correlation Coefficient: -0.1099853 
# P-value: 0.5293728 

# New column with 0 = Man, 1 = woman
temporary_data1 <- temporary_data1 %>%
  mutate(Men0Women1 = ifelse(`person-gender` == "Man", 0, 1))
view(temporary_data1)

# correlation of gender and target stimulus choice 
# Calculate the correlation between Men0Women1 and Conservative0Liberal1
correlation_result_gender <- cor.test(
  temporary_data1$Men0Women1, 
  temporary_data1$Prison0Education1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation_result_gender$estimate, "\n")
cat("P-value:", correlation_result_gender$p.value, "\n")
# Correlation Coefficient: -0.2975595  
# p-value: 0.08256008 

# New column with 0 = Hetero, 1 = queer (all of them)
temporary_data1 <- temporary_data1 %>%
  mutate(Hetero0Queer1 = ifelse(`person-sexual-orientation` == "Straight or heterosexual", 0, 1))

# Calculate the correlation between Prison0Education1 and Sexual Orientation
correlation_results_queer <- cor.test(
  temporary_data1$Hetero0Queer1, 
  temporary_data1$Prison0Education1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation_results_queer$estimate, "\n")
cat("P-value:", correlation_results_queer$p.value, "\n")
# Correlation Coefficient: 0.006143415 
# p-value: 0.9720596


# Calculate the correlation between trans attitudes and Sexual Orientation
correlation_results_queer <- cor.test(
  temporary_data1$Hetero0Queer1, 
  temporary_data1$transAttitudes0_1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation_results_queer$estimate, "\n")
cat("P-value:", correlation_results_queer$p.value, "\n")
# Correlation Coefficient: 0.2594633   
# p-value: 0.1322803

# Create a new column 'person-politics'
temporary_data1 <- temporary_data1 %>%
  mutate(person_politics = if_else(person_PoliticalOrientation == "Liberal", 1, 0))

# Rearranging columns in the specified order
temporary_data1 <- temporary_data1 %>%
  select(
    "participantID",
    "TargetStimuliGroup", 
    "targetStimuli",
    "Prison0Education1",
    "transStereotypesScore",
    "transAttitudesScore",
    "transAttitudes",
    "transAttitudes0_1",
    "cisStereotypesControl",
    "transVSCisDifference",
    "ControlsScore",
    "ControlsAttitudes",
    "person-age",
    "person_PoliticalOrientation",
    "person_politics",
    "person-sexual-orientation",
    "person-gender",
    "person-cis-trans"
  )


# save as a new updated dataset
write.csv(temporary_data1, "updated_data1.csv", row.names = FALSE)

# Questionnaire 2/4  

# set working directory to directory of script
setwd(this.dir)

# load the data from 
data2 = read_csv("../data/nodwcons2_4.csv") 
nrow(data2) #35 participants
head(data2)
summary(data2) 

# Step 2: Replace Timestamp with Participant ID
colnames(data2)[1] <- "participantID" # Rename the column
data2$participantID <- match(data2$participantID, unique(data2$participantID)) # Assign unique IDs
print(table(data2$participantID)) # View distribution of participant IDs

# Step 3: Rename Columns
colnames(data2)[2] <- "prisonVSeducationprogram"
colnames(data2)[3] <- "feel-strongly"
colnames(data2)[4] <- "trans-stereotypes-confused"
colnames(data2)[5] <- "trans-stereotypes-mentally-ill"
colnames(data2)[6] <- "trans-stereotypes-dangerous"
colnames(data2)[7] <- "trans-stereotypes-frauds"
colnames(data2)[8] <- "trans-stereotypes-unnatural"
colnames(data2)[9] <- "cis-stereotypes-confused"
colnames(data2)[10] <- "cis-stereotypes-mentally-ill"
colnames(data2)[11] <- "cis-stereotypes-dangerous"
colnames(data2)[12] <- "cis-stereotypes-frauds"
colnames(data2)[13] <- "cis-stereotypes-unnatural"
colnames(data2)[14] <- "gender-fairness-apartment"
colnames(data2)[15] <- "gender-fairness-sports"
colnames(data2)[16] <- "gender-fairness-street-harassment"
colnames(data2)[17] <- "gender-fairness-bullying"
colnames(data2)[18] <- "general-fairness-education"
colnames(data2)[19] <- "general-fairness-healthcare"
colnames(data2)[20] <- "fear-number-of-trans-people"
colnames(data2)[21] <- "fear-all-problems"
colnames(data2)[22] <- "equality-equal-chance"
colnames(data2)[23] <- "equality-shouldnt-worry"
colnames(data2)[24] <- "person-age"
colnames(data2)[25] <- "person-sexual-orientation"
colnames(data2)[26] <- "person-gender"
colnames(data2)[27] <- "person-cis-trans"


# Ensure all columns being pivoted have the same data type
data2 <- data2 %>%
  mutate(across(2:23, as.character)) # Convert columns 2 to 23 to character

# Pivot the data
data2_long <- data2 %>%
  pivot_longer(
    cols = 2:23, # Adjust this range based on your column selection
    names_to = "question",
    values_to = "response"
  )

# View the reshaped data
print(data2_long)
head(data2_long)
sum(is.na(data2_long$response))
unique(data2_long$question)


# now that we see that tmp works, make d = tmp
ls()
tmp2 <- data2


#assigning value to the target stimulus
data2 <- data2 %>%
  mutate(targetStimuli = case_when(
    prisonVSeducationprogram == "Building new prisons" & `feel-strongly` == "I feel very strongly about this." ~ 1,
    prisonVSeducationprogram == "Building new prisons" & `feel-strongly` == "I feel not very strongly about this." ~ 2,
    prisonVSeducationprogram == "Education programs" & `feel-strongly` == "I feel not very strongly about this." ~ 3,
    prisonVSeducationprogram == "Education programs" & `feel-strongly` == "I feel very strongly about this." ~ 4,
    TRUE ~ NA_real_ # Assign NA for any unmatched cases
  ))


# Print all column names to confirm exact names
colnames(data2)

# Filter for participants with targetStimuli 1 count them
targetStimuli_counts <- data2 %>%
  filter(targetStimuli %in% c(1)) %>%
  summarise(count = n())

# Print the result
print(targetStimuli_counts)

# Filter for participants with targetStimuli 2 count them
targetStimuli_counts <- data2 %>%
  filter(targetStimuli %in% c(2)) %>%
  summarise(count = n())

# Print the result
print(targetStimuli_counts)

# Filter for participants with targetStimuli 3 and count them
targetStimuli_counts <- data2 %>%
  filter(targetStimuli %in% c(3)) %>%
  summarise(count = n())

# Print the result
print(targetStimuli_counts)

# Filter for participants with targetStimuli 4 count them
targetStimuli_counts <- data2 %>%
  filter(targetStimuli %in% c(4)) %>%
  summarise(count = n())

# Print the result
print(targetStimuli_counts)
# Filter for participants with targetStimuli 4 count them
targetStimuli_counts <- data2 %>%
  filter(targetStimuli %in% c(3, 4)) %>%
  summarise(count = n())

print(targetStimuli_counts) 

# Filter for participants with targetStimuli 4 count them
targetStimuli_counts <- data2 %>%
  filter(targetStimuli %in% c(1,2)) %>%
  summarise(count = n())

print(targetStimuli_counts)

# Convert trans-stereotypes columns to numeric
data2 <- data2 %>%
  mutate(across(c(`trans-stereotypes-confused`, 
                  `trans-stereotypes-mentally-ill`, 
                  `trans-stereotypes-dangerous`, 
                  `trans-stereotypes-frauds`, 
                  `trans-stereotypes-unnatural`), 
                as.numeric))


# Sum the responses while handling missing values
data2 <- data2 %>%
  mutate(trans_stereotypes = rowSums(select(., 
                                            `trans-stereotypes-confused`, 
                                            `trans-stereotypes-mentally-ill`, 
                                            `trans-stereotypes-dangerous`, 
                                            `trans-stereotypes-frauds`, 
                                            `trans-stereotypes-unnatural`), 
                                     na.rm = TRUE))
# Confirm the change
colnames(data2)
summary(data2$trans_stereotypes)


# Convert cis-stereotypes columns to numeric
data2 <- data2 %>%
  mutate(across(c(`cis-stereotypes-confused`, 
                  `cis-stereotypes-mentally-ill`, 
                  `cis-stereotypes-dangerous`, 
                  `cis-stereotypes-frauds`, 
                  `cis-stereotypes-unnatural`), 
                as.numeric))


# Apply scoring and sum the responses into a new column
data2 <- data2 %>%
  mutate(across(`cis-stereotypes-confused`:`cis-stereotypes-unnatural`, as.numeric)) %>%
  mutate(cisStereotypesControl = rowSums(select(., 
                                                `cis-stereotypes-confused`, 
                                                `cis-stereotypes-mentally-ill`, 
                                                `cis-stereotypes-dangerous`, 
                                                `cis-stereotypes-frauds`, 
                                                `cis-stereotypes-unnatural`), na.rm = TRUE))


# Rename the column trans_stereotypes to transStereotypesScore
data2 <- data2 %>%
  rename(transStereotypesScore = trans_stereotypes)

#Gender Fairness: part of the trans attitudes test and consists of one yes/no question and 3 questions with a 1-7 scale
# Assign points for gender-fairness-apartment
data2 <- data2 %>%
  mutate(
    gender_fairness_apartment_score = ifelse(`gender-fairness-apartment` == "Yes", 1, 2)
  )

# For the reverse coded columns, assign points according to the reverse coding scheme
data2 <- data2 %>%
  mutate(
    gender_fairness_sports_score = 8 - as.numeric(`gender-fairness-sports`),
    gender_fairness_street_harassment_score = 8 - as.numeric(`gender-fairness-street-harassment`),
    gender_fairness_bullying_score = 8 - as.numeric(`gender-fairness-bullying`)
  )

# Combine the four scores into a new column: genderFairnessScore
data2 <- data2 %>%
  mutate(
    genderFairnessScore = gender_fairness_apartment_score +
      gender_fairness_sports_score +
      gender_fairness_street_harassment_score +
      gender_fairness_bullying_score
  )


# Remove specified columns from the dataset
data2 <- data2 %>%
  select(-gender_fairness_apartment_score, 
         -gender_fairness_street_harassment_score, 
         -gender_fairness_bullying_score,
         -gender_fairness_sports_score)


#General Fairness:Control
colnames(data2)
# Combine and code general-fairness-education and general-fairness-healthcare
# Combine points for general fairness education and healthcare
data2 <- data2 %>%
  mutate(
    generalFairnessScore = rowSums(
      mutate(
        select(data2, `general-fairness-education`, `general-fairness-healthcare`),
        across(everything(), ~ case_when(
          . == "Strongly agree" ~ 1,
          . == "Somewhat agree" ~ 2,
          . == "Somewhat disagree" ~ 3,
          . == "Strongly disagreee" ~ 4,
          TRUE ~ 0
        ))
      )
    )
  )



# Combine and code the 'fearOfTransPeopleScore' column
data2 <- data2 %>%
  mutate(
# Assign points to 'fear-number-of-trans-people'
fear_number_of_trans_people_score = case_when(
      `fear-number-of-trans-people` == "The number of trans people increased" ~ 3,
      `fear-number-of-trans-people` == "The number of trans people stayed about the same" ~ 2,
      `fear-number-of-trans-people` == "The number of trans people decreased" ~ 1,
      TRUE ~ NA_real_
    ),
    
# Assign points to 'fear-all-problems'
    fear_all_problems_score = case_when(
      `fear-all-problems` == "It is no more important than other problems" ~ 2,
      `fear-all-problems` == "It is the most important problem" ~ 1,
      `fear-all-problems` == "It is less important than other problems" ~ 3,
      TRUE ~ NA_real_
    ),

# Combine the two scores into 'fearOfTransPeopleScore'
fearOfTransPeopleScore = fear_number_of_trans_people_score + fear_all_problems_score
  )


# Combine and code the 'EqualityScore' column
data2 <- data2 %>%
  mutate(
  
# Assign points to 'equality-equal-chance'
equality_equal_chance_score = case_when(
      `equality-equal-chance` == "Strongly agree" ~ 4,
      `equality-equal-chance` == "Somewhat agree" ~ 3,
      `equality-equal-chance` == "Somewhat disagree" ~ 2,
      `equality-equal-chance` == "Strongly disagree" ~ 1,
      TRUE ~ NA_real_
    ),
    
# Assign points to 'equality-shouldnt-worry'
equality_shouldnt_worry_score = case_when(
      `equality-shouldnt-worry` == "Strongly agree" ~ 1,
      `equality-shouldnt-worry` == "Somewhat agree" ~ 2,
      `equality-shouldnt-worry` == "Somewhat disagree" ~ 3,
      `equality-shouldnt-worry` == "Strongly disagree" ~ 4,
      TRUE ~ NA_real_
    ),
    
# Combine the two scores into 'EqualityScore'
EqualityScore = equality_equal_chance_score + equality_shouldnt_worry_score
  )


# View the first 20 rows to check the values in person-gender column
head(data2[, 26], 20)

#count how many men and women participants we have
library(dplyr)

# Count the number of men and women in the dataset
data2 %>%
  group_by(`person-gender`) %>%
  summarise(count = n()) # 18 men, 17 women

# Removing unwanted columns from the dataset
data2 <- data2 %>%
  select(-fear_number_of_trans_people_score,
         -fear_all_problems_score,
         -equality_equal_chance_score,
         -equality_shouldnt_worry_score)


# Create the new column 'transAttitudesScore' by summing 'genderFairnessScore' and 'transStereotypesScore'
data2 <- data2 %>%
  mutate(transAttitudesScore = genderFairnessScore + transStereotypesScore + fearOfTransPeopleScore)
# the higher the score, the more transphobic the person is!

# Make a new column for the score of all controls together
data2 <- data2 %>%
  mutate(
    ControlsScore = generalFairnessScore + EqualityScore )
# the higher the score, the more they care about fairness and equality

# Create a new column to assign participants to groups based on targetStimuli
data2 <- data2%>%
  mutate(
    TargetStimuliGroup = case_when(
      targetStimuli %in% c(1, 2) ~ "Prison",
      targetStimuli %in% c(3, 4) ~ "Education",
      TRUE ~ NA_character_ # 
    )
  )

# View the updated dataset
head(data2)


# Filter data for trans attitudes and controls of the group who chose prison
prison_group_scores <- data2 %>%
  filter(TargetStimuliGroup == "Prison") %>%
  select(participantID, transAttitudesScore, ControlsScore)

# View the table of scores for the TargetStimuli_Prison group
print(prison_group_scores)


# Filter data for tans attitudes and controls of the group who chose education programs
education_group_scores <- data2 %>%
  filter(TargetStimuliGroup == "Education") %>%
  select(participantID, transAttitudesScore, ControlsScore)

# View the table of scores for the TargetStimuli_Education group
print(education_group_scores)


# Average scores for both groups for transAttitudesScore and for ControlsScore
# Filter for TargetStimuli_Prison group
prison_group <- data2 %>% filter(TargetStimuliGroup == "Prison")

# Calculate averages for Prison group
avg_prison <- prison_group %>%
  summarise(
    avg_transAttitudesScore = mean(transAttitudesScore, na.rm = TRUE),
    avg_ControlsScore = mean(ControlsScore, na.rm = TRUE)
  )

cat(
  "Average Trans Attitudes Score (Prison Group):", avg_prison$avg_transAttitudesScore, "\n",
  "Average Controls Score (Prison Group):", avg_prison$avg_ControlsScore, "\n"
) # average transAttitudesScore in Prison group: 48.6 average ControlsScore: 8.1


# Filter for Education group
education_group <- data2 %>% filter(TargetStimuliGroup == "Education")

# Calculate averages for Education group
avg_education <- education_group %>%
  summarise(
    avg_transAttitudesScore = mean(transAttitudesScore, na.rm = TRUE),
    avg_ControlsScore = mean(ControlsScore, na.rm = TRUE)
  )

cat(
  "Average Trans Attitudes Score (Education Group):", avg_education$avg_transAttitudesScore, "\n",
  "Average Controls Score (Education Group):", avg_education$avg_ControlsScore, "\n"
) #transAttitudesScore average: 41.8 ControlsScoreAverage: 9.5


# Create a new column for the difference between transStereotypesScore and cisStereotypesScore
data2 <- data2 %>%
  mutate(transVSCisDifference = transStereotypesScore - cisStereotypesControl)

# View the updated dataset
head(data2)

summary(data2$transStereotypesScore)
summary(data2$cisStereotypesControl)
nrow(prison_group)
data2 <- data2 %>%
  mutate(
    transVSCisDifference = ifelse(is.na(transStereotypesScore) | is.na(cisStereotypesControl), 
                                  NA, transStereotypesScore - cisStereotypesControl)
  )

# Recalculate average for transVSCisDifference
prison_group <- data2 %>% filter(TargetStimuliGroup == "Prison")
education_group <- data2 %>% filter(TargetStimuliGroup == "Education")

avg_prison <- prison_group %>%
  summarise(avg_transVSCisDifference = mean(transVSCisDifference, na.rm = TRUE))

avg_education <- education_group %>%
  summarise(avg_transVSCisDifference = mean(transVSCisDifference, na.rm = TRUE))

cat(
  "Average transVSCisDifference (Prison Group):", avg_prison$avg_transVSCisDifference, "\n",
  "Average transVSCisDifference (Education Group):", avg_education$avg_transVSCisDifference, "\n"
) # Prison group average transVSCisDifference: 16.8
# Education group average transVSCisDifference: 12.7


# Create a new dataset with only the selected columns
selected_columns <- c(
  "participantID",
  "TargetStimuliGroup",
  "targetStimuli",
  "transStereotypesScore",
  "cisStereotypesControl",
  "transAttitudesScore",
  "transVSCisDifference",
  "ControlsScore",
  "person-age",
  "person-sexual-orientation",
  "person-gender",
  "person-cis-trans"
)

# Select the columns and create a new dataset
temporary_data2 <- data2[, selected_columns]

# View the first few rows to check the data
head(temporary_data2)

# Create a new column transAttitudes based on the conditions
temporary_data2 <- temporary_data2 %>%
  mutate(transAttitudes = if_else(transAttitudesScore <= 36, "positive", "negative"))


# Add the PoliticalOrientation column and set it to "Liberal" for all rows
temporary_data2 <- temporary_data2 %>%
  mutate(person_PoliticalOrientation = "Conservative")

# Create a new column ControlsAttitudes based on ControlsScore
temporary_data2 <- temporary_data2 %>%
  mutate(ControlsAttitudes = ifelse(ControlsScore <= 9, "negative", "positive"))

# Rearranging columns in the specified order
temporary_data2 <- temporary_data2 %>%
  select(
    "participantID",
    "TargetStimuliGroup", 
    "targetStimuli",
    "transStereotypesScore",
    "transAttitudesScore",
    "transAttitudes",
    "cisStereotypesControl",
    "transVSCisDifference",
    "ControlsScore",
    "ControlsAttitudes",
    "person-age",
    "person_PoliticalOrientation",
    "person-sexual-orientation",
    "person-gender",
    "person-cis-trans"
  )

# Filter participants with "positive" in transAttitudes
positive_trans_attitudes <- temporary_data2 %>%
  filter(transAttitudes == "positive") %>%
  select(participantID, TargetStimuliGroup, transAttitudes)

# View the result
print(positive_trans_attitudes, n = Inf)


# Filter participants with "negative" in transAttitudes
negative_trans_attitudes <- temporary_data2 %>%
  filter(transAttitudes == "negative") %>%
  select(participantID, TargetStimuliGroup, transAttitudes)

# View the result
print(negative_trans_attitudes, n = Inf)

# Filter for participants with "negative" ControlsAttitudes who chose "Prison"
negative_controls_prison <- temporary_data2 %>%
  filter(ControlsAttitudes == "negative", TargetStimuliGroup == "Prison") %>%
  select(participantID, ControlsAttitudes, TargetStimuliGroup)

# View the results
print(negative_controls_prison)
# 6 participants (1,3,9,13,31,34)

# Filter for participants with "negative" ControlsAttitudes who chose "Education"
negative_controls_education <- temporary_data2 %>%
  filter(ControlsAttitudes == "negative", TargetStimuliGroup == "Education") %>%
  select(participantID, ControlsAttitudes, TargetStimuliGroup)

# View the results
print(negative_controls_education, Inf)
# 13 participants (4,6,8,11,12,14,15,17,18,22,30,33,35)

# Filter for participants with "negative" ControlsAttitudes and "negative" transAttitudes
negative_controls_transAttitudes <- temporary_data2 %>%
  filter(ControlsAttitudes == "negative", transAttitudes == "negative") %>%
  select(participantID, ControlsAttitudes, transAttitudes)

# View the results
print(negative_controls_transAttitudes, Inf)
# 16 participants (1,3,4,8,11,12,14,15,17,18,22,33,34,35)

# Filter for participants with "negative" and "positive" transAttitudes
negative_controls_positive_transAttitudes <- temporary_data2 %>%
  filter(ControlsAttitudes == "negative", transAttitudes == "positive") %>%
  select(participantID, ControlsAttitudes, transAttitudes)

# View the results
print(negative_controls_positive_transAttitudes, Inf)
# 3 participant (6,13,30)

# Filter for participants who chose "Prison" and view their transAttitudes
prison_trans_attitudes <- temporary_data2 %>%
  filter(TargetStimuliGroup == "Prison") %>%
  select(participantID, TargetStimuliGroup, transAttitudes)

# View the results
print(prison_trans_attitudes)

# Correlation between "Prison" in TargetStimuliGroup and "negative" in transAttitudes
#assigning numeric value to the target stimulus
temporary_data2 <- temporary_data2 %>%
  mutate(Prison0Education1 = ifelse(TargetStimuliGroup == "Prison", 0, 1))


#assigning numeric value to transAttitudes
temporary_data2 <- temporary_data2 %>%
  mutate(transAttitudes0_1 = ifelse(transAttitudes == "negative", 0, 1))
view(temporary_data2)
# Calculate the point-biserial correlation between target stimulus and trans attitudes
correlation_result <- cor.test(
  temporary_data2$Prison0Education1,
  temporary_data2$transAttitudes0_1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result$estimate, "\n")
cat("P-value:", correlation_result$p.value, "\n")
# Correlation Coefficient: 0.1645762
# P-value: 0.3447871 

#assigning numeric value to gender: Man = 0, Woman = 1
temporary_data2 <- temporary_data2 %>%
  mutate(Man0Woman1 = ifelse(`person-gender`== "Man", 0, 1))

# Calculate the point-biserial correlation between Gender and TargetStimulus
correlation_results_gender <- cor.test(
  temporary_data2$Prison0Education1,
  temporary_data2$Man0Woman1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_results_gender$estimate, "\n")
cat("P-value:", correlation_results_gender$p.value, "\n")
# Correlation Coefficient: -0.01555867
# P-value: 0.9293134

# New column with 0 = Man, 1 = woman
temporary_data2 <- temporary_data2 %>%
  mutate(Men0Women1 = ifelse(`person-gender` == "Man", 0, 1))
view(temporary_data1)

# correlation of gender and target stimulus choice 
# Calculate the correlation between Men0Women1 and Conservative0Liberal1
correlation_result_gender <- cor.test(
  temporary_data2$Men0Women1, 
  temporary_data2$Prison0Education1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation_result_gender$estimate, "\n")
cat("P-value:", correlation_result_gender$p.value, "\n")
# Correlation Coefficient: -0.01555867   
# p-value: 0.9293134 

# New column with 0 = Hetero, 1 = queer (all of them)
temporary_data2 <- temporary_data2 %>%
  mutate(Hetero0Queer1 = ifelse(`person-sexual-orientation` == "Straight or heterosexual", 0, 1))

# Calculate the correlation between Prison0Education1 and Sexual Orientation
correlation_results_queer <- cor.test(
  temporary_data2$Hetero0Queer1, 
  temporary_data2$Prison0Education1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation_results_queer$estimate, "\n")
cat("P-value:", correlation_results_queer$p.value, "\n")
# Correlation Coefficient: 0.1955295 
# p-value: 0.2603074

# Correlation between TargetStimuliGroup and Controls
#assigning numeric value to the controls
temporary_data2 <- temporary_data2 %>%
  mutate(Controls0_1 = ifelse(ControlsAttitudes == "negative", 0, 1))

# Calculate the point-biserial correlation
correlation_result_controls <- cor.test(
  temporary_data2$Prison0Education1,
  temporary_data2$Controls0_1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result_controls$estimate, "\n")
cat("P-value:", correlation_result_controls$p.value, "\n")
# Correlation Coefficient: 0.2263416
# P-value: 0.1910538 

# Correlation between Trans attitudes and Controls
# Calculate the point-biserial correlation
correlation_result_transVScontrols <- cor.test(
  temporary_data2$transAttitudes0_1,
  temporary_data2$Controls0_1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result_transVScontrols$estimate, "\n")
cat("P-value:", correlation_result_transVScontrols$p.value, "\n")
# Correlation Coefficient: 0.2474567 
# P-value: 0.1517988 


# Calculate the correlation between trans attitudes and Sexual Orientation
correlation_results_queer <- cor.test(
  temporary_data2$Hetero0Queer1, 
  temporary_data2$transAttitudes0_1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation_results_queer$estimate, "\n")
cat("P-value:", correlation_results_queer$p.value, "\n")
# Correlation Coefficient: 0.4050702    
# p-value: 0.01578357

# Create a new column 'person-politics'
temporary_data2 <- temporary_data2 %>%
  mutate(person_politics = if_else(person_PoliticalOrientation == "Liberal", 1, 0))

# Rearranging columns in the specified order
temporary_data2 <- temporary_data2 %>%
  select(
    "participantID",
    "TargetStimuliGroup", 
    "targetStimuli",
    "Prison0Education1",
    "transStereotypesScore",
    "transAttitudesScore",
    "transAttitudes",
    "transAttitudes0_1",
    "cisStereotypesControl",
    "transVSCisDifference",
    "ControlsScore",
    "ControlsAttitudes",
    "person-age",
    "person_PoliticalOrientation",
    "person_politics",
    "person-sexual-orientation",
    "person-gender",
    "person-cis-trans"
  )


# save as a new updated dataset
write.csv(temporary_data2, "updated_data2.csv", row.names = FALSE)

# Combine temporary_data1 and temporary_data2 to view the results in baseline based on partisanship
# Combine all datasets into one
baseline_temporary_data <- bind_rows(temporary_data1, temporary_data2)

# Calculate the correlation between political orientation and trans attitudes
baseline_correlation_partisanship_transattitudes <- cor.test(
  baseline_temporary_data$transAttitudes0_1, 
  baseline_temporary_data$person_politics, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", baseline_correlation_partisanship_transattitudes$estimate, "\n")
cat("P-value:", baseline_correlation_partisanship_transattitudes$p.value, "\n")
# Correlation Coefficient: 0.6350853
# p-value: 3.526854e-09

# New column with 0 = Man, 1 = woman
baseline_temporary_data <- baseline_temporary_data %>%
  mutate(Men0Women1 = ifelse(`person-gender` == "Man", 0, 1))

# correlation of gender and target stimulus choice 
# Calculate the correlation between Men0Women1 and Conservative0Liberal1
correlation_result_gender <- cor.test(
  baseline_temporary_data$Men0Women1, 
  baseline_temporary_data$Prison0Education1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation_result_gender$estimate, "\n")
cat("P-value:", correlation_result_gender$p.value, "\n")
# Correlation Coefficient: -0.1177603   
# p-value: 0.3315997 

# Correlation between political orientation and Controls
#assigning numeric value to the controls
baseline_temporary_data <- baseline_temporary_data %>%
  mutate(Controls0_1 = ifelse(ControlsAttitudes == "negative", 0, 1))

# Correlation between political orientation and Controls
#assigning numeric value to the controls
baseline_temporary_data <- baseline_temporary_data %>%
  mutate(Coservative0Liberal1 = ifelse(person_PoliticalOrientation == "Conservative", 0, 1))

# Correlation between Trans attitudes and Target Stimulus in baseline participants
# Calculate the point-biserial correlation
correlation_result_transTarget <- cor.test(
  baseline_temporary_data$transAttitudes0_1,
  baseline_temporary_data$Prison0Education1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result_transTarget$estimate, "\n")
cat("P-value:", correlation_result_transTarget$p.value, "\n")
# Correlation Coefficient: 0.3399447  
# P-value: 0.003986224 

# Calculate the point-biserial correlation of political orientation and controls
correlation_result_controls <- cor.test(
  baseline_temporary_data$Coservative0Liberal1,
  baseline_temporary_data$Controls0_1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result_controls$estimate, "\n")
cat("P-value:", correlation_result_controls$p.value, "\n")
# Correlation Coefficient: 0.492366 
# P-value: 1.495036e-05 

# Calculate the point-biserial correlation of response to target stimulus and controls
correlation_result_controls <- cor.test(
  baseline_temporary_data$Prison0Education1,
  baseline_temporary_data$Controls0_1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result_controls$estimate, "\n")
cat("P-value:", correlation_result_controls$p.value, "\n")
# Correlation Coefficient: 0.2150135 
# P-value: 0.07385489  


# Calculate the point-biserial correlation of trans attitudes and controls
correlation_result_controls <- cor.test(
  baseline_temporary_data$transAttitudes0_1,
  baseline_temporary_data$Controls0_1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result_controls$estimate, "\n")
cat("P-value:", correlation_result_controls$p.value, "\n")
# Correlation Coefficient: 0.4086347 
# P-value: 0.0004447808  

# New column with 0 = conservative, 1 liberal
baseline_temporary_data <- baseline_temporary_data %>%
  mutate(Conservative0Liberal1 = ifelse(person_PoliticalOrientation == "Conservative", 0, 1))

#New column with 0 = Prison, 1 = Education
baseline_temporary_data <- baseline_temporary_data %>%
  mutate(Prison0Education1 = ifelse(TargetStimuliGroup == "Prison", 0, 1))

# correlation of political orientation and target stimulus choice 
# Calculate the correlation between Prison0Education1 and Conservative0Liberal1
baseline_correlation_partisanship <- cor.test(
  baseline_temporary_data$Conservative0Liberal1, 
  baseline_temporary_data$Prison0Education1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", baseline_correlation_partisanship$estimate, "\n")
cat("P-value:", baseline_correlation_partisanship$p.value, "\n")
# Correlation Coefficient: 0.1962672  
# p-value: 0.1034395

# Correlation between target Stimulus and trans Attitudes in baseline
# Calculate the point-biserial correlation
correlation_result_target_transAttitudes <- cor.test(
  baseline_temporary_data$Prison0Education1,
  baseline_temporary_data$transAttitudes0_1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result_target_transAttitudes$estimate, "\n")
cat("P-value:", correlation_result_target_transAttitudes$p.value, "\n")
# Correlation Coefficient: 0.3399447 
# P-value: 0.003986224
# moderate positive correlation, statistically significant
view(baseline_temporary_data)
# Calculate the correlation between trans attitudes and Conservative0Liberal1
baseline_correlation_partisanship_transattitudes <- cor.test(
  baseline_temporary_data$transAttitudes0_1, 
  baseline_temporary_data$Conservative0Liberal1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", baseline_correlation_partisanship_transattitudes$estimate, "\n")
cat("P-value:", baseline_correlation_partisanship_transattitudes$p.value, "\n")
# Correlation Coefficient: 0.6350853 
# p-value: 3.526854e-09  

# Questionnaire 3/4

# set working directory to directory of script
setwd(this.dir)

# load the data from 
data3 = read_csv("../data/dwlib3_4.csv") 
nrow(data3) #35 participants
head(data3)
summary(data3) 

# Step 2: Replace Timestamp with Participant ID
colnames(data3)[1] <- "participantID" # Rename the column
data3$participantID <- match(data3$participantID, unique(data3$participantID)) # Assign unique IDs
print(table(data3$participantID)) # View distribution of participant IDs


# Step 3: Rename Columns
colnames(data3)[2] <- "prisonVSeducationprogram"
colnames(data3)[3] <- "feel-strongly"
colnames(data3)[4] <- "trans-stereotypes-confused"
colnames(data3)[5] <- "trans-stereotypes-mentally-ill"
colnames(data3)[6] <- "trans-stereotypes-dangerous"
colnames(data3)[7] <- "trans-stereotypes-frauds"
colnames(data3)[8] <- "trans-stereotypes-unnatural"
colnames(data3)[9] <- "cis-stereotypes-confused"
colnames(data3)[10] <- "cis-stereotypes-mentally-ill"
colnames(data3)[11] <- "cis-stereotypes-dangerous"
colnames(data3)[12] <- "cis-stereotypes-frauds"
colnames(data3)[13] <- "cis-stereotypes-unnatural"
colnames(data3)[14] <- "gender-fairness-apartment"
colnames(data3)[15] <- "gender-fairness-sports"
colnames(data3)[16] <- "gender-fairness-street-harassment"
colnames(data3)[17] <- "gender-fairness-bullying"
colnames(data3)[18] <- "general-fairness-education"
colnames(data3)[19] <- "general-fairness-healthcare"
colnames(data3)[20] <- "fear-number-of-trans-people"
colnames(data3)[21] <- "fear-all-problems"
colnames(data3)[22] <- "equality-equal-chance"
colnames(data3)[23] <- "equality-shouldnt-worry"
colnames(data3)[24] <- "person-age"
colnames(data3)[25] <- "person-sexual-orientation"
colnames(data3)[26] <- "person-gender"
colnames(data3)[27] <- "person-cis-trans"



# Ensure all columns being pivoted have the same data type
data3 <- data3 %>%
  mutate(across(2:23, as.character)) # Convert columns 2 to 23 to character

# Pivot the data
data3_long <- data3 %>%
  pivot_longer(
    cols = 2:23, # Adjust this range based on your column selection
    names_to = "question",
    values_to = "response"
  )

# View the reshaped data
print(data3_long)
head(data3_long)
sum(is.na(data3_long$response))
unique(data3_long$question)


# now that we see that tmp works, make d = tmp
ls()
tmp3 <- data3

#assigning value to the target stimulus
data3 <- data3 %>%
  mutate(targetStimuli = case_when(
    prisonVSeducationprogram == "Building new prisons" & `feel-strongly` == "I feel very strongly about this." ~ 1,
    prisonVSeducationprogram == "Building new prisons" & `feel-strongly` == "I feel not very strongly about this." ~ 2,
    prisonVSeducationprogram == "Education programs" & `feel-strongly` == "I feel not very strongly about this." ~ 3,
    prisonVSeducationprogram == "Education programs" & `feel-strongly` == "I feel very strongly about this." ~ 4,
    TRUE ~ NA_real_ # Assign NA for any unmatched cases
  ))


# Print all column names to confirm exact names
colnames(data3)

# Filter for participants with targetStimuli 1 count them
targetStimuli_counts <- data3 %>%
  filter(targetStimuli %in% c(1)) %>%
  summarise(count = n())

# Print the result
print(targetStimuli_counts)

# Filter for participants with targetStimuli 2 count them
targetStimuli_counts <- data3 %>%
  filter(targetStimuli %in% c(2)) %>%
  summarise(count = n())

# Print the result
print(targetStimuli_counts)

# Filter for participants with targetStimuli 3 and count them
targetStimuli_counts <- data3 %>%
  filter(targetStimuli %in% c(3)) %>%
  summarise(count = n())

# Print the result
print(targetStimuli_counts)

# Filter for participants with targetStimuli 4 count them
targetStimuli_counts <- data3 %>%
  filter(targetStimuli %in% c(4)) %>%
  summarise(count = n())

# Print the result
print(targetStimuli_counts)

# Filter for participants with targetStimuli 4 count them
targetStimuli_counts <- data3 %>%
  filter(targetStimuli %in% c(3, 4)) %>%
  summarise(count = n())

# Print the result
print(targetStimuli_counts)


# Convert trans-stereotypes columns to numeric
data3 <- data3 %>%
  mutate(across(c(`trans-stereotypes-confused`, 
                  `trans-stereotypes-mentally-ill`, 
                  `trans-stereotypes-dangerous`, 
                  `trans-stereotypes-frauds`, 
                  `trans-stereotypes-unnatural`), 
                as.numeric))


# Sum the responses while handling missing values
data3 <- data3 %>%
  mutate(trans_stereotypes = rowSums(select(., 
                                            `trans-stereotypes-confused`, 
                                            `trans-stereotypes-mentally-ill`, 
                                            `trans-stereotypes-dangerous`, 
                                            `trans-stereotypes-frauds`, 
                                            `trans-stereotypes-unnatural`), 
                                     na.rm = TRUE))
# Confirm the change
colnames(data3)
summary(data3$trans_stereotypes)


# Convert cis-stereotypes columns to numeric
data3 <- data3 %>%
  mutate(across(c(`cis-stereotypes-confused`, 
                  `cis-stereotypes-mentally-ill`, 
                  `cis-stereotypes-dangerous`, 
                  `cis-stereotypes-frauds`, 
                  `cis-stereotypes-unnatural`), 
                as.numeric))


# Apply scoring and sum the responses into a new column
data3 <- data3 %>%
  mutate(across(`cis-stereotypes-confused`:`cis-stereotypes-unnatural`, as.numeric)) %>%
  mutate(cisStereotypesControl = rowSums(select(., 
                                                `cis-stereotypes-confused`, 
                                                `cis-stereotypes-mentally-ill`, 
                                                `cis-stereotypes-dangerous`, 
                                                `cis-stereotypes-frauds`, 
                                                `cis-stereotypes-unnatural`), na.rm = TRUE))



# Rename the column trans_stereotypes to transStereotypesScore
data3 <- data3 %>%
  rename(transStereotypesScore = trans_stereotypes)


#Gender Fairness: part of the trans attitudes test and consists of one yes/no question and 3 questions with a 1-7 scale
# Assign points for gender-fairness-apartment
data3 <- data3 %>%
  mutate(
    gender_fairness_apartment_score = ifelse(`gender-fairness-apartment` == "Yes", 1, 2)
  )

# For the reverse coded columns, assign points according to the reverse coding scheme
data3 <- data3 %>%
  mutate(
    gender_fairness_sports_score = 8 - as.numeric(`gender-fairness-sports`),
    gender_fairness_street_harassment_score = 8 - as.numeric(`gender-fairness-street-harassment`),
    gender_fairness_bullying_score = 8 - as.numeric(`gender-fairness-bullying`)
  )

# Combine the four scores into a new column: genderFairnessScore
data3 <- data3 %>%
  mutate(
    genderFairnessScore = gender_fairness_apartment_score +
      gender_fairness_sports_score +
      gender_fairness_street_harassment_score +
      gender_fairness_bullying_score
  )


# Remove specified columns from the dataset
data3 <- data3 %>%
  select(-gender_fairness_apartment_score, 
         -gender_fairness_street_harassment_score, 
         -gender_fairness_bullying_score,
         -gender_fairness_sports_score)

#General Fairness:Control
colnames(data3)

# Combine and code general-fairness-education and general-fairness-healthcare
# Combine points for general fairness education and healthcare
data3 <- data3 %>%
  mutate(
    generalFairnessScore = rowSums(
      mutate(
        select(data3, `general-fairness-education`, `general-fairness-healthcare`),
        across(everything(), ~ case_when(
          . == "Strongly agree" ~ 1,
          . == "Somewhat agree" ~ 2,
          . == "Somewhat disagree" ~ 3,
          . == "Strongly disagreee" ~ 4,
          TRUE ~ 0
        ))
      )
    )
  )

# Combine and code the 'fearOfTransPeopleScore' column
data3 <- data3 %>%
  mutate(
# Assign points to 'fear-number-of-trans-people'
    fear_number_of_trans_people_score = case_when(
      `fear-number-of-trans-people` == "The number of trans people increased" ~ 3,
      `fear-number-of-trans-people` == "The number of trans people stayed about the same" ~ 2,
      `fear-number-of-trans-people` == "The number of trans people decreased" ~ 1,
      TRUE ~ NA_real_
    ),
    
# Assign points to 'fear-all-problems'
fear_all_problems_score = case_when(
      `fear-all-problems` == "It is no more important than other problems" ~ 2,
      `fear-all-problems` == "It is the most important problem" ~ 1,
      `fear-all-problems` == "It is less important than other problems" ~ 3,
      TRUE ~ NA_real_
    ),
    
# Combine the two scores into 'fearOfTransPeopleScore'
fearOfTransPeopleScore = fear_number_of_trans_people_score + fear_all_problems_score
  )


# Combine and code the 'EqualityScore' column
data3 <- data3 %>%
  mutate(
# Assign points to 'equality-equal-chance'
equality_equal_chance_score = case_when(
          `equality-equal-chance` == "Strongly agree" ~ 4,
          `equality-equal-chance` == "Somewhat agree" ~ 3,
          `equality-equal-chance` == "Somewhat disagree" ~ 2,
          `equality-equal-chance` == "Strongly disagree" ~ 1,
          TRUE ~ NA_real_
        ),
        
# Assign points to 'equality-shouldnt-worry'
equality_shouldnt_worry_score = case_when(
          `equality-shouldnt-worry` == "Strongly agree" ~ 1,
          `equality-shouldnt-worry` == "Somewhat agree" ~ 2,
          `equality-shouldnt-worry` == "Somewhat disagree" ~ 3,
          `equality-shouldnt-worry` == "Strongly disagree" ~ 4,
          TRUE ~ NA_real_
        ),
        
        
# Combine the two scores into 'EqualityScore'
EqualityScore = equality_equal_chance_score + equality_shouldnt_worry_score
  )


# View the first 20 rows to check the values in person-gender column
head(data3[, 26], 20)

#count how many men and women participants we have
library(dplyr)

# Count the number of men and women in the dataset
data3 %>%
  group_by(`person-gender`) %>%
  summarise(count = n()) # 18 men, 17 women

# Removing unwanted columns from the dataset
data3 <- data3 %>%
  select(-fear_number_of_trans_people_score,
         -fear_all_problems_score,
         -equality_equal_chance_score,
         -equality_shouldnt_worry_score)


# Create the new column 'transAttitudesScore' by summing 'genderFairnessScore' and 'transStereotypesScore'
data3 <- data3 %>%
  mutate(transAttitudesScore = genderFairnessScore + transStereotypesScore + fearOfTransPeopleScore)
# the higher the score, the more transphobic the person is!

# Make a new column for the score of all controls together
data3 <- data3 %>%
  mutate(
    ControlsScore = generalFairnessScore + EqualityScore )
# the higher the score, the more they care about fairness and equality

# Create a new column to assign participants to groups based on targetStimuli
data3 <- data3%>%
  mutate(
    TargetStimuliGroup = case_when(
      targetStimuli %in% c(1, 2) ~ "Prison",
      targetStimuli %in% c(3, 4) ~ "Education",
      TRUE ~ NA_character_ # 
    )
  )

# View the updated dataset
head(data3)


# Filter data for trans attitudes and controls of the group who chose prison
prison_group_scores <- data3 %>%
  filter(TargetStimuliGroup == "Prison") %>%
  select(participantID, transAttitudesScore, ControlsScore)

# View the table of scores for the TargetStimuli_Prison group
print(prison_group_scores)


# Filter data for tans attitudes and controls of the group who chose education programs
education_group_scores <- data3 %>%
  filter(TargetStimuliGroup == "Education") %>%
  select(participantID, transAttitudesScore, ControlsScore)

# View the table of scores for the TargetStimuli_Education group
print(education_group_scores)


# Average scores for both groups for transAttitudesScore and for ControlsScore
# Filter for TargetStimuli_Prison group
prison_group <- data3 %>% filter(TargetStimuliGroup == "Prison")

# Calculate averages for Prison group
avg_prison <- prison_group %>%
  summarise(
    avg_transAttitudesScore = mean(transAttitudesScore, na.rm = TRUE),
    avg_ControlsScore = mean(ControlsScore, na.rm = TRUE)
  )

cat(
  "Average Trans Attitudes Score (Prison Group):", avg_prison$avg_transAttitudesScore, "\n",
  "Average Controls Score (Prison Group):", avg_prison$avg_ControlsScore, "\n"
) # average transAttitudesScore in Prison group: 20.25 average ControlsScore: 14.25


# Filter for Education group
education_group <- data3 %>% filter(TargetStimuliGroup == "Education")

# Calculate averages for Education group
avg_education <- education_group %>%
  summarise(
    avg_transAttitudesScore = mean(transAttitudesScore, na.rm = TRUE),
    avg_ControlsScore = mean(ControlsScore, na.rm = TRUE)
  )

cat(
  "Average Trans Attitudes Score (Education Group):", avg_education$avg_transAttitudesScore, "\n",
  "Average Controls Score (Education Group):", avg_education$avg_ControlsScore, "\n"
) #transAttitudesScore average: 18.7, ControlsScoreAverage: 13.6


# Create a new column for the difference between transStereotypesScore and cisStereotypesScore
data3 <- data3 %>%
  mutate(transVSCisDifference = transStereotypesScore - cisStereotypesControl)

# View the updated dataset
head(data3)

summary(data3$transStereotypesScore)
summary(data3$cisStereotypesControl)
nrow(prison_group)
data3 <- data3 %>%
  mutate(
    transVSCisDifference = ifelse(is.na(transStereotypesScore) | is.na(cisStereotypesControl), 
                                  NA, transStereotypesScore - cisStereotypesControl)
  )

# Calculate average for transVSCisDifference
prison_group <- data3 %>% filter(TargetStimuliGroup == "Prison")
education_group <- data3 %>% filter(TargetStimuliGroup == "Education")

avg_prison <- prison_group %>%
  summarise(avg_transVSCisDifference = mean(transVSCisDifference, na.rm = TRUE))

avg_education <- education_group %>%
  summarise(avg_transVSCisDifference = mean(transVSCisDifference, na.rm = TRUE))

cat(
  "Average transVSCisDifference (Prison Group):", avg_prison$avg_transVSCisDifference, "\n",
  "Average transVSCisDifference (Education Group):", avg_education$avg_transVSCisDifference, "\n"
) # Prison group average transVSCisDifference: 3.75
# Education group average transVSCisDifference: 0.0.3


# Create a new dataset with only the selected columns
selected_columns <- c(
  "participantID",
  "TargetStimuliGroup",
  "targetStimuli",
  "transStereotypesScore",
  "cisStereotypesControl",
  "transAttitudesScore",
  "transVSCisDifference",
  "ControlsScore",
  "person-age",
  "person-sexual-orientation",
  "person-gender",
  "person-cis-trans"
)

# Select the columns and create a new dataset
temporary_data3 <- data3[, selected_columns]

# Create a new column ControlsAttitudes based on ControlsScore
temporary_data3 <- temporary_data3 %>%
  mutate(ControlsAttitudes = ifelse(ControlsScore <= 9, "negative", "positive"))

# Create a new column transAttitudes based on the conditions
temporary_data3 <- temporary_data3 %>%
  mutate(transAttitudes = if_else(transAttitudesScore <= 36, "positive", "negative"))

# View the first few rows to check the data
head(temporary_data3)

# Rearranging columns in the specified order
temporary_data3 <- temporary_data3 %>%
  select(
    "participantID",
    "TargetStimuliGroup", 
    "targetStimuli",
    "transStereotypesScore",
    "transAttitudesScore",
    "transAttitudes",
    "cisStereotypesControl",
    "transVSCisDifference",
    "ControlsScore",
    "ControlsAttitudes",
    "person-age",
    "person-sexual-orientation",
    "person-gender",
    "person-cis-trans"
  )

# Filter participants with "positive" in transAttitudes
positive_trans_attitudes <- temporary_data3 %>%
  filter(transAttitudes == "positive") %>%
  select(participantID, TargetStimuliGroup, transAttitudes)

# View the result
print(positive_trans_attitudes, n = Inf)


# Filter participants with "negative" in transAttitudes
negative_trans_attitudes <- temporary_data3 %>%
  filter(transAttitudes == "negative") %>%
  select(participantID, TargetStimuliGroup, transAttitudes)

# View the result
print(negative_trans_attitudes)

# Filter for participants with "negative" ControlsAttitudes who chose "Prison"
negative_controls_prison <- temporary_data3 %>%
  filter(ControlsAttitudes == "negative", TargetStimuliGroup == "Prison") %>%
  select(participantID, ControlsAttitudes, TargetStimuliGroup)

# View the results
print(negative_controls_prison)
# 0 participants with negative ControlsAttitudes

# Filter for participants with "negative" ControlsAttitudes who chose "Education"
negative_controls_education <- temporary_data3 %>%
  filter(ControlsAttitudes == "negative", TargetStimuliGroup == "Education") %>%
  select(participantID, ControlsAttitudes, TargetStimuliGroup)

# View the results
print(negative_controls_education, Inf)
# 0 participants with negative ControlsAttitudes


# Filter for participants with "negative" ControlsAttitudes and "negative" transAttitudes
negative_controls_transAttitudes <- temporary_data3 %>%
  filter(ControlsAttitudes == "negative", transAttitudes == "negative") %>%
  select(participantID, ControlsAttitudes, transAttitudes)

# View the results
print(negative_controls_transAttitudes, Inf)
# 0 participants 

# Filter for participants with "negative" and "positive" transAttitudes
negative_controls_positive_transAttitudes <- temporary_data3 %>%
  filter(ControlsAttitudes == "negative", transAttitudes == "positive") %>%
  select(participantID, ControlsAttitudes, transAttitudes)

# View the results
print(negative_controls_positive_transAttitudes, Inf)
# 0 participant 

# Correlation between "Prison" in TargetStimuliGroup and "negative" in transAttitudes
#assigning numeric value to the target stimulus
temporary_data3 <- temporary_data3 %>%
  mutate(Prison0Education1 = ifelse(TargetStimuliGroup == "Prison", 0, 1))

# Add the PoliticalOrientation column and set it to "Liberal" for all rows
temporary_data3 <- temporary_data3 %>%
  mutate(person_PoliticalOrientation = "Liberal")


#assigning numeric value to transAttitudes
temporary_data3 <- temporary_data3 %>%
  mutate(transAttitudes0_1 = ifelse(transAttitudes == "negative", 0, 1))

# Calculate the point-biserial correlation
correlation_result <- cor.test(
  temporary_data3$Prison0Education1,
  temporary_data3$transAttitudes0_1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result$estimate, "\n")
cat("P-value:", correlation_result$p.value, "\n")
# Correlation Coefficient: NA because the attitudes of all participants were positive
# P-value: NA

# Create a new column 'person-politics'
temporary_data3 <- temporary_data3 %>%
  mutate(person_politics = if_else(person_PoliticalOrientation == "Liberal", 1, 0))

# New column with 0 = Man, 1 = woman
temporary_data3 <- temporary_data3 %>%
  mutate(Men0Women1 = ifelse(`person-gender` == "Man", 0, 1))


# correlation of gender and target stimulus choice 
# Calculate the correlation between Men0Women1 and Conservative0Liberal1
correlation_result_gender <- cor.test(
  temporary_data3$Men0Women1, 
  temporary_data3$Prison0Education1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation_result_gender$estimate, "\n")
cat("P-value:", correlation_result_gender$p.value, "\n")
# Correlation Coefficient: -0.01026735   
# p-value: 0.9533202  

# New column with 0 = Hetero, 1 = queer (all of them)
temporary_data3 <- temporary_data3 %>%
  mutate(Hetero0Queer1 = ifelse(`person-sexual-orientation` == "Straight or heterosexual", 0, 1))

# Calculate the correlation between Prison0Education1 and Sexual Orientation
correlation_results_queer <- cor.test(
  temporary_data3$Hetero0Queer1, 
  temporary_data3$Prison0Education1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation_results_queer$estimate, "\n")
cat("P-value:", correlation_results_queer$p.value, "\n")
# Correlation Coefficient: 0.2431867
# p-value: 0.1592207 


# Calculate the correlation between trans attitudes and Sexual Orientation
correlation_results_queer <- cor.test(
  temporary_data3$Hetero0Queer1, 
  temporary_data3$transAttitudes0_1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation_results_queer$estimate, "\n")
cat("P-value:", correlation_results_queer$p.value, "\n")
# Correlation Coefficient: NA   
# p-value: NA


# Correlation between TargetStimuliGroup and Controls
#assigning numeric value to the controls
temporary_data3 <- temporary_data3 %>%
  mutate(Controls0_1 = ifelse(ControlsAttitudes == "negative", 0, 1))

# Calculate the point-biserial correlation
correlation_result_controls <- cor.test(
  temporary_data3$Prison0Education1,
  temporary_data3$Controls0_1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result_controls$estimate, "\n")
cat("P-value:", correlation_result_controls$p.value, "\n")
# Correlation Coefficient: NA, all controls attitudes are positive 
# P-value: NA, all controls attitudes are positive 

# Correlation between Trans attitudes and Controls
# Calculate the point-biserial correlation
correlation_result_transVScontrols <- cor.test(
  temporary_data3$transAttitudes0_1,
  temporary_data3$Controls0_1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result_transVScontrols$estimate, "\n")
cat("P-value:", correlation_result_transVScontrols$p.value, "\n")
# Correlation Coefficient: NA, all controls attitudes are positive 
# P-value: NA, all controls attitudes are positive 



# Rearranging columns in the specified order
temporary_data3 <- temporary_data3 %>%
  select(
    "participantID",
    "TargetStimuliGroup", 
    "targetStimuli",
    "Prison0Education1",
    "transStereotypesScore",
    "transAttitudesScore",
    "transAttitudes",
    "transAttitudes0_1",
    "cisStereotypesControl",
    "transVSCisDifference",
    "ControlsScore",
    "ControlsAttitudes",
    "person-age",
    "person_PoliticalOrientation",
    "person_politics",
    "person-sexual-orientation",
    "person-gender",
    "person-cis-trans"
  )

# save as a new updated dataset
write.csv(temporary_data3, "updated_data3.csv", row.names = FALSE)

# Questionnaire 4/4
  
# set working directory to directory of script
setwd(this.dir)

# load the data from 
data4 = read_csv("../data/dwcons4_4.csv") 
nrow(data4) #35 participants
head(data4)
summary(data4) 

# Step 2: Replace Timestamp with Participant ID
colnames(data4)[1] <- "participantID" # Rename the column
data4$participantID <- match(data4$participantID, unique(data4$participantID)) # Assign unique IDs
print(table(data4$participantID)) # View distribution of participant IDs


# Step 3: Rename Columns
colnames(data4)[2] <- "prisonVSeducationprogram"
colnames(data4)[3] <- "feel-strongly"
colnames(data4)[4] <- "trans-stereotypes-confused"
colnames(data4)[5] <- "trans-stereotypes-mentally-ill"
colnames(data4)[6] <- "trans-stereotypes-dangerous"
colnames(data4)[7] <- "trans-stereotypes-frauds"
colnames(data4)[8] <- "trans-stereotypes-unnatural"
colnames(data4)[9] <- "cis-stereotypes-confused"
colnames(data4)[10] <- "cis-stereotypes-mentally-ill"
colnames(data4)[11] <- "cis-stereotypes-dangerous"
colnames(data4)[12] <- "cis-stereotypes-frauds"
colnames(data4)[13] <- "cis-stereotypes-unnatural"
colnames(data4)[14] <- "gender-fairness-apartment"
colnames(data4)[15] <- "gender-fairness-sports"
colnames(data4)[16] <- "gender-fairness-street-harassment"
colnames(data4)[17] <- "gender-fairness-bullying"
colnames(data4)[18] <- "general-fairness-education"
colnames(data4)[19] <- "general-fairness-healthcare"
colnames(data4)[20] <- "fear-number-of-trans-people"
colnames(data4)[21] <- "fear-all-problems"
colnames(data4)[22] <- "equality-equal-chance"
colnames(data4)[23] <- "equality-shouldnt-worry"
colnames(data4)[24] <- "person-age"
colnames(data4)[25] <- "person-sexual-orientation"
colnames(data4)[26] <- "person-gender"
colnames(data4)[27] <- "person-cis-trans"


# Ensure all columns being pivoted have the same data type
data4 <- data4 %>%
  mutate(across(2:23, as.character)) # Convert columns 2 to 23 to character

# Pivot the data
data4_long <- data4 %>%
  pivot_longer(
    cols = 2:23, # Adjust this range based on your column selection
    names_to = "question",
    values_to = "response"
  )

# View the reshaped data
print(data4_long)
head(data4_long)
sum(is.na(data4_long$response))
unique(data4_long$question)


# now that we see that tmp works, make d = tmp
ls()
tmp4 <- data4

#assigning value to the target stimulus
data4 <- data4 %>%
  mutate(targetStimuli = case_when(
    prisonVSeducationprogram == "Building new prisons" & `feel-strongly` == "I feel very strongly about this." ~ 1,
    prisonVSeducationprogram == "Building new prisons" & `feel-strongly` == "I feel not very strongly about this." ~ 2,
    prisonVSeducationprogram == "Education programs" & `feel-strongly` == "I feel not very strongly about this." ~ 3,
    prisonVSeducationprogram == "Education programs" & `feel-strongly` == "I feel very strongly about this." ~ 4,
    TRUE ~ NA_real_ # Assign NA for any unmatched cases
  ))


# Print all column names to confirm exact names
colnames(data4)

# Filter for participants with targetStimuli 1 count them
targetStimuli_counts <- data4 %>%
  filter(targetStimuli %in% c(1)) %>%
  summarise(count = n())

# Print the result
print(targetStimuli_counts)

# Filter for participants with targetStimuli 2 count them
targetStimuli_counts <- data4 %>%
  filter(targetStimuli %in% c(2)) %>%
  summarise(count = n())

# Print the result
print(targetStimuli_counts)
# Filter for participants with targetStimuli 1 and 2 count them
targetStimuli_counts <- data4 %>%
  filter(targetStimuli %in% c(1, 2)) %>%
  summarise(count = n())

# Print the result
print(targetStimuli_counts)

# Filter for participants with targetStimuli 3 and count them
targetStimuli_counts <- data4 %>%
  filter(targetStimuli %in% c(3)) %>%
  summarise(count = n())

# Print the result
print(targetStimuli_counts)

# Filter for participants with targetStimuli 4 count them
targetStimuli_counts <- data4 %>%
  filter(targetStimuli %in% c(4)) %>%
  summarise(count = n())

# Print the result
print(targetStimuli_counts)

# Filter for participants with targetStimuli 3 and 4 count them
targetStimuli_counts <- data4 %>%
  filter(targetStimuli %in% c(3, 4)) %>%
  summarise(count = n())

# Print the result
print(targetStimuli_counts)



# Convert trans-stereotypes columns to numeric
data4 <- data4 %>%
  mutate(across(c(`trans-stereotypes-confused`, 
                  `trans-stereotypes-mentally-ill`, 
                  `trans-stereotypes-dangerous`, 
                  `trans-stereotypes-frauds`, 
                  `trans-stereotypes-unnatural`), 
                as.numeric))


# Sum the responses while handling missing values
data4 <- data4 %>%
  mutate(trans_stereotypes = rowSums(select(., 
                                            `trans-stereotypes-confused`, 
                                            `trans-stereotypes-mentally-ill`, 
                                            `trans-stereotypes-dangerous`, 
                                            `trans-stereotypes-frauds`, 
                                            `trans-stereotypes-unnatural`), 
                                     na.rm = TRUE))
# Confirm the change
colnames(data4)
summary(data4$trans_stereotypes)


# Convert cis-stereotypes columns to numeric
data4 <- data4 %>%
  mutate(across(c(`cis-stereotypes-confused`, 
                  `cis-stereotypes-mentally-ill`, 
                  `cis-stereotypes-dangerous`, 
                  `cis-stereotypes-frauds`, 
                  `cis-stereotypes-unnatural`), 
                as.numeric))


# Apply scoring and sum the responses into a new column
data4 <- data4 %>%
  mutate(across(`cis-stereotypes-confused`:`cis-stereotypes-unnatural`, as.numeric)) %>%
  mutate(cisStereotypesControl = rowSums(select(., 
                                                `cis-stereotypes-confused`, 
                                                `cis-stereotypes-mentally-ill`, 
                                                `cis-stereotypes-dangerous`, 
                                                `cis-stereotypes-frauds`, 
                                                `cis-stereotypes-unnatural`), na.rm = TRUE))


# Rename the column trans_stereotypes to transStereotypesScore
data4 <- data4 %>%
  rename(transStereotypesScore = trans_stereotypes)

#Gender Fairness: part of the trans attitudes test and consists of one yes/no question and 3 questions with a 1-7 scale
# Assign points for gender-fairness-apartment
data4 <- data4 %>%
  mutate(
    gender_fairness_apartment_score = ifelse(`gender-fairness-apartment` == "Yes", 1, 2)
  )

# For the reverse coded columns, assign points according to the reverse coding scheme
data4 <- data4 %>%
  mutate(
    gender_fairness_sports_score = 8 - as.numeric(`gender-fairness-sports`),
    gender_fairness_street_harassment_score = 8 - as.numeric(`gender-fairness-street-harassment`),
    gender_fairness_bullying_score = 8 - as.numeric(`gender-fairness-bullying`)
  )

# Combine the four scores into a new column: genderFairnessScore
data4 <- data4 %>%
  mutate(
    genderFairnessScore = gender_fairness_apartment_score +
      gender_fairness_sports_score +
      gender_fairness_street_harassment_score +
      gender_fairness_bullying_score
  )


# Remove specified columns from the dataset
data4 <- data4 %>%
  select(-gender_fairness_apartment_score, 
         -gender_fairness_street_harassment_score, 
         -gender_fairness_bullying_score,
         -gender_fairness_sports_score)

#General Fairness:Control
colnames(data4)

# Combine and code general-fairness-education and general-fairness-healthcare
# Combine points for general fairness education and healthcare
data4 <- data4 %>%
  mutate(
    generalFairnessScore = rowSums(
      mutate(
        select(data4, `general-fairness-education`, `general-fairness-healthcare`),
        across(everything(), ~ case_when(
          . == "Strongly agree" ~ 1,
          . == "Somewhat agree" ~ 2,
          . == "Somewhat disagree" ~ 3,
          . == "Strongly disagreee" ~ 4,
          TRUE ~ 0
        ))
      )
    )
  )




# Combine and code the 'fearOfTransPeopleScore' column
data4 <- data4 %>%
  mutate(
# Assign points to 'fear-number-of-trans-people'
fear_number_of_trans_people_score = case_when(
      `fear-number-of-trans-people` == "The number of trans people increased" ~ 3,
      `fear-number-of-trans-people` == "The number of trans people stayed about the same" ~ 2,
      `fear-number-of-trans-people` == "The number of trans people decreased" ~ 1,
      TRUE ~ NA_real_
    ),
    
# Assign points to 'fear-all-problems'
fear_all_problems_score = case_when(
      `fear-all-problems` == "It is no more important than other problems" ~ 2,
      `fear-all-problems` == "It is the most important problem" ~ 1,
      `fear-all-problems` == "It is less important than other problems" ~ 3,
      TRUE ~ NA_real_
    ),
    
# Combine the two scores into 'fearOfTransPeopleScore'
fearOfTransPeopleScore = fear_number_of_trans_people_score + fear_all_problems_score)

# Combine and code the 'EqualityScore' column
data4 <- data4 %>%
  mutate(
# Assign points to 'equality-equal-chance'
equality_equal_chance_score = case_when(
      `equality-equal-chance` == "Strongly agree" ~ 4,
      `equality-equal-chance` == "Somewhat agree" ~ 3,
      `equality-equal-chance` == "Somewhat disagree" ~ 2,
      `equality-equal-chance` == "Strongly disagree" ~ 1,
      TRUE ~ NA_real_
    ),
    
# Assign points to 'equality-shouldnt-worry'
equality_shouldnt_worry_score = case_when(
      `equality-shouldnt-worry` == "Strongly agree" ~ 1,
      `equality-shouldnt-worry` == "Somewhat agree" ~ 2,
      `equality-shouldnt-worry` == "Somewhat disagree" ~ 3,
      `equality-shouldnt-worry` == "Strongly disagree" ~ 4,
      TRUE ~ NA_real_
    ),
    
# Combine the two scores into 'EqualityScore'
EqualityScore = equality_equal_chance_score + equality_shouldnt_worry_score
  )


# View the first 20 rows to check the values in person-gender column
head(data4[, 26], 20)

#count how many men and women participants we have
library(dplyr)

# Count the number of men and women in the dataset
data4 %>%
  group_by(`person-gender`) %>%
  summarise(count = n()) # 18 men, 17 women


# Removing unwanted columns from the dataset
data4 <- data4 %>%
  select(-fear_number_of_trans_people_score,
         -fear_all_problems_score,
         -equality_equal_chance_score,
         -equality_shouldnt_worry_score)


# Create the new column 'transAttitudesScore' by summing 'genderFairnessScore' and 'transStereotypesScore'
data4 <- data4 %>%
  mutate(transAttitudesScore = genderFairnessScore + transStereotypesScore + fearOfTransPeopleScore)
# the higher the score, the more transphobic the person is!

# Make a new column for the score of all controls together
data4 <- data4 %>%
  mutate(
    ControlsScore = generalFairnessScore + EqualityScore)
# the higher the score, the more they care about fairness and equality

# Create the new column 'transAttitudesScore' by summing 'genderFairnessScore' and 'transStereotypesScore'
data4 <- data4 %>%
  mutate(transAttitudesScore = genderFairnessScore + transStereotypesScore + fearOfTransPeopleScore)
# the higher the score, the more transphobic the person is!

# Make a new column for the score of all controls together
data4 <- data4 %>%
  mutate(
    ControlsScore = generalFairnessScore + EqualityScore )
# the higher the score, the more they care about fairness and equality

# Create a new column to assign participants to groups based on targetStimuli
data4 <- data4%>%
  mutate(
    TargetStimuliGroup = case_when(
      targetStimuli %in% c(1, 2) ~ "Prison",
      targetStimuli %in% c(3, 4) ~ "Education",
      TRUE ~ NA_character_ # 
    )
  )

# View the updated dataset
head(data4)


# Filter data for trans attitudes and controls of the group who chose prison
prison_group_scores <- data4 %>%
  filter(TargetStimuliGroup == "Prison") %>%
  select(participantID, transAttitudesScore, ControlsScore)

# View the table of scores for the TargetStimuli_Prison group
print(prison_group_scores)


# Filter data for tans attitudes and controls of the group who chose education programs
education_group_scores <- data4 %>%
  filter(TargetStimuliGroup == "Education") %>%
  select(participantID, transAttitudesScore, ControlsScore)

# View the table of scores for the TargetStimuli_Education group
print(education_group_scores)


# Average scores for both groups for transAttitudesScore and for ControlsScore
# Filter for TargetStimuli_Prison group
prison_group <- data4 %>% filter(TargetStimuliGroup == "Prison")

# Calculate averages for Prison group
avg_prison <- prison_group %>%
  summarise(
    avg_transAttitudesScore = mean(transAttitudesScore, na.rm = TRUE),
    avg_ControlsScore = mean(ControlsScore, na.rm = TRUE)
  )

cat(
  "Average Trans Attitudes Score (Prison Group):", avg_prison$avg_transAttitudesScore, "\n",
  "Average Controls Score (Prison Group):", avg_prison$avg_ControlsScore, "\n"
) # average transAttitudesScore in Prison group: 50.5 average ControlsScore: 9.4


# Filter for Education group
education_group <- data4 %>% filter(TargetStimuliGroup == "Education")

# Calculate averages for Education group
avg_education <- education_group %>%
  summarise(
    avg_transAttitudesScore = mean(transAttitudesScore, na.rm = TRUE),
    avg_ControlsScore = mean(ControlsScore, na.rm = TRUE)
  )

cat(
  "Average Trans Attitudes Score (Education Group):", avg_education$avg_transAttitudesScore, "\n",
  "Average Controls Score (Education Group):", avg_education$avg_ControlsScore, "\n"
) #transAttitudesScore average: 44.5, ControlsScoreAverage: 10.2


# Create a new column for the difference between transStereotypesScore and cisStereotypesScore
data4 <- data4 %>%
  mutate(transVSCisDifference = transStereotypesScore - cisStereotypesControl)

# View the updated dataset
head(data4)

summary(data4$transStereotypesScore)
summary(data4$cisStereotypesControl)
nrow(prison_group)
data4 <- data4 %>%
  mutate(
    transVSCisDifference = ifelse(is.na(transStereotypesScore) | is.na(cisStereotypesControl), 
                                  NA, transStereotypesScore - cisStereotypesControl)
  )

# Recalculate average for transVSCisDifference
prison_group <- data4 %>% filter(TargetStimuliGroup == "Prison")
education_group <- data4 %>% filter(TargetStimuliGroup == "Education")

avg_prison <- prison_group %>%
  summarise(avg_transVSCisDifference = mean(transVSCisDifference, na.rm = TRUE))

avg_education <- education_group %>%
  summarise(avg_transVSCisDifference = mean(transVSCisDifference, na.rm = TRUE))

cat(
  "Average transVSCisDifference (Prison Group):", avg_prison$avg_transVSCisDifference, "\n",
  "Average transVSCisDifference (Education Group):", avg_education$avg_transVSCisDifference, "\n"
) # Prison group average transVSCisDifference: 18.5
# Education group average transVSCisDifference: 14.7


# Create a new dataset with only the selected columns
selected_columns <- c(
  "participantID",
  "TargetStimuliGroup",
  "targetStimuli",
  "transStereotypesScore",
  "cisStereotypesControl",
  "transAttitudesScore",
  "transVSCisDifference",
  "ControlsScore",
  "person-age",
  "person-sexual-orientation",
  "person-gender",
  "person-cis-trans"
)

# Select the columns and create a new dataset
temporary_data4 <- data4[, selected_columns]

# View the first few rows to check the data
head(temporary_data4)

# Create a new column transAttitudes based on the conditions
temporary_data4 <- temporary_data4 %>%
  mutate(transAttitudes = if_else(transAttitudesScore <= 36, "positive", "negative"))



# Create a new column ControlsAttitudes based on ControlsScore
temporary_data4 <- temporary_data4 %>%
  mutate(ControlsAttitudes = ifelse(ControlsScore <= 9, "negative", "positive"))

# Rearranging columns in the specified order
temporary_data4 <- temporary_data4 %>%
  select(
    "participantID",
    "TargetStimuliGroup", 
    "targetStimuli",
    "transStereotypesScore",
    "transAttitudesScore",
    "transAttitudes",
    "cisStereotypesControl",
    "transVSCisDifference",
    "ControlsScore",
    "ControlsAttitudes",
    "person-age",
    "person-sexual-orientation",
    "person-gender",
    "person-cis-trans"
  )

# Filter participants with "positive" in transAttitudes
positive_trans_attitudes <- temporary_data4 %>%
  filter(transAttitudes == "positive") %>%
  select(participantID, TargetStimuliGroup, transAttitudes)

# View the result
print(positive_trans_attitudes, n = Inf)


# Filter participants with "negative" in transAttitudes
negative_trans_attitudes <- temporary_data4 %>%
  filter(transAttitudes == "negative") %>%
  select(participantID, TargetStimuliGroup, transAttitudes)

# View the result
print(negative_trans_attitudes, n = Inf)

# Filter for participants with "negative" ControlsAttitudes who chose "Prison"
negative_controls_prison <- temporary_data4 %>%
  filter(ControlsAttitudes == "negative", TargetStimuliGroup == "Prison") %>%
  select(participantID, ControlsAttitudes, TargetStimuliGroup)

# View the results
print(negative_controls_prison, Inf)
# 7 participants (1, 11, 13, 17, 25, 33, 34)

# Filter for participants with "negative" ControlsAttitudes who chose "Education"
negative_controls_education <- temporary_data4 %>%
  filter(ControlsAttitudes == "negative", TargetStimuliGroup == "Education") %>%
  select(participantID, ControlsAttitudes, TargetStimuliGroup)

# View the results
print(negative_controls_education, Inf)
# 6 participants (2,3,4,15,18,28)


# Filter for participants with "negative" ControlsAttitudes and "negative" transAttitudes
negative_controls_transAttitudes <- temporary_data4 %>%
  filter(ControlsAttitudes == "negative", transAttitudes == "negative") %>%
  select(participantID, ControlsAttitudes, transAttitudes)

# View the results
print(negative_controls_transAttitudes, Inf)
# 12 participants (1,2,3,4,11,13,15,17,25,28,33,34)

# Filter for participants with "negative" and "positive" transAttitudes
negative_controls_positive_transAttitudes <- temporary_data4 %>%
  filter(ControlsAttitudes == "negative", transAttitudes == "positive") %>%
  select(participantID, ControlsAttitudes, transAttitudes)

# View the results
print(negative_controls_positive_transAttitudes, Inf)
# 1 participant (18)

# Correlation between "Prison" in TargetStimuliGroup and "negative" in transAttitudes
#assigning numeric value to the target stimulus
temporary_data4 <- temporary_data4 %>%
  mutate(Prison0Education1 = ifelse(TargetStimuliGroup == "Prison", 0, 1))

#assigning numeric value to transAttitudes
temporary_data4 <- temporary_data4 %>%
  mutate(transAttitudes0_1 = ifelse(transAttitudes == "negative", 0, 1))

# Calculate the point-biserial correlation
correlation_result <- cor.test(
  temporary_data4$Prison0Education1,
  temporary_data4$transAttitudes0_1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result$estimate, "\n")
cat("P-value:", correlation_result$p.value, "\n")
# Correlation Coefficient: 0.3154466
# P-value: 0.06490998 

# New column with 0 = Man, 1 = woman
temporary_data4 <- temporary_data4 %>%
  mutate(Men0Women1 = ifelse(`person-gender` == "Man", 0, 1))

# correlation of gender and target stimulus choice 
# Calculate the correlation between Men0Women1 and Conservative0Liberal1
correlation_result_gender <- cor.test(
  temporary_data4$Men0Women1, 
  temporary_data4$Prison0Education1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation_result_gender$estimate, "\n")
cat("P-value:", correlation_result_gender$p.value, "\n")
# Correlation Coefficient: 0.3180345   
# p-value: 0.06262339  


# Add the PoliticalOrientation column and set it to "Liberal" for all rows
temporary_data4 <- temporary_data4 %>%
  mutate(person_PoliticalOrientation = "Conservative")

# New column with 0 = Hetero, 1 = queer (all of them)
temporary_data4 <- temporary_data4 %>%
  mutate(Hetero0Queer1 = ifelse(`person-sexual-orientation` == "Straight or heterosexual", 0, 1))

# Calculate the correlation between Prison0Education1 and Sexual Orientation
correlation_results_queer <- cor.test(
  temporary_data4$Hetero0Queer1, 
  temporary_data4$Prison0Education1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation_results_queer$estimate, "\n")
cat("P-value:", correlation_results_queer$p.value, "\n")
# Correlation Coefficient: NA
# p-value: NA


# Calculate the correlation between trans attitudes and Sexual Orientation
correlation_results_queer <- cor.test(
  temporary_data4$Hetero0Queer1, 
  temporary_data4$transAttitudes0_1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation_results_queer$estimate, "\n")
cat("P-value:", correlation_results_queer$p.value, "\n")
# Correlation Coefficient: NA   
# p-value: NA

# Correlation between TargetStimuliGroup and Controls
#assigning numeric value to the controls
temporary_data4 <- temporary_data4 %>%
  mutate(Controls0_1 = ifelse(ControlsAttitudes == "negative", 0, 1))

# Calculate the point-biserial correlation
correlation_result_controls <- cor.test(
  temporary_data4$Prison0Education1,
  temporary_data4$Controls0_1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result_controls$estimate, "\n")
cat("P-value:", correlation_result_controls$p.value, "\n")
# Correlation Coefficient: 0.1254821 
# P-value: 0.4726042 

# Correlation between Trans attitudes and Controls
# Calculate the point-biserial correlation
correlation_result_transVScontrols <- cor.test(
  temporary_data4$transAttitudes0_1,
  temporary_data4$Controls0_1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result_transVScontrols$estimate, "\n")
cat("P-value:", correlation_result_transVScontrols$p.value, "\n")
# Correlation Coefficient: 0.236525 
# P-value: 0.1713172


# Rearranging columns in the specified order
temporary_data4 <- temporary_data4 %>%
  select(
    "participantID",
    "TargetStimuliGroup", 
    "targetStimuli",
    "Prison0Education1",
    "transStereotypesScore",
    "transAttitudesScore",
    "transAttitudes",
    "transAttitudes0_1",
    "cisStereotypesControl",
    "transVSCisDifference",
    "ControlsScore",
    "ControlsAttitudes",
    "person-age",
    "person_PoliticalOrientation",
    "person-sexual-orientation",
    "person-gender",
    "person-cis-trans"
  )

# save as a new updated dataset
write.csv(temporary_data4, "updated_data4.csv", row.names = FALSE)


# Combine temporary_data3 and temporary_data4 to view the results in dogwhistle condition based on partisanship
dogwhistle_temporary_data <- bind_rows(temporary_data3, temporary_data4)

# Correlation between target Stimulus and trans Attitudes in dogwhistle condition
# Calculate the point-biserial correlation
correlation_result_target_transAttitudes <- cor.test(
  dogwhistle_temporary_data$Prison0Education1,
  dogwhistle_temporary_data$transAttitudes0_1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result_target_transAttitudes$estimate, "\n")
cat("P-value:", correlation_result_target_transAttitudes$p.value, "\n")
# Correlation Coefficient: 0.4518481
# P-value: 8.619715e-05 
# moderate to strong correlation, statistically significant

# Correlation between political orientation and Controls
#assigning numeric value to the controls
dogwhistle_temporary_data <- dogwhistle_temporary_data %>%
  mutate(Controls0_1 = ifelse(ControlsAttitudes == "negative", 0, 1))

#assigning numeric value to the political Orientation
dogwhistle_temporary_data <- dogwhistle_temporary_data %>%
  mutate(Conservative0Liberal1 = ifelse(person_PoliticalOrientation == "Conservative", 0, 1))

# Calculate the point-biserial correlation
correlation_result_controls <- cor.test(
  dogwhistle_temporary_data$Conservative0Liberal1,
  dogwhistle_temporary_data$Controls0_1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result_controls$estimate, "\n")
cat("P-value:", correlation_result_controls$p.value, "\n")
# Correlation Coefficient: 0.4775669 
# P-value: 2.908919e-05 
# moderate correlation that is statistically significant

# Calculate the point-biserial correlation of response to target stimulus and controls
correlation_result_controls <- cor.test(
  dogwhistle_temporary_data$Prison0Education1,
  dogwhistle_temporary_data$Controls0_1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result_controls$estimate, "\n")
cat("P-value:", correlation_result_controls$p.value, "\n")
# Correlation Coefficient: 0.2671891  
# P-value: 0.0253495 


# Calculate the point-biserial correlation of trans attitudes and controls
correlation_result_controls <- cor.test(
  dogwhistle_temporary_data$transAttitudes0_1,
  dogwhistle_temporary_data$Controls0_1,
  method = "pearson"
)

# Print the results
cat("Correlation Coefficient:", correlation_result_controls$estimate, "\n")
cat("P-value:", correlation_result_controls$p.value, "\n")
# Correlation Coefficient: 0.5099108 
# P-value: 6.517938e-06   

# correlation of political orientation and target stimulus choice 
# Calculate the correlation between Prison0Education1 and Conservative0Liberal1
dogwhistle_correlation_partisanship <- cor.test(
  dogwhistle_temporary_data$Conservative0Liberal1, 
  dogwhistle_temporary_data$Prison0Education1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", dogwhistle_correlation_partisanship$estimate, "\n")
cat("P-value:", dogwhistle_correlation_partisanship$p.value, "\n")
# Correlation Coefficient: 0.3794733  
# p-value: 0.001195992 

# Combine temporary_data3 and temporary_data4 to view the results in dogwhistle condition based on partisanship
# Combine two datasets into one
# New column with 0 = conservative, 1 liberal
dogwhistle_temporary_data <- dogwhistle_temporary_data %>%
  mutate(Conservative0Liberal1 = ifelse(person_PoliticalOrientation == "Conservative", 0, 1))

# Calculate the correlation between trans attitudes and Conservative0Liberal1
dogwhistle_correlation_partisanship_transattitudes <- cor.test(
  dogwhistle_temporary_data$transAttitudes0_1, 
  dogwhistle_temporary_data$Conservative0Liberal1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", dogwhistle_correlation_partisanship_transattitudes$estimate, "\n")
cat("P-value:", dogwhistle_correlation_partisanship_transattitudes$p.value, "\n")
# Correlation Coefficient: 0.8164966 
# p-value: 7.030103e-18 


#All data combined!

#Finding out data about the participants
#Calculate mean age for each dataset
mean_age_data1 <- mean(data1$`person-age`, na.rm = TRUE)
mean_age_data2 <- mean(data2$`person-age`, na.rm = TRUE)
mean_age_data3 <- mean(data3$`person-age`, na.rm = TRUE)
mean_age_data4 <- mean(data4$`person-age`, na.rm = TRUE)

# Display results
cat("Mean age in data1:", mean_age_data1, "\n")
cat("Mean age in data2:", mean_age_data2, "\n")
cat("Mean age in data3:", mean_age_data3, "\n")
cat("Mean age in data4:", mean_age_data4, "\n")

# Combine all datasets into one
all_temporary_data <- bind_rows(temporary_data1, temporary_data2, temporary_data3, temporary_data4)
view(all_temporary_data)

# New column with 0 = conservative, 1 liberal
all_temporary_data <- all_temporary_data %>%
  mutate(Conservative0Liberal1 = ifelse(person_PoliticalOrientation == "Conservative", 0, 1))

# New column with 0 = Man, 1 = woman
all_temporary_data <- all_temporary_data %>%
  mutate(Hetero0Queer1 = ifelse(`person-sexual-orientation` == "Straight or heterosexual", 0, 1))
view(all_temporary_data)

# correlation of political orientation and target stimulus choice 
# Calculate the correlation between Prison0Education1 and Conservative0Liberal1
correlation_results <- cor.test(
  all_temporary_data$Conservative0Liberal1, 
  all_temporary_data$Prison0Education1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation_results$estimate, "\n")
cat("P-value:", correlation_results$p.value, "\n")
# Correlation Coefficient: 0.2924522 
# p-value: 0.0004543893

# New column with 0 = Man, 1 = woman
all_temporary_data <- all_temporary_data %>%
  mutate(Men0Women1 = ifelse(`person-gender` == "Man", 0, 1))
view(all_temporary_data)


# correlation of gender and target stimulus choice 
# Calculate the correlation between Men0Women1 and Conservative0Liberal1
correlation_result_gender <- cor.test(
  all_temporary_data$Men0Women1, 
  all_temporary_data$Prison0Education1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation_result_gender$estimate, "\n")
cat("P-value:", correlation_result_gender$p.value, "\n")
# Correlation Coefficient: 0.04399519  
# p-value: 0.6057559


# correlation of trans attitudes and target stimulus choice in all conservative participants
# Step 1: Filter participants who are "Liberal" in person_PoliticalOrientation
conservative_data <- all_temporary_data %>%
  filter(person_PoliticalOrientation == "Conservative")

# Step 2: Calculate the correlation between transAttitudes0_1 and Prison0Education1
correlation_result <- cor.test(
  conservative_data$transAttitudes0_1, 
  conservative_data$Prison0Education1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation_result$estimate, "\n")
cat("P-value:", correlation_result$p.value, "\n")
# Correlation Coefficient: 0.2498322 
# p-value: 0.03699582 

# Calculate the average of transAttitudesScore in data1
average_transAttitudesScore <- mean(data1$transAttitudesScore, na.rm = TRUE)

# Display the result
cat("Average transAttitudesScore in data1:", average_transAttitudesScore, "\n")
#23.5

# Calculate the average of transAttitudesScore in data2
average_transAttitudesScore <- mean(data2$transAttitudesScore, na.rm = TRUE)

# Display the result
cat("Average transAttitudesScore in data2:", average_transAttitudesScore, "\n")
#43.4

# Calculate the average of transAttitudesScore in data3
average_transAttitudesScore <- mean(data3$transAttitudesScore, na.rm = TRUE)

# Display the result
cat("Average transAttitudesScore in data3:", average_transAttitudesScore, "\n")
#18.9

# Calculate the average of transAttitudesScore in data4
average_transAttitudesScore <- mean(data4$transAttitudesScore, na.rm = TRUE)

# Display the result
cat("Average transAttitudesScore in data4:", average_transAttitudesScore, "\n")
#47.3

# correlation of trans attitudes and target stimulus choice in all liberal participants
# Step 1: Filter participants who are "Liberal" in person_PoliticalOrientation
liberal_data <- all_temporary_data %>%
  filter(person_PoliticalOrientation == "Liberal")

# Step 2: Calculate the correlation between transAttitudes0_1 and Prison0Education1
correlation_result <- cor.test(
  liberal_data$transAttitudes0_1, 
  liberal_data$Prison0Education1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation_result$estimate, "\n")
cat("P-value:", correlation_result$p.value, "\n")
# Correlation Coefficient: 0.328244
# p-value: 0.005534614 

# correlation of trans attitudes and target stimulus choice in all conservative participants
# Step 1: Filter participants who are "Liberal" in person_PoliticalOrientation
conservative_data <- all_temporary_data %>%
  filter(person_PoliticalOrientation == "Conservative")

# Step 2: Calculate the correlation between transAttitudes0_1 and Prison0Education1
correlation_result <- cor.test(
  conservative_data$transAttitudes0_1, 
  conservative_data$Prison0Education1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation_result$estimate, "\n")
cat("P-value:", correlation_result$p.value, "\n")
# Correlation Coefficient: 0.2498322 
# p-value: 0.03699582 



# Calculate the mean age of all participants
mean_age_all <- mean(all_data$`person-age`, na.rm = TRUE)

# Display the result
cat("Mean age of all participants:", mean_age_all, "\n")
# mean age of all participants: 40.19

# Count participants in each dataset
heterosexual_count_data1 <- data1 %>%
  filter(`person-sexual-orientation` == "Straight or heterosexual") %>%
  nrow()

heterosexual_count_data2 <- data2 %>%
  filter(`person-sexual-orientation` == "Straight or heterosexual") %>%
  nrow()

heterosexual_count_data3 <- data3 %>%
  filter(`person-sexual-orientation` == "Straight or heterosexual") %>%
  nrow()

heterosexual_count_data4 <- data4 %>%
  filter(`person-sexual-orientation` == "Straight or heterosexual") %>%
  nrow()

# Combine all datasets and count participants
combined_data <- bind_rows(data1, data2, data3, data4)
heterosexual_count_combined <- combined_data %>%
  filter(`person-sexual-orientation` == "Straight or heterosexual") %>%
  nrow()

# Output results
cat("Number of heterosexual participants in data1:", heterosexual_count_data1, "\n")
cat("Number of heterosexual participants in data2:", heterosexual_count_data2, "\n")
cat("Number of heterosexual participants in data3:", heterosexual_count_data3, "\n")
cat("Number of heterosexual participants in data4:", heterosexual_count_data4, "\n")
cat("Number of heterosexual participants in all combined data:", heterosexual_count_combined, "\n")


# Define sexual orientation labels to count
orientation_labels <- c("Bisexual", "Gay or Lesbian", "Other", "Queer")

# Function to count participants for each orientation in a dataset
count_orientation <- function(data, label) {
  data %>%
    filter(`person-sexual-orientation` == label) %>%
    nrow()
}

# Calculate the correlation between Prison0Education1 and Sexual Orientation
correlation_results_queer <- cor.test(
  all_temporary_data$Hetero0Queer1, 
  all_temporary_data$Prison0Education1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation_results_queer$estimate, "\n")
cat("P-value:", correlation_results_queer$p.value, "\n")
# Correlation Coefficient: 0.2170789  
# p-value: 0.009986937

# Calculate the correlation between trans attitudes and Sexual Orientation
correlation_results_queer <- cor.test(
  all_temporary_data$Hetero0Queer1, 
  all_temporary_data$transAttitudes0_1, 
  method = "pearson"
)

# Display the correlation coefficient and p-value
cat("Correlation Coefficient:", correlation_results_queer$estimate, "\n")
cat("P-value:", correlation_results_queer$p.value, "\n")
# Correlation Coefficient: 0.3743497  
# p-value: 0.3743497

# Loop through labels for each dataset
results <- list()
for (label in orientation_labels) {
  results[[label]] <- list(
    data1 = count_orientation(data1, label),
    data2 = count_orientation(data2, label),
    data3 = count_orientation(data3, label),
    data4 = count_orientation(data4, label),
    combined = count_orientation(bind_rows(data1, data2, data3, data4), label)
  )
}

# Print results
for (label in orientation_labels) {
  cat("\nCounts for", label, "participants:\n")
  cat("Data1:", results[[label]]$data1, "\n")
  cat("Data2:", results[[label]]$data2, "\n")
  cat("Data3:", results[[label]]$data3, "\n")
  cat("Data4:", results[[label]]$data4, "\n")
  cat("Combined:", results[[label]]$combined, "\n")
}
#bi combined: 18, gay or lesbian: 6, other: 2, Queer:1
# Combine all four datasets into one
combined_data <- bind_rows(data1, data2, data3, data4)

# Count the number of men and women
gender_counts <- combined_data %>%
  filter(`person-gender` %in% c("Man", "Woman")) %>% # Include only "Man" and "Woman"
  group_by(`person-gender`) %>%
  summarise(count = n())

# Print results
print(gender_counts) #men: 71. Women: 69

# Filter for participants in the "Prison" group and select their gender
prison_genders_data1 <- temporary_data1 %>%
  filter(TargetStimuliGroup == "Prison") %>%
  select(participantID, 'person-gender')

prison_genders_data2 <- temporary_data2 %>%
  filter(TargetStimuliGroup == "Prison") %>%
  select(participantID, 'person-gender')

prison_genders_data3 <- temporary_data3 %>%
  filter(TargetStimuliGroup == "Prison") %>%
  select(participantID, 'person-gender')

prison_genders_data4 <- temporary_data4 %>%
  filter(TargetStimuliGroup == "Prison") %>%
  select(participantID, 'person-gender')

# Combine the results
prison_genders_all <- bind_rows(
  prison_genders_data1,
  prison_genders_data2,
  prison_genders_data3,
  prison_genders_data4
)

# View the results for data1
print(prison_genders_data1, n = Inf)
# View the results for data2
print(prison_genders_data2, n = Inf)
# View the results for data3
print(prison_genders_data3, n = Inf)
# View the results for data4
print(prison_genders_data4, n = Inf)
# View the combined data
print(prison_genders_all, n = Inf)

# Filter for participants in the "Prison" group and find out their transVSCisStereotypes
prison_difference_data1 <- temporary_data1 %>%
  filter(TargetStimuliGroup == "Prison") %>%
  select(participantID, 'transVSCisDifference')

prison_difference_data2 <- temporary_data2 %>%
  filter(TargetStimuliGroup == "Prison") %>%
  select(participantID, 'transVSCisDifference')

prison_difference_data3 <- temporary_data3 %>%
  filter(TargetStimuliGroup == "Prison") %>%
  select(participantID, 'transVSCisDifference')

prison_difference_data4 <- temporary_data4 %>%
  filter(TargetStimuliGroup == "Prison") %>%
  select(participantID, 'transVSCisDifference')

# Combine the results
prison_difference_all <- bind_rows(
  prison_difference_data1,
  prison_difference_data2,
  prison_difference_data3,
  prison_difference_data4
)

# View the results for data1
print(prison_difference_data1, n = Inf)
# View the results for data2
print(prison_difference_data2, n = Inf)
# View the results for data3
print(prison_difference_data3, n = Inf)
# View the results for data4
print(prison_difference_data4, n = Inf)

# Define a function to calculate the average transVSCisDifference for "Prison" group
calculate_average_prison <- function(data) {
  data %>%
    filter(TargetStimuliGroup == "Prison") %>%
    summarise(avg_transVSCisDifference = mean(transVSCisDifference, na.rm = TRUE)) %>%
    mutate(dataset = deparse(substitute(data))) # Add dataset name
}

# Apply the function to each dataset
avg_prison_data1 <- calculate_average_prison(temporary_data1)
avg_prison_data2 <- calculate_average_prison(temporary_data2)
avg_prison_data3 <- calculate_average_prison(temporary_data3)
avg_prison_data4 <- calculate_average_prison(temporary_data4)

# Combine the results into a single table
all_avg_prison <- bind_rows(avg_prison_data1, avg_prison_data2, avg_prison_data3, avg_prison_data4)

# Print the result
print(all_avg_prison)
print(avg_prison_data1)
print(avg_prison_data2)
print(avg_prison_data3)
print(avg_prison_data4)

# Define a function to calculate the average transVSCisDifference for "Education" group
calculate_average_education <- function(data) {
  data %>%
    filter(TargetStimuliGroup == "Education") %>%
    summarise(avg_transVSCisDifference = mean(transVSCisDifference, na.rm = TRUE)) %>%
    mutate(dataset = deparse(substitute(data))) # Add dataset name
}

# Apply the function to each dataset
avg_education_data1 <- calculate_average_education(temporary_data1)
avg_education_data2 <- calculate_average_education(temporary_data2)
avg_education_data3 <- calculate_average_education(temporary_data3)
avg_education_data4 <- calculate_average_education(temporary_data4)

# Combine the results into a single table
all_avg_education <- bind_rows(avg_education_data1, avg_education_data2, avg_education_data3, avg_education_data4)

# Print the result
print(all_avg_education)
# Print the result seperately to make sure
print(avg_education_data1)
print(avg_education_data2)
print(avg_education_data3)
print(avg_education_data4)

print(all_avg_education)
print(all_avg_prison)
