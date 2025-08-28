# dog whistles by politicians' party affiliation
# preprocessing.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load relevant packages and set background color
library(tidyverse)
theme_set(theme_bw())

#change language to English
Sys.setenv(LANG = "en")

# 1/4: load the data for list 1/4 ----
d1 = read.csv("../data/Umfrage1.4.csv", sep =",", header = FALSE)

nrow(d1) #39 rows
names(d1)
head(d1)
summary(d1) 
#view(d1)

# make first row the column row
colnames(d1) = as.character(d1[1,])

# delete the first row
d1 = d1[-1,]
#view(d1)

# delete empty rows
d1 = d1[!apply(d1 == "", 1, all),]


# replace question column headers with shorter code (Xenia, I've given the items labels but
# you might have better ones, of course!)
colnames(d1)
colnames(d1)[2] ="afd-rechspopulistisch-dw"
colnames(d1)[3] ="gruen-neuzugezogen-ndw"
colnames(d1)[4] ="spd-mutter"
colnames(d1)[5] ="afd-muslime-ndw"
colnames(d1)[6] ="gruen-asylindustrie-dw"
colnames(d1)[7] ="spd-fachkraeftemangel"
colnames(d1)[8] ="afd-fluchtursachen-dw"
colnames(d1)[9] ="gruen-heimat-ndw"
colnames(d1)[10] ="afd-fatal-ndw"
colnames(d1)[11] ="spd-kriminell"
colnames(d1)[12] ="gruen-inlaenderfreundlich-dw"
colnames(d1)[13] ="afd-nationalstaat-dw"
colnames(d1)[14] ="gruen-renten-ndw"
colnames(d1)[15] ="spd-mehrwert"
# questions about the participants
colnames(d1)[16] ="age"
colnames(d1)[17] ="gender"
colnames(d1)[18] ="education"
colnames(d1)[19] ="migration"
colnames(d1)[20] ="ethnicity"
colnames(d1)[21] ="partyChoice"
# wahl-o-mat questions
colnames(d1)[22] ="womNachzug"
colnames(d1)[23] ="womKopftuch"
colnames(d1)[24] ="womStaatsangehoerigkeit"
colnames(d1)[25] ="womIslam"
colnames(d1)[26] ="womAsyl"

colnames(d1)

# get the data from wide into long format
d1 = d1 %>%
  pivot_longer(cols = "afd-rechspopulistisch-dw":"spd-mehrwert",
               names_to = "question", 
               values_to = "response")
#view(d1)

length(unique(d1$Timestamp)) #38 participants

# now make separate columns for party and items from the question column
table(d1$question)
d1 = d1 %>% 
  mutate(party = case_when(grepl("afd", question) ~ "afd",
                           grepl("spd", question) ~ "spd",
                           grepl("gruen", question) ~ "gruen",
                           TRUE ~ "ERROR"))
table(d1$party)

d1$item = d1$question
d1$item = gsub("afd-|spd-|gruen-", "", d1$item)
d1$item = gsub("-dw|-ndw", "", d1$item)
table(d1$item)
table(d1$party,d1$item)

d1 = d1 %>% 
  mutate(dw = case_when(grepl("-dw", question) ~ "dw",
                           grepl("-ndw", question) ~ "ndw",
                           TRUE ~ "control"))
table(d1$dw)

# remove the question column 
d1 <- d1 %>% select(-question)

names(d1)
summary(d1)
#view(d1)

# 2/4: load the data for list 2/4 ----
d2 = read.csv("../data/Umfrage2.4.csv", sep =",", header = FALSE)

#view(d2)
nrow(d2) #40 rows
names(d2)
head(d2)
summary(d2) 
#view(d2)

# make first row the column row
colnames(d2) = as.character(d2[1,])

# delete the first row
d2 = d2[-1,]
#view(d2)

# delete empty rows
d2 = d2[!apply(d2 == "", 1, all),]


# replace question column headers with shorter code (Xenia, I've given the items labels but
# you might have better ones, of course!)
colnames(d2)
colnames(d2)[2] ="gruen-rechspopulistisch-dw"
colnames(d2)[3] ="afd-neuzugezogen-ndw"
colnames(d2)[4] ="spd-mutter"
colnames(d2)[5] ="gruen-muslime-ndw"
colnames(d2)[6] ="afd-asylindustrie-dw"
colnames(d2)[7] ="spd-fachkraeftemangel"
colnames(d2)[8] ="gruen-fluchtursachen-dw"
colnames(d2)[9] ="afd-heimat-ndw"
colnames(d2)[10] ="gruen-fatal-ndw"
colnames(d2)[11] ="spd-kriminell"
colnames(d2)[12] ="afd-inlaenderfreundlich-dw"
colnames(d2)[13] ="gruen-nationalstaat-dw"
colnames(d2)[14] ="afd-renten-ndw"
colnames(d2)[15] ="spd-mehrwert"
# questions about the participants
colnames(d2)[16] ="age"
colnames(d2)[17] ="gender"
colnames(d2)[18] ="education"
colnames(d2)[19] ="migration"
colnames(d2)[20] ="ethnicity"
colnames(d2)[21] ="partyChoice"
# wahl-o-mat questions
colnames(d2)[22] ="womNachzug"
colnames(d2)[23] ="womKopftuch"
colnames(d2)[24] ="womStaatsangehoerigkeit"
colnames(d2)[25] ="womIslam"
colnames(d2)[26] ="womAsyl"

colnames(d2)

# get the data from wide into long format 
d2 = d2 %>%
  pivot_longer(cols = "gruen-rechspopulistisch-dw":"spd-mehrwert",
               names_to = "question", 
               values_to = "response")
#view(d2)

length(unique(d2$Timestamp)) #39 participants

# now make separate columns for party and items from the question column
table(d2$question)
d2 = d2 %>% 
  mutate(party = case_when(grepl("afd", question) ~ "afd",
                           grepl("spd", question) ~ "spd",
                           grepl("gruen", question) ~ "gruen",
                           TRUE ~ "ERROR"))
table(d2$party)

d2$item = d2$question
d2$item = gsub("afd-|spd-|gruen-", "", d2$item)
d2$item = gsub("-dw|-ndw", "", d2$item)
table(d2$item)
table(d2$party,d2$item)

d2 = d2 %>% 
  mutate(dw = case_when(grepl("-dw", question) ~ "dw",
                        grepl("-ndw", question) ~ "ndw",
                        TRUE ~ "control"))
table(d2$dw)

# remove the question column 
d2 <- d2 %>% select(-question)

names(d2)
summary(d2)
#view(d2)

# 3/4: load the data for list 1/4 ----
d3 = read.csv("../data/Umfrage3.4.csv", sep =",", header = FALSE)

nrow(d3) #40 rows
names(d3)
head(d3)
summary(d3) 
#view(d3)

# make first row the column row
colnames(d3) = as.character(d3[1,])

# delete the first row
d3 = d3[-1,]
#view(d3)

# delete empty rows
d3 = d3[!apply(d3 == "", 1, all),]


# replace question column headers with shorter code (Xenia, I've given the items labels but
# you might have better ones, of course!)
colnames(d3)
colnames(d3)[2] ="afd-rechspopulistisch-ndw"
colnames(d3)[3] ="gruen-neuzugezogen-dw"
colnames(d3)[4] ="spd-mutter"
colnames(d3)[5] ="afd-muslime-dw"
colnames(d3)[6] ="gruen-asylindustrie-ndw"
colnames(d3)[7] ="spd-fachkraeftemangel"
colnames(d3)[8] ="afd-fluchtursachen-ndw"
colnames(d3)[9] ="gruen-heimat-dw"
colnames(d3)[10] ="afd-fatal-dw"
colnames(d3)[11] ="spd-kriminell"
colnames(d3)[12] ="gruen-inlaenderfreundlich-ndw"
colnames(d3)[13] ="afd-nationalstaat-ndw"
colnames(d3)[14] ="gruen-renten-dw"
colnames(d3)[15] ="spd-mehrwert"
# questions about the participants
colnames(d3)[16] ="age"
colnames(d3)[17] ="gender"
colnames(d3)[18] ="education"
colnames(d3)[19] ="migration"
colnames(d3)[20] ="ethnicity"
colnames(d3)[21] ="partyChoice"
# wahl-o-mat questions
colnames(d3)[22] ="womNachzug"
colnames(d3)[23] ="womKopftuch"
colnames(d3)[24] ="womStaatsangehoerigkeit"
colnames(d3)[25] ="womIslam"
colnames(d3)[26] ="womAsyl"

colnames(d3)

# get the data from wide into long format
d3 = d3 %>%
  pivot_longer(cols = "afd-rechspopulistisch-ndw":"spd-mehrwert",
               names_to = "question", 
               values_to = "response")
#view(d3)

length(unique(d3$Timestamp)) #39 participants

# now make separate columns for party and items from the question column
table(d3$question)
d3 = d3 %>% 
  mutate(party = case_when(grepl("afd", question) ~ "afd",
                           grepl("spd", question) ~ "spd",
                           grepl("gruen", question) ~ "gruen",
                           TRUE ~ "ERROR"))
table(d3$party)

d3$item = d3$question
d3$item = gsub("afd-|spd-|gruen-", "", d3$item)
d3$item = gsub("-dw|-ndw", "", d3$item)
table(d3$item)
table(d3$party,d3$item)

d3 = d3 %>% 
  mutate(dw = case_when(grepl("-dw", question) ~ "dw",
                        grepl("-ndw", question) ~ "ndw",
                        TRUE ~ "control"))
table(d3$dw)

# remove the question column 
d3 <- d3 %>% select(-question)

names(d3)
summary(d3)
#view(d3)

# 4/4: load the data for list 1/4 ----
d4 = read.csv("../data/Umfrage4.4.csv", sep =",", header = FALSE)

nrow(d4) #39 rows
names(d4)
head(d4)
summary(d4) 
#view(d4)

# make first row the column row
colnames(d4) = as.character(d4[1,])

# delete the first row
d4 = d4[-1,]
#view(d4)

# delete empty rows
d4 = d4[!apply(d4 == "", 1, all),]


# replace question column headers with shorter code (Xenia, I've given the items labels but
# you might have better ones, of course!)
colnames(d4)
colnames(d4)[2] ="gruen-rechspopulistisch-ndw"
colnames(d4)[3] ="afd-neuzugezogen-dw"
colnames(d4)[4] ="spd-mutter"
colnames(d4)[5] ="gruen-muslime-dw"
colnames(d4)[6] ="afd-asylindustrie-ndw"
colnames(d4)[7] ="spd-fachkraeftemangel"
colnames(d4)[8] ="gruen-fluchtursachen-ndw"
colnames(d4)[9] ="afd-heimat-dw"
colnames(d4)[10] ="gruen-fatal-dw"
colnames(d4)[11] ="spd-kriminell"
colnames(d4)[12] ="afd-inlaenderfreundlich-ndw"
colnames(d4)[13] ="gruen-nationalstaat-ndw"
colnames(d4)[14] ="afd-renten-dw"
colnames(d4)[15] ="spd-mehrwert"
# questions about the participants
colnames(d4)[16] ="age"
colnames(d4)[17] ="gender"
colnames(d4)[18] ="education"
colnames(d4)[19] ="migration"
colnames(d4)[20] ="ethnicity"
colnames(d4)[21] ="partyChoice"
# wahl-o-mat questions
colnames(d4)[22] ="womNachzug"
colnames(d4)[23] ="womKopftuch"
colnames(d4)[24] ="womStaatsangehoerigkeit"
colnames(d4)[25] ="womIslam"
colnames(d4)[26] ="womAsyl"

colnames(d4)

# get the data from wide into long format
d4 = d4 %>%
  pivot_longer(cols = "gruen-rechspopulistisch-ndw":"spd-mehrwert",
               names_to = "question", 
               values_to = "response")
#view(d4)

length(unique(d4$Timestamp)) #38 participants

# now make separate columns for party and items from the question column
table(d4$question)
d4 = d4 %>% 
  mutate(party = case_when(grepl("afd", question) ~ "afd",
                           grepl("spd", question) ~ "spd",
                           grepl("gruen", question) ~ "gruen",
                           TRUE ~ "ERROR"))
table(d4$party)

d4$item = d4$question
d4$item = gsub("afd-|spd-|gruen-", "", d4$item)
d4$item = gsub("-dw|-ndw", "", d4$item)
table(d4$item)
table(d4$party,d4$item)

d4 = d4 %>% 
  mutate(dw = case_when(grepl("-dw", question) ~ "dw",
                        grepl("-ndw", question) ~ "ndw",
                        TRUE ~ "control"))
table(d4$dw)

# remove the question column 
d4 <- d4 %>% select(-question)

names(d4)
summary(d4)
#view(d4)



# now bind the data ----
d = rbind(d1,d2) %>%
  rbind(.,d3) %>%
  rbind(.,d4)
nrow(d) #(38 + 39 + 39 + 38 participants x 14 questions = 2156)

# replace timestamps with participant ID
colnames(d)[1] ="participantID"
d$participantID <- match(d$participantID, unique(sort(d$participantID)))
table(d$participantID)

# remove participants' data based on responses to controls
str(d$party)
controls <- subset(d, d$party == "spd")
#view(controls)

# plot the controls data
ggplot(controls, aes(x=participantID,y=response)) +
  geom_point() +
  facet_wrap(. ~ item)

controls.good <- subset(controls, controls$item == "mehrwert"| controls$item == "fachkraeftemangel" )
controls.bad <- subset(controls, controls$item == "kriminell"| controls$item == "mutter" )

# the original plan was toexclude participants' data if they gave responses below 4 to the controls 
# that are not anti-migration or above 2 to controls that are anti-migration
exclude.good <- subset(controls.good, controls.good$response < 4)
nrow(exclude.good) #24 cases
exclude.bad <- subset(controls.bad, controls.bad$response > 2)
nrow(exclude.bad) #52 cases

# plot the controls data
ggplot(exclude.good, aes(x=participantID,y=response)) +
  geom_point() +
  facet_wrap(. ~ item)

ggplot(exclude.bad, aes(x=participantID,y=response)) +
  geom_point() +
  facet_wrap(. ~ item)

# of the 154 participants, 50 did not respond as expected to the controls (so almost 1/3!)
# instead of excluding participants' data based on the controls, we use the controls to 
# get to know our participants better in the graphs.R file

# # exclude the participants who got the controls wrong
# length(unique(d$participantID)) #154 participants
# d <- droplevels(subset(d, !(d$participantID %in% exclude.good$participantID)))
# d <- droplevels(subset(d, !(d$participantID %in% exclude.bad$participantID)))
# length(unique(d$participantID)) #104 participants

nrow(d) #2156

# age 
table(d$age) 
str(d$age)
# fix participants weird age
d[d$age == "1 8",]$age = "18"
str(d$age)
d$age<- as.numeric(d$age)
table(d$age) #ages 18-71
mean(d$age) #31

# gender
d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())
# gender                            count
# 1 Divers                                6
# 2 Keine der oben genannten Optionen     1
# 3 Männlich                             76
# 4 Weiblich                             71

# get rid of space between "Ja,  mindestens"
table(d$migration)
d$migration = gsub("Ja,  mindestens", "", d$migration)
d$migration = gsub("Ja, mindestens", "", d$migration )
table(d$migration)

## save cleaned up data ----
write_csv(d, "../data/d.csv")

