# dogwhistle project
# graphs.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load relevant packages and set background color
library(tidyverse)
theme_set(theme_bw())

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# plot the data with criticalQuestion ----

##### plot the data to approximate Hurwitz & Peffley Fig 1A ----

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

# x-axis: transStereotypeIndex (5...35; the higher, the more the participant accepts the negative stereotypes about trans people)
# y-axis: the proportion of participants who had that transStereotypeIndex score and
# chose the prison response in the critical question
# plot the proportion by dogwhistle/no dogwhistle

table(d[d$participantID < 11,]$transStereotypeIndex) 
# the first 10 participants have these transStereotypeIndex values

names(d)
# these are the columns where the information is
table(d$transStereotypeIndex) # number of participants with transStereotypeIndex 5 to 35
table(d[d$transStereotypeIndex == 5,]$transStereotypeIndex,d[d$transStereotypeIndex == 5,]$participantID)
# these are the 30 participants with transStereotypeIndex = 5
table(d$criticalQuestion) # what did the participants choose (prison, education program)
table(d$targetResponse) # numerical representation of choice
table(d$dw) # were they in the dw condition?

# create a new data frame with the relevant information, for participants who got the dw
A.tmp.dw <- as.data.frame.matrix(table(d[d$dw == "yes",]$transStereotypeIndex,d[d$dw == "yes",]$criticalQuestion))
A.tmp.dw
# for each transStereotypeIndex, how many participants chose which answer to the critical question?
#    Building new prisons Education programs
#5                     2                 15
#6                     0                  4
#7                     1                  5

# give the first column a name
A.tmp.dw$transStereotypeIndex <- rownames(A.tmp.dw)

# add a proportion column
A.tmp.dw$prop = A.tmp.dw$`Building new prisons` / (A.tmp.dw$`Building new prisons` + A.tmp.dw$`Education programs`)
A.tmp.dw
# proportion of individuals who got the dw and chose one of the two answers, 
# given the transStereotypeIndex
# Building new prisons Education programs transStereotypeIndex      prop
# 5                     2                 15                    5 0.1176471
# 6                     0                  4                    6 0.0000000
# 7                     1                  5                    7 0.1666667

# add the info on "dw"
A.tmp.dw$dw = "yes"
A.tmp.dw

# create a new data frame with the relevant information, for participants who didn't get the dw
A.tmp.ndw <- as.data.frame.matrix(table(d[d$dw == "no",]$transStereotypeIndex,d[d$dw == "no",]$criticalQuestion))
A.tmp.ndw

# give the first column a name
A.tmp.ndw$transStereotypeIndex <- rownames(A.tmp.ndw)

# add a proportion column
A.tmp.ndw$prop = A.tmp.ndw$`Building new prisons` / (A.tmp.ndw$`Building new prisons` + A.tmp.ndw$`Education programs`)

# add the info on "dw"
A.tmp.ndw$dw = "no"
A.tmp.ndw

# bind the two dataframes
A = rbind(A.tmp.dw,A.tmp.ndw)
A

# make sure transStereotypeIndex is numeric (so that it is plotted numerically on x-axis)
str(A$transStereotypeIndex)
A$transStereotypeIndex = as.numeric(A$transStereotypeIndex)

#view(A)

# transform A to long
A = A %>%
  pivot_longer(!c(prop,dw,transStereotypeIndex), names_to = "choice", values_to = "count")
A

ggplot(data=A, aes(x=transStereotypeIndex, y=count,fill=choice)) +
  geom_bar(position="stack", stat="identity") +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  scale_x_continuous(n.breaks = 10, limits=c(4.5, 35.5)) +
  scale_y_continuous(n.breaks = 5, limits=c(0,30)) +
  facet_grid(. ~ dw) +
  ylab("count") +
  xlab("Trans Stereotype Index") 
ggsave("../graphs/1A-based-on-data.pdf",height=3,width=6)

# #plot 
# ggplot(data=A, aes(x=transStereotypeIndex, y=prop, group = dw, color = dw, fill = dw)) +
#   geom_bar(shape=21, size = 2) +
#   scale_fill_manual(values=c("#E69F00","#56B4E9")) +
#   geom_smooth(method = "lm", se = TRUE) + 
#   theme(legend.position="top") +
#   theme(axis.text.y = element_text(size=10)) +
#   scale_x_continuous(n.breaks = 10, limits=c(5, 35)) +
#   scale_y_continuous(n.breaks = 5, limits=c(-0.1, 1)) +
#   ylab("Proportion in favor of prisons") +
#   xlab("Trans Stereotype Index") 
# 
#  
# ggplot(data=A, aes(x=transStereotypeIndex, y=prop, group = dw, color = dw, fill = dw)) +
#   geom_point(shape=21, size = 2) + 
#   scale_fill_manual(values=c("#E69F00","#56B4E9")) +
#   geom_smooth(method = "glm", 
#               method.args = list(family = "quasibinomial"), 
#               se = TRUE) + 
#   theme(legend.position="top") +
#   theme(axis.text.y = element_text(size=10)) +
#   scale_x_continuous(n.breaks = 10, limits=c(5, 35)) +
#   scale_y_continuous(n.breaks = 5, limits=c(-0.1, 1)) +
#   ylab("Proportion in favor of prisons") +
#   xlab("Trans Stereotype Index") 
# 
# ggsave("../graphs/1A-based-on-data.pdf",height=3,width=6)
# 
# d = d %>%
#   mutate(criticalQuestionBinary = case_when(
#     criticalQuestion == "Education programs" ~ 0,
#     criticalQuestion == "Building new prisons" ~ 1,
#     TRUE ~ 666))
# table(d$criticalQuestionBinary)
# d$criticalQuestionBinary = as.integer(d$criticalQuestionBinary)
# str(d$criticalQuestionBinary)
# 
# ggplot(d, aes(x=transStereotypeIndex, y=criticalQuestionBinary, group = dw, color = dw, fill = dw)) + 
#   geom_jitter(shape=21, size = 2, width = 0.1, height = 0.1) + 
#   scale_fill_manual(values=c("#E69F00","#56B4E9")) +
#   stat_smooth(method="glm", se=TRUE, 
#               method.args = list(family=binomial))


#### plot the data to approximate Hurwitz & Peffley Fig 1B ----

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

# x-axis: genderFairnessIndex (4...23, the higher, the more the participant believes that trans people are treated less fairly than cis people)
# y-axis: the proportion of participants who had that genderFairnessIndex score and
# chose the prison response in the critical question
# plot the proportion by dogwhistle/no dogwhistle

table(d[d$participantID < 11,]$genderFairnessIndex) 
# the first 10 participants have these genderFairnessIndex values

length(unique(d$participantID)) #140 participants

# these are the columns where the information is
table(d$genderFairnessIndex) # number of participants with genderFairnessIndex 4 to 23
table(d[d$genderFairnessIndex == 4,]$genderFairnessIndex,d[d$genderFairnessIndex == 4,]$participantID)
# these are the 12 participants with genderFairnessIndex = 4
table(d$criticalQuestion) # what did the participants choose (prison, education program)
table(d$dw) # were they in the dw condition?

# create a new data frame with the relevant information, for participants who got the dw
B.tmp.dw <- as.data.frame.matrix(table(d[d$dw == "yes",]$genderFairnessIndex,d[d$dw == "yes",]$criticalQuestion))
B.tmp.dw
# for each genderFairnessIndex, how many participants chose which answer to the critical question?
#    Building new prisons Education programs
#4                     2                  6
#5                     0                  5
#6                     0                  8

# give the first column a name
B.tmp.dw$genderFairnessIndex <- rownames(B.tmp.dw)

# add a proportion column
B.tmp.dw$prop = B.tmp.dw$`Building new prisons` / (B.tmp.dw$`Building new prisons` + B.tmp.dw$`Education programs`)
B.tmp.dw
# proportion of individuals who got the dw and chose one of the two answers, 
# given the genderFairnessIndex
# Building new prisons Education programs genderFairnessIndex      prop
# 4                     2                  6                   4 0.2500000
# 5                     0                  5                   5 0.0000000
# 6                     0                  8                   6 0.0000000
# 7                     2                  1                   7 0.6666667

# add the info on "dw"
B.tmp.dw$dw = "yes"
B.tmp.dw

# create a new data frame with the relevant information, for participants who didn't get the dw
B.tmp.ndw <- as.data.frame.matrix(table(d[d$dw == "no",]$genderFairnessIndex,d[d$dw == "no",]$criticalQuestion))
B.tmp.ndw

# give the first column a name
B.tmp.ndw$genderFairnessIndex <- rownames(B.tmp.ndw)

# add a proportion column
B.tmp.ndw$prop = B.tmp.ndw$`Building new prisons` / (B.tmp.ndw$`Building new prisons` + B.tmp.ndw$`Education programs`)

# add the info on "dw"
B.tmp.ndw$dw = "no"
B.tmp.ndw

# bind the two dataframes
B = rbind(B.tmp.dw,B.tmp.ndw)
B

# make sure genderFairnessIndex is numeric (so that it is plotted numerically on x-axis)
str(B$genderFairnessIndex)
B$genderFairnessIndex = as.numeric(B$genderFairnessIndex)

#view(B)

# transform B to long
B = B %>%
  pivot_longer(!c(prop,dw,genderFairnessIndex), names_to = "choice", values_to = "count")
B

ggplot(data=B, aes(x=genderFairnessIndex, y=count,fill=choice)) +
  geom_bar(position="stack", stat="identity") +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  scale_x_continuous(n.breaks = 5, limits=c(3.5, 24.5)) +
  scale_y_continuous(n.breaks = 5, limits=c(0,20)) +
  facet_grid(. ~ dw) +
  ylab("count") +
  xlab("Gender Fairness Index") 
ggsave("../graphs/1B-based-on-data.pdf",height=3,width=6)

# #plot 
# ggplot(data=B, aes(x=genderFairnessIndex, y=prop, group = dw, color = dw, fill = dw)) +
#   geom_point(shape=21, size = 2) + 
#   scale_fill_manual(values=c("#E69F00","#56B4E9")) +
#   geom_smooth(method = "lm", se = TRUE) + 
#   theme(legend.position="top") +
#   theme(axis.text.y = element_text(size=10)) +
#   scale_x_continuous(limits=c(4, 23), breaks=c(4,10,15,20,23),
#                      labels=c("4","10","15","20","23")) +
#   scale_y_continuous(n.breaks = 5, limits=c(-0.1, 1)) +
#   ylab("Proportion in favor of prisons") +
#   xlab("Gender Fairness Index") 
# ggsave("../graphs/1B-based-on-data.pdf",height=3,width=6)
# 
# ggplot(data=B, aes(x=genderFairnessIndex, y=prop, group = dw, color = dw, fill = dw)) +
#   geom_point(shape=21, size = 2) + 
#   scale_fill_manual(values=c("#E69F00","#56B4E9")) +
#   geom_smooth(method = "glm", 
#               method.args = list(family = "quasibinomial"), 
#               se = TRUE) + 
#   theme(legend.position="top") +
#   theme(axis.text.y = element_text(size=10)) +
#   scale_x_continuous(limits=c(4, 23), breaks=c(4,10,15,20,23),
#                      labels=c("4","10","15","20","23")) +
#   scale_y_continuous(n.breaks = 5, limits=c(-0.1, 1)) +
#   ylab("Proportion in favor of prisons") +
#   xlab("Gender Fairness Index")
# ggsave("../graphs/1Bv2-based-on-data.pdf",height=3,width=6)
# 
# d = d %>%
#   mutate(criticalQuestionBinary = case_when(
#     criticalQuestion == "Education programs" ~ 0,
#     criticalQuestion == "Building new prisons" ~ 1,
#     TRUE ~ 666))
# table(d$criticalQuestionBinary)
# d$criticalQuestionBinary = as.integer(d$criticalQuestionBinary)
# str(d$criticalQuestionBinary)
# 
# ggplot(d, aes(x=genderFairnessIndex, y=criticalQuestionBinary, group = dw, color = dw, fill = dw)) + 
#   geom_jitter(shape=21, size = 2, width = 0.1, height = 0.2) + 
#   scale_fill_manual(values=c("#E69F00","#56B4E9")) +
#   stat_smooth(method="glm", se=TRUE, 
#               method.args = list(family=binomial)) +
#   ylab("Proportion in favor of prisons") +
#   xlab("Gender Fairness Index")
# ggsave("../graphs/1Bv3-based-on-data.pdf",height=3,width=6)

# plot the data with targetResponse ----

##### plot the data to approximate Hurwitz & Peffley Fig 1A ----

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

table(d$targetResponse) # numerical representation of choice
table(d$dw) # were they in the dw condition?

# create a new data frame with the relevant information, for participants who got the dw
A.tmp.dw <- as.data.frame.matrix(table(d[d$dw == "yes",]$transStereotypeIndex,d[d$dw == "yes",]$targetResponse))
A.tmp.dw

# give the first column a name
A.tmp.dw$transStereotypeIndex <- rownames(A.tmp.dw)

# add the info on "dw"
A.tmp.dw$dw = "yes"
A.tmp.dw

# create a new data frame with the relevant information, for participants who didn't get the dw
A.tmp.ndw <- as.data.frame.matrix(table(d[d$dw == "no",]$transStereotypeIndex,d[d$dw == "no",]$targetResponse))
A.tmp.ndw

# give the first column a name
A.tmp.ndw$transStereotypeIndex <- rownames(A.tmp.ndw)

# add the info on "dw"
A.tmp.ndw$dw = "no"
A.tmp.ndw

# bind the two dataframes
A = rbind(A.tmp.dw,A.tmp.ndw)
A

# make sure transStereotypeIndex is numeric (so that it is plotted numerically on x-axis)
str(A$transStereotypeIndex)
A$transStereotypeIndex = as.numeric(A$transStereotypeIndex)

#view(A)

# transform A to long
A = A %>%
  pivot_longer(!c(dw,transStereotypeIndex), names_to = "choice", values_to = "count")
A


ggplot(data=A, aes(x=transStereotypeIndex, y=count,fill=choice)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual("legend", values = c("1" = "black", "2" = "gray40", "3" = "#E69F00", "4" = "#F0E442")) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  scale_x_continuous(n.breaks = 10, limits=c(4.5, 35.5)) +
  scale_y_continuous(n.breaks = 5, limits=c(0,30)) +
  facet_grid(. ~ dw) +
  ylab("count") +
  xlab("Trans Stereotype Index") 
ggsave("../graphs/1A-based-on-data-NEW.pdf",height=3,width=8)

##### plot the data to approximate Hurwitz & Peffley Fig 1B ----

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

table(d$genderFairnessIndex) # number of participants with genderFairnessIndex 4 to 23

# create a new data frame with the relevant information, for participants who got the dw
B.tmp.dw <- as.data.frame.matrix(table(d[d$dw == "yes",]$genderFairnessIndex,d[d$dw == "yes",]$targetResponse))
B.tmp.dw

# give the first column a name
B.tmp.dw$genderFairnessIndex <- rownames(B.tmp.dw)

# add the info on "dw"
B.tmp.dw$dw = "yes"
B.tmp.dw

# create a new data frame with the relevant information, for participants who didn't get the dw
B.tmp.ndw <- as.data.frame.matrix(table(d[d$dw == "no",]$genderFairnessIndex,d[d$dw == "no",]$targetResponse))
B.tmp.ndw

# give the first column a name
B.tmp.ndw$genderFairnessIndex <- rownames(B.tmp.ndw)

# add the info on "dw"
B.tmp.ndw$dw = "no"
B.tmp.ndw

# bind the two dataframes
B = rbind(B.tmp.dw,B.tmp.ndw)
B

# make sure genderFairnessIndex is numeric (so that it is plotted numerically on x-axis)
str(B$genderFairnessIndex)
B$genderFairnessIndex = as.numeric(B$genderFairnessIndex)

#view(B)

# transform B to long
B = B %>%
  pivot_longer(!c(dw,genderFairnessIndex), names_to = "choice", values_to = "count")
B

ggplot(data=B, aes(x=genderFairnessIndex, y=count,fill=choice)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual("legend", values = c("1" = "black", "2" = "gray40", "3" = "#E69F00", "4" = "#F0E442")) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  scale_x_continuous(n.breaks = 5, limits=c(3.5, 24.5)) +
  scale_y_continuous(n.breaks = 5, limits=c(0,30)) +
  facet_grid(. ~ dw) +
  ylab("count") +
  xlab("genderFairnessIndex") 
ggsave("../graphs/1B-based-on-data-NEW.pdf",height=3,width=8)

# correlate the two main predictors, transStereotypeIndex and genderFairnessIndex ----

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

# for every participant, get their transStereotypeIndex and genderFairnessIndex 
# and the dw condition
corr = d %>%
  select(participantID,dw,transStereotypeIndex,genderFairnessIndex)
corr

cor(corr$transStereotypeIndex, corr$genderFairnessIndex)
# 0.754904

ggplot(data=corr, aes(x=transStereotypeIndex, y=genderFairnessIndex,fill=dw)) +
  geom_point(shape=21,size=3) +
  scale_fill_manual(values = c("no" = "black", "yes" = "#E69F00")) +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  scale_x_continuous(n.breaks = 5, limits=c(2,36)) +
  scale_y_continuous(n.breaks = 5, limits=c(2,36)) +
  #facet_grid(. ~ dw) +
  ylab("genderFairnessIndex") +
  xlab("transStereotypeIndex") 
ggsave("../graphs/correlation-of-main-predictors.pdf",height=4,width=4)

# plots about all the predictors ----

#### preregistered (conservative or liberal) ----

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

# names(d)
# these are the columns where the information is

table(d$preregistered) # number of participants who are preregistered as conservative or liberal
# cons liberal 
# 70      70

# x-axis: preregistered (conservative, liberal)
# y-axis: the proportion of participants preregistered as conservative/liberal and
# who chose the prison response in the critical question
# plot the proportion by dogwhistle/no dogwhistle

# view(d$preregistered)

table(d[d$participantID < 31,]$preregistered) 
# the first 30 participants have these preregistered values

table(d$preregistered) # number of participants preregistered as conservative or liberal
table(d[d$preregistered == "liberal",]$preregistered,d[d$preregistered == "liberal",]$participantID)
# these are all the participants preregistered as liberal

table(d$criticalQuestion) # what did the participants choose (prison, education program)
table(d$targetResponse) # numerical representation of choice
table(d$dw) # were they in the dw condition?

# create a new data frame with the relevant information, for participants who got the dw
C.tmp.dw <- as.data.frame.matrix(table(d[d$dw == "yes",]$preregistered,d[d$dw == "yes",]$criticalQuestion))
C.tmp.dw
# for cons/liberal, how many participants chose which answer to the critical question?
# Building new prisons Education programs
# cons                      16                 19
# liberal                    4                 31

# give the first column a name
C.tmp.dw$preregistered <- rownames(C.tmp.dw)

# add a proportion column
C.tmp.dw$prop = C.tmp.dw$`Building new prisons` / (C.tmp.dw$`Building new prisons` + C.tmp.dw$`Education programs`)
C.tmp.dw
# proportion of individuals who got the dw and chose one of the two answers, 
# given their preregistered political orientation
# Building new prisons Education programs preregistered      prop
# cons                      16                 19          cons 0.4571429
# liberal                    4                 31       liberal 0.1142857

# add the info on "dw"
C.tmp.dw$dw = "yes"
C.tmp.dw

# create a new data frame with the relevant information, for participants who didn't get the dw
C.tmp.ndw <- as.data.frame.matrix(table(d[d$dw == "no",]$preregistered,d[d$dw == "no",]$criticalQuestion))
C.tmp.ndw

# give the first column a name
C.tmp.ndw$preregistered <- rownames(C.tmp.ndw)

# add a proportion column
C.tmp.ndw$prop = C.tmp.ndw$`Building new prisons` / (C.tmp.ndw$`Building new prisons` + C.tmp.ndw$`Education programs`)

# add the info on "dw"
C.tmp.ndw$dw = "no"
C.tmp.ndw

# bind the two dataframes
C = rbind(C.tmp.dw,C.tmp.ndw)
C

# view(C)

# transform C to long
C = C %>%
  pivot_longer(!c(prop,dw,preregistered), names_to = "choice", values_to = "count")
C

ggplot(data=C, aes(x=preregistered, y=count,fill=choice)) +
  geom_bar(position="stack", stat="identity") +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  scale_x_discrete() +
  scale_y_continuous(n.breaks = 10, limits=c(0,35)) +
  facet_grid(. ~ dw) +
    ylab("count") +
  xlab("political orientation") 
ggsave("../graphs/preregistered-political-orientation.pdf",height=3,width=6)



#### plot the data for proportion of transStereotypeIndex ----
# the higher, the more the participant accepts the negative stereotypes about trans people

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

names(d)
# these are the columns where the information is

table(d$transStereotypeIndex) # number of participants with transStereotypeIndex 5 to 35
# 5  6  7  8  9 10 11 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 
# 30  7 13  5  4  4  3  2  3  2  1  2  2  2  6  3  2  1  5  4  5  6  7  4  2  4  3  1  2  5 

# x-axis: transStereotypeIndex (5 to 35)
# y-axis: the proportion of participants who had that transStereotypeIndex score

# view(d$transStereotypeIndex)

table(d[d$participantID < 31,]$transStereotypeIndex) 
# the first 30 participants have these transStereotypeIndex values

table(d$transStereotypeIndex) # number of participants with transStereotypeIndex 5 to 35
table(d[d$transStereotypeIndex == "5",]$transStereotypeIndex,d[d$transStereotypeIndex == "5",]$participantID)
# these are all the participants with a transStereotypeIndex of 5

count <- table(d$transStereotypeIndex)

ggplot(d, aes(x = factor(transStereotypeIndex))) +
  geom_bar() +
    labs(
    x = "Trans Stereotype Index",
    y = "number of participants",
    title = "Proportion of Trans Stereotype Index"
  ) +
  theme_minimal()
ggsave("../graphs/trans-stereotypes-index.pdf",height=3,width=6)


#### plot the data for proportion of genderFairnessIndex ----

# the higher, the more the participant believes that transgender people are treated unfairly

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

names(d)
# these are the columns where the information is

table(d$genderFairnessIndex) # number of participants with genderFairnessIndex 4 to 23
#  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 
# 12  8 15 11 11  3  8 13  2  7  6  4  6  4  4  4  2  6  5  9 

# x-axis: genderFairnessIndex (4 to 23)
# y-axis: the proportion of participants who had that genderFairnessIndex score

# view(d$genderFairnessIndex)

table(d[d$participantID < 31,]$genderFairnessIndex) 
# the first 30 participants have these genderFairnessIndex values

table(d$genderFairnessIndex) # number of participants with genderFairnessIndex 4 to 23
table(d[d$genderFairnessIndex == "4",]$genderFairnessIndex,d[d$genderFairnessIndex == "4",]$participantID)
# these are all the participants with a genderFairnessIndex of 4

count <- table(d$genderFairnessIndex)

ggplot(d, aes(x = factor(genderFairnessIndex))) +
  geom_bar() +
  labs(
    x = "Gender Fairness Index",
    y = "number of participants",
    title = "Proportion of Gender Fairness Index"
  ) +
  theme_minimal()
ggsave("../graphs/gender-fairness-index.pdf",height=3,width=6)

#### plot the data for proportion of cisStereotypeIndex ----
# the higher, the more the participant accepts the negative stereotypes about cis people

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

names(d)
# these are the columns where the information is

table(d$cisStereotypeIndex) # number of participants with cisStereotypeIndex 5 to 35
# 5  6  7  8  9 10 11 12 13 14 15 16 17 18 21 25 26 32 
# 72 12  8  7  6  5  5  3  2  1  3  2  5  2  3  2  1  1  

# x-axis: cisStereotypeIndex (5 to 35)
# y-axis: the proportion of participants who had that cisStereotypeIndex score

# view(d$cisStereotypeIndex)

table(d[d$participantID < 31,]$cisStereotypeIndex) 
# the first 30 participants have these cisStereotypeIndex values

table(d$cisStereotypeIndex) # number of participants with cisStereotypeIndex 5 to 35
table(d[d$cisStereotypeIndex == "5",]$cisStereotypeIndex,d[d$cisStereotypeIndex == "5",]$participantID)
# these are all the participants with a cisStereotypeIndex of 5

count <- table(d$cisStereotypeIndex)

ggplot(d, aes(x = factor(cisStereotypeIndex))) +
  geom_bar() +
  labs(
    x = "Cis Stereotype Index",
    y = "number of participants",
    title = "Proportion of Cis Stereotype Index"
  ) +
  theme_minimal()
ggsave("../graphs/cis-stereotype-index.pdf",height=3,width=6)


##### plot the data for cisStereotypeIndex ----

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

# x-axis: cisStereotypeIndex (5...35; the higher, the more the participant accepts the negative stereotypes about cis people)
# y-axis: the proportion of participants who had that cisStereotypeIndex score and
# chose the prison response in the critical question
# plot the proportion by dogwhistle/no dogwhistle

table(d[d$participantID < 11,]$cisStereotypeIndex) 
# the first 10 participants have these cisStereotypeIndex values

names(d)
# these are the columns where the information is
table(d$cisStereotypeIndex) # number of participants with cisStereotypeIndex 5 to 35
table(d[d$cisStereotypeIndex == 5,]$cisStereotypeIndex,d[d$cisStereotypeIndex == 5,]$participantID)
# these are the 72 participants with cisStereotypeIndex = 5
table(d$criticalQuestion) # what did the participants choose (prison, education program)
table(d$targetResponse) # numerical representation of choice
table(d$dw) # were they in the dw condition?

# create a new data frame with the relevant information, for participants who got the dw
D.tmp.dw <- as.data.frame.matrix(table(d[d$dw == "yes",]$cisStereotypeIndex,d[d$dw == "yes",]$criticalQuestion))
D.tmp.dw
# for each cisStereotypeIndex, how many participants chose which answer to the critical question?
#     Building new prisons Education programs
# 5                    11                 29
# 6                     2                  5
# 7                     3                  1

# give the first column a name
D.tmp.dw$cisStereotypeIndex <- rownames(D.tmp.dw)

# add a proportion column
D.tmp.dw$prop = D.tmp.dw$`Building new prisons` / (D.tmp.dw$`Building new prisons` + D.tmp.dw$`Education programs`)
D.tmp.dw
# proportion of individuals who got the dw and chose one of the two answers, 
# given the cisStereotypeIndex
# Building new prisons Education programs cisStereotypeIndex      prop
# 5                    11                 29                  5 0.2750000
# 6                     2                  5                  6 0.2857143
# 7                     3                  1                  7 0.7500000

# add the info on "dw"
D.tmp.dw$dw = "yes"
D.tmp.dw

# create a new data frame with the relevant information, for participants who didn't get the dw
D.tmp.ndw <- as.data.frame.matrix(table(d[d$dw == "no",]$cisStereotypeIndex,d[d$dw == "no",]$criticalQuestion))
D.tmp.ndw

# give the first column a name
D.tmp.ndw$cisStereotypeIndex <- rownames(D.tmp.ndw)

# add a proportion column
D.tmp.ndw$prop = D.tmp.ndw$`Building new prisons` / (D.tmp.ndw$`Building new prisons` + D.tmp.ndw$`Education programs`)

# add the info on "dw"
D.tmp.ndw$dw = "no"
D.tmp.ndw

# bind the two dataframes
D = rbind(D.tmp.dw,D.tmp.ndw)
D

# make sure cisStereotypeIndex is numeric (so that it is plotted numerically on x-axis)
str(D$cisStereotypeIndex)
D$cisStereotypeIndex = as.numeric(D$cisStereotypeIndex)

# view(D)

# transform D to long
D = D %>%
  pivot_longer(!c(prop,dw,cisStereotypeIndex), names_to = "choice", values_to = "count")
D

ggplot(data=D, aes(x=cisStereotypeIndex, y=count,fill=choice)) +
  geom_bar(position="stack", stat="identity") +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  scale_x_continuous(n.breaks = 10, limits=c(4.5, 35.5)) +
  scale_y_continuous(n.breaks = 5, limits=c(0,30)) +
  facet_grid(. ~ dw) +
  ylab("count") +
  xlab("Cis Stereotype Index") 
ggsave("../graphs/cis-stereotype-index-1.pdf",height=3,width=6)



#### plot the data for proportion of generalFairnessIndex ----
# the higher, the more the participant believes that people are treated unfairly

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

names(d)
# these are the columns where the information is

table(d$generalFairnessIndex) # number of participants with generalFairnessIndex 2 to 8
#  2  3  4  5  6  7  8 
# 13  7 24 18 24 27 27  

# x-axis: generalFairnessIndex (2 to 8)
# y-axis: the proportion of participants who had that generalFairnessIndex score

# view(d$generalFairnessIndex)

table(d[d$participantID < 31,]$generalFairnessIndex) 
# the first 30 participants have these generalFairnessIndex values

table(d$generalFairnessIndex) # number of participants with generalFairnessIndex 2 to 8
table(d[d$generalFairnessIndex == "2",]$generalFairnessIndex,d[d$generalFairnessIndex == "2",]$participantID)
# these are all the participants with a generalFairnessIndex of 2

count <- table(d$generalFairnessIndex)

ggplot(d, aes(x = factor(generalFairnessIndex))) +
  geom_bar() +
  labs(
    x = "General Fairness Index",
    y = "number of participants",
    title = "Proportion of General Fairness Index"
  ) +
  theme_minimal()
ggsave("../graphs/general-fairness-index.pdf",height=3,width=6)

#### plot the data for generalFairnessIndex ----

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

# x-axis: generalFairnessIndex (2...8)
# y-axis: the proportion of participants who had that generalFairnessIndex score and
# chose the prison response in the critical question
# plot the proportion by dogwhistle/no dogwhistle

table(d[d$participantID < 11,]$generalFairnessIndex) 
# the first 10 participants have these genderFairnessIndex values

length(unique(d$participantID)) #140 participants

# these are the columns where the information is
table(d$generalFairnessIndex) # number of participants with generalFairnessIndex 2 to 8
table(d[d$generalFairnessIndex == 2,]$generalFairnessIndex,d[d$generalFairnessIndex == 2,]$participantID)
# these are the 13 participants with generalFairnessIndex = 2
table(d$criticalQuestion) # what did the participants choose (prison, education program)
table(d$dw) # were they in the dw condition?

# create a new data frame with the relevant information, for participants who got the dw
E.tmp.dw <- as.data.frame.matrix(table(d[d$dw == "yes",]$generalFairnessIndex,d[d$dw == "yes",]$criticalQuestion))
E.tmp.dw
# for each generalFairnessIndex, how many participants chose which answer to the critical question?
#    Building new prisons Education programs
# 2                    3                  3
# 3                    2                  1
# 4                    2                  6

# give the first column a name
E.tmp.dw$generalFairnessIndex <- rownames(E.tmp.dw)

# add a proportion column
E.tmp.dw$prop = E.tmp.dw$`Building new prisons` / (E.tmp.dw$`Building new prisons` + E.tmp.dw$`Education programs`)
E.tmp.dw
# proportion of individuals who got the dw and chose one of the two answers, 
# given the generalFairnessIndex
# Building new prisons Education programs genderFairnessIndex      prop
# 2                    3                  3                    2 0.50000000
# 3                    2                  1                    3 0.66666667
# 4                    2                  6                    4 0.25000000

# add the info on "dw"
E.tmp.dw$dw = "yes"
E.tmp.dw

# create a new data frame with the relevant information, for participants who didn't get the dw
E.tmp.ndw <- as.data.frame.matrix(table(d[d$dw == "no",]$generalFairnessIndex,d[d$dw == "no",]$criticalQuestion))
E.tmp.ndw

# give the first column a name
E.tmp.ndw$generalFairnessIndex <- rownames(E.tmp.ndw)

# add a proportion column
E.tmp.ndw$prop = E.tmp.ndw$`Building new prisons` / (E.tmp.ndw$`Building new prisons` + E.tmp.ndw$`Education programs`)

# add the info on "dw"
E.tmp.ndw$dw = "no"
E.tmp.ndw

# bind the two dataframes
E = rbind(E.tmp.dw,E.tmp.ndw)
E

# make sure generalFairnessIndex is numeric (so that it is plotted numerically on x-axis)
str(E$generalFairnessIndex)
E$generalFairnessIndex = as.numeric(E$generalFairnessIndex)

#view(E)

# transform E to long
E = E %>%
  pivot_longer(!c(prop,dw,generalFairnessIndex), names_to = "choice", values_to = "count")
E

ggplot(data=E, aes(x=generalFairnessIndex, y=count,fill=choice)) +
  geom_bar(position="stack", stat="identity") +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  scale_x_continuous(n.breaks = 8, limits=c(1.5, 8.5)) +
  scale_y_continuous(n.breaks = 5, limits=c(0,20)) +
  facet_grid(. ~ dw) +
  ylab("count") +
  xlab("General Fairness Index") 
ggsave("../graphs/general-fairness-index-1.pdf",height=3,width=6)




#### plot the data for proportion of moreTransPeopleIndex ----
# the higher, the more the participant believes that there are more trans people

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

names(d)
# these are the columns where the information is

table(d$moreTransPeopleIndex) # number of participants with moreTransPeopleIndex 1 to 3
#   1   2   3 
#   2  27 111   

# x-axis: moreTransPeopleIndex  (1 to 3)
# y-axis: the proportion of participants who had that moreTransPeopleIndex score

# view(d$moreTransPeopleIndex)

table(d[d$participantID < 61,]$moreTransPeopleIndex) 
# the first 60 participants have these moreTransPeopleIndex values

table(d$moreTransPeopleIndex) # number of participants with moreTransPeopleIndex 1 to 3
table(d[d$moreTransPeopleIndex == "1",]$moreTransPeopleIndex,d[d$moreTransPeopleIndex == "1",]$participantID)
# these are all the participants with a moreTransPeopleIndex of 1

count <- table(d$moreTransPeopleIndex)

ggplot(d, aes(x = factor(moreTransPeopleIndex))) +
  geom_bar() +
  labs(
    x = "More Trans People Index",
    y = "number of participants",
    title = "Proportion of More Trans People Index"
  ) +
  theme_minimal()
ggsave("../graphs/more-trans-people-index.pdf",height=3,width=6)


#### plot the data for moreTransPeopleIndex ----

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

# x-axis: moreTransPeopleIndex (1...3)
# y-axis: the proportion of participants who had that moreTransPeopleIndex score and
# chose the prison response in the critical question
# plot the proportion by dogwhistle/no dogwhistle

table(d[d$participantID < 11,]$moreTransPeopleIndex) 
# the first 10 participants have these moreTransPeopleIndex values

length(unique(d$participantID)) #140 participants

# these are the columns where the information is
table(d$moreTransPeopleIndex) # number of participants with moreTransPeopleIndex 1 to 3
table(d[d$moreTransPeopleIndex == 1,]$moreTransPeopleIndex,d[d$moreTransPeopleIndex == 1,]$participantID)
# these are the 2 participants with moreTransPeopleIndex = 1
table(d$criticalQuestion) # what did the participants choose (prison, education program)
table(d$dw) # were they in the dw condition?

# create a new data frame with the relevant information, for participants who got the dw
F.tmp.dw <- as.data.frame.matrix(table(d[d$dw == "yes",]$moreTransPeopleIndex,d[d$dw == "yes",]$criticalQuestion))
F.tmp.dw
# for each moreTransPeopleIndex score, how many participants chose which answer to the critical question?
#    Building new prisons Education programs
# 1                    1                  0
# 2                    1                 14
# 3                   18                 36

# give the first column a name
F.tmp.dw$moreTransPeopleIndex <- rownames(F.tmp.dw)

# add a proportion column
F.tmp.dw$prop = F.tmp.dw$`Building new prisons` / (F.tmp.dw$`Building new prisons` + F.tmp.dw$`Education programs`)
F.tmp.dw
# proportion of individuals who got the dw and chose one of the two answers, 
# given the moreTransPeopleIndex
# Building new prisons Education programs moreTransPeopleIndex      prop
# 1                    1                  0                    1 1.00000000
# 2                    1                 14                    2 0.06666667
# 3                   18                 36                    3 0.33333333

# add the info on "dw"
F.tmp.dw$dw = "yes"
F.tmp.dw

# create a new data frame with the relevant information, for participants who didn't get the dw
F.tmp.ndw <- as.data.frame.matrix(table(d[d$dw == "no",]$moreTransPeopleIndex,d[d$dw == "no",]$criticalQuestion))
F.tmp.ndw

# give the first column a name
F.tmp.ndw$moreTransPeopleIndex <- rownames(F.tmp.ndw)

# add a proportion column
F.tmp.ndw$prop = F.tmp.ndw$`Building new prisons` / (F.tmp.ndw$`Building new prisons` + F.tmp.ndw$`Education programs`)

# add the info on "dw"
F.tmp.ndw$dw = "no"
F.tmp.ndw

# bind the two dataframes
F = rbind(F.tmp.dw,F.tmp.ndw)
F

# make sure moreTransPeopleIndex is numeric (so that it is plotted numerically on x-axis)
str(F$moreTransPeopleIndex)
F$moreTransPeopleIndex = as.numeric(F$moreTransPeopleIndex)

#view(F)

# transform F to long
F = F %>%
  pivot_longer(!c(prop,dw,moreTransPeopleIndex), names_to = "choice", values_to = "count")
F

ggplot(data=F, aes(x=moreTransPeopleIndex, y=count,fill=choice)) +
  geom_bar(position="stack", stat="identity") +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  scale_x_continuous(n.breaks = 3, limits=c(0.5, 3.5)) +
  scale_y_continuous(n.breaks = 5, limits=c(0,70)) +
  facet_grid(. ~ dw) +
  ylab("count") +
  xlab("More Trans People Index") 
ggsave("../graphs/more-trans-people-index-1.pdf",height=3,width=6)



#### plot the data for proportion fearComparisonProblemsIndex ----
# 1/3: the participant believes that the problems of trans people are more/less important than other problems

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

names(d)
# these are the columns where the information is

table(d$fearComparisonProblemsIndex) # number of participants with fearComparisonProblemsIndex 1 to 3
#   1  2  3 
#   6 75 59  

# x-axis: fearComparisonProblemsIndex  (1 to 3)
# y-axis: the proportion of participants who had that fearComparisonProblemsIndex score

# view(d$fearComparisonProblemsIndex)

table(d[d$participantID < 31,]$fearComparisonProblemsIndex) 
# the first 30 participants have these fearComparisonProblemsIndex values

table(d$fearComparisonProblemsIndex) # number of participants with fearComparisonProblemsIndex 1 to 3
table(d[d$fearComparisonProblemsIndex == "1",]$fearComparisonProblemsIndex,d[d$fearComparisonProblemsIndex == "1",]$participantID)
# these are all the participants with a fearComparisonProblemsIndex of 1

count <- table(d$fearComparisonProblemsIndex)

ggplot(d, aes(x = factor(fearComparisonProblemsIndex))) +
  geom_bar() +
  labs(
    x = "Fear Comparison Problems Index",
    y = "number of participants",
    title = "Proportion of Fear Comparison Problems Index"
  ) +
  theme_minimal()
ggsave("../graphs/fear-comparison-problems-index.pdf",height=3,width=6)

#### plot the data for fearComparisonProblemsIndex ----

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

# x-axis: fearComparisonProblemsIndex (1...3)
# y-axis: the proportion of participants who had that o	fearComparisonProblemsIndex score and
# chose the prison response in the critical question
# plot the proportion by dogwhistle/no dogwhistle

table(d[d$participantID < 11,]$fearComparisonProblemsIndex) 
# the first 10 participants have these moreTransPeopleIndex values

length(unique(d$participantID)) #140 participants

# these are the columns where the information is
table(d$fearComparisonProblemsIndex) # number of participants with fearComparisonProblemsIndex 1 to 3
table(d[d$fearComparisonProblemsIndex == 1,]$fearComparisonProblemsIndex,d[d$fearComparisonProblemsIndex == 1,]$participantID)
# these are the 6 participants with fearComparisonProblemsIndex = 1
table(d$criticalQuestion) # what did the participants choose (prison, education program)
table(d$dw) # were they in the dw condition?

# create a new data frame with the relevant information, for participants who got the dw
G.tmp.dw <- as.data.frame.matrix(table(d[d$dw == "yes",]$fearComparisonProblemsIndex,d[d$dw == "yes",]$criticalQuestion))
G.tmp.dw
# for each  score, how many participants chose which answer to the critical question?
#   Building new prisons Education programs
# 1                    1                  1
# 2                    7                 31
# 3                   12                 18

# give the first column a name
G.tmp.dw$fearComparisonProblemsIndex <- rownames(G.tmp.dw)

# add a proportion column
G.tmp.dw$prop = G.tmp.dw$`Building new prisons` / (G.tmp.dw$`Building new prisons` + G.tmp.dw$`Education programs`)
G.tmp.dw
# proportion of individuals who got the dw and chose one of the two answers, 
# given the fearComparisonProblemsIndex
# Building new prisons Education programs fearComparisonProblemsIndex      prop
# 1                  1                  1                           1 0.5000000
# 2                  7                 31                           2 0.1842105
# 3                 12                 18                           3 0.4000000

# add the info on "dw"
G.tmp.dw$dw = "yes"
G.tmp.dw

# create a new data frame with the relevant information, for participants who didn't get the dw
G.tmp.ndw <- as.data.frame.matrix(table(d[d$dw == "no",]$fearComparisonProblemsIndex,d[d$dw == "no",]$criticalQuestion))
G.tmp.ndw

# give the first column a name
G.tmp.ndw$fearComparisonProblemsIndex <- rownames(G.tmp.ndw)

# add a proportion column
G.tmp.ndw$prop = G.tmp.ndw$`Building new prisons` / (G.tmp.ndw$`Building new prisons` + G.tmp.ndw$`Education programs`)

# add the info on "dw"
G.tmp.ndw$dw = "no"
G.tmp.ndw

# bind the two dataframes
G = rbind(G.tmp.dw,G.tmp.ndw)
G

# make sure fearComparisonProblemsIndex is numeric (so that it is plotted numerically on x-axis)
str(G$fearComparisonProblemsIndex)
G$fearComparisonProblemsIndex = as.numeric(G$fearComparisonProblemsIndex)

#view(G)

# transform G to long
G = G %>%
  pivot_longer(!c(prop,dw,fearComparisonProblemsIndex), names_to = "choice", values_to = "count")
G

ggplot(data=G, aes(x=fearComparisonProblemsIndex, y=count,fill=choice)) +
  geom_bar(position="stack", stat="identity") +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  scale_x_continuous(n.breaks = 3, limits=c(0.5, 3.5)) +
  scale_y_continuous(n.breaks = 5, limits=c(0,40)) +
  facet_grid(. ~ dw) +
  ylab("count") +
  xlab("Fear Comparison Problems Index") 
ggsave("../graphs/fear-comparison-problems-index-1.pdf",height=3,width=6)




#### plot the data for proportion of equalityIndex ----
# the higher, the more the participant cares about fairness and equality

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

names(d)
# these are the columns where the information is

table(d$equalityIndex) # number of participants with equalityIndex 2 to 8
#    2  3  4  5  6  7  8 
#    11 13 11 19 22 20 44 

# x-axis: equalityIndex  (2 to 8)
# y-axis: the proportion of participants who had that equalityIndex score

# view(d$equalityIndex)

table(d[d$participantID < 31,]$equalityIndex) 
# the first 30 participants have these equalityIndex values

table(d$equalityIndex) # number of participants with equalityIndex 2 to 8
table(d[d$equalityIndex == "2",]$equalityIndex,d[d$equalityIndex == "2",]$participantID)
# these are all the participants with a equalityIndex of 2

count <- table(d$equalityIndex)

ggplot(d, aes(x = factor(equalityIndex))) +
  geom_bar() +
  labs(
    x = "Equality Index",
    y = "number of participants",
    title = "Proportion of Equality Index"
  ) +
  theme_minimal()
ggsave("../graphs/equality-index.pdf",height=3,width=6)


#### plot the data for equalityIndex ----

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

# x-axis: equalityIndex (2...8)
# y-axis: the proportion of participants who had that equalityIndex score and
# chose the prison response in the critical question
# plot the proportion by dogwhistle/no dogwhistle

table(d[d$participantID < 11,]$equalityIndex) 
# the first 10 participants have these moreTransPeopleIndex values

length(unique(d$participantID)) #140 participants

# these are the columns where the information is
table(d$equalityIndex) # number of participants with equalityIndex 2 to 8
table(d[d$equalityIndex == "2",]$equalityIndex,d[d$equalityIndex == "2",]$participantID)
# these are the 11 participants with equalityIndex = 2
table(d$criticalQuestion) # what did the participants choose (prison, education program)
table(d$dw) # were they in the dw condition?

# create a new data frame with the relevant information, for participants who got the dw
H.tmp.dw <- as.data.frame.matrix(table(d[d$dw == "yes",]$equalityIndex,d[d$dw == "yes",]$criticalQuestion))
H.tmp.dw
# for each equalityIndex score, how many participants chose which answer to the critical question?
#    Building new prisons Education programs
# 2                    4                  2
# 3                    3                  3
# 4                    3                  2

# give the first column a name
H.tmp.dw$equalityIndex <- rownames(H.tmp.dw)

# add a proportion column
H.tmp.dw$prop = H.tmp.dw$`Building new prisons` / (H.tmp.dw$`Building new prisons` + H.tmp.dw$`Education programs`)
H.tmp.dw
# proportion of individuals who got the dw and chose one of the two answers, 
# given the equalityIndex
#    Building new prisons Education programs equalityIndex      prop
# 2                    4                  2             2 0.6666667
# 3                    3                  3             3 0.5000000
# 4                    3                  2             4 0.6000000

# add the info on "dw"
H.tmp.dw$dw = "yes"
H.tmp.dw

# create a new data frame with the relevant information, for participants who didn't get the dw
H.tmp.ndw <- as.data.frame.matrix(table(d[d$dw == "no",]$equalityIndex,d[d$dw == "no",]$criticalQuestion))
H.tmp.ndw

# give the first column a name
H.tmp.ndw$equalityIndex <- rownames(H.tmp.ndw)

# add a proportion column
H.tmp.ndw$prop = H.tmp.ndw$`Building new prisons` / (H.tmp.ndw$`Building new prisons` +H.tmp.ndw$`Education programs`)

# add the info on "dw"
H.tmp.ndw$dw = "no"
H.tmp.ndw

# bind the two dataframes
H = rbind(H.tmp.dw,H.tmp.ndw)
H

# make sure equalityIndex is numeric (so that it is plotted numerically on x-axis)
str(H$equalityIndex)
H$equalityIndex = as.numeric(H$equalityIndex)

#view(H)

# transform G to long
H = H %>%
  pivot_longer(!c(prop,dw,equalityIndex), names_to = "choice", values_to = "count")
H

ggplot(data=H, aes(x=equalityIndex, y=count,fill=choice)) +
  geom_bar(position="stack", stat="identity") +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  scale_x_continuous(n.breaks = 8, limits=c(1.5, 8.5)) +
  scale_y_continuous(n.breaks = 5, limits=c(0,25)) +
  facet_grid(. ~ dw) +
  ylab("count") +
  xlab("Equality Index") 
ggsave("../graphs/equality-index-1.pdf",height=3,width=6)



#### plot the data for proportion of gender among participants ----

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

names(d)
# these are the columns where the information is

table(d$participantGender) # number of participants with gender Man/Woman
#     Man Woman 
#     71   69  

# x-axis: participantGender  (Man/Woman)
# y-axis: the proportion of participants who have that gender

table(d[d$participantID < 31,]$participantGender) 
# the first 30 participants have these participantGender values

table(d$participantGender) # number of participants with participantGender Man/Woman
table(d[d$participantGender == "Man",]$participantGender,d[d$participantGender == "Man",]$participantID)
# these are all the participants with a participantGender Man

count <- table(d$participantGender)

ggplot(d, aes(x = factor(participantGender))) +
  geom_bar() +
  labs(
    x = "Participant Gender",
    y = "number of participants",
    title = "Proportion of Participants' Gender"
  ) +
  theme_minimal()
ggsave("../graphs/participant-gender.pdf",height=3,width=6)

#### plot the data for participantGender ----

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

# x-axis: participantGender (21...83)
# y-axis: the proportion of participants who had that gender and
# chose the prison response in the critical question
# plot the proportion by dogwhistle/no dogwhistle

table(d[d$participantID < 11,]$participantGender) 
# the first 10 participants have these participantGender values

length(unique(d$participantID)) #140 participants

# these are the columns where the information is
table(d$participantGender) # number of participants with participantGender Man/Woman
table(d[d$participantGender == "Man",]$participantGender,d[d$participantGender == "Man",]$participantID)
# these are the 71 participants with participantGender = Man
table(d$criticalQuestion) # what did the participants choose (prison, education program)
table(d$dw) # were they in the dw condition?

# create a new data frame with the relevant information, for participants who got the dw
I.tmp.dw <- as.data.frame.matrix(table(d[d$dw == "yes",]$participantGender,d[d$dw == "yes",]$criticalQuestion))
I.tmp.dw
# for each participantGender, how many participants chose which answer to the critical question?
#    Building new prisons Education programs
# Man                     13                 23
# Woman                    7                 27

# give the first column a name
I.tmp.dw$participantGender <- rownames(I.tmp.dw)

# add a proportion column
I.tmp.dw$prop = I.tmp.dw$`Building new prisons` / (I.tmp.dw$`Building new prisons` + I.tmp.dw$`Education programs`)
I.tmp.dw
# proportion of individuals who got the dw and chose one of the two answers, 
# given the participantGender
#        Building new prisons Education programs participantGender      prop
# Man                     13                 23               Man     0.3611111
# Woman                    7                 27             Woman     0.2058824

# add the info on "dw"
I.tmp.dw$dw = "yes"
I.tmp.dw

# create a new data frame with the relevant information, for participants who didn't get the dw
I.tmp.ndw <- as.data.frame.matrix(table(d[d$dw == "no",]$participantGender,d[d$dw == "no",]$criticalQuestion))
I.tmp.ndw

# give the first column a name
I.tmp.ndw$participantGender <- rownames(I.tmp.ndw)

# add a proportion column
I.tmp.ndw$prop = I.tmp.ndw$`Building new prisons` / (I.tmp.ndw$`Building new prisons` +I.tmp.ndw$`Education programs`)

# add the info on "dw"
I.tmp.ndw$dw = "no"
I.tmp.ndw

# bind the two dataframes
I = rbind(I.tmp.dw,I.tmp.ndw)
I

# make sure participantGender is numeric (so that it is plotted numerically on x-axis)
str(I$participantGender)

#view(I)

# transform G to long
I = I %>%
  pivot_longer(!c(prop,dw,participantGender), names_to = "choice", values_to = "count")
I

ggplot(data = I, aes(x = participantGender, y = count, fill = choice)) +
  geom_bar(position = "stack", stat = "identity") +
  facet_grid(. ~ dw) +
  theme(legend.position = "top",
        axis.text.y = element_text(size = 10)) +
  labs(
    y = "count",
    x = "Participants' Gender"
  )
ggsave("../graphs/participant-gender-1.pdf",height=3,width=6)

#### plot the data for proportion of participantAge ----

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

names(d)
# these are the columns where the information is

table(d$participantAge) # number of participants with age 21 to 83
#    21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 49 50 52 53 55 56 57 58 60 61 62 63 64 65 70 76 77 78 80 83 
#    1  4  4  2  5  4  3  5  5  4  7  3  5  3  5  3  4  6  7  3  3  2  6  4  6  3  3  2  1  3  1  2  1  1  3  1  3  1  3  1  1  1  1  1  1  1  1

# x-axis: participantAge  (21 to 83)
# y-axis: the proportion of participants who are that age

# view(d$participantAge)

table(d[d$participantID < 31,]$participantAge) 
# the first 30 participants have these participantAge values

table(d$participantAge) # number of participants with participantAge 21 to 83
table(d[d$participantAge == "21",]$participantAge,d[d$participantAge == "21",]$participantID)
# these are all the participants with a participantAge of 21

count <- table(d$participantAge)

ggplot(d, aes(x = participantAge)) +
  geom_bar() +
  scale_x_continuous(breaks = seq(20, 90, by = 10)) +
  labs(
    x = "Participant Age",
    y = "number of participants",
    title = "Proportion of Participant Age"
  ) 
ggsave("../graphs/participant-age.pdf",height=3,width=6)

##### plot the data for participantAge ----

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

# x-axis: participantAge (21...83)
# y-axis: the proportion of participants who had that participantAge and
# chose the prison response in the critical question
# plot the proportion by dogwhistle/no dogwhistle

table(d[d$participantID < 11,]$participantAge) 
# the first 10 participants have these participantAge values

names(d)
# these are the columns where the information is
table(d$participantAge) # number of participants with participantAge 21 to 83
table(d[d$participantAge == 21,]$participantAge,d[d$participantAge == 21,]$participantID)
# this the 1 participant with participantAge = 21
table(d$criticalQuestion) # what did the participants choose (prison, education program)
table(d$dw) # were they in the dw condition?

# create a new data frame with the relevant information, for participants who got the dw
J.tmp.dw <- as.data.frame.matrix(table(d[d$dw == "yes",]$participantAge,d[d$dw == "yes",]$criticalQuestion))
J.tmp.dw 

# give the first column a name
J.tmp.dw$participantAge <- rownames(J.tmp.dw)

# add a proportion column
J.tmp.dw$prop = J.tmp.dw$`Building new prisons` / (J.tmp.dw$`Building new prisons` + J.tmp.dw$`Education programs`)
J.tmp.dw
# proportion of individuals who got the dw and chose one of the two answers, 
# given the participantAge
#     Building new prisons Education programs participantAge      prop
# 21                    0                  1             21 0.0000000
# 22                    1                  2             22 0.3333333
# 23                    2                  2             23 0.5000000

# add the info on "dw"
J.tmp.dw$dw = "yes"
J.tmp.dw

# create a new data frame with the relevant information, for participants who didn't get the dw
J.tmp.ndw <- as.data.frame.matrix(table(d[d$dw == "no",]$participantAge,d[d$dw == "no",]$criticalQuestion))
J.tmp.ndw

# give the first column a name
J.tmp.ndw$participantAge <- rownames(J.tmp.ndw)

# add a proportion column
J.tmp.ndw$prop = J.tmp.ndw$`Building new prisons` / (J.tmp.ndw$`Building new prisons` + J.tmp.ndw$`Education programs`)

# add the info on "dw"
J.tmp.ndw$dw = "no"
J.tmp.ndw

# bind the two dataframes
J = rbind(J.tmp.dw,J.tmp.ndw)
J

# make sure participantAge is numeric (so that it is plotted numerically on x-axis)
str(J$participantAge)
J$participantAge = as.numeric(J$participantAge)

# view(J)

# transform J to long
J = J %>%
  pivot_longer(!c(prop,dw,participantAge), names_to = "choice", values_to = "count")
J

ggplot(data=J, aes(x=participantAge, y=count,fill=choice)) +
  geom_bar(position="stack", stat="identity") +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  scale_x_continuous(n.breaks = 10, limits=c(20.5, 84)) +
  scale_y_continuous(n.breaks = 5, limits=c(0,5)) +
  facet_grid(. ~ dw) +
  ylab("count") +
  xlab("Participants' Age") 
ggsave("../graphs/participant-age-1.pdf",height=3,width=6)

