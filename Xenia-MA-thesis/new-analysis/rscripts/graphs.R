# dogwhistle project
# graphs.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load relevant packages and set background color
library(tidyverse)

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

theme_set(theme_bw())

# plot the data to approximate Hurwitz & Peffley Fig 1A ----

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

# x-axis: transStereotypeIndex (5...35)
# y-axis: the proportion of participants who had that transStereotypeIndex score and
# chose the prison response in the critical question
# plot the proportion by dogwhistle/no dogwhistle

table(d[d$participantID < 11,]$transStereotypeIndex) 
# the first 10 participants have these transStereotypeIndex values

length(unique(d$participantID)) #140 participants

# these are the columns where the information is
table(d$transStereotypeIndex) # number of participants with transStereotypeIndex 5 to 35
table(d[d$transStereotypeIndex == 5,]$transStereotypeIndex,d[d$transStereotypeIndex == 5,]$participantID)
# these are the 30 participants with transStereotypeIndex = 5
table(d$criticalQuestion) # what did the participants choose (prison, education program)
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

#plot 
ggplot(data=A, aes(x=transStereotypeIndex, y=prop, group = dw, color = dw, fill = dw)) +
  geom_point(shape=21, size = 2) + 
  scale_fill_manual(values=c("#E69F00","#56B4E9")) +
  geom_smooth(method = "lm", se = TRUE) + 
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  scale_x_continuous(n.breaks = 10, limits=c(5, 35)) +
  scale_y_continuous(n.breaks = 5, limits=c(-0.1, 1)) +
  ylab("Proportion in favor of prisons") +
  xlab("Trans Stereotype Index") 
ggsave("../graphs/1A-based-on-data.pdf",height=3,width=6)
 
ggplot(data=A, aes(x=transStereotypeIndex, y=prop, group = dw, color = dw, fill = dw)) +
  geom_point(shape=21, size = 2) + 
  scale_fill_manual(values=c("#E69F00","#56B4E9")) +
  geom_smooth(method = "glm", 
              method.args = list(family = "quasibinomial"), 
              se = TRUE) + 
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  scale_x_continuous(n.breaks = 10, limits=c(5, 35)) +
  scale_y_continuous(n.breaks = 5, limits=c(-0.1, 1)) +
  ylab("Proportion in favor of prisons") +
  xlab("Trans Stereotype Index") 

d = d %>%
  mutate(criticalQuestionBinary = case_when(
    criticalQuestion == "Education programs" ~ 0,
    criticalQuestion == "Building new prisons" ~ 1,
    TRUE ~ 666))
table(d$criticalQuestionBinary)
d$criticalQuestionBinary = as.integer(d$criticalQuestionBinary)
str(d$criticalQuestionBinary)

ggplot(d, aes(x=transStereotypeIndex, y=criticalQuestionBinary, group = dw, color = dw, fill = dw)) + 
  geom_jitter(shape=21, size = 2, width = 0.1, height = 0.1) + 
  scale_fill_manual(values=c("#E69F00","#56B4E9")) +
  stat_smooth(method="glm", se=TRUE, 
              method.args = list(family=binomial))

# plot the data to approximate Hurwitz & Peffley Fig 1B ----

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

# x-axis: genderFairnessIndex (4...23)
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

#plot 
ggplot(data=B, aes(x=genderFairnessIndex, y=prop, group = dw, color = dw, fill = dw)) +
  geom_point(shape=21, size = 2) + 
  scale_fill_manual(values=c("#E69F00","#56B4E9")) +
  geom_smooth(method = "lm", se = TRUE) + 
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  scale_x_continuous(limits=c(4, 23), breaks=c(4,10,15,20,23),
                     labels=c("4","10","15","20","23")) +
  scale_y_continuous(n.breaks = 5, limits=c(-0.1, 1)) +
  ylab("Proportion in favor of prisons") +
  xlab("Gender Fairness Index") 
ggsave("../graphs/1B-based-on-data.pdf",height=3,width=6)

ggplot(data=B, aes(x=genderFairnessIndex, y=prop, group = dw, color = dw, fill = dw)) +
  geom_point(shape=21, size = 2) + 
  scale_fill_manual(values=c("#E69F00","#56B4E9")) +
  geom_smooth(method = "glm", 
              method.args = list(family = "quasibinomial"), 
              se = TRUE) + 
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  scale_x_continuous(limits=c(4, 23), breaks=c(4,10,15,20,23),
                     labels=c("4","10","15","20","23")) +
  scale_y_continuous(n.breaks = 5, limits=c(-0.1, 1)) +
  ylab("Proportion in favor of prisons") +
  xlab("Gender Fairness Index")
ggsave("../graphs/1Bv2-based-on-data.pdf",height=3,width=6)

d = d %>%
  mutate(criticalQuestionBinary = case_when(
    criticalQuestion == "Education programs" ~ 0,
    criticalQuestion == "Building new prisons" ~ 1,
    TRUE ~ 666))
table(d$criticalQuestionBinary)
d$criticalQuestionBinary = as.integer(d$criticalQuestionBinary)
str(d$criticalQuestionBinary)

ggplot(d, aes(x=genderFairnessIndex, y=criticalQuestionBinary, group = dw, color = dw, fill = dw)) + 
  geom_jitter(shape=21, size = 2, width = 0.1, height = 0.2) + 
  scale_fill_manual(values=c("#E69F00","#56B4E9")) +
  stat_smooth(method="glm", se=TRUE, 
              method.args = list(family=binomial)) +
  ylab("Proportion in favor of prisons") +
  xlab("Gender Fairness Index")
ggsave("../graphs/1Bv3-based-on-data.pdf",height=3,width=6)
