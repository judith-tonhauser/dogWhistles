# dogwhistle project
# analysis.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load relevant packages 
library(tidyverse)
library(lme4)
library(lmerTest)

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

theme_set(theme_bw())

# Hurwitz & Peffley 2005
# p.105: To test our hypotheses, we regressed Anticrime Policy Preferences on the predictor
# variables, a dummy variable representing the question frame (coded 1 for inner-city reference)
# and 0 otherwise), and interactions for each of the predictors and question frame

# logistic mixed-effects regression to approximate Fig 1A in Hurwitz & Peffley ----

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

# predict answer to critical question (prison, education program) from 
# dogwhistle (dw: yes, no)
# transStereotypeIndex (numeric)


# change variables for logistic mixed-effects model

# critical question is dependent variable (0,1)
d = d %>%
  mutate(criticalQuestionBinary = case_when(
    criticalQuestion == "Education programs" ~ 0,
    criticalQuestion == "Building new prisons" ~ 1,
    TRUE ~ 666))
table(d$criticalQuestionBinary)

# transStereotypeIndex is numeric
str(d$transStereotypeIndex)

# dogwhistle is a factor (0, 1)
d = d %>%
  mutate(dwBinary = case_when(
    dw == "yes" ~ 1,
    dw == "no" ~ 0,
    TRUE ~ 666))
d$dwBinary <- as.factor(d$dwBinary)
str(d$dwBinary)
table(d$dwBinary)
table(d$dwBinary,d$criticalQuestionBinary)

# check that all is well
summary(d)
  
# set reference levels
d = d %>%
  mutate(dwBinary = fct_relevel(dwBinary, "0"))

# logistic mixed-effects model

m = glm(criticalQuestionBinary ~ dw+transStereotypeIndex, data=d, 
          family = binomial)
summary(m)

m2 = glm(criticalQuestionBinary ~ dw*transStereotypeIndex, data=d, 
        family = binomial)
summary(m2)

anova(m,m2) # model with interaction is not better

