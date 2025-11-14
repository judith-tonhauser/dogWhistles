# dogwhistle project
# analysis.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load relevant packages 
library(tidyverse)
library(ordinal)

# Hurwitz & Peffley 2005
# p.105: To test our hypotheses, we regressed Anticrime Policy Preferences on the predictor
# variables, a dummy variable representing the question frame (coded 1 for inner-city reference)
# and 0 otherwise), and interactions for each of the predictors and question frame, 
# using ordered probit analysis

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

# make sure the variables have the write types

# targetResponse is the dependent variable (1-4)
table(d$targetResponse)
str(d$targetResponse) #needs to be an ordered factor
d$targetResponse <- as.factor(d$targetResponse)

# the two predictor variables are transStereotypeIndex and genderFairnessIndex
str(d$transStereotypeIndex) #numeric
str(d$genderFairnessIndex) #numeric

# the question frame is a factor, with no/0 and yes/1
str(d$dw)
d = d %>%
  mutate(dwFactor = case_when(
    dw == "yes" ~ 1,
    dw == "no" ~ 0,
    TRUE ~ 666))
d$dwFactor <- as.factor(d$dwFactor)
str(d$dwFactor)

# fit the model ----
m <- clmm(
  targetResponse ~ transStereotypeIndex + genderFairnessIndex 
  + transStereotypeIndex:dwFactor + genderFairnessIndex:dwFactor
  + participantGenderNum
  + participantAge
  + preregistered
  #+ (1|participantID),
  + (1|cisStereotypeIndex) + (1|generalFairnessIndex),
  data = d,
  link = "probit"
)
summary(m)
# only transStereotypeIndex is significant

m <- clmm(
  targetResponse ~ transStereotypeIndex 
  + transStereotypeIndex:dwFactor 
  + participantGenderNum
  + participantAge
  + preregistered
  #+ (1|participantID),
  + (1|cisStereotypeIndex) + (1|generalFairnessIndex),
  data = d,
  link = "probit"
)
summary(m)
# transStereotypeIndex and interaction significant

m <- clmm(
  targetResponse ~ genderFairnessIndex 
  + genderFairnessIndex:dwFactor 
  + participantGenderNum
  + participantAge
  + preregistered
  #+ (1|participantID),
  + (1|cisStereotypeIndex) + (1|generalFairnessIndex),
  data = d,
  link = "probit"
)
summary(m)
# genderFairnessIndex significant, interaction marginal

# plot the model output (haven't adjusted this yet) ----

library(ggeffects)
library(ggplot2)

pred <- ggpredict(m, terms = c("transStereotypeIndex", "dwFactor"))
pred
colnames(pred)

prob_high <- pred %>%
  filter(response.level %in% c("3", "4")) %>%   # Use response.level, not facet
  group_by(x, group) %>%
  summarize(prob = sum(predicted),
            conf.low = sum(conf.low),
            conf.high = sum(conf.high),
            .groups = "drop")

# Plot
ggplot(prob_high, aes(x = x, y = prob, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(
    x = "transStereotypeIndex",
    y = "Probability of targetResponse = 3 or 4",
    color = "dwFactor",
    fill = "dwFactor",
    title = "Predicted Probability of High targetResponse by transStereotypeIndex and dwFactor"
  ) +
  theme_minimal(base_size = 14)

pred <- ggpredict(m, terms = c("genderFairnessIndex", "dwFactor"))
pred
colnames(pred)

prob_high <- pred %>%
  filter(response.level %in% c("3", "4")) %>%   # Use response.level, not facet
  group_by(x, group) %>%
  summarize(prob = sum(predicted),
            conf.low = sum(conf.low),
            conf.high = sum(conf.high),
            .groups = "drop")

# Plot
ggplot(prob_high, aes(x = x, y = prob, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(
    x = "transStereotypeIndex",
    y = "Probability of targetResponse = 3 or 4",
    color = "dwFactor",
    fill = "dwFactor",
    title = "Predicted Probability of High targetResponse by transStereotypeIndex and dwFactor"
  ) +
  theme_minimal(base_size = 14)
