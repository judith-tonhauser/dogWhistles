# dog whistles pilot 1: does the political party of the speaker have an effect on 
# dog whistle interpretation?
# analysis.R

# load required libraries
library(ordinal)

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load clean data ----
d = read.csv("../data/d.csv")
nrow(d) #1456

# only consider the target data
t = d %>%
  filter(party != "spd")
table(t$party)

# make response a factor
t$response = as.factor(t$response)

# set reference levels
t = t %>% 
  mutate(dw = fct_relevel(dw, "dw")) %>%
  mutate(party = fct_relevel(party, "afd"))

# fit ordinal model
m = clmm(response ~ dw*party + (1|participantID) + (1|item), data = t)
summary(m)
m.1 = clmm(response ~ dw+party + (1|participantID) + (1|item), data = t)
summary(m.1)
anova(m,m.1) # more complex model is not better
