# dog whistles pilot 1: does the political party of the speaker have an effect on 
# dog whistle interpretation?
# graphs.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load helper functions
source('helpers.R')

# color-blind-friendly palette
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# load clean data ----
d = read.csv("../data/d.csv")
nrow(d) #2156

# which properties do the participants have?

# gender
d %>% 
  select(gender, participantID) %>% 
  unique() %>% 
  group_by(gender) %>% 
  summarize(count=n())

# age
table(d$age) #ages 18-71
mean(d$age) #31

# ethnicity
d %>% 
  select(ethnicity, participantID) %>% 
  unique() %>% 
  group_by(ethnicity) %>% 
  summarize(count=n())

# education
d %>% 
  select(education, participantID) %>% 
  unique() %>% 
  group_by(education) %>% 
  summarize(count=n())

# migration
d %>% 
  select(migration, participantID) %>% 
  unique() %>% 
  group_by(migration) %>% 
  summarize(count=n())

# which party did they say they would vote for?
d %>% 
  select(partyChoice, participantID) %>% 
  unique() %>% 
  group_by(partyChoice) %>% 
  summarize(count=n())

# create "other" category for minor choices
d = d %>%
  mutate(partyChoice = case_when(partyChoice == "AfD" ~ "AfD",
                                 partyChoice == "CDU" ~ "CDU",
                                 partyChoice == "FDP" ~ "FDP",
                                 partyChoice == "Grüne" ~ "Grüne",
                                 partyChoice == "Linke" ~ "Linke",
                                 partyChoice == "SPD" ~ "SPD",
                                 TRUE ~ "other"))

# responses to 5 WOM questions ----

names(d)
# what does "ich stimme zu" mean for the different questions?
# migrationsfeindlich or migrationsfreundlich?
# [8] "womNachzug": "ich stimme zu" = feindlich
# [9] "womKopftuch": "ich stimme zu" = freundlich
# [10] "womStaatsangehörigkeit" "ich stimme zu" = freundlich
# [11] "womIslam" "ich stimme zu" = freundlich
# [12] "womAsyl" "ich stimme zu" = feindlich

# define numeric values for each position (higher = more feindlich)
d$womNachzug.num <- ifelse(d$womNachzug == "Ich stimme zu", 1, 
                             ifelse(d$womNachzug == "Neutral", 0, -1))
d$womKopftuch.num <- ifelse(d$womKopftuch == "Ich stimme nicht zu", 1, 
                              ifelse(d$womKopftuch == "Neutral", 0, -1))
d$womStaatsangehoerigkeit.num <- ifelse(d$womStaatsangehoerigkeit == "Ich stimme nicht zu", 1, 
                                ifelse(d$womStaatsangehoerigkeit == "Neutral", 0, -1))
d$womIslam.num <- ifelse(d$womIslam == "Ich stimme nicht zu", 1, 
                          ifelse(d$womIslam == "Neutral", 0, -1))
d$womAsyl.num <- ifelse(d$womAsyl == "Ich stimme zu", 1, 
                           ifelse(d$womAsyl == "Neutral", 0, -1))


# how migration (un)friendly is each participant? 

d$mean.wom.score = (d$womNachzug.num + d$womKopftuch.num + d$womStaatsangehoerigkeit.num + d$womIslam.num + d$womAsyl.num) / 5
# lowest possible: -1 (least friendly)
# highest possible: 1 (most friendly)

d %>%
  group_by(participantID,mean.wom.score) %>% 
  tally

# sort participants by their mean.wom.score
d = d %>%
  mutate(participantID = fct_reorder(as.factor(participantID),mean.wom.score))  

# merge partyChoice to sensible categories: AfD (blau), FDP, Grüne, Linke, SPD, CDU, other
table(d$partyChoice)
d$partyChoice <- as.factor(d$partyChoice)
levels(d$partyChoice)

# plot participants by their mean.wom.score, color code by party
ggplot(d, aes(x=participantID, y=mean.wom.score,color=party,fill=partyChoice)) +
  geom_point(shape=21, size=3, alpha=1,color="black") +
  scale_fill_manual(values=c("#56B4E9", "black", "yellow","#009E73","red", "white", "purple")) +
  xlab("Participant") +
  ylab("Mean wom score (higher = more conservative)")
ggsave("../graphs/mean-response-to-wom-questions-by-participant.pdf",height=4,width=9)

# anti-migration responses by item, expression and politician's party ----

# reduce data to target data
t = d %>%
  filter(party != "spd")
table(t$party)

table(t$party,t$dw,t$item)

means = t %>%
  group_by(item, dw, party) %>%
  summarize(Mean = mean(response),CILow=ci.low(response),CIHigh=ci.high(response)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
  select(-c(CILow, CIHigh))
means

# order items based on mean response for afd/dw variant
# get mean response for afd/dw variant
means.afdDw = means %>%
  filter(party == "afd" & dw == "dw") %>%
  mutate(item = fct_reorder(as.factor(item),Mean)) 
means.afdDw
levels(means.afdDw$item)

# order items based on mean response for afd/dw variant
means = means %>%
  mutate(item = fct_relevel(item,levels(means.afdDw$item))) %>%
  mutate(dw = fct_relevel(dw,"ndw")) %>%
  mutate(party = fct_relevel(party, "afd"))
t = t %>%
  mutate(item = fct_relevel(item,levels(means.afdDw$item))) %>%
  mutate(dw = fct_relevel(dw,"ndw")) %>%
  mutate(party = fct_relevel(party, "afd"))
levels(means$item)
levels(t$item)
levels(means$party)
levels(t$party)

# boxplot of responses, with mean response
ggplot() +
  geom_violin(data=t, aes(x=dw, y=response, fill = party), alpha=1) +
  scale_fill_manual(values=c("#56B4E9","#009E73"))  +
  facet_wrap(. ~ item, ncol = 1, strip.position = "right") +
  theme(strip.background = element_rect(fill="gray90"), 
        strip.text.y = element_text(angle = 0)) +
  geom_point(data=means, aes(x=dw, y=Mean), shape=20, size=3, 
             alpha=1, color = "black", position = position_dodge(width=.85)) +
  geom_errorbar(data=means, aes(ymin=YMin, ymax=YMax)) +
  theme(legend.position = "top") +
  xlab("") +
  ylab("Response \n (1 = unfriendly towards migration, 5 = friendly towards migration)") + 
  coord_flip()
ggsave("../graphs/response-by-item-and-party-and-type.pdf",height=10,width=8)

# further questions to ask:
# how should the participants demographic information enter the model? (predictor or random effect?)
# what can we say about how the ingroup vs outgroup responds to the items?

