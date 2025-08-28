

# Set working directory to directory of script:

tryCatch(
  setwd(dirname(rstudioapi::getSourceEditorContext()$path)),
  error = function(e) setwd("experiments/Pilot02/scripts/"))

# Load general definitions (also loads standard packages such as
# tidyverse):

theme_set(theme_bw())

source("../../../scripts/general.R")
source("../../../scripts/helpers.R")

# load clean data ----

d = read.csv("../generated/data/d-preprocessed.csv")
nrow(d) #110

# do the Wahlomat questions correlate with the party that the participants would have voted for? ----

stringdistmatrix(d$party, names(colors.party), method="jw") -> m

d$party <- names(colors.party)[apply(m, 1, which.min)]

c("CDU"                  = "black",
  "SPD"                  = "red",
  "AfD"                  = "blue",
  "FDP"                  = "yellow",
  "Linke"                = "purple",
  "Grüne"                = "darkgreen",
  "Die Partei"           = "#B92837",
  "Humanisten"           = "#d90368",
  "Volt"                 = "#562883",
  "keine Angabe"         = "grey") -> colors.party

ggplot(d,aes(x=wom.citizenship, fill=party)) +
  geom_bar(stat="count", width=1) +
  scale_fill_manual(values=colors.party) + 
  scale_x_discrete("In Deutschland soll es generell möglich sein, neben der deutschen eine zweite Staatsbürgerschaft zu haben.")
ggsave("../generated/plots/wom.citizenship-by-party.pdf",height=4,width=9)


ggplot(d,aes(x=wom.refugees , fill=party)) +
  geom_bar(stat="count", width=1) +
  scale_fill_manual(values=colors.party) + 
  scale_x_discrete("Asyl soll weiterhin nur politisch Verfolgten gewährt werden.")
ggsave("../generated/plots/wom.refugees-by-party.pdf",height=4,width=9)

ggplot(d,aes(x=wom.headscarf , fill=party)) +
  geom_bar(stat="count", width=1) +
  scale_fill_manual(values=colors.party) + 
  scale_x_discrete("Das Tragen eines Kopftuchs soll Beamtinnen im Dienst generell erlaubt sein.")
ggsave("../generated/plots/wom.headscarf-by-party.pdf",height=4,width=9)

ggplot(d,aes(x=wom.islam , fill=party)) +
  geom_bar(stat="count", width=1) +
  scale_fill_manual(values=colors.party) + 
  scale_x_discrete("Islamische Verbände sollen als Religionsgemeinschaften staatlich anerkannt werden können.")
ggsave("../generated/plots/wom.islam-by-party.pdf",height=4,width=9)

ggplot(d,aes(x=wom.asylum , fill=party)) +
  geom_bar(stat="count", width=1) +
  scale_fill_manual(values=colors.party) + 
  scale_x_discrete("Das Recht anerkannter Flüchtlinge auf Familiennachzug soll abgeschafft werden.")
ggsave("../generated/plots/wom.asylum-by-party.pdf",height=4,width=9)

# does the presence of a dog whistle have an impact on how the politician is evaluated ----

pdf("../generated/plots/social_dimensions.pdf", 6, 6)

### Age:

d %>%
  group_by(sentence.label, dog.whistle, page) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(page, p)) +
  geom_col() +
  scale_x_discrete("How old (alt) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(sentence.label))

### Progressive:

d %>%
  group_by(sentence.label, dog.whistle, pprog) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(pprog, p)) +
  geom_col() +
  scale_x_discrete("How progressive (fortschrittlich) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(sentence.label))

### Racism:

d %>%
  group_by(sentence.label, dog.whistle, pracism) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(pracism, p)) +
  geom_col() +
  scale_x_discrete("How racist (rassistisch) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(sentence.label))

### Honesty:

d %>%
  group_by(sentence.label, dog.whistle, phonesty) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(phonesty, p)) +
  geom_col() +
  scale_x_discrete("How honest (ehrlich) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(sentence.label))

### Helpfulness:

d %>%
  group_by(sentence.label, dog.whistle, phelpful) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(phelpful, p)) +
  geom_col() +
  scale_x_discrete("How helpful (hilfsbereit) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(sentence.label))

### Intelligence:

d %>%
  group_by(sentence.label, dog.whistle, pintel) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(pintel, p)) +
  geom_col() +
  scale_x_discrete("How intelligent (intelligent) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(sentence.label))

### Christianity:

d %>%
  group_by(sentence.label, dog.whistle, pchristian) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(pchristian, p)) +
  geom_col() +
  scale_x_discrete("How christian (christlich) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(sentence.label))

### Friendliness:

d %>%
  group_by(sentence.label, dog.whistle, pfriendly) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(pfriendly, p)) +
  geom_col() +
  scale_x_discrete("How friendly (freundlich) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(sentence.label))

### Party:

d %>%
  group_by(sentence.label, dog.whistle, pparty) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(pparty, p, fill=pparty)) +
  geom_col() +
  scale_fill_manual(values=colors.party) +
  scale_x_discrete("To which party does the politican belong?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  theme(axis.text.x=element_text(angle = 45, hjust=1),
        legend.position="none") +
  facet_grid(vars(dog.whistle), vars(sentence.label))

dev.off()


# how did the participants answer the 5 WOM questions? ----

names(d)
#"wom.refugees": conservative = agree      
#"wom.headscarf": conservative = not agree
#"wom.citizenship": conservative = not agree
#"wom.islam": conservative = not agree
#"wom.asylum": conservative = agree

table(d$wom.asylum)

# define numeric values for each position (higher = more conservative)
d$wom.refugees.num <- ifelse(d$wom.refugees == "Ich stimme zu", 2, 
                             ifelse(d$wom.refugees == "Neutral", 1, 0))
d$wom.headscarf.num <- ifelse(d$wom.headscarf == "Ich stimme nicht zu", 2, 
                             ifelse(d$wom.headscarf == "Neutral", 1, 0))
d$wom.citizenship.num <- ifelse(d$wom.citizenship == "Ich stimme nicht zu", 2, 
                              ifelse(d$wom.citizenship == "Neutral", 1, 0))
d$wom.islam.num <- ifelse(d$wom.islam == "Ich stimme nicht zu", 2, 
                             ifelse(d$wom.islam == "Neutral", 1, 0))
d$wom.asylum.num <- ifelse(d$wom.asylum == "Ich stimme zu", 2, 
                             ifelse(d$wom.asylum == "Neutral", 1, 0))


# how conservative is each participant? 

d$mean.wom.score = (d$wom.refugees.num + d$wom.headscarf.num + d$wom.citizenship.num + d$wom.islam.num + d$wom.asylum.num) / 5
# lowest possible: 0 (least conservative)
# highest possible: 10/5=2 (most conservative)

d = d %>%
  mutate(
    conservative = ifelse(mean.wom.score > .99, "more conservative",
                          ifelse(mean.wom.score < .5, "less conservative", "neutral")))

length(unique(d$subj)) #55 participants

table(d$conservative)
#less conservative more conservative           neutral 
#50                32                28

d = d %>%
  mutate(subj = fct_reorder(as.factor(subj),mean.wom.score))  

ggplot(d, aes(x=subj, y=mean.wom.score,color=party,fill=party)) +
  geom_point(shape=21, size=3, alpha=1,color="black") +
  scale_fill_manual(values=colors.party) +
  xlab("Participant") +
  ylab("Mean wom score (higher = more conservative)")
ggsave("../generated/plots/mean-response-to-wom-questions-by-participant.pdf",height=4,width=9)

# highest mean score is 2 (out of 2)!


# which party did these participants say they would vote for?
table(d$party,d$conservative)

# less conservative
# AfD                          0
# CDU                          2
# Die Partei                   2
# FDP                          4
# Grüne                       30
# Humanisten                   2
# keine Angabe                 6
# Linke                       18
# SPD                          8
# Volt                         6
# 
# more conservative
# AfD                          2
# CDU                          4
# Die Partei                   2
# FDP                          6
# Grüne                        6
# Humanisten                   0
# keine Angabe                 2
# Linke                        0
# SPD                          8
# Volt                         2


# participants' ratings on "Staatsvolk" by how conservative they are ----  

table(d$sentence.label)

table(cons$dog.whistle)

pdf("../generated/plots/social_dimensions-on-Staatsvolk-by-wom-mean-score.pdf", 6, 6)

### Age:

cons %>%
  group_by(conservative, dog.whistle, page) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(page, p)) +
  geom_col() +
  scale_x_discrete("How old (alt) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(conservative))

### Progressive:

cons %>%
  group_by(conservative, dog.whistle, pprog) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(pprog, p)) +
  geom_col() +
  scale_x_discrete("How progressive (fortschrittlich) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(conservative))

### Racism:

cons %>%
  group_by(conservative, dog.whistle, pracism) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(pracism, p)) +
  geom_col() +
  scale_x_discrete("How racist (rassistisch) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(conservative))

### Honesty:

cons %>%
  group_by(conservative, dog.whistle, phonesty) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(phonesty, p)) +
  geom_col() +
  scale_x_discrete("How honest (ehrlich) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(conservative))

### Helpfulness:

cons %>%
  group_by(conservative, dog.whistle, phelpful) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(phelpful, p)) +
  geom_col() +
  scale_x_discrete("How helpful (hilfsbereit) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(conservative))

### Intelligence:

cons %>%
  group_by(conservative, dog.whistle, pintel) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(pintel, p)) +
  geom_col() +
  scale_x_discrete("How intelligent (intelligent) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(conservative))

### Christianity:

cons %>%
  group_by(conservative, dog.whistle, pchristian) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(pchristian, p)) +
  geom_col() +
  scale_x_discrete("How christian (christlich) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(conservative))

### Friendliness:

cons %>%
  group_by(conservative, dog.whistle, pfriendly) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(pfriendly, p)) +
  geom_col() +
  scale_x_discrete("How friendly (freundlich) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(conservative))

### Party:

cons %>%
  group_by(conservative, dog.whistle, pparty) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(pparty, p, fill=pparty)) +
  geom_col() +
  scale_fill_manual(values=colors.party) +
  scale_x_discrete("To which party does the politican belong?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  theme(axis.text.x=element_text(angle = 45, hjust=1),
        legend.position="none") +
  facet_grid(vars(dog.whistle), vars(conservative))

dev.off()

# participants' ratings on "Volk" by how conservative they are ----  

table(d$sentence.label)

cons = d %>%
  filter(sentence.label == "Zukunft-unseres-Volkes") %>%
  mutate(
    conservative = ifelse(mean.wom.score > .99, "more conservative","less conservative"))

length(unique(cons$subj)) #55 participants

table(cons$conservative)
table(cons$dog.whistle)

pdf("../generated/plots/social_dimensions-Volk-by-wom-mean-score.pdf", 6, 6)

### Age:

cons %>%
  group_by(conservative, dog.whistle, page) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(page, p)) +
  geom_col() +
  scale_x_discrete("How old (alt) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(conservative))

### Progressive:

cons %>%
  group_by(conservative, dog.whistle, pprog) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(pprog, p)) +
  geom_col() +
  scale_x_discrete("How progressive (fortschrittlich) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(conservative))

### Racism:

cons %>%
  group_by(conservative, dog.whistle, pracism) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(pracism, p)) +
  geom_col() +
  scale_x_discrete("How racist (rassistisch) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(conservative))

### Honesty:

cons %>%
  group_by(conservative, dog.whistle, phonesty) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(phonesty, p)) +
  geom_col() +
  scale_x_discrete("How honest (ehrlich) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(conservative))

### Helpfulness:

cons %>%
  group_by(conservative, dog.whistle, phelpful) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(phelpful, p)) +
  geom_col() +
  scale_x_discrete("How helpful (hilfsbereit) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(conservative))

### Intelligence:

cons %>%
  group_by(conservative, dog.whistle, pintel) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(pintel, p)) +
  geom_col() +
  scale_x_discrete("How intelligent (intelligent) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(conservative))

### Christianity:

cons %>%
  group_by(conservative, dog.whistle, pchristian) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(pchristian, p)) +
  geom_col() +
  scale_x_discrete("How christian (christlich) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(conservative))

### Friendliness:

cons %>%
  group_by(conservative, dog.whistle, pfriendly) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(pfriendly, p)) +
  geom_col() +
  scale_x_discrete("How friendly (freundlich) is the politician?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  facet_grid(vars(dog.whistle), vars(conservative))

### Party:

cons %>%
  group_by(conservative, dog.whistle, pparty) %>%
  summarize(n = n()) %>%
  mutate(p = n / sum(n)) %>%
  ggplot(aes(pparty, p, fill=pparty)) +
  geom_col() +
  scale_fill_manual(values=colors.party) +
  scale_x_discrete("To which party does the politican belong?") +
  scale_y_continuous("", labels = scales::percent, limits=c(0, 1)) +
  theme(axis.text.x=element_text(angle = 45, hjust=1),
        legend.position="none") +
  facet_grid(vars(dog.whistle), vars(conservative))

dev.off()

# plot mean scores rather than percentages ----

# age
table(d$page,d$conservative,d$dog.whistle,d$sentence.label)

# recode politician age as numeric
d = d %>%
  mutate(
    page_num = ifelse(page == "20-39", 0,
                      ifelse(page == "40-59",1,2)))

mean.age = d %>%
  group_by(conservative,dog.whistle,sentence.label) %>%
  summarize(Mean = mean(page_num), CILow = ci.low(page_num), CIHigh = ci.high(page_num)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)  
mean.age

ggplot(mean.age, aes(x=dog.whistle, y=Mean, color = conservative)) +
  geom_point(shape=20, size=3, alpha=1) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_color_manual(values=c("black","blue")) +
  xlab("Condition") +
  ylab("Mean numeric age rating (higher = older)") +
  facet_grid(. ~ sentence.label)
ggsave("../generated/plots/age.pdf",height=5,width=5)

# progressive
mean.prog = d %>%
  group_by(conservative,dog.whistle,sentence.label) %>%
  summarize(Mean = mean(pprog), CILow = ci.low(pprog), CIHigh = ci.high(pprog)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)  
mean.prog

ggplot(mean.prog, aes(x=dog.whistle, y=Mean, color = conservative)) +
  geom_point(shape=20, size=3, alpha=1) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_color_manual(values=c("black","blue")) +
  xlab("Condition") +
  ylab("Mean progressive rating (higher = more progressive)") +
  facet_grid(. ~ sentence.label)
ggsave("../generated/plots/progressive.pdf",height=5,width=5)

# racist
mean.racism = d %>%
  group_by(conservative,dog.whistle,sentence.label) %>%
  summarize(Mean = mean(pracism), CILow = ci.low(pracism), CIHigh = ci.high(pracism)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)  
mean.racism

ggplot(mean.racism, aes(x=dog.whistle, y=Mean, color = conservative)) +
  geom_point(shape=20, size=3, alpha=1) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_color_manual(values=c("red","blue","black")) +
  xlab("Condition") +
  ylab("Mean racism rating (higher = more racist)") +
  facet_grid(. ~ sentence.label)
ggsave("../generated/plots/racism.pdf",height=5,width=5)

# honest
mean.honest = d %>%
  group_by(conservative,dog.whistle,sentence.label) %>%
  summarize(Mean = mean(phonesty), CILow = ci.low(phonesty), CIHigh = ci.high(phonesty)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)  
mean.honest

ggplot(mean.honest, aes(x=dog.whistle, y=Mean, color = conservative)) +
  geom_point(shape=20, size=3, alpha=1) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_color_manual(values=c("black","blue")) +
  xlab("Condition") +
  ylab("Mean honesty rating (higher = more honest)") +
  facet_grid(. ~ sentence.label)
ggsave("../generated/plots/honesty.pdf",height=5,width=5)

# helpful
mean.helpful = d %>%
  group_by(conservative,dog.whistle,sentence.label) %>%
  summarize(Mean = mean(phelpful), CILow = ci.low(phelpful), CIHigh = ci.high(phelpful)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)  
mean.helpful

ggplot(mean.helpful, aes(x=dog.whistle, y=Mean, color = conservative)) +
  geom_point(shape=20, size=3, alpha=1) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_color_manual(values=c("black","blue")) +
  xlab("Condition") +
  ylab("Mean helpful rating (higher = more helpful)") +
  facet_grid(. ~ sentence.label)
ggsave("../generated/plots/helpful.pdf",height=5,width=5)

# intelligent
mean.intelligent = d %>%
  group_by(conservative,dog.whistle,sentence.label) %>%
  summarize(Mean = mean(pintel), CILow = ci.low(pintel), CIHigh = ci.high(pintel)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)  
mean.intelligent

ggplot(mean.intelligent, aes(x=dog.whistle, y=Mean, color = conservative)) +
  geom_point(shape=20, size=3, alpha=1) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_color_manual(values=c("black","blue")) +
  xlab("Condition") +
  ylab("Mean intelligence rating (higher = more intelligent)") +
  facet_grid(. ~ sentence.label)
ggsave("../generated/plots/intelligent.pdf",height=5,width=5)

# christian
mean.christian = d %>%
  group_by(conservative,dog.whistle,sentence.label) %>%
  summarize(Mean = mean(pchristian), CILow = ci.low(pchristian), CIHigh = ci.high(pchristian)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)  
mean.christian

ggplot(mean.christian, aes(x=dog.whistle, y=Mean, color = conservative)) +
  geom_point(shape=20, size=3, alpha=1) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_color_manual(values=c("black","blue")) +
  xlab("Condition") +
  ylab("Mean christian rating (higher = more christian)") +
  facet_grid(. ~ sentence.label)
ggsave("../generated/plots/christian.pdf",height=5,width=5)

# friendly
mean.friendly = d %>%
  group_by(conservative,dog.whistle,sentence.label) %>%
  summarize(Mean = mean(pfriendly), CILow = ci.low(pfriendly), CIHigh = ci.high(pfriendly)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)  
mean.friendly

ggplot(mean.friendly, aes(x=dog.whistle, y=Mean, color = conservative)) +
  geom_point(shape=20, size=3, alpha=1) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_color_manual(values=c("black","blue")) +
  xlab("Condition") +
  ylab("Mean friendliness rating (higher = more friendly)") +
  facet_grid(. ~ sentence.label)
ggsave("../generated/plots/friendly.pdf",height=5,width=5)

# party
table(d$dog.whistle,d$conservative,d$pparty,d$sentence.label)



# results for Aufnahme in das deutsche Staatsvolk:
# age: no difference
# progressive: less conservative give lower progressive rating on DW than more conservative
# rassistisch: trend: less conservative give higher racism rating for DW than more conservative 
# ehrlich: trend: less conservative give lower honesty rating on DW than more conservative
# hilfsbereit: less conservative give lower helpful ratings than more conservative on DW
# intelligent: trend: less conservative give lower intelligence rating on DW than more conservative
# christian: trend: more conservative give higher christian rating on DW than less conservative 
# friendly: more conservative give higher friendly ratings than less conservative
# party: with Staatsvolk, both less and more conservative participants take PDW speaker to be more likely to be AfD than NDW speakers

# AfD,  = Aufnahme-in-dt-Staatsvolk


#            less conservative more conservative
#Control                     7                 2
#Dog whistle                12                 5

# results for Zukunft unseres Volkes:
# age: no difference
# progressive: no difference
# rassistisch: no difference (both groups don't think this is racist)
# and for "Volk" than "deutsches Staatsvolk"
# ehrlich: more conservative give higher honesty rating on DW than less conservative
# hilfsbereit: more conservative give higher helpful rating than less conservative
# intelligent: trend: less conservative give lower intelligence rating than more conservative
# christian: no difference
# friendly: trend: more conservative give higher friendly rating than less conservative
# party: 

#= AfD,  = Zukunft-unseres-Volkes


#             less conservative more conservative
#Control                     1                 0
#Dog whistle                 1                 0
