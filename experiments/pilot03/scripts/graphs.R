# pilot 03
# (imports data from pilot 02 and pilot 03, to plot together)

# Set working directory to directory of script:

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# Load general definitions (also loads standard packages such as
# tidyverse):

source("../../../scripts/general.R")
source("../../../scripts/helpers.R")

# load clean data ----

d1 = read.csv("../generated/data/d-preprocessed.csv")
nrow(d1) #120
length(unique(d1$subj)) #60
table(d1$subj)

# also load the data from the other pilot (pilot 02)
d2 = read.csv("../../Pilot02/generated/data/d-preprocessed.csv")
nrow(d2) #110
length(unique(d2$subj)) #55
table(d2$subj)

head(d1)
head(d2)

# bind the data
d = rbind(d1,d2)
head(d)
nrow(d) #230

length(unique(d$subj)) #115

# how did the participants answer the 5 WOM questions? ----

names(d)
#"wom.refugees": conservative = agree      
#"wom.headscarf": conservative = not agree
#"wom.citizenship": conservative = not agree
#"wom.islam": conservative = not agree
#"wom.asylum": conservative = agree

table(d$wom.asylum)

# define numeric values for each position (higher = more conservative)
d$wom.refugees.num <- ifelse(d$wom.refugees == "Ich stimme zu", 1, 
                             ifelse(d$wom.refugees == "Neutral", 0, -1))
d$wom.headscarf.num <- ifelse(d$wom.headscarf == "Ich stimme nicht zu", 1, 
                              ifelse(d$wom.headscarf == "Neutral", 0, -1))
d$wom.citizenship.num <- ifelse(d$wom.citizenship == "Ich stimme nicht zu", 1, 
                                ifelse(d$wom.citizenship == "Neutral", 0, -1))
d$wom.islam.num <- ifelse(d$wom.islam == "Ich stimme nicht zu", 1, 
                          ifelse(d$wom.islam == "Neutral", 0, -1))
d$wom.asylum.num <- ifelse(d$wom.asylum == "Ich stimme zu", 1, 
                           ifelse(d$wom.asylum == "Neutral", 0, -1))


# how conservative is each participant? 

d$mean.wom.score = (d$wom.refugees.num + d$wom.headscarf.num + d$wom.citizenship.num + d$wom.islam.num + d$wom.asylum.num) / 5
# lowest possible: -1 (least conservative)
# highest possible: 1 (most conservative)
table(d$mean.wom.score)

d = d %>%
  mutate(
    conservative = ifelse(mean.wom.score > .99, "more conservative", "less conservative"))
                          

length(unique(d$subj)) #115 participants

table(d$conservative)
#less conservative more conservative 
#174  (= 87 participants)              56 (= 28 participants)

# plot mean anti-immigrant score by party selection (grant) ----

d = d %>%
  mutate(subj = fct_reorder(as.factor(subj),mean.wom.score))  

# remove minor parties or (no answer)
table(d$party)
d = d %>%
  filter(party == "CDU" | party == "FDP" | party == "SPD" | party == "AfD" | party == "Grüne"
         | party == "Linke" | party == "Die Partei" | party == "Volt")

ggplot(d, aes(x=subj, y=mean.wom.score,color=party,fill=party)) +
  geom_jitter(shape=21, size=3, alpha=1,color="black") +
  scale_fill_manual(values=colors.party) +
  theme(axis.text.x=element_blank()) +
  theme(axis.ticks=element_blank(),panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),plot.background=element_blank()) +
  labs(fill = "Party selection") +
  xlab("Participant") +
  ylab("Mean anti-immigrant score \n (higher = more anti-immigrant)")
ggsave("../generated/plots/mean-conservativism-by-participant.pdf",height=3,width=10)

# highest mean score is 2 (out of 2)!

# plot mean scores by how conservative the participants are----

# age
table(d$page,d$conservative,d$dog.whistle,d$sentence.label)

# recode politician age as numeric
d = d %>%
  mutate(
    page_num = ifelse(page == "20-39", 0,
                      ifelse(page == "40-59",1,2)))

mean.age = d %>%
  group_by(dog.whistle,sentence.label) %>%
  summarize(Mean = mean(page_num), CILow = ci.low(page_num), CIHigh = ci.high(page_num)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)  
mean.age

ggplot(mean.age, aes(x=dog.whistle, y=Mean)) +
  geom_point(shape=20, size=3, alpha=1) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  #scale_color_manual(values=c("black","blue")) +
  xlab("Condition") +
  ylab("Mean numeric age rating (higher = older)") +
  facet_grid(. ~ sentence.label)
ggsave("../generated/plots/age.pdf",height=5,width=5)

# age, by conservative
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
  group_by(dog.whistle,sentence.label) %>%
  summarize(Mean = mean(pprog), CILow = ci.low(pprog), CIHigh = ci.high(pprog)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)  
mean.prog

ggplot(mean.prog, aes(x=dog.whistle, y=Mean)) +
  geom_point(shape=20, size=3, alpha=1) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  #scale_color_manual(values=c("black","blue")) +
  theme(legend.position="top") +
  xlab("Condition") +
  ylab("Mean progressive rating (higher = more progressive)") +
  facet_grid(. ~ sentence.label)
ggsave("../generated/plots/progressive.pdf",height=4,width=8)

# progressive, by conservative
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
  theme(legend.position="top") +
  xlab("Condition") +
  ylab("Mean progressive rating (higher = more progressive)") +
  facet_grid(. ~ sentence.label)
ggsave("../generated/plots/progressive.pdf",height=4,width=8)

# racist
mean.racism = d %>%
  group_by(dog.whistle,sentence.label) %>%
  summarize(Mean = mean(pracism), CILow = ci.low(pracism), CIHigh = ci.high(pracism)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)  
mean.racism

ggplot(mean.racism, aes(x=dog.whistle, y=Mean)) +
  geom_point(shape=20, size=3, alpha=1) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  #scale_color_manual(values=c("black","blue")) +
  theme(legend.position="top") +
  xlab("Condition") +
  ylab("Mean racist rating (higher = more racist)") +
  facet_grid(. ~ sentence.label)
ggsave("../generated/plots/racist.pdf",height=4,width=8)

# racist, by conservative
mean.racism = d %>%
  group_by(conservative,dog.whistle,sentence.label) %>%
  summarize(Mean = mean(pracism), CILow = ci.low(pracism), CIHigh = ci.high(pracism)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)  
mean.racism

ggplot(mean.racism, aes(x=dog.whistle, y=Mean, color = conservative)) +
  geom_point(shape=20, size=3, alpha=1) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_color_manual(values=c("black","blue")) +
  theme(legend.position="top") +
  xlab("Condition") +
  ylab("Mean racist rating (higher = more racist)") +
  facet_grid(. ~ sentence.label)


# racism figure included in grant ----
table(d$conservative,d$pracism,d$sentence.label,d$dog.whistle) # we always have some ratings
table(d$sentence.label)
table(d$dog.whistle)

ggplot(d[d$sentence.label != "Volk",], aes(x=dog.whistle, y=pracism, color = conservative, fill = conservative)) +
  geom_violin(position=position_dodge(0.5), alpha = 0.3) +
  scale_color_manual(values=c("black","blue")) +
  scale_fill_manual(values=c("black","blue")) +
  stat_summary(position=position_dodge(width = 0.5), fun=median, geom="point", shape=23, size=2) + 
  theme(legend.position="right") +
  scale_y_discrete(limits=c("1","2", "3", "4", "5")) +
  theme(axis.ticks=element_blank(),panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),plot.background=element_blank(),
        axis.title.x=element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  guides(fill=guide_legend(title="participant group"),color=guide_legend(title="participant group")) +
  theme(strip.text = element_text(colour = 'black'),strip.text.x = element_text(size = 12)) +
  xlab("Condition") +
  ylab("Kernel probability density \n of racism ratings \n (higher ratings = more racist)") +
  facet_grid(. ~ sentence.label)
ggsave("../generated/plots/racist.pdf",height=2.5,width=8)

# racism against anti-immigrant score ----
ggplot(d[d$sentence.label != "Volk",], aes(x=mean.wom.score, y=pracism)) +
  geom_jitter() +
  geom_smooth(method='lm') +
  theme(legend.position="right") +
  scale_y_discrete(limits=c("1","2", "3", "4", "5")) +
  xlim(-1,1) +
  #scale_x_discrete(limits=c("-1.0","0","1.0")) +
  #theme(axis.ticks=element_blank(),panel.grid.major.x=element_blank(),
  #      panel.grid.minor.x=element_blank(),plot.background=element_blank(),
  #      axis.title.x=element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  theme(strip.text = element_text(colour = 'black'),strip.text.x = element_text(size = 12)) +
  xlab("Mean anti-immigrant score \n (higher ratings = more anti-immigrant)") +
  ylab("Racism ratings \n (higher ratings = more racist)") +
  facet_grid(dog.whistle ~ sentence.label,scale="free")
ggsave("../generated/plots/racist.pdf",height=3,width=6)


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
ggsave("../generated/plots/honest.pdf",height=5,width=5)


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
  theme(legend.position="top") +
  ylab("Mean helpful rating (higher = more helpful)") +
  facet_grid(. ~ sentence.label)
ggsave("../generated/plots/helpful.pdf",height=4,width=8)


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
table(d$dog.whistle,d$pparty,d$sentence.label)


# results for "Hilfe vor Ort"
# age: trend is that DW speaker is judged younger than control speaker (in both groups)
# progressive: no difference in control, more conservative give higher progressive rating than less conservative
# rassistisch: no difference in control, more conservative give lower racist rating than less conservative
# ehrlich: no difference, but trend: more conservative give higher christian rating than less conservative
# hilfsbereit: no difference in control, more conservative give higher helpful rating than less conservative
# intelligent: no difference, but trend: more conservative give higher intelligence rating than less conservative
# christian: no difference
# friendly: no difference, both groups give higher friendliness rating for DW than control
# party: 
# AfD,  = Hilfe-vor-Ort

# less conservative more conservative
# Control                     2                 2
# Dog whistle                 1                 0

# results for "Kampf fuer Deutschland"
# age: trend is that DW speaker is judged older than control speaker (in both groups)
# progressive: no difference
# rassistisch: both groups take DW speaker to be more racist than control, more conservative take control to be less racist than less conservative
# ehrlich: no difference in PDW, more conservative give much lower rating to DW than control, no difference for less conservative
# hilfsbereit: both groups take DW speaker to be less helpful than control speaker (no diff between groups)
# intelligent: both groups take DW speaker to be less intelligent than control speaker
# christian: no difference, but trend: more conservative give higher christian rating to DW than less conservative speakers 
# (for whom there is no difference between control and DW)
# friendly: both groups take DW speaker to be less friendly than control speaker (no diff between groups)
# party: 
# = AfD,  = Kampf-fuer-D

# less conservative more conservative
# Control                     2                 0
# Dog whistle                11                 5


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

