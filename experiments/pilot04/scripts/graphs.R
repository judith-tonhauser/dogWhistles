
# Set working directory to directory of script:

tryCatch(
  setwd(dirname(rstudioapi::getSourceEditorContext()$path)),
  error = function(e) setwd("experiments/Pilot02/scripts/"))

# Load general definitions (also loads standard packages such as
# tidyverse):

source("../../../scripts/general.R")
source("../../../scripts/helpers.R")

# load clean data ----

d = read.csv("../generated/data/d-preprocessed.csv")
nrow(d) #168

# which ratings did the two controls and the PDW targets receive? ----

means = d %>%
  group_by(item) %>%
  summarize(Mean = mean(rating), CILow = ci.low(rating), CIHigh = ci.high(rating)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)  
means

ggplot(means, aes(x=item, y=Mean)) +
  geom_point(shape=20, size=3, alpha=1) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  geom_jitter(data=d,aes(x=item,y=rating),shape=20,size=2, alpha = .5, color = "red", width = .1, height = .1) +
  #scale_color_manual(values=c("black","blue")) +
  xlab("Item") +
  ylab("Mean contradictoriness rating (higher = more contradictory)") 
ggsave("../generated/plots/plausible-deniability.pdf",height=5,width=8)

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
    conservative = ifelse(mean.wom.score > .99, "more conservative","less conservative"))

length(unique(d$subj)) #56 participants

table(d$conservative)
# less conservative  123 (41 participants)
# more conservative 45 (15 participants)

d = d %>%
  mutate(subj = fct_reorder(as.factor(subj),mean.wom.score))  

ggplot(d, aes(x=subj, y=mean.wom.score,color=party,fill=party)) +
  geom_point(shape=21, size=3, alpha=1,color="black") +
  scale_fill_manual(values=colors.party) +
  xlab("Participant") +
  ylab("Mean wom score (higher = more conservative)")
ggsave("../generated/plots/mean-response-to-wom-questions-by-participant.pdf",height=4,width=9)


# plot plausible deniability with conservativity info ----

means = d %>%
  group_by(item,conservative) %>%
  summarize(Mean = mean(rating), CILow = ci.low(rating), CIHigh = ci.high(rating)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)  
means

height = ifelse(d$conservative == "less conservative", .3, .1)
width = ifelse(d$conservative == "less conservative", .1, .3)

ggplot(means, aes(x=item, y=Mean,group=conservative, color=conservative)) +
  geom_point(shape=20, size=3, alpha=1) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  scale_color_manual(values=c("black","blue")) +
  geom_jitter(data=d,aes(x=item,y=rating,group=conservative),
              shape=20, size=2, alpha = .5, width = .1, height = height) +
  xlab("Item") +
  ylab("Mean contradictoriness rating (higher = more contradictory)") 
ggsave("../generated/plots/plausible-deniability-by-conservative.pdf",height=5,width=10)

