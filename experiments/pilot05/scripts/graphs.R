# Pilot 05

# Set working directory to directory of script:

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# Load general definitions (also loads standard packages such as
# tidyverse):

source("../../../scripts/general.R")
source("../../../scripts/helpers.R")

# load clean data ----

d = read.csv("../generated/data/d-preprocessed.csv")
nrow(d) #744

# which ratings did the two controls and the targets receive? ----

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
  ylab("Mean contradictoriness rating (higher = more contradictory)") +
  #facet_wrap(. ~ pair) +
  coord_flip()
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

length(unique(d$subj)) #248 participants

table(d$conservative)
# less conservative  576 (192 participants)
# more conservative 168 (56 participants)

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
  #geom_jitter(data=d,aes(x=item,y=rating,group=conservative), shape=20, size=2, alpha = .5, width = .1, height = height) +
  xlab("Item") +
  ylab("Mean contradictoriness rating (higher = more contradictory)") +
  coord_flip()


# boxplot included in grant application ----
table(d$item.type)

t <- d %>%
  filter(item.type != "control.c") %>%
  filter(item.type != "control.nc")

table(t$item.type)
table(t$pair)

table(t$pair,t$conservative,t$item.type)
# now we have at least 5-7 judgments for each DW

ggplot(t, aes(x=item.type, y=rating, color = conservative)) +
  geom_boxplot() +
  scale_color_manual(values=c("black","blue")) +
  scale_fill_manual(values=c("black","blue")) +
  #stat_summary(position=position_dodge(width = 0.5), fun=median, geom="point", shape=23, size=2) + 
  theme(legend.position="right") +
  scale_y_discrete(limits=c("1","2", "3", "4", "5")) +
  theme(axis.ticks=element_blank(),panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),plot.background=element_blank(),
        axis.title.x=element_blank()) +
  theme(strip.background =element_rect(fill="white")) +
  theme(strip.text = element_text(colour = 'black'),strip.text.x = element_text(size = 12)) +
  guides(fill=guide_legend(title="participant group"),color=guide_legend(title="participant group")) +
  xlab("Condition") +
  ylab("Deniability ratings \n (higher ratings = less deniable)") +
  facet_grid(. ~ pair,scale="free")
ggsave("../generated/plots/plausible-deniability-by-conservative.pdf",height=2.5,width=8)

# plausible deniability against anti-immigrant score ----
summary(t)
table(t$item.type)
table(t$pair)
table(t$item.string)

ggplot(t, aes(x=mean.wom.score, y=rating)) +
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
  ylab("Deniability ratings \n (higher ratings = less deniable)") +
  facet_grid(item.type ~ pair,scale="free")
ggsave("../generated/plots/plausible-deniability-by-conservative.pdf",height=3,width=8)


# plot plausible deniability ----

means = d %>%
  group_by(item) %>%
  summarize(Mean = mean(rating), CILow = ci.low(rating), CIHigh = ci.high(rating)) %>%
  ungroup() %>%
  mutate(YMin = Mean - CILow, YMax = Mean + CIHigh)  
means

ggplot(means, aes(x=item, y=Mean)) +
  geom_point(shape=20, size=3, alpha=1) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.25) +
  #scale_color_manual(values=c("black","blue")) +
  #geom_jitter(data=d,aes(x=item,y=rating,group=conservative), shape=20, size=2, alpha = .5, width = .1, height = height) +
  xlab("Item") +
  ylab("Mean contradictoriness rating (higher = more contradictory)") +
  coord_flip()
ggsave("../generated/plots/plausible-deniability.pdf",height=5,width=10)

# violin plot ----
table(d$item.type)

t <- d %>%
  filter(item.type != "control.c") %>%
  filter(item.type != "control.nc")

table(t$item.type)
table(t$pair)

table(t$pair,t$conservative,t$item.type)
# now we have at least 5-7 judgments for each DW

ggplot(t, aes(x=item.type, y=rating, color = conservative, fill = conservative)) +
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
  theme(strip.text = element_text(colour = 'black'),strip.text.x = element_text(size = 12)) +
  guides(fill=guide_legend(title="participant group"),color=guide_legend(title="participant group")) +
  xlab("Condition") +
  ylab("Kernel probability density \n of deniability ratings \n (higher ratings = less deniable)") +
  facet_grid(. ~ pair,scale="free")
ggsave("../generated/plots/plausible-deniability-by-conservative.pdf",height=2.5,width=8)


