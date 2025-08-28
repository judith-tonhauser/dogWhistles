# pilot 02
# (graphs.R of pilot03 is where the data are plotted)

# Set working directory to directory of script:

tryCatch(
  setwd(dirname(rstudioapi::getSourceEditorContext()$path)),
  error = function(e) setwd("experiments/Pilot02/scripts/"))

# Load general definitions (also loads standard packages such as
# tidyverse):

source("../../../scripts/general.R")

# read data and whip into shape ----

rbind(
  read_csv("../data/responses_1.csv") %>% mutate(list=1),
  read_csv("../data/responses_2.csv") %>% mutate(list=2)) %>%
  mutate(subj = 1:n()) %>%
  select(subj,
         list,
         time            = Timestamp,
         page1           = `Wie alt ist Politiker 1?`,
         pprog1          = `Wie fortschrittlich ist Politiker 1?`,
         pracism1        = `Wie rassistisch ist Politiker 1?`,
         phonesty1       = `Wie ehrlich ist Politiker 1?`,
         phelpful1       = `Wie hilfsbereit ist Politiker 1?`,
         pintel1         = `Wie intelligent ist Politiker 1?`,
         pchristian1     = `Wie christlich ist Politiker 1?`,
         pfriendly1      = `Wie freundlich ist Politiker 1?`,
         pparty1         = `Zu welcher Partei gehört Politiker 1?`,
         page2           = `Wie alt ist Politiker 2?`,
         pprog2          = `Wie fortschrittlich ist Politiker 2?`,
         pracism2        = `Wie rassistisch ist Politiker 2?`,
         phonesty2       = `Wie ehrlich ist Politiker 2?`,
         phelpful2       = `Wie hilfsbereit ist Politiker 2?`,
         pintel2         = `Wie intelligent ist Politiker 2?`,
         pchristian2     = `Wie christlich ist Politiker 2?`,
         pfriendly2      = `Wie freundlich ist Politiker 2?`,
         pparty2         = `Zu welcher Partei gehört Politiker 2?`,
         age             = `Wie alt sind Sie?`,
         gender          = `Ihr Geschlecht`,
         education       = `Was ist Ihr höchster Bildungsabschluss?`,
         party           = `Wenn am Sonntag Bundestagswahl wäre, welche Partei würden Sie wählen?`,
         wom.refugees    = `Wie bewerten Sie persönlich folgende Aussage: “Das Recht anerkannter Flüchtlinge auf Familiennachzug soll abgeschafft werden.”`,
         wom.headscarf   = `Wie bewerten Sie persönlich folgende Aussage: “Das Tragen eines Kopftuchs soll Beamtinnen im Dienst generell erlaubt sein.”`,
         wom.citizenship = `Wie bewerten Sie persönlich folgende Aussage: “In Deutschland soll es generell möglich sein, neben der deutschen eine zweite Staatsbürgerschaft zu haben.”`,
         wom.islam       = `Wie bewerten Sie persönlich folgende Aussage: “Islamische Verbände sollen als Religionsgemeinschaften staatlich anerkannt werden können.”`,
         wom.asylum      = `Wie bewerten Sie persönlich folgende Aussage: “Asyl soll weiterhin nur politisch Verfolgten gewährt werden.”`,
         comment         = `Haben Sie Kommentare zu unserer Umfrage?`) -> d

d %>%
  select(-page2, -pprog2, -pracism2, -phonesty2, -phelpful2, -pintel2, -pchristian2, -pfriendly2, -pparty2) %>%
  rename(page=page1, pprog=pprog1, pracism=pracism1, phonesty=phonesty1, phelpful=phelpful1, pintel=pintel1, pchristian=pchristian1, pfriendly=pfriendly1, pparty=pparty1) %>%
  mutate(
    sentence = 1,
    dog.whistle = ifelse(list==1, "Dog whistle", "Control")) -> d1

d %>%
  select(-page1, -pprog1, -pracism1, -phonesty1, -phelpful1, -pintel1, -pchristian1, -pfriendly1, -pparty1) %>%
  rename(page=page2, pprog=pprog2, pracism=pracism2, phonesty=phonesty2, phelpful=phelpful2, pintel=pintel2, pchristian=pchristian2, pfriendly=pfriendly2, pparty=pparty2) %>%
  mutate(
    sentence = 2,
    dog.whistle = ifelse(list==2, "Dog whistle", "Control")) -> d2

rbind(d1, d2) %>%
  arrange(subj, sentence) %>%
  mutate(
    sentence.label = ifelse(sentence==1, "Volk", "deutsches Staatsvolk")) -> d

d %>%
  mutate(party = str_squish(party)) %>%
  unique() -> d

# change "keine Angabe" to "no answer"
table(d$party)
d = d %>%
  mutate(party=recode(party, "keine Angabe" = "(no answer)"))

# save the preprocessed data
write.csv(d, "../generated/data/d-preprocessed.csv")
nrow(d) #110

length(unique(d$subj)) #55
# two rows per subject because each subject rated a PDW and another item

#View(d)

# participant information
names(d)

str(d$age)
min(d$age,na.rm=T) #19
max(d$age,na.rm=T) #59
mean(d$age,na.rm=T) #29.5
ggplot(d, aes(x=age)) +
  geom_histogram()

str(d$gender)
table(d$gender)
#keine Angabe     männlich 
# 2           66 
# weiblich 
# 42

library(stringdist)

stringdistmatrix(d$party, names(colors.party), method="jw") -> m

d$party <- names(colors.party)[apply(m, 1, which.min)]

ggsave("../generated/plots/demographics.pdf", plot.demographics(x), width=20, height=10, unit="cm")
ggsave("../generated/plots/demographics.png", plot.demographics(x), width=1500, height=500, unit="px", dpi=150)

