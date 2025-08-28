
# Set working directory to directory of script:

tryCatch(
  setwd(dirname(rstudioapi::getSourceEditorContext()$path)),
  error = function(e) setwd("experiments/Pilot02/scripts/"))

# Load general definitions (also loads standard packages such as
# tidyverse):

source("../../../scripts/general.R")

# read in the raw data ----
d1 = read_csv("../data/responses_1.csv") %>% mutate(list=1) %>% 
  rename(
       time            = Timestamp,
       control.c     = `Wie widersprüchlich ist diese Aussage?...2`,
       control.nc      = `Wie widersprüchlich ist diese Aussage?...3`,
       target       = `Wie widersprüchlich ist diese Aussage?...4`,
       age             = `Wie alt sind Sie?`,
       gender          = `Ihr Geschlecht`,
       education       = `Was ist Ihr höchster Bildungsabschluss?`,
       party           = `Wenn am Sonntag Bundestagswahl wäre, welche Partei würden Sie wählen?`,
       wom.refugees    = `Wie bewerten Sie persönlich folgende Aussage: “Das Recht anerkannter Flüchtlinge auf Familiennachzug soll abgeschafft werden.”`,
       wom.headscarf   = `Wie bewerten Sie persönlich folgende Aussage: “Das Tragen eines Kopftuchs soll Beamtinnen im Dienst generell erlaubt sein.”`,
       wom.citizenship = `Wie bewerten Sie persönlich folgende Aussage: “In Deutschland soll es generell möglich sein, neben der deutschen eine zweite Staatsbürgerschaft zu haben.”`,
       wom.islam       = `Wie bewerten Sie persönlich folgende Aussage: “Islamische Verbände sollen als Religionsgemeinschaften staatlich anerkannt werden können.”`,
       wom.asylum      = `Wie bewerten Sie persönlich folgende Aussage: “Asyl soll weiterhin nur politisch Verfolgten gewährt werden.”`,
       comment         = `Haben Sie Kommentare zu unserer Umfrage?`)

d2 = read_csv("../data/responses_2.csv") %>% mutate(list=2) %>% 
  rename(
    time            = Timestamp,
    control.c     = `Wie widersprüchlich ist diese Aussage?...2`,
    control.nc      = `Wie widersprüchlich ist diese Aussage?...3`,
    target       = `Wie widersprüchlich ist diese Aussage?...4`,
    age             = `Wie alt sind Sie?`,
    gender          = `Ihr Geschlecht`,
    education       = `Was ist Ihr höchster Bildungsabschluss?`,
    party           = `Wenn am Sonntag Bundestagswahl wäre, welche Partei würden Sie wählen?`,
    wom.refugees    = `Wie bewerten Sie persönlich folgende Aussage: “Das Recht anerkannter Flüchtlinge auf Familiennachzug soll abgeschafft werden.”`,
    wom.headscarf   = `Wie bewerten Sie persönlich folgende Aussage: “Das Tragen eines Kopftuchs soll Beamtinnen im Dienst generell erlaubt sein.”`,
    wom.citizenship = `Wie bewerten Sie persönlich folgende Aussage: “In Deutschland soll es generell möglich sein, neben der deutschen eine zweite Staatsbürgerschaft zu haben.”`,
    wom.islam       = `Wie bewerten Sie persönlich folgende Aussage: “Islamische Verbände sollen als Religionsgemeinschaften staatlich anerkannt werden können.”`,
    wom.asylum      = `Wie bewerten Sie persönlich folgende Aussage: “Asyl soll weiterhin nur politisch Verfolgten gewährt werden.”`,
    comment         = `Haben Sie Kommentare zu unserer Umfrage?`)

d3 = read_csv("../data/responses_3.csv") %>% mutate(list=3) %>% 
  rename(
    time            = Timestamp,
    control.c     = `Wie widersprüchlich ist diese Aussage?...2`,
    control.nc      = `Wie widersprüchlich ist diese Aussage?...3`,
    target       = `Wie widersprüchlich ist diese Aussage?...4`,
    age             = `Wie alt sind Sie?`,
    gender          = `Ihr Geschlecht`,
    education       = `Was ist Ihr höchster Bildungsabschluss?`,
    party           = `Wenn am Sonntag Bundestagswahl wäre, welche Partei würden Sie wählen?`,
    wom.refugees    = `Wie bewerten Sie persönlich folgende Aussage: “Das Recht anerkannter Flüchtlinge auf Familiennachzug soll abgeschafft werden.”`,
    wom.headscarf   = `Wie bewerten Sie persönlich folgende Aussage: “Das Tragen eines Kopftuchs soll Beamtinnen im Dienst generell erlaubt sein.”`,
    wom.citizenship = `Wie bewerten Sie persönlich folgende Aussage: “In Deutschland soll es generell möglich sein, neben der deutschen eine zweite Staatsbürgerschaft zu haben.”`,
    wom.islam       = `Wie bewerten Sie persönlich folgende Aussage: “Islamische Verbände sollen als Religionsgemeinschaften staatlich anerkannt werden können.”`,
    wom.asylum      = `Wie bewerten Sie persönlich folgende Aussage: “Asyl soll weiterhin nur politisch Verfolgten gewährt werden.”`,
    comment         = `Haben Sie Kommentare zu unserer Umfrage?`)


View(d)

nrow(d1) #23
nrow(d2) #25
nrow(d3) #26

# get the data into shape

d <- rbind(d1,d2,d3) %>% 
  mutate(subj = 1:n()) 


d %>%
  mutate(party = str_squish(party)) %>%
  unique() -> d

nrow(d) #74 = 23 + 25 + 26

# long format
d <- d%>%
  pivot_longer(cols = control.c:target,
               names_to = "item", 
               values_to = "rating")

str(d$item)  
str(d$list)
d$list <- as.factor(d$list)
table(d$list)

d = d %>%
  mutate(item = case_when(
    list==1 & item == "target" ~ "Kampf für Deutschland", 
    list==2 & item == "target" ~ "Hilfe vor Ort",
    list==3 & item == "target" ~ "deutsches Staatsvolk",
    TRUE ~ item))
#View(d)
                       
nrow(d) # 222 = 3 x 74
head(d)

# exclude participants who didn't judge the control items appropriately
# contradictory: 4 or 5
# non-contradictory: 1 or 2
control.c <- d %>%
  filter(item == "control.c") %>%
  filter(rating < 4)
control.c # 7 participants

control.nc <- d %>%
  filter(item == "control.nc") %>%
  filter(rating > 2)
control.nc # 13 participants

d <- droplevels(subset(d, !(d$subj %in% control.c$subj | d$subj %in% control.nc$subj)))
nrow(d) #168 (data from 18 participants excluded)

# check that all is well
table(d$item,d$rating)  


# save the preprocessed data
write.csv(d, "../generated/data/d-preprocessed.csv")

length(unique(d$subj)) #56

# participant information
names(d)

str(d$age)
table(d$age) #one participant entered non-sense
d$age <- as.numeric(d$age)
min(d$age,na.rm=T) #18
max(d$age,na.rm=T) #52
mean(d$age,na.rm=T) #29.6
ggplot(d, aes(x=age)) +
  geom_histogram()

str(d$gender)
table(d$gender)
#männlich weiblich 
#90       78

library(stringdist)

stringdistmatrix(d$party, names(colors.party), method="jw") -> m

d$party <- names(colors.party)[apply(m, 1, which.min)]

ggsave("../generated/plots/demographics.pdf", plot.demographics(d), width=20, height=10, unit="cm")
ggsave("../generated/plots/demographics.png", plot.demographics(d), width=1500, height=500, unit="px", dpi=150)

