# pilot 05

# Set working directory to directory of script:

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# Load general definitions (also loads standard packages such as
# tidyverse):

source("../../../scripts/general.R")

# read in the raw data ----
d1 = read_csv("../data/old/responses_1.csv") %>% mutate(list=1) %>% 
  rename(
       time            = Timestamp,
       control.c     = `Wie widersprüchlich ist Politiker 1?`,
       control.nc      = `Wie widersprüchlich ist Politiker 2?`,
       target       = `Wie widersprüchlich ist Politiker 3?`,
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

d1 = read_csv("../data/old/responses_1.csv") %>% mutate(list=1) %>% 
  rename(
    time            = Timestamp,
    control.c     = `Wie widersprüchlich ist Politiker 1?`,
    control.nc      = `Wie widersprüchlich ist Politiker 2?`,
    target       = `Wie widersprüchlich ist Politiker 3?`,
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

d2 = read_csv("../data/old/responses_2.csv") %>% mutate(list=2) %>% 
  rename(
    time            = Timestamp,
    control.c     = `Wie widersprüchlich ist Politiker 1?`,
    control.nc      = `Wie widersprüchlich ist Politiker 2?`,
    target       = `Wie widersprüchlich ist Politiker 3?`,
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

d3 = read_csv("../data/old/responses_3.csv") %>% mutate(list=3) %>% 
  rename(
    time            = Timestamp,
    control.c     = `Wie widersprüchlich ist Politiker 1?`,
    control.nc      = `Wie widersprüchlich ist Politiker 2?`,
    target       = `Wie widersprüchlich ist Politiker 3?`,
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

d4 = read_csv("../data/old/responses_4.csv") %>% mutate(list=4) %>% 
  rename(
    time            = Timestamp,
    control.c     = `Wie widersprüchlich ist Politiker 1?`,
    control.nc      = `Wie widersprüchlich ist Politiker 2?`,
    target       = `Wie widersprüchlich ist Politiker 3?`,
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

d5 = read_csv("../data/old/responses_5.csv") %>% mutate(list=5) %>% 
  rename(
    time            = Timestamp,
    control.c     = `Wie widersprüchlich ist Politiker 1?`,
    control.nc      = `Wie widersprüchlich ist Politiker 2?`,
    target       = `Wie widersprüchlich ist Politiker 3?`,
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

d6 = read_csv("../data/old/responses_6.csv") %>% mutate(list=6) %>% 
  rename(
    time            = Timestamp,
    control.c     = `Wie widersprüchlich ist Politiker 1?`,
    control.nc      = `Wie widersprüchlich ist Politiker 2?`,
    target       = `Wie widersprüchlich ist Politiker 3?`,
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

d7 = read_csv("../data/new/responses_1.csv") %>% mutate(list=7) %>% 
  rename(
    time            = Timestamp,
    control.c     = `Wie widersprüchlich ist Politiker 1?`,
    control.nc      = `Wie widersprüchlich ist Politiker 2?`,
    target       = `Wie widersprüchlich ist Politiker 3?`,
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

d8 = read_csv("../data/new/responses_2.csv") %>% mutate(list=8) %>% 
  rename(
    time            = Timestamp,
    control.c     = `Wie widersprüchlich ist Politiker 1?`,
    control.nc      = `Wie widersprüchlich ist Politiker 2?`,
    target       = `Wie widersprüchlich ist Politiker 3?`,
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

d9 = read_csv("../data/new/responses_3.csv") %>% mutate(list=9) %>% 
  rename(
    time            = Timestamp,
    control.c     = `Wie widersprüchlich ist Politiker 1?`,
    control.nc      = `Wie widersprüchlich ist Politiker 2?`,
    target       = `Wie widersprüchlich ist Politiker 3?`,
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

d10 = read_csv("../data/new/responses_4.csv") %>% mutate(list=10) %>% 
  rename(
    time            = Timestamp,
    control.c     = `Wie widersprüchlich ist Politiker 1?`,
    control.nc      = `Wie widersprüchlich ist Politiker 2?`,
    target       = `Wie widersprüchlich ist Politiker 3?`,
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

d11 = read_csv("../data/new/responses_5.csv") %>% mutate(list=11) %>% 
  rename(
    time            = Timestamp,
    control.c     = `Wie widersprüchlich ist Politiker 1?`,
    control.nc      = `Wie widersprüchlich ist Politiker 2?`,
    target       = `Wie widersprüchlich ist Politiker 3?`,
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

d12 = read_csv("../data/new/responses_6.csv") %>% mutate(list=12) %>% 
  rename(
    time            = Timestamp,
    control.c     = `Wie widersprüchlich ist Politiker 1?`,
    control.nc      = `Wie widersprüchlich ist Politiker 2?`,
    target       = `Wie widersprüchlich ist Politiker 3?`,
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

nrow(d1) #19
nrow(d2) #17
nrow(d3) #18
nrow(d4) #20
nrow(d5) #19
nrow(d6) #18
nrow(d7) #24
nrow(d8) #22
nrow(d9) #23
nrow(d10) #25
nrow(d11) #24
nrow(d12) #23

# get the data into shape

d <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12) %>% 
  mutate(subj = 1:n()) 


d %>%
  mutate(party = str_squish(party)) %>%
  unique() -> d

nrow(d) #252

# long format
d <- d%>%
  pivot_longer(cols = control.c:target,
               names_to = "item", 
               values_to = "rating")

str(d$item)  
str(d$list)
d$list <- as.factor(d$list)
table(d$list)

# code what the "target" item of each of the 6 lists were
d = d %>%
  mutate(item.string = case_when(
    list==1 & item == "target" ~ "Kampf für Deutschland weiter", 
    list==2 & item == "target" ~ "setzen wir uns für Deutschland ein",
    list==3 & item == "target" ~ "Hilfe vor Ort",
    list==4 & item == "target" ~ "lokale Unterstützung", 
    list==5 & item == "target" ~ "deutsche Staatsvolk",
    list==6 & item == "target" ~ "deutschen Staatsangehörigkeit",
    list==7 & item == "target" ~ "Kampf für Deutschland weiter", 
    list==8 & item == "target" ~ "setzen wir uns für Deutschland ein",
    list==9 & item == "target" ~ "Hilfe vor Ort",
    list==10 & item == "target" ~ "lokale Unterstützung", 
    list==11 & item == "target" ~ "deutsche Staatsvolk",
    list==12 & item == "target" ~ "deutschen Staatsangehörigkeit",
    TRUE ~ item))

# code whether the item is a dog whistle or a control
d = d %>%
  mutate(item.type = case_when(
    list==1 & item == "target" ~ "dog whistle",
    list==2 & item == "target" ~ "control",
    list==3 & item == "target" ~ "dog whistle",
    list==4 & item == "target" ~ "control",
    list==5 & item == "target" ~ "dog whistle",
    list==6 & item == "target" ~ "control",
    list==7 & item == "target" ~ "dog whistle",
    list==8 & item == "target" ~ "control",
    list==9 & item == "target" ~ "dog whistle",
    list==10 & item == "target" ~ "control",
    list==11 & item == "target" ~ "dog whistle",
    list==12 & item == "target" ~ "control",
    TRUE ~ item))

table(d$item)
table(d$item.string)
table(d$item.type)

d$pair <- "1"
table(d$pair)

# code which two items belong to the same dog whistle item
d = d %>%
  mutate(pair = case_when(
    list==1 ~ "Kampf für Deutschland",
    list==2 ~ "Kampf für Deutschland",
    list==3 ~ "Hilfe vor Ort",
    list==4 ~ "Hilfe vor Ort",
    list==5 ~ "deutsches Staatsvolk",
    list==6 ~ "deutsches Staatsvolk",
    list==7 ~ "Kampf für Deutschland",
    list==8 ~ "Kampf für Deutschland",
    list==9 ~ "Hilfe vor Ort",
    list==10 ~ "Hilfe vor Ort",
    list==11 ~ "deutsches Staatsvolk",
    list==12 ~ "deutsches Staatsvolk",
    TRUE ~ pair))
                       
nrow(d) # 756
head(d)

# exclude participants who didn't judge the control items appropriately
# contradictory: 4 or 5
# non-contradictory: 1 or 2
control.c <- d %>%
  filter(item == "control.c") %>%
  filter(rating < 4)
control.c # 0 participants

control.nc <- d %>%
  filter(item == "control.nc") %>%
  filter(rating > 2)
control.nc # 2 participants

d <- droplevels(subset(d, !(d$subj %in% control.c$subj | d$subj %in% control.nc$subj)))
nrow(d) #744 (data from 2 participants excluded)

# check that all is well
table(d$rating,d$item)  


# save the preprocessed data
write.csv(d, "../generated/data/d-preprocessed.csv")

length(unique(d$subj)) #248

# participant information
names(d)

str(d$age)
table(d$age) #one participant entered non-sense
d$age <- as.numeric(d$age)
min(d$age,na.rm=T) #18
max(d$age,na.rm=T) #65
mean(d$age,na.rm=T) #29.1
ggplot(d, aes(x=age)) +
  geom_histogram()

str(d$gender)
table(d$gender)
#divers männlich weiblich 
#15 189       123

library(stringdist)

stringdistmatrix(d$party, names(colors.party), method="jw") -> m

d$party <- names(colors.party)[apply(m, 1, which.min)]

ggsave("../generated/plots/demographics.pdf", plot.demographics(d), width=20, height=10, unit="cm")
ggsave("../generated/plots/demographics.png", plot.demographics(d), width=1500, height=500, unit="px", dpi=150)

