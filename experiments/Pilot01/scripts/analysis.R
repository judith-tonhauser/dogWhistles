
# Set working directory to directory of script:

tryCatch(
  setwd(dirname(rstudioapi::getSourceEditorContext()$path)),
  error = function(e) setwd("experiments/Pilot01/scripts/"))

# Load general definitions (also loads standard packages such as
# tidyverse):

source("../../../scripts/general.R")

# Read data and whip into shape:

read_csv("../data/responses_1.csv") %>%
  filter(Timestamp != "2022/02/14 10:40:56 AM GMT+1") %>%  # That's Titus' test run.
  mutate(subj = 1:n()) %>%
  rename(
    a1.1 = `Eigenschaft #1...2`,
    a1.2 = `Eigenschaft #2...3`,
    a1.3 = `Eigenschaft #3...4`,
    a1.4 = `Eigenschaft #4...5`,
    a2.1 = `Eigenschaft #1...6`,
    a2.2 = `Eigenschaft #2...7`,
    a2.3 = `Eigenschaft #3...8`,
    a2.4 = `Eigenschaft #4...9`) %>%
  pivot_longer(2:9, names_to="response", values_to="attr") %>%
  rename(
    time   = Timestamp,
    age    = `Wie alt sind Sie?`,
    gender = `Was ist Ihr Geschlecht?`,
    party  = `Wenn nächsten Sonntag Wahl wäre, welche Partei würden Sie wählen?`) %>%
  mutate(
    sentence = gsub("a(.)\\.(.)", "\\1", response),
    a.index  = gsub("a(.)\\.(.)", "\\2", response)) %>%
  select(-response) %>%
  select(subj, time, sentence, a.index, attr, age, gender, party) -> d1

read_csv("../data/responses_2.csv") %>%
  mutate(subj = 1:n()) %>%
  rename(
    a3.1 = `Eigenschaft #1...2`,
    a3.2 = `Eigenschaft #2...3`,
    a3.3 = `Eigenschaft #3...4`,
    a3.4 = `Eigenschaft #4...5`,
    a4.1 = `Eigenschaft #1...6`,
    a4.2 = `Eigenschaft #2...7`,
    a4.3 = `Eigenschaft #3...8`,
    a4.4 = `Eigenschaft #4...9`) %>%
  pivot_longer(2:9, names_to="response", values_to="attr") %>%
  rename(
    time   = Timestamp,
    age    = `Wie alt sind Sie?`,
    gender = `Was ist Ihr Geschlecht?`,
    party  = `Wenn nächsten Sonntag Wahl wäre, welche Partei würden Sie wählen?`) %>%
  mutate(
    sentence = gsub("a(.)\\.(.)", "\\1", response),
    a.index  = gsub("a(.)\\.(.)", "\\2", response)) %>%
  select(-response) %>%
  select(subj, time, sentence, a.index, attr, age, gender, party) -> d2

rbind(d1, d2) %>%
  mutate(attr = str_squish(attr),
         attr = str_to_lower(attr)) -> d

# FIXME: subj starts from 1 in both parts.

# Ranking of attributes across items and conditions:

d %>%
  group_by(attr) %>%
  summarize(count = n()) %>%
  arrange(-count) %>%
  write_csv(file="../generated/data/attribute_ranking_overall.csv")

# Ranking by item:

d %>%
  group_by(sentence, attr) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(-count) -> x

x %>% filter(sentence==1) %>% select(attr.1=attr, count.1=count)-> x1
x %>% filter(sentence==2) %>% select(attr.2=attr, count.2=count)-> x2
x %>% filter(sentence==3) %>% select(attr.3=attr, count.3=count)-> x3
x %>% filter(sentence==4) %>% select(attr.4=attr, count.4=count)-> x4

min.n <- min(nrow(x1), nrow(x2), nrow(x3), nrow(x4))

bind_cols(x1[1:min.n,], x2[1:min.n,], x3[1:min.n,], x4[1:min.n,]) %>%
  write_csv(file="../generated/data/attribute_ranking_by_sentence.csv")

# Attributes that appear more often in DogWhistle condition:

## Kinder sind die Zukunft unseres Volkes / unserer Gesellschaft:

d %>%
  filter(sentence %in% c(1,3)) %>%
  group_by(sentence, attr) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from="sentence", values_from="count") %>%
  rename(
    dogwhistle = `1`,
    control = `3`) %>%
  mutate(
    control = ifelse(is.na(control), 0, control),
    dogwhistle = ifelse(is.na(dogwhistle), 0, dogwhistle),
    difference = dogwhistle - control) %>%
  arrange(-difference) %>%
  write_csv(file="../generated/data/attribute_count_dogwhistle_vs_control_kinder.csv")
  
## Der Erwerb der deutschen Staatsbürgerschaft / Die Aufnahme in das
## deutsche Staatsvolk ist an strenge Bedingungen geknüpft:

d %>%
  filter(sentence %in% c(2,4)) %>%
  group_by(sentence, attr) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  pivot_wider(names_from="sentence", values_from="count") %>%
  rename(
    dogwhistle = `4`,
    control = `2`) %>%
  mutate(
    control = ifelse(is.na(control), 0, control),
    dogwhistle = ifelse(is.na(dogwhistle), 0, dogwhistle),
    difference = dogwhistle - control) %>%
  arrange(-difference) %>%
  write_csv(file="../generated/data/attribute_count_dogwhistle_vs_control_staatsvolk.csv")

# Participant overlap:

read_delim("../data/participants_1.csv", delim=";") %>%
  filter(status=="APPROVED") -> p1
read_delim("../data/participants_2.csv", delim=";") %>%
  filter(status=="APPROVED") -> p2

nrow(p1)
nrow(p2)

intersect(p1$participant_id, p2$participant_id) %>% length()

# Participant demographics:

library(stringdist)

d %>%
  select(subj, age, gender, party) %>%
  mutate(party = str_squish(party)) %>%
  unique() -> x

stringdistmatrix(x$party, names(colors.party), method="jw") -> m

x$party2 <- names(colors.party)[apply(m, 1, which.min)]

ggsave("../generated/plots/demographics.pdf", plot.demographics(x), width=21, height=7, unit="cm")
ggsave("../generated/plots/demographics.png", plot.demographics(x), width=1500, height=500, unit="px", dpi=150)

# Annotation to develop scales:

d <- read_csv(file="../generated/data/attribute_ranking_overall.csv")

# Visual inspection of the named properties (at least 2 mentions)
# suggests the following scales:
# Question to participants: Wie...ist der Sprecher? (gar nicht...total)

alt             <- c("jung")
fortschrittlich <- c("konservativ", "traditionell", "altmodisch", "progressiv", "bürgerlich", "rückwärtsgewandt", "zukunftsorientiert", "weitsichtig", "vorausschauend", "zukunftsgewand")
rassistisch     <- c("patriotisch", "patriotistisch", "nationalistisch", "rechts", "rassistisch", "rechts/konservativ", "xenophob")
ehrlich         <- c("ehrlich", "souverän", "heuchlerisch", "verantwortungsvoll", "zuverlässig", "vernünftig", "kalkulierend")
hilfsbereit     <- c("sozial", "fair", "altruistisch", "geizig", "intolerant", "unmenschlich", "offen", "verschlossen", "abweisend", "egoistisch", "engstirnig", "ausschliessend", "streng", "strikt", "dominant")
intelligent     <- c("intelligent", "gebildet", "klug", "kompetent", "erfahren", "selbstsicher")
christlich      <- c("christlich")
freundlich      <- c("freundlich, nett, unfreundlich", "unsympatisch", "kalt", "schroff", "sympatisch", "warm", "kritisch", "zuversichtlich")

# Properties that don't appear relevant (e.g., about a different
# topic, not about the speaker):

irrelevant <- c("unkonventionell","stärkend","langweilig", "nichtssagend","nachhaltig","redegewandt","fokussiert","kinderlieb","familiär","familienmensch","familienorientiert","familienfreundlich","kinderfreundlich","umweltbewusst","bürokratisch", "juristisch","realistisch", "informiert", "mitdenkend")

d$scale <- with(d, case_when(
  attr %in% alt             ~ "alt",
  attr %in% fortschrittlich ~ "fortschrittlich",
  attr %in% rassistisch     ~ "rassistisch",
  attr %in% ehrlich         ~ "ehrlich",
  attr %in% hilfsbereit     ~ "hilfsbereit",
  attr %in% intelligent     ~ "intelligent",
  attr %in% religioes       ~ "religioes",
  attr %in% freundlich      ~ "freundlich",
  TRUE                      ~ "other"))
