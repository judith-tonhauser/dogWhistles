# dogwhistle project
# analysis.R

# set working directory to directory of script
this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

# load relevant packages 
library(tidyverse)
library(ordinal)

# Hurwitz & Peffley 2005
# p.105: To test our hypotheses, we regressed Anticrime Policy Preferences on the predictor
# variables, a dummy variable representing the question frame (coded 1 for inner-city reference)
# and 0 otherwise), and interactions for each of the predictors and question frame, 
# using ordered probit analysis

# load the data 
d = read_csv("../data/d.csv")
nrow(d) #140

# make sure the variables have the right types

# targetResponse is the dependent variable (1-4)
table(d$targetResponse)
str(d$targetResponse) #needs to be an ordered factor
d$targetResponse <- as.factor(d$targetResponse)
levels(d$targetResponse) #now it is an ordered factor
# 4: new prisons/feel very strongly, 1: education programs/feel very strongly

# the two predictor variables are transStereotypeIndex and genderFairnessIndex
str(d$transStereotypeIndex) #numeric
table(d$transStereotypeIndex) # 5 (person doesn't accept the negative stereotypes) to 
# 35 (person accepts the negative stereotypes)
str(d$genderFairnessIndex) #numeric
table(d$genderFairnessIndex) # 4 (person believes that transgender people are treated very unfairly)
# to 23 (person believes that transgender people are treated very fairly)

# mean center the two predictors
d$transStereotypeIndex_c <- scale(d$transStereotypeIndex, center = TRUE, scale = FALSE) |> as.numeric()
mean(d$transStereotypeIndex_c)

d$genderFairnessIndex_c <- scale(d$genderFairnessIndex, center = TRUE, scale = FALSE) |> as.numeric()
mean(d$genderFairnessIndex_c)

d = d %>%
  filter(participantSexualOrientation == "Straight or heterosexual")

# the question frame is a factor, with no/0 and yes/1
str(d$dw)
d = d %>%
  mutate(dwFactor = case_when(
    dw == "yes" ~ 1,
    dw == "no" ~ 0,
    TRUE ~ 666))
d$dwFactor <- as.factor(d$dwFactor)
str(d$dwFactor)

# fit the model ----
m <- clmm(
  targetResponse ~ transStereotypeIndex_c + genderFairnessIndex_c 
  + transStereotypeIndex_c:dwFactor + genderFairnessIndex_c:dwFactor
  + preregistered
  + participantGender # only two levels, so can't be random effect
  + (1|cisStereotypeIndex) 
  + (1|generalFairnessIndex)
  + (1|equalityIndex)
  + (1|participantAge)
  #+ (1|participantSexualOrientation)
  ,
  data = d,
  link = "probit")

summary(m)
# only transStereotypeIndex is significant
# the more the person accepts negative stereotypes about transgender people,
# the more they are in favor of punitive measures

# plot predicted probabilities ----

# model <- clmm(response ~ A_c * C + B_c * C + (1 | subject), data = df)

# Create a grid of values for prediction
A_seq <- seq(min(df$A_c), max(df$A_c), length.out = 50)
B_seq <- seq(min(df$B_c), max(df$B_c), length.out = 50)
C_vals <- c(0, 1) # binary

# Fix B at mean and vary A to visualize A*C interaction
pred_grid_A <- expand.grid(
  A_c = A_seq,
  B_c = 0,       # mean-centered, so 0 = mean
  C = C_vals
)

# Predict probabilities
pred_probs_A <- predict(model, newdata = pred_grid_A, type = "prob")

# Combine predictions with grid
pred_grid_A <- cbind(pred_grid_A, pred_probs_A)

# Convert to long format for ggplot
pred_long_A <- pred_grid_A %>%
  pivot_longer(cols = starts_with("Pr("), names_to = "Response", values_to = "Probability")

# Plot predicted probabilities of outcome by A for C=0 vs C=1
ggplot(pred_long_A, aes(x = A_c, y = Probability, color = as.factor(C), linetype = Response)) +
  geom_line(size = 1) +
  labs(
    x = "A (mean-centered)",
    y = "Predicted probability",
    color = "C",
    linetype = "Response category"
  ) +
  theme_minimal()
You can do the same for B:
  
  r
Copy code
pred_grid_B <- expand.grid(
  A_c = 0,       # mean-centered
  B_c = B_seq,
  C = C_vals
)

pred_probs_B <- predict(model, newdata = pred_grid_B, type = "prob")
pred_grid_B <- cbind(pred_grid_B, pred_probs_B)

pred_long_B <- pred_grid_B %>%
  pivot_longer(cols = starts_with("Pr("), names_to = "Response", values_to = "Probability")

ggplot(pred_long_B, aes(x = B_c, y = Probability, color = as.factor(C), linetype = Response)) +
  geom_line(size = 1) +
  labs(
    x = "B (mean-centered)",
    y = "Predicted probability",
    color = "C",
    linetype = "Response category"
  ) +
  theme_minimal()

#Interpretation tip:
# If the lines for C=0 vs C=1 are nearly overlapping for 
# each response category, this confirms no meaningful interaction 
# at typical levels of A or B.

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra) # for arranging panels

# -----------------------------
# Step 1: Prepare prediction grids
# -----------------------------
A_seq <- seq(min(df$A_c), max(df$A_c), length.out = 50)
B_seq <- seq(min(df$B_c), max(df$B_c), length.out = 50)
C_vals <- c(0, 1)

# Grid for A
pred_grid_A <- expand.grid(A_c = A_seq, B_c = 0, C = C_vals)
pred_probs_A <- predict(model, newdata = pred_grid_A, type = "prob")
pred_grid_A <- cbind(pred_grid_A, pred_probs_A)
pred_long_A <- pred_grid_A %>%
  pivot_longer(cols = starts_with("Pr("), names_to = "Response", values_to = "Probability") %>%
  mutate(Predictor = "A")

# Grid for B
pred_grid_B <- expand.grid(A_c = 0, B_c = B_seq, C = C_vals)
pred_probs_B <- predict(model, newdata = pred_grid_B, type = "prob")
pred_grid_B <- cbind(pred_grid_B, pred_probs_B)
pred_long_B <- pred_grid_B %>%
  pivot_longer(cols = starts_with("Pr("), names_to = "Response", values_to = "Probability") %>%
  mutate(Predictor = "B")

# Combine for plotting
pred_long <- bind_rows(pred_long_A, pred_long_B)

# -----------------------------
# Step 2: Plot panels with consistent colors/line types
# -----------------------------
plot <- ggplot(pred_long, aes(x = ifelse(Predictor=="A", A_c, B_c),
                              y = Probability,
                              color = as.factor(C),
                              linetype = Response)) +
  geom_line(size = 1) +
  facet_wrap(~Predictor, scales = "free_x", ncol = 2,
             labeller = as_labeller(c(A="Predictor A", B="Predictor B"))) +
  scale_color_manual(values = c("0" = "blue", "1" = "red"),
                     labels = c("C = 0", "C = 1")) +
  labs(
    x = "Mean-centered predictor",
    y = "Predicted probability",
    color = "C",
    linetype = "Response category"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

# Display the combined figure
print(plot)


# end plot predicted probabilities ----

m <- clmm(
  targetResponse ~ transStereotypeIndex_c 
  + transStereotypeIndex_c:dwFactor 
  + participantGenderNum
  + participantAge
  + preregistered
  #+ (1|participantID),
  + (1|cisStereotypeIndex) + (1|generalFairnessIndex),
  data = d,
  link = "probit"
)
summary(m)
# transStereotypeIndex and interaction significant

m <- clmm(
  targetResponse ~ genderFairnessIndex_c 
  + genderFairnessIndex_c:dwFactor 
  + participantGenderNum
  + participantAge
  + preregistered
  #+ (1|participantID),
  + (1|cisStereotypeIndex) + (1|generalFairnessIndex),
  data = d,
  link = "probit"
)
summary(m)
# genderFairnessIndex significant, interaction marginal

# plot the model output (haven't adjusted this yet) ----

library(ggeffects)
library(ggplot2)

pred <- ggpredict(m, terms = c("transStereotypeIndex", "dwFactor"))
pred
colnames(pred)

prob_high <- pred %>%
  filter(response.level %in% c("3", "4")) %>%   # Use response.level, not facet
  group_by(x, group) %>%
  summarize(prob = sum(predicted),
            conf.low = sum(conf.low),
            conf.high = sum(conf.high),
            .groups = "drop")

# Plot
ggplot(prob_high, aes(x = x, y = prob, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(
    x = "transStereotypeIndex",
    y = "Probability of targetResponse = 3 or 4",
    color = "dwFactor",
    fill = "dwFactor",
    title = "Predicted Probability of High targetResponse by transStereotypeIndex and dwFactor"
  ) +
  theme_minimal(base_size = 14)

pred <- ggpredict(m, terms = c("genderFairnessIndex", "dwFactor"))
pred
colnames(pred)

prob_high <- pred %>%
  filter(response.level %in% c("3", "4")) %>%   # Use response.level, not facet
  group_by(x, group) %>%
  summarize(prob = sum(predicted),
            conf.low = sum(conf.low),
            conf.high = sum(conf.high),
            .groups = "drop")

# Plot
ggplot(prob_high, aes(x = x, y = prob, color = group)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2, color = NA) +
  labs(
    x = "transStereotypeIndex",
    y = "Probability of targetResponse = 3 or 4",
    color = "dwFactor",
    fill = "dwFactor",
    title = "Predicted Probability of High targetResponse by transStereotypeIndex and dwFactor"
  ) +
  theme_minimal(base_size = 14)
