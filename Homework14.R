###################
### Homework 14 ###
###################

# Group members: Keeley Kuru
# Date: 12/2/25

# From Homework 11:

# == Working with partner's data: Lonnie's Brook Trout Activity Data == #

# from Lonnie: #In this simulated dataset, brook trout activity (movements/hr) declines as stream temperature increases. 
#               Streams with cold-water refugia have higher baseline activity and a weaker decline in activity with increasing temperature compared to streams without refugia. 
#               The ANCOVA model tests whether the slopes of activity–temperature relationships differ between the two stream types, 
#               answering this ecological question: Do brook trout with cold-water refugia maintain higher activity levels as temperatures rise compared to those without refugia?

# Load tidyverse
library(tidyverse)

# Load the data Lonnie shared
lonnie_data <- read.csv("brook_trout_activity.csv")

# Inspect the data
head(lonnie_data)
str(lonnie_data)

# Full model with interaction
full_model <- lm(activity ~ temp * group, data = lonnie_data)
summary(full_model)

# Reduced model without interaction
no_interaction <- lm(activity ~ temp + group, data = lonnie_data)
summary(no_interaction)

# Plot the data and model fits
ggplot(lonnie_data, aes(x = temp, y = activity, color = group)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  scale_color_manual(values = c("#1b9e77", "#d95f02")) +
  labs(
    title = "Brook Trout Activity vs. Stream Temperature",
    x = "Stream Temperature (°C)",
    y = "Brook Trout Activity (movements/hr)",
    color = "Stream Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Compare models using ANOVA
anova(no_interaction, full_model)
# -> If the interaction is significant (p < 0.05), it means the relationship between temperature and activity differs by group. 
# -> If not significant, we would drop the interaction and use the simpler model.
# -> In this case, we see that the interaction is significant, indicating that the slopes differ between groups.


# -- Objective 1 for Homework 14 -- #
# 1a. Exchange data with partner (from Homework 11)

# 1b. Fit two alternative models to the data given to you:
#  - The full model (both X variables and their interaction) and
#  - A reduced model with no interaction term. 
# Full model (with interaction)
full_model <- lm(activity ~ temp * group, data = lonnie_data)

# Reduced model (no interaction)
no_interaction <- lm(activity ~ temp + group, data = lonnie_data)

# 1c. Calculate the negative log-likelihood (NLL) for both models.
NLL_full <- -as.numeric(logLik(full_model))
NLL_reduced <- -as.numeric(logLik(no_interaction))

NLL_full
NLL_reduced

# 1d. Compare the two models using likelihood ratio test (lmtest::lrtest) to see whether the NLL of
#    the full model is sufficiently lower to justify the additional parameter. 
library(lmtest)
lrtest(no_interaction, full_model)
# -> If the p-value is less than 0.05, we reject the null hypothesis that the reduced model is sufficient,
#    indicating that the full model with interaction provides a significantly better fit to the data.
# -> In this case, we see that the p-value is less than 0.05, indicating that the full model is justified.

# Which model is preferred and what does that mean ecologically?
# -> The preferred model is the full model with interaction, suggesting that brook trout activity responds differently 
# to temperature depending on whether the stream has cold-water refugia or not.

# How does your answer compare to the result you obtained when doing backward model selection?
# -> The result is consistent with backward model selection, which also indicated that the interaction term was significant

# -- Objective 2 for Homework 14 -- #
# 2a. Develop an AIC table (in a data frame) to compare the following models:
# Candidate models
m_full <- lm(activity ~ temp * group, data = lonnie_data)       # full model (interaction)
m_main <- lm(activity ~ temp + group, data = lonnie_data)       # main effects only
m_temp <- lm(activity ~ temp, data = lonnie_data)               # single predictor: temp
m_group <- lm(activity ~ group, data = lonnie_data)             # single predictor: group
m_intercept <- lm(activity ~ 1, data = lonnie_data)             # intercept-only (null model)

# Create the AIC table
aic_table <- data.frame(
  Model = c("Full (temp * group)",
            "Main effects (temp + group)",
            "Temp only",
            "Group only",
            "Intercept-only"),
  AIC = c(AIC(m_full),
          AIC(m_main),
          AIC(m_temp),
          AIC(m_group),
          AIC(m_intercept))
)

# Sort table so the best (lowest AIC) is first
aic_table <- aic_table %>% arrange(AIC)
aic_table

# 2b. The full model (with the temperature × refugia interaction) has the lowest AIC value, 
#     more than 80 AIC units better than the next-best model. Because ΔAIC ≫ 2 for all other models, 
#    the full model is overwhelmingly supported. This indicates that including the interaction term provides
#    much better explanatory power and is necessary to describe the data.
