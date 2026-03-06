################################################################################
######Plot Conditional and Unconditional SES-Achievement Relationship###########
################################################################################
# Define a common Y-axis limit for both plots
common_y_lims <- c(0, 0.6)

SES.null <- ggplot(coef_df_null, aes(x = tau, y = coef.ses)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0.1, 0.9, by = 0.1)) + # Standard 0.1 increments
  scale_y_continuous(limits = common_y_lims, breaks = seq(0, 0.6, by = 0.1)) + 
  labs(x = "Quantile (τ)", y = "Standardized SES Coefficient", title = "A") +
  theme_minimal()

SES.full <- ggplot(coef_df_full, aes(x = tau, y = coef.ses)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(0.1, 0.9, by = 0.1)) +
  scale_y_continuous(limits = common_y_lims, breaks = seq(0, 0.6, by = 0.1)) +
  labs(x = "Quantile (τ)", y = "", title = "B") +
  theme_minimal()

##put them next to each other
cowplot::plot_grid(
  SES.null,
  SES.full
)

################################################################################
######Plot Conditional SES-Achievement Relationship for Subsamples##############
################################################################################
##White subsample:

SES.white <- ggplot(coef_df_white, aes(x = tau, y = coef.ses)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = common_y_lims) +
  labs(
    x = "Quantile (τ)",
    y = "",
    title = "A",
    #subtitle = "With 95% Confidence Intervals"
  ) +
  theme_minimal()

##Hispanic subsample:
SES.hispanic <- ggplot(coef_df_hispanic, aes(x = tau, y = coef.ses)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = common_y_lims) +
  labs(
    x = "Quantile (τ)",
    y = "",
    title = "B",
    #subtitle = "With 95% Confidence Intervals"
  ) +
  theme_minimal()

##Black Subsample: 

SES.black <- ggplot(coef_df_black, aes(x = tau, y = coef.ses)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = common_y_lims) +
  labs(
    x = "Quantile (τ)",
    y = "",
    title = "C",
    #subtitle = "With 95% Confidence Intervals"
  ) +
  theme_minimal()

plot_grid(
  SES.white,
  SES.hispanic,
  SES.black,
  ncol = 3
)


################################################################################
####################Plot Gender*SES Interaction Variables#######################
################################################################################
common_y_lims <- range(
  c(
    coef_df_full$lower,
    coef_df_full$upper,
    coef_df_white$lower,
    coef_df_white$upper,
    coef_df_hispanic$lower,
    coef_df_hispanic$upper,
    coef_df_black$lower,
    coef_df_black$upper
  ),
  na.rm = TRUE
)


interaction.full <- ggplot(coef_df_interaction_full , aes(x = tau, y = coef.ses)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(limits = common_y_lims) +
  labs(
    x = "Quantile (τ)",
    y = "SES × Male Interaction Coefficient",
    title = "Full Sample"
  ) +
  theme_minimal()


##Full Model:
interaction.full <- ggplot(coef_df_interaction_full , aes(x = tau, y = coef.ses)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Quantile (τ)",
    y = "SES × Male Interaction Coefficient",
    title = "A",
    #subtitle = "With 95% Confidence Intervals"
  ) +
  theme_minimal()

##White Subsample:
genderXSES.white <- ggplot(coef_df_interaction_white , aes(x = tau, y = coef.ses)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Quantile (τ)",
    y = "",
    title = "B",
    #subtitle = "With 95% Confidence Intervals"
  ) +
  theme_minimal()

##Hispanic subsample:
genderXSES.hispanic <- ggplot(coef_df_interaction_hispanic , aes(x = tau, y = coef.ses)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Quantile (τ)",
    y = "SES × Male Interaction Coefficient",
    title = "C",
    #subtitle = "With 95% Confidence Intervals"
  ) +
  theme_minimal()

##Black subsample:
genderXSES.black <- ggplot(coef_df_interaction , aes(x = tau, y = coef.ses)) +
  geom_line(color = "blue", size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1, fill = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = "Quantile (τ)",
    y = "",
    title = "D",
    #subtitle = "With 95% Confidence Intervals"
  ) +
  theme_minimal()



plot_grid(
  interaction.full,
  genderXSES.white,
  genderXSES.hispanic,
  genderXSES.black,
  ncol = 2
)
