# File: SMSS_final_results.R
# Author: [Walker Haskins] ([walkersth@gmail.com])
# School: University of Amsterdam
# Programme: Research MA Social Sciences
# Course: Statistical Models in the Social Sciences
# Teacher: Dr. Sara Geven
# Last Modified: 2025-06-26
#
# Description: Script with final steps of running my regression models: 
# creating final data, creating models, checking assumptions, & results 
#
# Usage:
# Should be run after the filtering script, SMSS_paper_data_filtering.R
#
# ------------------------------------------------------------------------
library(lubridate)
library(survival)
library(lme4)
library(stargazer)
library(dplyr)
library(multiwayvcov)
library(sandwich)
library(lmtest)
library(car)
library(psych)
library(gtsummary)
library(ggplot2)
library(knitr)
library(gt)
library(scales)
library(modelsummary)
library(kableExtra)
library(psych)



# SUMMARIZING NAs FOR MODEL VARAIBLES -----------------------------------------

model_vars <- c(                                          # Defining model vars
  "civil_war_onset",
  "log_WB_pop_lag1",
  "log_GDP_lag1",
  "newstate",
  "anoc",
  "WB_oil_dummy",
  "polity_instability_dummy",
  "ncontig",
  "log_newlmtnest"
)


NA_tibble_wide <- pro_rrf[model_vars] %>%                  # Wide tibble of NAs
  summarise(
    across(
      everything(),
      list(
        NA_count = ~sum(is.na(.)),
        NA_pct = ~round(mean(is.na(.)) * 100, 1)
      )
    )
  )


NA_tibble_long <- NA_tibble_wide %>%  # Long tibble cause the wide was annoying
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", ".value"),
    names_sep = "_NA_"                            # this splits names at "_NA_"
  )

# View(NA_tibble_long)                         # commenting out to avoid popups


# FINAL CLEANING  --------------------------------------------------------------

sapply(
  pro_rrf[model_vars],
  function(x) {                          # Filtering out the NAs and some weird
    sum(is.na(x) | is.infinite(x) | is.nan(x))     # inf vals that got in there
  }
)

pro_rrf_clean <- pro_rrf %>%       # Making pro_rrf_clean, final data for model
  filter(
    across(
      all_of(model_vars),
      ~ !(is.na(.) | is.infinite(.) | is.nan(.))
    )
  )


# DESCRIPTIVE STATS (FOR CLEANED DATA) -----------------------------------------

model_vars_no_ns <- c(
  "civil_war_onset",
  "log_WB_pop_lag1",
  "log_GDP_lag1",
  "anoc",
  "WB_oil_dummy",
  "polity_instability_dummy",
  "ncontig",
  "log_newlmtnest"
)

describe(pro_rrf_clean[, model_vars_no_ns])                     # summary stats

                                                  
total_country_years <- nrow(pro_rrf_clean)        # Total # of country-year obs

                                                         # Total # of countries
num_unique_countries <- length(unique(pro_rrf_clean$iso3_code))

                                    

num_onsets <- sum(pro_rrf_clean$civil_war_onset == 1,   # Total # & % of onsets
                  na.rm = TRUE)

total_obs <- nrow(pro_rrf_clean)
print(
  paste(
    "Number of Civil War Onsets (1s):", 
    num_onsets))
print(
  paste(
    "Total Observations (country-years): ", 
    total_obs))
print(
  paste(
    "Percentage of Onsets:", 
    round(
      num_onsets / total_obs * 100, 2), "%"))



# --- Creating Desc stats table ---
# Simple descriptive statistics table
clean_desc_table <- pro_rrf_clean %>%
  select(all_of(model_vars_no_ns)) %>%
  tbl_summary(
    
    statistic = list(
      all_continuous() ~ "{mean} ({sd}) [{min}, {max}]",
      all_categorical() ~ "{n} ({p}%)"
    ),
    
    label = list(
      civil_war_onset ~ "Civil War Onset",
      log_WB_pop_lag1 ~ "Log Population (lag)",
      log_GDP_lag1 ~ "Log GDP (lag)",
      anoc ~ "Anocracy",
      WB_oil_dummy ~ "Oil Exporter",
      polity_instability_dummy ~ "Political Instability",
      ncontig ~ "Noncontiguous",
      log_newlmtnest ~ "Log Terrain Ruggedness"
    ),
    
    digits = all_continuous() ~ 2
  ) %>% 
  modify_caption("**Descriptive Statistics of Variables (Complete cases only)**")


clean_desc_table

clean_desc_table %>%
  as_gt() %>%                                            # Convert to gt object
  gt::as_latex() %>%                                         # Convert to LaTeX
  writeLines("clean_descriptive_stats.tex")                      # Save to file

# Basic sample info
cat("\nSample Info:\n")
cat("Total observations:", nrow(pro_rrf_clean), "\n")
cat("Civil war onsets:", sum(pro_rrf_clean$civil_war_onset == 1, na.rm = TRUE), 
    paste0("(", round(mean(pro_rrf_clean$civil_war_onset == 1, na.rm = TRUE) * 100, 1), "%)"))


# REGRESSION MODELS ------------------------------------------------------------

model_glm <- glm(                                            # base logit model
  civil_war_onset ~ log_WB_pop_lag1 + log_GDP_lag1 + anoc +
    WB_oil_dummy + polity_instability_dummy + ncontig + log_newlmtnest,
  data = pro_rrf_clean,
  family = binomial(link = "logit")
)

model_clogit <- clogit(                                              # FE model
  civil_war_onset ~ log_WB_pop_lag1 + log_GDP_lag1 + anoc +
    WB_oil_dummy + polity_instability_dummy + ncontig + log_newlmtnest +
    strata(iso3_code),                        # this arg does FEs for countries
  data = pro_rrf_clean,
  method = "efron"
)

model_pooled <- glm(                        # w/ new-state - the removed MC var
  civil_war_onset ~ log_WB_pop_lag1 + log_GDP_lag1 + newstate + anoc +
    WB_oil_dummy + polity_instability_dummy + ncontig + log_newlmtnest,
  data = pro_rrf_clean,
  family = binomial(link = "logit")
)

model_pooled_2 <- glm(                                           # w/o newstate
  civil_war_onset ~ log_WB_pop_lag1 + log_GDP_lag1 + anoc +
    WB_oil_dummy + polity_instability_dummy + ncontig + log_newlmtnest,
  data = pro_rrf_clean,
  family = binomial(link = "logit")
)


# RESULTS ----------------------------------------------------------------------

# --- Country-clustered SEs calculations ---
cluster_se <- cluster.vcov(                
  model_pooled,
  ~ iso3_code
)                                      # Clustering on iso3code to avoid errors
pooled_results <- coeftest(
  model_pooled,
  vcov = cluster_se
)


# --- Results, ORs, and OR-CIs for clustered SEs ---  
# this calculation's different b/c of the clustering procedure
print(pooled_results)

print(
  exp(
    coef(
      model_glm)))                                                # Odds ratios

lower_ci_pooled <- exp(
  pooled_results[, "Estimate"] - 1.96 * pooled_results[, "Std. Error"]) #OR CIs

upper_ci_pooled <- exp(
  pooled_results[, "Estimate"] + 1.96 * pooled_results[, "Std. Error"])

ci_pooled_df <- data.frame(
  Lower = lower_ci_pooled,
  Upper = upper_ci_pooled
)
print(ci_pooled_df)


# --- Results, ORs, & OR-CIs - Base model ---
summary(model_glm)                                                    # summary

print(
  exp(
    coef(
      model_glm)))                                                # Odds ratios

print(
  exp(
    confint(
      model_glm)))                                                     # OR CIs

# --- Results,ORs & OR-CIs - CLOGIT (FE) Model ---
summary(model_clogit)                                                 # Summary

print(
  exp(
    coef(
      model_clogit)))                                             # Odds Ratios

print(
  exp(
    confint(
      model_clogit)))                               # OR - Confidence Intervals



# LIKELIHOOD RATIO TESTS -------------------------------------------------------

# --- Base Logit ---
null_model_glm <- glm(                                # Defining the null model
  civil_war_onset ~ 1,
  data = pro_rrf_clean,
  family = binomial(link = "logit")
)

lr_test_glm <- anova(                                                # LRT Test
  null_model_glm,
  model_glm,
  test = "LRT"
)
print(lr_test_glm)


# --- Clustered Logit --- 
lr_test_pooled <- anova(         # null model is the same as standard GLM above
  null_model_glm,
  model_pooled,
  test = "LRT"
)
print(lr_test_pooled)


# --- CLogit/ FE model ---
null_model_clogit <- clogit(                              # Defining Null Model
  civil_war_onset ~ strata(iso3_code),
  data = pro_rrf_clean,
  method = "efron"
)

lr_test_clogit <- anova(                                             # LRT test
  null_model_clogit,
  model_clogit,
  test = "LRT"
)
print(lr_test_clogit)

# --- Log likelihood for the pooled (GLM) models ---
loglik_null_glm <- as.numeric(
  logLik(
    glm(
      civil_war_onset ~ 1,
      data = pro_rrf_clean,
      family = binomial(link = "logit"))))

loglik_full_glm <- as.numeric(
  logLik(
    model_glm))

loglik_null_pooled <- loglik_null_glm              # same null model for pooled
loglik_full_pooled <- as.numeric(
  logLik(
    model_pooled))


# --- Chi-squares and dfs for GLMs ---
chi_sq_glm <- 2 * (loglik_full_glm - loglik_null_glm)
df_glm <- attr(
  logLik(
    model_glm), "df") - attr(
      logLik(
        glm(civil_war_onset ~ 1,
            data = pro_rrf_clean, 
            family = binomial(link = "logit"))), "df")

p_val_glm <- 1 - pchisq(chi_sq_glm,
                        df_glm)

chi_sq_pooled <- 2 * (loglik_full_pooled - loglik_null_pooled)
df_pooled <- attr(
  logLik(
    model_pooled), "df") - attr(
      logLik(
        glm(
          civil_war_onset ~ 1,
          data = pro_rrf_clean,
          family = binomial(link = "logit"))), "df")

p_val_pooled <- 1 - pchisq(chi_sq_pooled, df_pooled)

# --- have to use this model for clogit otherwise it doesn't work ---
null_model_clogit <- clogit(
  civil_war_onset ~ strata(iso3_code),
  data = pro_rrf_clean,
  method = "efron"
)
lr_test_clogit <- anova(null_model_clogit,
                        model_clogit, test = "LRT")

# Build table

lrt_table <- data.frame(
  Model = c("Base Logit", "Clustered Logit", "Conditional Logit"),
  
  `Log-Likelihood (Null)` = c(
    round(loglik_null_glm, 2),
    round(loglik_null_pooled, 2),
    NA                                                # Clogit doesn't take AIC
  ),
  
  `Log-Likelihood (Full)` = c(
    round(loglik_full_glm, 2),
    round(loglik_full_pooled, 2),
    round(lr_test_clogit$loglik[2], 2)
  ),
  
  `Chi-square` = c(
    round(chi_sq_glm, 2),
    round(chi_sq_pooled, 2),
    round(lr_test_clogit$Chisq[2], 2)
  ),
  
  df = c(
    df_glm,
    df_pooled,
    lr_test_clogit$Df[2]
  ),
  
  `p-value` = c(
    paste0("$\\mathit{p}$ = ", formatC(p_val_glm,
                                       format = "f",
                                       digits = 3)),
    paste0("$\\mathit{p}$ = ", formatC(p_val_pooled,
                                       format = "f",
                                       digits = 3)),
    paste0("$\\mathit{p}$ = ", formatC(lr_test_clogit$`Pr(>|Chi|)`[2],
                                       format = "f", 
                                       digits = 3))
  )
)

# Output to LaTeX
latex_code <- kable(
  lrt_table,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
  caption = "Likelihood Ratio Tests for Civil War Onset Models"
)

writeLines(latex_code, "lrt_table_APA.tex")




# P VALUE CALCULATIONS (FOR REFERENCE ON PAPER) --------------------------------

# --- Base Logit ---
summary_model_glm <- summary(model_glm)           # Saving results to an object
coefficients_table_glm <- summary_model_glm$coefficients    # Extracting coeffs
p_values_glm <- coefficients_table_glm[, "Pr(>|z|)"]        # Extracting p-vals
print(p_values_glm)

# --- Clustered Logit ---
p_values_c <- pooled_results[, "Pr(>|z|)"]  # P-vals created in clustering step
print(p_values_c)

# --- CLogit/FE ---
summary_model_clogit <- summary(model_clogit)     # Saving results to an object
coefficients_table_cl <- summary_model_clogit$coefficients  # Extracting coeffs
p_values_cl <- coefficients_table_cl[, "Pr(>|z|)"]          # Extracting p-vals
print(p_values_cl)


# MULTICOLLINEARITY ------------------------------------------------------------

# --- Base/Clustered ---
vif_results <- vif(model_pooled_2)      # Dropped new-state cause of perfect MC
print(vif_results)

vif_df <- data.frame(
  Variable = names(vif_results),
  VIF = as.numeric(vif_results)
)

vif_df %>%
  kable(
    caption = "Variance Inflation Factors (VIF) for Predictor Variables",
    digits = 2,
    col.names = c("Variable", "VIF"),
    booktabs = TRUE,
    align = c("l", "r"),
    format = "latex"
  ) %>%
  kable_styling(
    position = "left",                           # align left, like your tables
    full_width = FALSE,
    font_size = 10                    # match typical LaTeX font size in tables
  ) %>%
  add_footnote(
    "Note: VIF values above 5 indicate potential multicollinearity concerns.",
    notation = "none",
    escape = FALSE
  )

alias_results <- alias(model_pooled)# no perfect MC detected after dropping NS
print(alias_results)

# --- CLogit/FE ---                   # Doesn't work the same way so excluding




# STARGAZER TABLE (FOR REGRESSION RESULTS) -------------------------------------

stargazer(
  model_glm,
  model_pooled_2,           # double-check this is the right one (it is, no NS)
  model_clogit, 
  type = "latex",                                            # changed to latex
  title = "Comparison of Civil War Onset Models",
  dep.var.labels = "Civil War Onset",
  covariate.labels = c(
    "Log Population (Lag 1)",
    "Log GDP Per Capita (Lag 1)",
    "New State Dummy",
    "Anocracy Dummy",
    "Oil Exporter Dummy",
    "Political Instability Dummy",
    "Noncontiguous Territory",
    "Log Terrain Ruggedness"
  ),
  column.labels = c("Base Logit", "Clustered SEs", "Conditional w/ FEs"),
  model.names = FALSE,
  model.numbers = FALSE,
  keep.stat = c("n", "aic", "ll"), 
  
  
  #  --- Manually providing  clustered SEs and p-values for model_pooled, to 
  # avoid a long standing bug---

  se = list(NULL, sqrt(diag(cluster_se)), NULL), # make glm/clogit use defaults
  p = list(NULL, pooled_results[, "Pr(>|z|)"], NULL),                   # ditto
  
  add.lines = list(
    c("Country Clustered SEs", "No", "Yes", "No"),    # APA style notes attempt
    c("Country Fixed Effects", "No", "No", "Yes"),
    c("Country Random Effects", "No", "No", "No")
  ),
  notes = c(
    "Standard errors in parentheses",
    "Pooled logit with country-clustered standard errors",
    "Conditional logit with country fixed effects",
    "* p<0.1; ** p<0.05; *** p<0.01"
  ),
  notes.align = "l",
  align = TRUE,
  out = "civil_war_models_table.html"
)

# ODDS RATIOS ------------------------------------------------------------------


# --- Getting Coeffs and CIs ---
clogit_estimates <- coef(model_clogit)                    # takes Clogit coeffs
clogit_conf_intervals <- confint(model_clogit)                  # takes the CIs

# --- sending them to a df ---
clogit_results_for_plot <- data.frame(
  term = names(clogit_estimates),
  estimate = as.numeric(clogit_estimates),
  conf.low = as.numeric(clogit_conf_intervals[, 1]),
  conf.high = as.numeric(clogit_conf_intervals[, 2])
) %>%
  filter(term != "(Intercept)") %>%              # intercept is conditioned out
  mutate(
    Odds_Ratio = exp(estimate),
    Lower_CI = exp(conf.low),
    Upper_CI = exp(conf.high)
  ) %>%
  select(term, Odds_Ratio, Lower_CI, Upper_CI)


# --- fixing variable names ---
clogit_results_for_plot <- clogit_results_for_plot %>%
  mutate(
    term = case_when(         # changing to regular names instead of code names
      term == "log_WB_pop_lag1" ~ "Log Population (Lag 1)",
      term == "log_GDP_lag1" ~ "Log GDP Per Capita (Lag 1)",
      term == "anoc" ~ "Anocracy Dummy",
      term == "WB_oil_dummy" ~ "Oil Exporter Dummy",
      term == "polity_instability_dummy" ~ "Political Instability Dummy",
      TRUE ~ term
    )
  )

# --- reordering for plot ---
plot_term_order_clogit <- c(
  "Log Population (Lag 1)",
  "Log GDP Per Capita (Lag 1)",
  "Anocracy Dummy",
  "Oil Exporter Dummy",
  "Political Instability Dummy"
)

clogit_results_for_plot$term <- factor(
  clogit_results_for_plot$term,
  levels = rev(
    plot_term_order_clogit))


# --- Coeff/Forest plot, but for clogit model only ---
# Styling mimics APA guidelines as far as possible, from the APA guide and
# student checklist
clogit_coeff_plot <- ggplot(clogit_results_for_plot,
                            aes(x = Odds_Ratio,
                                y = term)) +
  geom_vline(xintercept = 1,
             linetype = "dashed",
             color = "grey50") +           # Puts a line for reference @ OR = 1
  geom_point(size = 3,
             color = "darkblue") + 
  geom_errorbarh(aes(xmin = Lower_CI,
                     xmax = Upper_CI),
                 height = 0.2,
                 color = "darkblue") +
  scale_x_log10(
    breaks = c(0.1, 0.5, 1, 2, 5, 10, 20, 50, 100),           # OR range breaks
    labels = scales::label_comma(accuracy = 0.1)
  ) +
  labs(
    x = "Odds Ratio (Log Scale)", 
    y = NULL                  #  Don't need y label as there are variable names
  ) +
  theme_classic() +                                  # suggested online for APA
  theme(
    text = element_text(family = "sans",
                        size = 11),                         # APA is sans serif
    axis.text.y = element_text(angle = 0,
                               hjust = 1,
                               size = 10),                       # y-axis ticks
    axis.text.x = element_text(size = 10),                       # x-axis ticks
    axis.title.x = element_text(size = 11,
                                face = "bold"),          # bold like other figs
    axis.title.y = element_blank(),                         # again, no y-label
    panel.background = element_rect(fill = "white",
                                    color = NA),                    # APA white
    plot.background = element_rect(fill = "white",
                                   color = NA),               # APA likes white
    panel.grid.major = element_blank(),                   # APA hates gridlines
    panel.grid.minor = element_blank(),                                 # Ditto
    axis.line = element_line(colour = "black"),     # APA says black axis lines
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm") # plot margins to avoid bug
  )

# --- printing the plot ---
print(clogit_coeff_plot)

# ROBUSTNESS CHECK - STATECAPE_BASELINE ----------------------------------------
# _ I switched to modelsummary for these results cause stargazer bugged out

# --- Model 1 with statecap  baseline (+SC) ---
robust_model_glm <- glm(
  civil_war_onset ~ log_WB_pop_lag1 + log_GDP_lag1 + anoc +
    WB_oil_dummy + polity_instability_dummy + ncontig + log_newlmtnest +
    statecap_baseline, 
  data = pro_rrf_clean,
  family = binomial(link = "logit")
)

# --- Model 2 w/ statecap baseline (+SC) ---
robust_model_pooled_glm_no_newstate <- glm(
  civil_war_onset ~ log_WB_pop_lag1 + log_GDP_lag1 + anoc + 
    WB_oil_dummy + polity_instability_dummy + ncontig + log_newlmtnest +
    statecap_baseline, 
  data = pro_rrf_clean,
  family = binomial(link = "logit")
)
robust_cluster_se_no_newstate <- multiwayvcov::cluster.vcov(robust_model_pooled_glm_no_newstate, ~ iso3_code)
robust_pooled_results_no_newstate_coeftest <- lmtest::coeftest(robust_model_pooled_glm_no_newstate,
                                                               vcov = robust_cluster_se_no_newstate)


# --- Converting original results to coeftest for consistency ---
glm_results_coeftest <- lmtest::coeftest(model_glm)
robust_glm_results_coeftest <- lmtest::coeftest(robust_model_glm)


# --- CLogit with State Capacity ---
model_clogit_statecap <- survival::clogit(
  civil_war_onset ~ log_WB_pop_lag1 + log_GDP_lag1 + anoc +
    WB_oil_dummy + polity_instability_dummy + ncontig + log_newlmtnest +
    statecap_baseline +
    strata(iso3_code),
  data = pro_rrf_clean
)


#--- defining models for modelsummary ---
models <- list(
  "Base Logit (+SC)" = robust_model_glm,
  "Clustered SEs (+SC)" = robust_model_pooled_glm_no_newstate,
  "Cond. w/ FEs (+SC)" = model_clogit_statecap
)

# --- renaming for model summary to match the stargzzer table ---
coef_map <- c(
  "(Intercept)" = "Constant",
  "log_WB_pop_lag1" = "Log Population (Lag 1)",
  "log_GDP_lag1" = "Log GDP Per Capita (Lag 1)",
  "anoc" = "Anocracy Dummy",
  "WB_oil_dummy" = "Oil Exporter Dummy",
  "polity_instability_dummy" = "Political Instability Dummy",
  "ncontig" = "Noncontiguous Territory",
  "statecap_baseline" = "State Capacity (Baseline)",
  "log_newlmtnest" = "Log Terrain Ruggedness"
)

# --- adding the rows APA likes at the bottom ---
apa_rows <- tibble::tibble(
  Term = c("Log-Likelihood", "AIC", "Clustered SEs", "Fixed Effects"),
  `Base Logit (+SC)` = c(
    round(logLik(robust_model_glm), 2),
    round(AIC(robust_model_glm), 2),
    "No", "No"
  ),
  `Clustered SEs (+SC)` = c(
    round(logLik(robust_model_pooled_glm_no_newstate), 2),
    round(AIC(robust_model_pooled_glm_no_newstate), 2),
    "Yes", "No"
  ),
  `Cond. w/ FEs (+SC)` = c(
    round(logLik(model_clogit_statecap), 2),
    round(AIC(model_clogit_statecap), 2),
    "No", "Yes"
  )
)

#--- Modelsummary ---
modelsummary(
  models,
  coef_map = coef_map,
  fmt = 3,                                                   # 3 decimal places
  stars = TRUE,                                                 # yes sig stars
  statistic = "std.error",                                 # SEs in parentheses
  gof_omit = ".*",                                # add own APA style gof stats
  add_rows = apa_rows,
  output = "kableExtra",
  title = "Comparison of Civil War Onset Models (Robustness Check)",
  header = c(" " = 1, "Model Specifications" = 3)
) %>%
  kable_styling(
    full_width = FALSE,                                        # Not full width
    latex_options = c("hold_position"), # removing striped to be like stargazer
    position = "center"
  ) %>%
  footnote(
    general = "Standard errors in parentheses.",           
    threeparttable = TRUE,                                 
    footnote_as_chunk = TRUE                            # Single block footnote
  )



# OUTLIERS ---------------------------------------------------------------------


boxplot(pro_rrf_clean$log_WB_pop_lag1,                  # boxplotting variables
        data = pro_rrf_clean
)

nrow(pro_rrf)                                            # double-checking rows
nrow(pro_rrf_clean)


# - Calculating Cooks distance --
# hlm_influence() from lab 7 wasn't working, so I had to define it myself. 
# Thank you random Stack overflow comment.
check_glm_influence <- function(model_object,
                                model_name,
                                data_source) {
  print(paste0("\n--- Checking Cook's Distance for ", model_name, " ---"))
  
  
  cooks_d_values <- cooks.distance(model_object) # Calcing Cook's distance vals
  

  n_obs <- length(cooks_d_values)                     # Setting a 4/N threshold
  threshold <- 4 / n_obs
  
  # --- Identifying influential obs ---
  influential_indices <- which(cooks_d_values > threshold) 
  influential_cooks_values <- cooks_d_values[influential_indices]
  
  if (length(influential_indices) > 0) {
    print(paste("Threshold for influence (4/N):", round(threshold, 5)))
    print(paste("Number of influential observations:", length(influential_indices)))
    print("Top 5 influential observations (index and Cook's D value):")
    top_5_influential <- sort(influential_cooks_values, decreasing = TRUE)[1:min(5, length(influential_cooks_values))]
    print(top_5_influential)
    
    plot(model_object,
         which = 4,
         main = paste("Cook's Distance Plot for", model_name),
         sub = paste("Threshold =", round(threshold, 4)))
  } else {
    print("No observations exceed the threshold.")
  }
}


# --- Not checking Clogit cause cook's distance doesn't work the same ---

check_glm_influence(model_glm,             # checking for base/clustered models
                    "Base Logit Model (model_glm)",
                    pro_rrf_clean)


check_glm_influence(robust_model_glm, # For the robustness check - add if time
                    "Robust Base Logit Model (robust_model_glm)",
                    pro_rrf_clean)



# Heteroskedasticity and other checks ------------------------------------------

# --- Extract Residuals and Fitted Values ---
pro_rrf_clean <- pro_rrf_clean %>%
  mutate(
    glm_pearson_resid = residuals(model_glm,
                                  type = "pearson"),
    glm_fitted_values = fitted(model_glm)
  )

# --- Histogram of residuals ---
hist_glm_resid_plot_apa <- pro_rrf_clean %>%
  ggplot(aes(x = glm_pearson_resid)) +
  geom_histogram(binwidth = 0.5,
                 fill = "gray70",
                 color = "black") +                            # APA likes grey
  labs(
    title = NULL,                                        # APA wants title here 
    x = "Pearson Residuals",
    y = "Frequency"
  ) +
  theme_classic() +                          # Again APA recommends  this style
  theme(
    text = element_text(family = "sans",
                        size = 14),                            # APA likes sans
    axis.title.x = element_text(size = 16,
                                margin = margin(t = 10)), # fixing weird margin
    axis.title.y = element_text(size = 16,
                                margin = margin(r = 10)), # Y title, adjust margin
    axis.text.x = element_text(size = 15),                 # x- axis ticks labs
    axis.text.y = element_text(size = 15),                   # y-axis tick labs
    panel.background = element_rect(fill = "white",
                                    color = NA),         # APA white background
    plot.background = element_rect(fill = "white",
                                   color = NA),          # AAP white background
    panel.grid.major = element_blank(),                          # No gridlines
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),              # Black axis/ticks
    axis.ticks = element_line(colour = "black"),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm") # plot margins
  )
print(hist_glm_resid_plot_apa)


# --- QQ plot of residuals ---
qq_glm_resid_plot_apa <- pro_rrf_clean %>%
  ggplot(aes(sample = glm_pearson_resid)) +
  stat_qq(size = 1.5,
          color = "black") +               # Make points bigger to see in paper
  stat_qq_line(color = "red",
               linetype = "dashed",
               linewidth = 0.8) +                             # Make it visible
  labs(
    title = NULL,                               # Again title in figure for APA
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  ) +
  theme_classic() +                                  # APA theme recommendation
  theme(
    text = element_text(family = "sans",
                        size = 11),                             # APA sans font
    axis.title.x = element_text(size = 11,
                                margin = margin(t = 10)),    # fix title amrgin
    axis.title.y = element_text(size = 11,
                                margin = margin(r = 10)),    # fix title margin
    axis.text.x = element_text(size = 10),                   # X-labs for ticks
    axis.text.y = element_text(size = 10),                         # same for y
    
    panel.background = element_rect(fill = "white",
                                    color = NA), 
    plot.background = element_rect(fill = "white",
                                   color = NA), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),                    # black axes 
    axis.ticks = element_line(colour = "black"),                  # black ticks
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")  # margins to center it
  )

print(qq_glm_resid_plot_apa)


# --- Fitted values chart ---
resid_vs_fitted_glm_plot_apa <- pro_rrf_clean %>%
  ggplot(aes(x = glm_fitted_values,
             y = glm_pearson_resid)) +
  geom_point(alpha = 0.5,
             color = "black",
             size = 1.5) +                              # making points visible
  geom_smooth(method = "loess",
              color = "red",
              se = FALSE,
              linewidth = 0.8) +                         # making lines visible
  geom_hline(yintercept = 0,
             linetype = "solid",
             color = "gray50",
             linewidth = 0.6) +                        # making grey line for 0
  labs(
    title = NULL, 
    x = "Fitted Values (Predicted Probability)",
    y = "Pearson Residuals"
  ) +
  theme_classic() +     # See above for style notes - very similar reasons here
  theme(
   
    text = element_text(family = "sans",
                        size = 13), 
    axis.title.x = element_text(size = 13,
                                margin = margin(t = 10)), 
    axis.title.y = element_text(size = 13,
                                margin = margin(r = 10)), 
    axis.text.x = element_text(size = 12), 
    axis.text.y = element_text(size = 12), 
    panel.background = element_rect(fill = "white",
                                    color = NA), 
    plot.background = element_rect(fill = "white",
                                   color = NA), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  )
print(resid_vs_fitted_glm_plot_apa)


# --- deleting the extra columns ---
pro_rrf_clean <- pro_rrf_clean %>%
  select(-glm_pearson_resid, -glm_fitted_values)