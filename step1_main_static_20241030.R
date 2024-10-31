rm(list=ls())
library(dplyr)
library(fixest)
library(ggplot2)
library(panelView)
library(ggthemes)
library(data.table)
library(Cairo)
library(modelsummary)
library(stargazer)

###### Load data
data = fread('C:/Users/weilu/Dropbox/Nielsen_US_cleaned_hgl24/analysis_all_1722_20241030.csv')
data = data[(data$purchase_week>1) & (data$purchase_week<52),]
data$sum_pop <- sum(data$Projection_Factor)
data$weight = data$Projection_Factor/data$sum_pop
data$pandemic = 0
data$pandemic[data$purchase_year==2020 & data$purchase_week>=11] = 1
data$pandemic[data$purchase_year>2020] = 0
data$locked_down[data$purchase_year>2020] = 0
data$reopening[data$purchase_year>2020] = 0
data$ind2021 <- as.numeric(data$purchase_year>2020)
data$convpct <- data$conv_purchase/data$total_price_deflated
data$otherpct <- data$other_purchase/data$total_price_deflated
data$NOVA1pct[data$total_price_deflated==0] = NA
data$NOVA2pct[data$total_price_deflated==0] = NA
data$NOVA3pct[data$total_price_deflated==0] = NA
data$NOVA4pct[data$total_price_deflated==0] = NA
gc()

# List of variables for different model specifications
dep_vars1 <- list("n_trips", "total_price_deflated", "wHealthIndex_per100g", 
                  "wFSANZ", "expNOVA1", "NOVA1pct", "expNOVA2","NOVA2pct",
                  "expNOVA3","NOVA3pct", "expNOVA4", "NOVA4pct")
dep_vars3 <- list("conv_purchase", "convpct", "other_purchase", "otherpct")
model_labels <- c("Weekly Shopping Frequency", "Weekly Food Spending",
                  "HEI", "FSANZ Score", "NOVA 1 Spending","NOVA 1 Spending %",
                   "NOVA 2 Spending","NOVA 2 Spending %",
                   "NOVA 3 Spending","NOVA 3 Spending %",
                   "NOVA 4 Spending","NOVA 4 Spending %")
model_labels3 <- c("Convenience Store Spending", "Convenience Store Spending %",
                   "Gift Card/SNAP/WIC Spending", "Gift Card/SNAP/WIC Spending %")
dep_vars4 <- list("wconv","conv_purchase1", "conv_purchase2","conv_purchase3")
model_labels4 <- c("Average convenience","Convenience 1", "Convenience 2","Convenience 3")

# Output paths
output_dir <- 'C:/Users/weilu/Dropbox/research projects/covid_purchase/P3_Results/main/'

fit_models <- function(dep_vars, treat_var) {
  models <- list()
  for (var in dep_vars) {
    formula <- as.formula(paste0(var, " ~ ", treat_var, "|household_code+purchase_week+Fips_County_Cd"))
    models[[var]] <- feols(formula, data=data, cluster=c("household_code", "Fips_County_Cd"), weights=data$weight)
  }
  return(models)
}

# # Static Treatment Effect - full controls
full_models <- fit_models(dep_vars1, "pandemic + locked_down + reopened + ind2021 + rel_covid")
full_models_3 <- fit_models(dep_vars3, "pandemic + locked_down + reopened + ind2021 + rel_covid")
names(full_models) <- model_labels
names(full_models_3) <- model_labels3
full_models_4 <- fit_models(dep_vars4, "pandemic + locked_down + reopened + ind2021 + rel_covid")
names(full_models_4) <- model_labels4

# Save output
modelsummary(full_models, statistic='std.error', gof_omit='DF|Deviance|AIC|BIC|RMSE|Adj|Within|Std',
             stars=c('*'=.1, '**'=.05, '***'=.01), coef_rename=c("pandemic"="Pandemic", "locked_down"="Lockdown", "reopened"="Reopen", "ind2021"="Post-Pandemic", "rel_covid"="Rel. COVID"),
             fmt=fmt_decimal(digits=3),
             output=paste0(output_dir, 'table1_20241030.docx'))

modelsummary(full_models_3, statistic='std.error', gof_omit='DF|Deviance|AIC|BIC|RMSE|Adj|Within|Std',
             stars=c('*'=.1, '**'=.05, '***'=.01), coef_rename=c("pandemic"="Pandemic", "locked_down"="Lockdown", "reopened"="Reopen", "ind2021"="Post-Pandemic", "rel_covid"="Rel. COVID"),
             fmt=fmt_decimal(digits=3),
             output=paste0(output_dir, 'table_aux_channels_20241030.docx'))

modelsummary(full_models_4, statistic='std.error', gof_omit='DF|Deviance|AIC|BIC|RMSE|Adj|Within|Std',
             stars=c('*'=.1, '**'=.05, '***'=.01), coef_rename=c("pandemic"="Pandemic", "locked_down"="Lockdown", "reopened"="Reopen", "ind2021"="Post-Pandemic", "rel_covid"="Rel. COVID"),
             fmt=fmt_decimal(digits=3), 
             output=paste0(output_dir, 'table_aux_convenience_20241030.docx'))
