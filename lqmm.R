library(lqmm)

fit.lqmm <- lqmm(fixed = LOG_THOUSANDS_SHIFTED ~ CYEAR + URBAN + T1 + BUSINESS_IND + FARM_IND + 
                   FISH_IND + GARD_IND + LVST_IND + OTHR_IND + RETIRE_IND + 
                   SUB_IND  + CYEAR * T1 + CYEAR * FARM_IND + CYEAR * 
                   RETIRE_IND + CYEAR * BUSINESS_IND + CYEAR * LVST_IND + URBAN * 
                   T1 + CYEAR * OTHR_IND,
                 random = ~ 1,	group = HHID,data = df_trimmed,
                 control = lqmmControl(method = "df", UP_max_iter = 10000))

  
qqmath(resid(fit.lqmm))
plot(fit.lqmm,resid(.,scaled=TRUE)~predict(.,level = 1),abline=0)
plot(residuals(fit.lqmm,level = 1) ~ predict(fit.lqmm,level = 1))


variables_of_interest = c("CYEAR * URBAN", 
                          "CYEAR * T1", 
                          "CYEAR * PRIMARY", 
                          "URBAN * T1", 
                          "URBAN * PRIMARY")#, 
                          #"T1 * PRIMARY")

remaining_variables_of_interest = variables_of_interest

current_variables = c("CYEAR",
                      "URBAN",
                      "T1",
                      "PRIMARY")
current_formula = as.formula(paste("log(THOUSANDS_SHIFTED) ~ ",paste(current_variables, collapse="+"),sep = ""))

while(TRUE){
  print("current baseline")
  print(current_variables)
  # get baseline
  current_formula = as.formula(paste("log(THOUSANDS_SHIFTED) ~ ",paste(current_variables, collapse="+"),sep = ""))
  
  baseline =lqmm(fixed = current_formula,random = ~ 1,	group = HHID,data = df,control = lqmmControl(method = "df", UP_max_iter = 600))
  
  # loop through remaining variables of interest and create a new model
  models = c()
  for (var in remaining_variables_of_interest){
    curr_var_loop = c(current_variables, var)
    current_formula = as.formula(paste("log(THOUSANDS_SHIFTED) ~ ",paste(curr_var_loop, collapse="+"),sep = ""))
    new_model = lqmm(fixed = current_formula,random = ~ 1,	group = HHID,data = df,control = lqmmControl(method = "df", UP_max_iter = 600))
    models = c(models, new_model)
  }
  
  # perform anova on baseline and a new model, record the resulting p-value
  p_vals = c()
  for (m in models){
    anova_result = anova(baseline, m)
    p_vals= c(p_vals, anova_result$`Pr(>Chisq)`[2])
  }
  
  # if the smallest p-value is greater than 0.05, stop because no new variable is significant
  if (min(p_vals)>0.05){
    print("no remaining significant variables of interest")
    break
  }
  print(paste("selected", remaining_variables_of_interest[which.min(p_vals)], ", p-value of", min(p_vals) ))
  
  # add most signficant variable to model
  current_variables = c(current_variables, remaining_variables_of_interest[which.min(p_vals)])
  
  # remove the most significant variable from search space
  remaining_variables_of_interest = remaining_variables_of_interest[-which.min(p_vals)]
  if (length(remaining_variables_of_interest)==0){
    break
  }
}
