require(car, quietly = TRUE)
require(knitr, quietly = TRUE)


reduce_vifs <- function(model, df, mycol, cutoff = 5, trace = T){
  # This function attempts to remove high levels of collinearity
  # using the vif() funtion from the car package, the function 
  # sequentially drops the covariate with the highest VIF,
  # then recalculated VIFs and repeates the process until all 
  # Vifs are smaller than a pre-selected threshold.
  # This process follows the suggestions from 
  # Zuur et al. (2009) A protocol for data exploration to avoid commonstatistical problems
  #  http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2009.00001.x/full
  
  vifs <- car::vif(all_fit)
  vifs <- as.data.frame(vifs)
  row <- which.max(vifs$`GVIF^(1/(2*Df))`)
  max_vif <- vifs$`GVIF^(1/(2*Df))`[row]
  
  while (max_vif > cutoff){
    
    if (trace){
      # print out the row being removed
      s <- 'Removed: %s, GVIF^(1/(2*Df)) = %0.3f'
      s <- sprintf(s, rownames(vifs)[row], max_vif)
      print (s)
    }
    
    # remove that row from the vifs
    new_vifs <- vifs[-row, ]
    cols <- rownames(new_vifs)
    
    # building the forumla
    vars <- paste0(cols[1:length(cols)-1], sep = ' + ', collapse = '');
    vars <- paste0(vars, cols[length(cols)])
    model <- paste0(mycol, ' ~ ', vars)
    
    # perform the regression
    fit <- lm(as.formula(model), data = df)
    
    vifs <- car::vif(fit)
    vifs <- as.data.frame(vifs)
    row <- which.max(vifs$`GVIF^(1/(2*Df))`)
    max_vif <- vifs$`GVIF^(1/(2*Df))`[row]
  }
  
  return(vifs)
}

make_formula <- function(cols, mycol){
  
  # building the forumla
  vars <- paste0(cols[1:length(cols)-1], sep = ' + ', collapse = '');
  vars <- paste0(vars, cols[length(cols)])
  f <- paste0(mycol, ' ~ ', vars)
  
  return(as.formula(f))
}

summary_table <- function(model, title = ''){
  # This function is used to format the summary 
  # for a regression model. It prints out the 
  # residuals, the coefficients, and additional
  # summary statistics such as the p-value, R-squared
  # values, etc. It is written with the intention 
  # of producing a LaTeX pdf file.
  
  # grab the residuals
  residuals_table <- t(as.matrix(summary(summary(model)$residuals)))
  
  # print out the table
  label = paste0(title, ' Residuals')
  tab1 <- knitr::kable(residuals_table, digits = 4, caption = label)
  
  # print out the table
  label = paste0(title, ' Coefficients')
  tab2 <- knitr::kable(summary(model)$coef, caption = label, digits = 3,format.args = list(scientific = TRUE, nsmall = 3), escape = FALSE)
  
  
  # grab the extra summary statistics  
  y <- capture.output(summary(model))
  row <- which(y == '---')
  tab3 <- y[(row+3):(length(y)-1)]
  # tab3 <- as.matrix(tab3)
  tab3 <- data.frame(Continued = tab3) 
  # print out the table
  label = paste0(title, ' Summary Continued')
  
  #tab3 <- knitr::kable(tab3, col.names = 'Summary Continued', format = 'markdown')
  tab3 <- knitr::kable(tab3, caption = label)
  
  
  print(tab1) 
  print(tab2)
  print(tab3)
}



anova_table <- function(model, title = ''){
  # This function is used to format the anova summary 
  # for a regression model. It prints out the 
  # residuals, the coefficients, and additional
  # summary statistics such as the p-value, R-squared
  # values, etc. It is written with the intention 
  # of producing a LaTeX pdf file.
 

  anova_tab <- anova(model) 
  label = paste0(title, ' Analysis of Variance')
  tab <- knitr::kable(anova_tab, digits = 4, caption = label) 
  
  print(tab) 
}



anova_table_2 <- function(model1, model2, title = ''){
  # This function is used to format the anova summary 
  # for a regression model. It prints out the 
  # residuals, the coefficients, and additional
  # summary statistics such as the p-value, R-squared
  # values, etc. It is written with the intention 
  # of producing a LaTeX pdf file.
 

  anova_tab <- anova(model1, model2) 
  tab <- knitr::kable(anova_tab, digits = 4, caption = title) 
  
  print(tab) 
}








