convert_levels <- function(column, new_levels){
  new <- as.character(column)
  new[which(new == "<0.1")] <- '0.055'
  
  new <- as.numeric(new)
  result <- cut(new, new_levels)
  return(result)
}
# 
#  # ==================================
# # for maternal deaths 2013
# # ==================================
# test <- data$`Maternal_Deaths_2013`
# summary(test)
# 
# test <- as.character(test)
# test[which(test == "<0.1")] <- '0.055'
# 
# test <- as.numeric(test)
# results <- cut(test, c(0, 0.09,20,40,90,400,1100))
# 
# summary(results)
# plot(data$LifeExp_Male ~ results)
# # ==================================
# # for males
# # ==================================
# test <- data$`Percent_HIV-AIDS_Male`
# 
# levels(test)
# summary(test)
# 
# test <- as.character(test)
# test[which(test == '<0.1')] <- "0.055"
# 
# f_test = as.numeric(test)
# f_test
# 
# result <- cut(f_test, c(0,0.09, 0.15, 0.25, 0.5, 1, 10))
# summary(result)
# 
# 
# # plot(data$LifeExp_Male ~ result)
# # summary(lm(data$LifeExp_Male ~ result))
# 
# # ==================================
# # for females
# # ==================================
# test <- data$`Percent_HIV-AIDS_Female`
# summary(test)
# test <- as.character(test)
# test[which(test == "<0.1")] <- '0.055'
# test <- as.numeric(test)
# test
# 
# summary(factor(test))
# sum(is.na(test))
# 
# results <- cut(test, c(0, 0.09, 0.25, 1, 50))
# summary(results)
