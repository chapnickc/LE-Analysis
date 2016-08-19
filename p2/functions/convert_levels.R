convert_levels <- function(column, new_levels){
  new <- as.character(column)
  new[which(new == "<0.1")] <- '0.055'
  
  new <- as.numeric(new)
  result <- cut(new, new_levels)
  return(result)
}
