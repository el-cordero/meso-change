# conf_table

# This function evaluates one model's performance against several other models using
# various machine learning metrics for classified data. 

# the conf function is required to be loaded
source("/Users/EC13/Documents/Projects/USACE/ML Mesohabitats/Data/R/Mesohabitat_Detection/ML-Mesohabitats/_scripts/00_funcConfTable.R")

# comparisons <- name of columns for comparison models
# data <- database containing a column for area 
#         and model presence for various models
# model <- name of column for model A
# area <- name of column for area

conf_table <- function(comparisons, model, area){
  for (comparison in comparisons){
    if(exists("conf.table") == TRUE){
      conf.table <- rbind(conf.table,
                          conf(model, comparison, area))
    }
    else {
      conf.table <- conf(model, comparison, area)
    }
  }
  return(conf.table)
}
