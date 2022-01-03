list.of.packages <- c("kernlab", "xtable", "caret", "class")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
