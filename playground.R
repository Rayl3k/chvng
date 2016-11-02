# ===================================================================== #
# Preparing Playground                                                  #
# ===================================================================== #
setwd("~/GitHub/chvng")
require(dplyr)
require(googlesheets)
require(ggplot2)
library(tidyr)
source("processData.R")
source("plottingData.R")

# ===================================================================== #
# READY TO PLAY                                                         #
# ===================================================================== #
processWeeks(c(1:4))
deleteWeek(c(1:4))

# Example results comparison
loadData()
resultsComparison(1, 8)
