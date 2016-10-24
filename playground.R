# ===================================================================== #
# Preparing Playground                                                  #
# ===================================================================== #
setwd("~/GitHub/chvng")
source("processData.R")
source("plottingData.R")

# ===================================================================== #
# READY TO PLAY                                                         #
# ===================================================================== #
processWeeks(c(1:4))
deleteWeek(c(1:4))

# Example results comparison
loadData()
resultsComparison(1, 12)