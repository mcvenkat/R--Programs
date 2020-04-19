library(shiny)
library(shinyjs)
install.packages("shinydashboard")
library(shinydashboard)
library(ggplot2)
library(dplyr)
install.packages("highcharter")
library(highcharter)

# Ratios go from 1 to max_ratio
max_ratio=3.5

# Create a sequence with all possible ratios
ratios=seq(from=1, to=max_ratio, by=0.05)

# Filename to save results
logfilename="out/prp_output.txt"