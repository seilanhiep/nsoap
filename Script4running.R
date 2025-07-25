source("data-cleaning.R")
source("main-script.R")
source("Write-excel.R")
source("Keyfindings-ggplot.R")
source("CPA2.R")

# Render Quarto document
library(quarto)
quarto_render("FinalSAOP-20250710.qmd")
quarto_render("FinalSAOP-report20250629.qmd")
quarto_render("FinalSAOP-20250629.qmd")
quarto_render("Acknowledge staff.qmd")
quarto_render("mapping.qmd")
quarto_render("Adjusted workforce.qmd")
quarto_render("CPA2.qmd")
quarto_render("Final-report4Vital.qmd")

