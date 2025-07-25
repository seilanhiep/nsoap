# nsoap
Analyses of Surgery Obstetrics and Anesthesia data to inform the development of national SOA plan by identifying critical gaps that need to be addressed, and by guiding priorities and interventions

# To run the R script you just run Script4running.R which constains sources as following:

source("data-cleaning.R")
source("main-script.R")
source("Write-excel.R")
source("Keyfindings-ggplot.R")
source("CPA2.R")

library(quarto)
quarto_render("FinalSAOP-20250710.qmd")
quarto_render("FinalSAOP-report20250629.qmd")
quarto_render("FinalSAOP-20250629.qmd")
quarto_render("Acknowledge staff.qmd")
quarto_render("mapping.qmd")
quarto_render("Adjusted workforce.qmd")
quarto_render("CPA2.qmd")
quarto_render("Final-report4Vital.qmd")

After running the script, the raw dataset in the data folder will be cleaned and saved in the clean folder. Subsequently, the script will generate graphics using ggplot, and export the results into Excel files located in the ggplot and Excel_datatable folders, respectively.
