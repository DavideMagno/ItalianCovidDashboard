source(here::here("R/ItalianMap.R"))
source(here::here("R/Analysis.R"))
source(here::here("R/MultipleGraphs.R"))
source(here::here("R/Data_Wrangling.R"))
source(here::here("R/Plot_R0.R"))

Data <- GetRawData()
last.date <- dplyr::last(Data$covid.regions$Date)