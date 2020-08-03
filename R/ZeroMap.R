library(ggplot2)
library(magrittr)
source(here::here("R/Data_Wrangling.R"))

GetStatistics <- function(Data) {
  currently.zero <- Data %>% 
    dplyr::slice_tail(n = 1) %>% 
    dplyr::pull("Total") %>% 
    {. == 0}
  
  analysis <- Data %>% 
    {rle(.$Total == 0)} 
  
  if(analysis$values[1] == TRUE) {
    analysis <- analysis$lengths[analysis$values][-1]
  } else {
    analysis <- analysis$lengths[analysis$values]
  }
  
  if(length(analysis) == 0) {
    result <- tibble::tibble(Current = 0, Best = 0)
  } else {
    if(currently.zero) {
      result <- tibble::tibble(Current = dplyr::last(analysis), 
                               Best = max(analysis))     
    } else {
      result <- tibble::tibble(Current = 0, Best = max(analysis))
    }
  }
  
  return(result)
}

Data <- GetRawData() %>% 
  purrr::pluck("covid.province") %>% 
  dplyr::select(Date, Province, Total = Increment_Total) %>% 
  dplyr::filter(!stringr::str_detect(Province, "/")) %>% 
  dplyr::group_by(Province) %>% 
  dplyr::group_modify(~GetStatistics(.x))

ggplot(Data) +
  geom_segment(aes(x = forcats::fct_reorder(Province, Current, max), 
                   xend = forcats::fct_reorder(Province, Current, max), 
                   y = Current, yend = Best), 
               color = "grey") +
  geom_point(aes(x = forcats::fct_reorder(Province, Current, max), y = Best), 
             color = rgb(0.2,0.7,0.1,0.8), size = 3) +
  geom_point(aes(x = forcats::fct_reorder(Province, Current, max), y = Current), 
             color = rgb(0.7,0.2,0.1,0.8), size = 2) +
  coord_flip() +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.border = element_blank(),
  ) +
  xlab("") +
  ylab("Value of Y")
