library(magrittr)

InnerPlot <- function(df) {
  df %>% 
    ggplot2::ggplot(ggplot2::aes(x = region, col = `Expected change in daily cases`)) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = lower, ymax = upper), size = 4, alpha = 0.7) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = mid_lower, ymax = mid_upper), size = 4, alpha = 1) +
    ggplot2::geom_hline(yintercept = 1, linetype = 2) +
    ggplot2::facet_wrap(~ metric, ncol = 1, scales = "free_y") +
    cowplot::theme_cowplot() +
    cowplot::panel_border() +
    ggplot2::scale_color_manual(   values = c(
      "Increasing" = "#e75f00",
      "Likely increasing" = "#fd9e49",
      "Likely decreasing" = "#5fa2ce",
      "Decreasing" = "#1170aa",
      "Unsure" = "#7b848f"), drop = FALSE) 
}

CasesPlot <- function(summary_results) {
  summary_results %>% 
    dplyr::filter(metric %in% "New confirmed cases by infection date") %>% 
    InnerPlot() +
    ggplot2::labs(x = "Region", y = "") +
    ggplot2::expand_limits(y = 0) +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank()) +
    ggplot2::theme(legend.position = "none")
}

PlotConfidenceIntervals <- function(data) {
  
  data.nowcast <- data %>% 
    dplyr::filter(grepl("nowcast", rt_type)) %>% 
    dplyr::select(-rt_type)
  
  p <- plotly::plot_ly() %>% 
    plotly::add_ribbons(x = data.nowcast$date, ymin = data.nowcast$lower, 
                        ymax = data.nowcast$upper, color = I("#cbe0dc"), 
                        name = "50% confidence", 
                        line = list(color = 'transparent'),
                        showlegend = FALSE) %>% 
    plotly::add_ribbons(x = data.nowcast$date, ymin = data.nowcast$mid_lower, 
                        ymax = data.nowcast$mid_upper, color = I("#86DBCD"), 
                        name = "90% confidence",
                        line = list(color = 'transparent'),
                        showlegend = FALSE) %>%
    plotly::config(displayModeBar = FALSE) %>% 
    plotly::layout(
      legend = list(orientation = 'h'),
      yaxis = list(title = "Reproduction Rate"),
      xaxis = list(title = ""))
  
  if ("forecast" %in% data$rt_type) {
    data.forecast <- data %>% 
      dplyr::filter(grepl("forecast", rt_type)) %>% 
      dplyr::select(-rt_type)
    
    p <- p %>% 
      plotly::add_ribbons(x = data.forecast$date, ymin = data.forecast$lower, 
                          ymax = data.forecast$upper, color = I("#EFC095"), 
                          name = "50% confidence", 
                          line = list(color = 'transparent'),
                          showlegend = FALSE) %>% 
      plotly::add_ribbons(x = data.forecast$date, ymin = data.forecast$mid_lower, 
                          ymax = data.forecast$mid_upper, color = I("#EC8126"), 
                          name = "90% confidence", 
                          line = list(color = 'transparent'),
                          showlegend = FALSE)
  }
  return(p)
}

PlotR0 <- function(region) {
  region %<>% 
    stringr::str_replace("Emilia Romagna", "Emilia-Romagna") %>% 
    stringr::str_replace("Friuli V.G.", "Friuli%20Venezia%20Giulia") %>% 
    stringr::str_replace("Trentino-Alto Adige", "Trentino-Alto%20Adige") %>% 
    stringr::str_replace("Valle d'Aosta", "Valle%20d'Aosta") 
  
  if (grepl("Italy", region)) {
    address <- paste0("https://github.com/epiforecasts/covid-global/raw/master/",
    "national/Italy/latest/")
  } else {
    address <- paste0("https://github.com/epiforecasts/covid-regional/raw/master/",
                      "italy/regional/",region,"/latest/")
  }
  
  file.R0 <- paste0(address, "time_varying_params.rds")
  file.cases <- paste0(address, "plot_cases.rds")
    
  rt <- readRDS(url(file.R0))
  
  R0.data <- rt$R0 %>% 
    tibble::as_tibble(.) %>% 
    dplyr::mutate(date = date - lubridate::days(5)) %>% 
    dplyr::select(date, rt_type, R0_range) %>% 
    tidyr::unnest_wider(R0_range) 
  
  cases <- readRDS(url(file.cases))
  
  cases.estimates <- cases$data %>% 
    dplyr::filter(grepl("nowcast",type)) %>% 
    dplyr::select(date = date, rt_type = type, lower = bottom, upper = top, 
                  mid_lower = lower, mid_upper = upper, point = mean)
  
  if ("case_forecast" %in% names(rt)) {
    cases.forecast <-  rt$case_forecast %>% 
      dplyr::mutate(date = date) %>% 
      dplyr::select(date = date, rt_type, lower = bottom, upper = top, mid_lower = lower, 
                    mid_upper = upper, point = mean)
    
    cases.data <- cases.estimates %>% 
      dplyr::bind_rows(cases.forecast)
  } else {
    cases.data <- cases.estimates
  }

  actual.cases <- cases$data %>% 
    dplyr::filter(grepl("Observed by report date",type)) %>% 
    dplyr::select(date, median)
  
  p.R0 <- PlotConfidenceIntervals(R0.data) %>% 
    plotly::add_lines(x = actual.cases$date, y = 1,
                      line = list(dash = "dot", color = "grey"), name = "Reproduction 1",
                      showlegend = FALSE)
  
  p.cases <- PlotConfidenceIntervals(cases.data) %>% 
    plotly::add_trace(type='bar', x = actual.cases$date, 
                      y = actual.cases$median, 
                      marker = list(color = '#C4D6E0'), name = "Actual Cases") %>% 
    plotly::layout(showlegend = FALSE,
                   xaxis = list(
                     range=c(min(actual.cases$date),max(c(actual.cases$date,
                                                          cases.data$date)))
                   ))

  plotly::subplot(p.cases, p.R0, nrows = 2)  
  
  date.infection <- R0.data %>% 
    dplyr::filter(grepl("nowcast", rt_type)) %>% 
    {dplyr::last(.$date)}
  
return(list(p.R0 = p.R0, p.cases = p.cases, date.infection = date.infection))  
}

CasesSummary <- function() {
  
  summary.results <- paste0("https://github.com/epiforecasts/covid-regional/raw/",
                            "master/italy/regional-summary/summary_data.rds") %>%
    url %>% 
    readRDS 
  
  cases.plot <- CasesPlot(summary.results)
    
  rt.plot <- summary.results %>% 
    dplyr::filter(metric %in% "Effective reproduction no.") %>% 
    {
      InnerPlot(.) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::guides(col = ggplot2::guide_legend(nrow = 2)) +
        ggplot2::labs(x = "Region", y = "") +
        ggplot2::expand_limits(y = c(0, min(max(.$upper), 3)))
    }
  
  ##join plots together
  plot <- ggpubr::ggarrange(cases.plot, rt.plot, ncol = 1, nrow = 2)
  
  summary.table <- paste0("https://github.com/epiforecasts/covid-regional/raw/",
                            "master/italy/regional-summary/summary_table.rds") %>%
    url %>% 
    readRDS 
  
  return(list(plot = plot, summary.table = summary.table))
}

