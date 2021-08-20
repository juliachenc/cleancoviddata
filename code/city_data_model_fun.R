city_data <- function(cityname){
  case_data <- as.data.frame(data.table::fread("https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/latimes-place-totals.csv", stringsAsFactors = TRUE) )
  
  case_data <- case_data %>% dplyr::filter(county=="Los Angeles")
  case_data$date <- as.Date(case_data$date)
  case_data$name <- as.character(case_data$name)
  data <- case_data
  
  summary.LA <- data %>%
    group_by(date) %>% 
    summarise(confirmed_cases = sum(confirmed_cases)) %>%
    ungroup()
  
  summary.LA = summary.LA %>%
    arrange(date) %>%  # first sort by day
    mutate(Diff_day = date - lag(date),  # Difference in time (just in case there are gaps)
           new_cases = confirmed_cases - lag(confirmed_cases)) # Difference in case between days 
  
  summary.city <- data %>%
    group_by(date, name) %>% 
    summarise(confirmed_cases = sum(confirmed_cases)) %>%
    ungroup()
  
  summary.city = summary.city %>%
    group_by(name) %>%
    arrange(date) %>%  # first sort by day
    mutate(Diff_day = date - lag(date),  # Difference in time (just in case there are gaps)
           new_cases = confirmed_cases - lag(confirmed_cases)) %>% # Difference in case between days 
    arrange(name)
  
  output = subset(summary.city, name %in% cityname)
  return(output)
}  


  
