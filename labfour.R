library(sparklyr)
library(dplyr)
library(tidyr)

download_covid_data <- function() {
  covid_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  
  covid_data <- read.csv(covid_url, stringsAsFactors = FALSE, check.names = FALSE)
  
  return(covid_data)
}

get_column_name <- function(data, possible_names) {
  found_col <- names(data)[names(data) %in% possible_names]
  if(length(found_col) == 0) {
    stop("No column found matching: ", paste(possible_names, collapse = ", "))
  }
  return(found_col[1])
}

analyze_covid_data <- function(spark_version = "3.5.5") {

  sc <- spark_connect(master = "local", version = spark_version)

  covid_raw <- download_covid_data()

  country_col <- get_column_name(covid_raw, 
                                 c("Country.Region", "Country/Region", "Country", "Countries"))
  province_col <- get_column_name(covid_raw, 
                                  c("Province.State", "Province/State", "Province", "State"))
  

  covid_long <- covid_raw %>%
    select(!!sym(country_col),
           !!sym(province_col),
           matches("^[0-9]+/[0-9]+/[0-9]+$")) %>%
    pivot_longer(
      cols = matches("^[0-9]+/[0-9]+/[0-9]+$"),
      names_to = "date",
      values_to = "confirmed_cases"
    ) %>%
    mutate(
      date = as.Date(date, format = "%m/%d/%y"),
      country = coalesce(!!sym(country_col), !!sym(province_col)),
      confirmed_cases = as.numeric(confirmed_cases)
    ) %>%
    group_by(country, date) %>%
    summarise(
      confirmed_cases = sum(confirmed_cases, na.rm = TRUE),
      .groups = "drop"
    )

  covid_spark <- copy_to(sc, covid_long, "covid_data", overwrite = TRUE)

  top_countries <- covid_spark %>%
    group_by(country) %>%
    summarise(
      total_cases = max(confirmed_cases),
      latest_date = max(date)
    ) %>%
    arrange(desc(total_cases)) %>%
    head(10) %>%
    collect()

  message("\nTop-10 countries by COVID-19 cases:")
  print(top_countries)

  top_5_country_names <- top_countries$country[1:5]

  top_5_countries_dynamics <- covid_spark %>%
    filter(country %in% local(top_5_country_names)) %>%
    group_by(country, date) %>%
    summarise(
      daily_cases = max(confirmed_cases),
      .groups = "drop"
    ) %>%
    collect() %>%
    group_by(country) %>%
    arrange(date) %>%
    mutate(
      cases_increase = pmax(daily_cases - lag(daily_cases, default = daily_cases[1]), 0)
    )


  if (requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)
    p <- top_5_countries_dynamics %>%
      ggplot(aes(x = date, y = cases_increase, color = country)) +
      geom_line() +
      labs(
        title = "COVID-19 Case Increase in Top 5 Countries",
        x = "Date",
        y = "Number of New Cases"
      ) +
      scale_y_continuous(
        labels = function(x) format(x, big.mark = " ", scientific = FALSE)
      ) +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10))

    ggsave("covid_cases_dynamics.png", p, width = 10, height = 6)
    message("Plot saved as 'covid_cases_dynamics.png'")
  }

  spark_disconnect(sc)

  return(list(
    top_countries = top_countries,
    top_5_dynamics = top_5_countries_dynamics
  ))
}


result <- analyze_covid_data()