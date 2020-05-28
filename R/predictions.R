library(ggplot2)
library(dplyr)

FILE <- '../data/predictions.asv'
data <- read.csv(FILE, sep = '@', stringsAsFactors = FALSE) %>%
     dplyr::filter(!is.na(happened)) %>%
     dplyr::mutate(probability = probability / 100,
                   date = as.Date(as.character(date), "%Y%m%d")) %>%
     dplyr::arrange(date)                                     
data %<>% dplyr::mutate(
  diff = probability - happened,
  brier_contrib = diff^2,
  predictions_to_date = 1:dplyr::n(),
  brier_to_date = cumsum(brier_contrib) / predictions_to_date)

data %>%
  ggplot(aes(x = date, y = brier_to_date)) +
  geom_point() +
  geom_line() +
  theme_bw()
