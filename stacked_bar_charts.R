library(readr)
library(purrr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(highcharter)


#download the data into R
bavarian_unemployment <- read_delim("data/Zahlen-Quoten-Regio-monatl-gs+ (Arbeitsmarktzahlen).csv", delim = ";")


#limiting to the latest 13 months
num_rows <- nrow(bavarian_unemployment)
bavarian_unemployment_latest <- bavarian_unemployment[(num_rows-12):num_rows, ]

#getting rig of duplicate date columns, number of rows and data on whole Germany
bavarian_unemployment_latest <- bavarian_unemployment_latest %>%
  select(-`Datum (Quoten-BY-DE-monatl-gs)`, -`Datum (Zahlen-BY-DE-monatl-gs)`, -`Number of Records`, -`Deutschland Arbeitslose`, -`Deutschland Quote`)


# making the date long to be more comfortable to work with
bavarian_unemployment_latest <- bavarian_unemployment_latest %>%
  gather( -Datum, -`Bayern gesamt Quote`, -`Bayern gesamt Arbeitslose`, key = "Region_measure", value = "not_clean_data_yet")


bavarian_unemployment_latest <- bavarian_unemployment_latest %>%
  separate(col = "Region_measure", into = c("Region", "Measure"), sep = " ")


bavarian_unemployment_latest <- bavarian_unemployment_latest %>%
  spread(key = "Measure", value = "not_clean_data_yet")

#now data is more "long", but still not clean

bavarian_unemployment_latest$Datum <- parse_date(bavarian_unemployment_latest$Datum, format = "%d.%m.%Y")

bavarian_unemployment_latest$`Bayern gesamt Quote` <- ifelse(
  bavarian_unemployment_latest$`Bayern gesamt Quote` <10, 
  bavarian_unemployment_latest$`Bayern gesamt Quote`,
  bavarian_unemployment_latest$`Bayern gesamt Quote`/10)

bavarian_unemployment_latest$`Quote` <- ifelse(
  bavarian_unemployment_latest$`Quote` <10, 
  bavarian_unemployment_latest$`Quote`,
  bavarian_unemployment_latest$`Quote`/10)

# stacked bar with changed colours and background

ggplot(bavarian_unemployment_latest) +
  geom_bar(aes(x = Datum, y = Arbeitslose, fill = Region), stat = "summary", position = "stack") +
  scale_x_date(name = NULL, date_labels = "%b %y") +
  scale_y_continuous(name = "Unemployed") +
  scale_fill_brewer(direction = -1) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))


# dodge bar + totals
bavarian_unemployment_latest %>%
  filter(Datum >= as.Date("2017-12-01")) %>%
  ggplot(aes(x = Datum, y = Arbeitslose)) +
  geom_bar(aes(fill = Region), stat = "summary", position = "dodge") +
  geom_errorbar(aes(ymax=`Bayern gesamt Arbeitslose`, ymin=`Bayern gesamt Arbeitslose`, colour = "Bavaria, total")) +
  scale_x_date(name = NULL, date_labels = "%b %y") +
  scale_y_continuous(name = "Unemployed", breaks = seq(0,250000,25000)) +
  scale_fill_brewer(direction = -1) +
  scale_colour_manual(name = "", values = "black" ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))


# dodge bar + stack bar
bavarian_unemployment_latest %>%
  filter(Datum >= as.Date("2017-12-01")) %>%
  ggplot() +
  geom_bar(aes(x = Datum, y = Arbeitslose, fill = Region), stat = "summary", position = "dodge") +
  geom_bar(aes(x = Datum, y = Arbeitslose), stat = "summary", fun.y = sum, alpha = 0, color = "black") +
  scale_x_date(name = NULL, date_labels = "%b %y") +
  scale_y_continuous(name = "Unemployed", breaks = seq(0,250000,25000)) +
  scale_fill_brewer(direction = -1) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))


#Highcharter interactive bar chart

bavarian_unemployment_latest %>%
  hchart(type = "column",
         hcaes(x = Datum,
               y = Arbeitslose,
               group = Region)) %>%
  hc_plotOptions(column = list(stacking = "stack", pointWidth = 30)) %>%
  hc_xAxis(title = NULL) %>%
  hc_yAxis(title = list(text = "Unemployed"))

#unemployment rate instead of absolute numbers

bavarian_unemployment_latest %>%
  filter(Datum >= as.Date("2017-12-01")) %>%
  ggplot(aes(x = Datum, y = Quote)) +
  geom_bar(aes(fill = Region), stat = "summary", position = "dodge") +
  geom_errorbar(aes(ymax = `Bayern gesamt Quote`, ymin = `Bayern gesamt Quote`, colour = "Bavaria, total"), fun.y = mean,size = 1.25) +
  scale_x_date(name = NULL, date_labels = "%b %y") +
  scale_y_continuous(name = "Unemployment rate, %") +
  scale_fill_brewer(direction = -1) +
  scale_colour_manual(name = "", values = "black" ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1))

#Highcharter line plot

bv <- bavarian_unemployment_latest %>%
  arrange(desc(Datum))

highchart(type = "stock") %>%
  hc_add_series(data = bv,
                type = "line",
                hcaes(x = Datum,
                      y = Quote,
                      group = Region)) %>%
  hc_add_series(data = bv,
                name = "Bavaria, total",
                type = "line",
                hcaes(x = Datum,
                      y = `Bayern gesamt Quote`),
                width = 5) %>%
  hc_legend(enabled = TRUE) %>%
  hc_yAxis(title = list(text = "Unemployment rate, %"))