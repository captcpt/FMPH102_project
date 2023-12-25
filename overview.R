library(tidyverse)
library(tidymodels)

# Loading data
pm_df <- read.csv("pm_dataset.csv",na.strings=c("","NA"))
who_df <- read.csv("who_aap_2021_v9_11august2022.csv")

# Plotting
ggplot(who_df) + 
  geom_boxplot(aes(x="PM02.5",y=PM2.5..ug.m3.),outlier.shape=NA) +
  geom_boxplot(aes(x="PM10",y=PM10..ug.m3.),outlier.shape=NA) +
  geom_boxplot(aes(x="PMNO2",y=NO2..ug.m3.),outlier.shape=NA) +
  ylim(0,75) +
  labs(title="Boxplots for particulate matter data collected by WHO",
       subtitle="Depth per particulate matter type",
       x="Particulate Matter Type",
       y="Measurement of PM (ug)")

# Data Summaries of particulate matter
fivenum(who_df$PM2.5..ug.m3.)
fivenum(who_df$PM10..ug.m3.)
fivenum(who_df$NO2..ug.m3.)

# Average measure of particulate matter by region
who_df |>
  filter(Measurement.Year >= 2010,WHO.Region != "") |>
  group_by(WHO.Region,Measurement.Year) |>
  summarise(avg2.5=mean(PM2.5..ug.m3.,na.rm=TRUE),avg10=mean(PM10..ug.m3.,na.rm=TRUE),avgNO=mean(NO2..ug.m3.,na.rm=TRUE)) |>
  ggplot(aes(x=Measurement.Year)) +
  geom_line(aes(y=avg2.5,color="blue")) +
  geom_line(aes(y=avg10,color="red")) +
  geom_line(aes(y=avgNO)) +
  theme(legend.position="none") +
  facet_wrap(~WHO.Region, nrow = 2) +
  labs(title="Average measure of particulate matter by type over time",
       subtitle="Depth for WHO Region",
       x="Measurement Year",
       y="Average PM2.5 (ug)")
