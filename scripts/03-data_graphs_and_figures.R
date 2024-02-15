#### Preamble ####
# Purpose: Creates the graphs and tables used across in the paper.
# Author: Raghav Bhatia
# Date: 15th February 2024
# Contact: raghav.bhatia@mail.utoronto.ca 

#### Workspace setup ####
library(tidyverse)
library(knitr)
library(janitor)
library(lubridate)
library(dplyr)
library(tidyr)

#### Read the CSV files of the cleaned data ####

Model_Data_Moments <- read_csv("outputs/data/Model_Data_Moments_cleaned.csv")
Exit_rate_Moments <- read_csv("outputs/data/Exit_rate_Moments.csv")
Innovation_Rate_Moments <- read_csv("outputs/data/Innovation_Rate_Moments.csv")
Growth_rate_moments <- read_csv("outputs/data/Growth_rate_moments.csv")
Baseline_Policy_Data <- read_csv("outputs/data/Baseline_Policy_Data.csv")
Incumbent_Subsidy_Policy_Data <- read_csv("outputs/data/Incumbent_Subsidy_Policy_Data.csv")
Optimal_Tax_Policy_Data <- read_csv("outputs/data/Optimal_Tax_Policy_Data.csv")
Policy_Data <- read_csv("outputs/data/Policy_Data.csv")

#### Build the tables for the cleaned data ####

## Table for the data section Moment Estimates data

Cleaned_Model_Data_Moments <- Kable(Model_Data_Moments)
head(Cleaned_Model_Data_Moments)

## Tables for results section 'Comparing the moments estimates from the datasets to the
## models'

Cleaned_Exit_Rate_Moments <- kable(Exit_rate_Moments)
Cleaned_Exit_Rate_Moments

Cleaned_Innovation_Rate_Moments <- kable(Innovate_Rate_Moments)
Cleaned_Innovation_Rate_Moments

Cleaned_Growth_Rate_Moments <- kable(Growth_rate_moments)
Cleaned_Growth_Rate_Moments

## Graphs for each moment table 

Exit_rate_graph <- Exit_rate_Moments |>
  ggplot(mapping = aes(x = Description)) + 
  geom_bar(aes(y = Model), stat = "identity", fill = "red", position = "dodge2", alpha = 0.25) +
  geom_bar(aes(y = Data), stat = "identity", fill = "blue", position = "dodge2", alpha = 0.25) +
  labs(x = "Economic Metric", y = "Moment Value") +
  ggtitle("Exit Rate Moment across firm types") +
  theme(legend.position = "bottom")

Exit_rate_graph

Innovation_rate_graph <- Innovate_Rate_Moments |>
  ggplot(mapping = aes(x = Description)) + 
  geom_bar(aes(y = Model), stat = "identity", fill = "red", position = "dodge2", alpha = 0.25) +
  geom_bar(aes(y = Data), stat = "identity", fill = "blue", position = "dodge2", alpha = 0.25) +
  labs(x = "Economic Metric", y = "Moment Value") +
  ggtitle("Innovation Rate Moment across firm types") +
  theme(legend.position = "bottom")
Innovation_rate_graph

Growth_rate_graph <- Growth_rate_moments|>
  ggplot(mapping = aes(x = Description)) + 
  geom_bar(aes(y = Model), stat = "identity", fill = "red", position = "dodge2", alpha = 0.25) +
  geom_bar(aes(y = Data), stat = "identity", fill = "blue", position = "dodge2", alpha = 0.25) +
  labs(x = "Economic Metric", y = "Moment Value") +
  ggtitle("Growth Rate Moment across firm types") +
  theme(legend.position = "bottom") +
  guides(x = guide_axis(angle = 10))

Growth_rate_graph

## The results Policy analysis section's table

Cleaned_Policy_data <- kable(Policy_Data)
Cleaned_Policy_data

## Graph of the metrics

Policy_data_pivoted <- Policy_Data |>
  pivot_longer(-"Policy Type",
               names_to = "Metric",
               values_to = "Values"
               )

Policy_data_graph <- Policy_data_pivoted |>
  ggplot(mappings = aes(x = Metric, y = Values, fill = 'Policy Type')) +
  geom_bar(stat = 'identity', position = "dodge")

Policy_data_graph
