#### Preamble ####
# Purpose: Cleans the moment data for the dataset, the model, and also the measures
# of innovation under different policies.
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

#### Clean data ####

## This reads the raw data files into the program.

Data_moments_raw <- read_csv("inputs/data/baselineMoments.csv") 
Model_Data_Moments_raw <- read_csv("inputs/data/Model_Data_Moments.csv")
Model_Data_Inovation_rates_raw <- read_csv("inputs/data/Model Policy Data.csv")

## This cleans the data file for the moments calculated from the model and data.
## It consists of selecting around 17 moments from the data and then specifically
## isolating exit moments, R&D innovation rate moments, and Employment and GDP growth
## moments.

Model_Data_Moments <- Model_Data_Moments_raw
colnames(Model_Data_Moments) <- c("Main")
Model_Data_Moments <- Model_Data_Moments |> separate("Main",
                                                     into = c("Model", "Data",
                                                              "#", "Description",
                                                              "Weight"),
                                                     sep = '\\s+') |> 
                                            rename(Moment_ID = '#') |>
                                            as_tibble() |>
                                            mutate(Moment_ID = as.numeric(Moment_ID) |> 
                                                     round()) |>
                                            filter(Moment_ID < 18 & Moment_ID >= 1)
                                            
Model_Data_Moments <- Model_Data_Moments[, c("Model", "Data", "Moment_ID", "Description")]

## This is for the exit rate moments.

Exit_rate_Moments <- Model_Data_Moments |>
                      filter(6 <= Moment_ID & Moment_ID <= 8) |>
                      mutate(Description = c("Firm Exit (Small-Young)",
                                             "Firm Exit (Small-Old)",
                                             "Firm Exit (Large-Old)")) |>
                      select(Model, Data, Description)
            

## This is for the innovation rate moments.

Innovation_Rate_Moments <- Model_Data_Moments |>
                        filter(9 <= Moment_ID & Moment_ID <= 11) |>
                        mutate(Description = c("R&D to Sales (Small-Young)",
                                               "R&D to Sales (Small-Old)",
                                               "R&D to Sales (Large-Old)")) |>
                        select(Model, Data, Description)

## This is for the growth rate moments. 
Growth_rate_moments <- Model_Data_Moments |>
                      filter(15 <= Moment_ID & Moment_ID <= 17 | Moment_ID == 5) |>
                      mutate(Description = c("Aggregate Growth",
                                             "Employment Growth (Small-Young)",
                                             "Employment Growth (Small-Old)",
                                             "Employment Growth (Large-Old)")) |>
                      select(Model, Data, Description)



## From the model's computed values for the innovation rates of different types of
## firms, the creative destruction rate, and GDP growth rate, we look at the
## evolution of those rates under different policy counterfactuals such as
## a subsidy on R&D or tax on operations.

## This is the baseline case as is the current state of the economy. 

Baseline_Policy_Data <- Model_Data_Inovation_rates[4:5,]
Baseline_Policy_Data <- Baseline_Policy_Data |>
                        separate("Baseline:",
                                 into = as.character(1:11),
                                 sep = '\\s+') |>
                        select(c("1", "2", "3", "9", "10"))
Baseline_Policy_Data <- Baseline_Policy_Data |>
                        rename( "Innovation Rate (Entry Firm)" = "1",
                                "Innovation Rate (Low Type Firm)" = "2",
                                "Innovation Rate (High Type Firm)" = "3",
                                "Creative Destruction Rate" = "9",
                                "Growth Rate" = "10")

Baseline_Policy_Data <- Baseline_Policy_Data[2,]
Baseline_Policy_Data$"Policy Type" <- c("Baseline")
Baseline_Policy_Data <- Baseline_Policy_Data[, 
                                      c("Policy Type",
                                      names(Baseline_Policy_Data)[
                                        -which(names(Baseline_Policy_Data) == "Policy Type")])]

## This is the impact of a subsidy program totalling 1% of GDP for R&D.

Incumbent_Subsidy_Policy_Data <- Model_Data_Inovation_rates[10:11,]
Incumbent_Subsidy_Policy_Data <- Incumbent_Subsidy_Policy_Data |>
  separate("Baseline:",
           into = as.character(1:11),
           sep = '\\s+') |>
  select(c("1", "2", "3", "9", "10"))
Incumbent_Subsidy_Policy_Data <- Incumbent_Subsidy_Policy_Data |>
  rename( "Innovation Rate (Entry Firm)" = "1",
          "Innovation Rate (Low Type Firm)" = "2",
          "Innovation Rate (High Type Firm)" = "3",
          "Creative Destruction Rate" = "9",
          "Growth Rate" = "10")

Incumbent_Subsidy_Policy_Data <- Incumbent_Subsidy_Policy_Data[2,]
Incumbent_Subsidy_Policy_Data$"Policy Type" <- c("Incumbent Subsidy")
Incumbent_Subsidy_Policy_Data <- Incumbent_Subsidy_Policy_Data[, 
                                             c("Policy Type",
                                               names(Incumbent_Subsidy_Policy_Data)[
                                                 -which(names(
                                                   Incumbent_Subsidy_Policy_Data) == "Policy Type")])]

## This is the optimal tax scenario under which we implement the tax on operations
## that gives most optimal values.

Optimal_Tax_Policy_Data <- Model_Data_Inovation_rates[52:53,]
Optimal_Tax_Policy_Data <- Optimal_Tax_Policy_Data |>
  separate("Baseline:",
           into = as.character(1:11),
           sep = '\\s+') |>
  select(c("1", "2", "3", "9", "10"))
Optimal_Tax_Policy_Data <- Optimal_Tax_Policy_Data |>
  rename( "Innovation Rate (Entry Firm)" = "1",
          "Innovation Rate (Low Type Firm)" = "2",
          "Innovation Rate (High Type Firm)" = "3",
          "Creative Destruction Rate" = "9",
          "Growth Rate" = "10")

Optimal_Tax_Policy_Data <- Optimal_Tax_Policy_Data[2,]
Optimal_Tax_Policy_Data$"Policy Type" <- c("Optimal Tax")
Optimal_Tax_Policy_Data <- Optimal_Tax_Policy_Data[, 
                                             c("Policy Type",
                                               names(Optimal_Tax_Policy_Data)[
                                                 -which(names(Optimal_Tax_Policy_Data) == "Policy Type")])]

## This dataset combines the prior three scenarios.

Policy_Data <- rbind(Baseline_Policy_Data, Incumbent_Subsidy_Policy_Data, 
                       Optimal_Tax_Policy_Data)
#### Save data ####

## This stores the cleaned data files into the GitHub repository.

write_csv(Model_Data_Moments, "outputs/data/Model_Data_Moments_cleaned.csv")
write_csv(Exit_rate_Moments,
          "outputs/data/Exit_rate_Moments.csv")
write_csv(Innovation_Rate_Moments,
          "outputs/data/Innovation_Rate_Moments.csv")
write_csv(Growth_rate_moments,
          "outputs/data/Growth_rate_moments.csv")
write_csv(Baseline_Policy_Data,
          "outputs/data/Baseline_Policy_Data.csv")
write_csv(Incumbent_Subsidy_Policy_Data,
          "outputs/data/Incumbent_Subsidy_Policy_Data.csv")
write_csv(Optimal_Tax_Policy_Data,
          "outputs/data/Optimal_Tax_Policy_Data.csv")
write_csv(Policy_Data,
          "outputs/data/Policy_Data.csv")