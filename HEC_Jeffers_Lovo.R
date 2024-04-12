#This script contains the code used to answer Questions 1-3 for Data Task
#HEC Predoctroal Research Assistant

#Load required packages

library(tidyverse)
library(stargazer)
library(ggrepel)

#Set Working Directory to Task Folder & Read in the data
installation <- read_csv("installation.csv")
compliance <- read_csv("compliance.csv")

#renamed 'installation_id" in order to have clear key for merge data sets
compliance <- compliance |> rename(id = installation_id)

#merge data sets
big_data <- left_join(compliance, installation)

#Pull out relevant variables for plots and regressions
EU_Emissions <- big_data |> select(country_id, id, year, free, emission)

#Question 1: Generate a graph showing distribution of allowance shortfall in 2020
#The required data is in the "compliance" dataframe. 

#Add column for shortfall defined as (Eit-Fit)/Eit
EU_Emissions <- EU_Emissions |> mutate(shortfall = (emission - free) / emission)

#noticed that some shortfalls fall to "-Inf" due to 0 emissions
#this counts those obs.
inf <- EU_Emissions |> count(shortfall == "-Inf")

#remove "-Inf" obs. less than 5% of the data and the interpretation is that
#these firms did not produce emissions. They could sell licenses in excess
EU_Emissions_Clean <- EU_Emissions |> filter(shortfall != "-Inf")

#Extract obs. for year 2020
EU_2020 <- EU_Emissions_Clean |> select(id, year, shortfall) |>
                  filter(year == "2020")

#Plot 2020 distribution, removed the bottom 10% as these data lead to a large skew

distribution_2020 <- ggplot(EU_2020 |> filter(shortfall > quantile(shortfall, 0.1)), aes(shortfall))
distribution_2020 + geom_histogram(binwidth = .1)


#Question 2: Observe the countries with an excess of 20 million tons of emissions
#show the cross country relation between the country level

#compute the aggregate emissions for each country for 2013 and filter by those
#who exceed 20mil tons

country_excess <- EU_Emissions_Clean |> filter(year == "2013") |> 
                group_by(country_id) |> summarise(aggregate = sum(emission)) |>
                filter(aggregate >= 20000000) |> pull(country_id)

#This pulls all the countries that have meet the threshold of 20mil tons of agg emission

Excess_Emissions <- EU_Emissions_Clean |> filter(country_id %in% country_excess, year %in% c(2013, 2020))
                    #group_by(id) |> filter(all(c(2013, 2020) %in% year)) |>
                    #ungroup()

#Here we create a dataframe containing the emissions data for countries in 2013
#Also growth rate is calculated on a per country basis
sf_2013 <- Excess_Emissions |> filter(year == 2013) |> select(country_id, id, free, emission) |> 
                  group_by(country_id) |> 
                  summarise(agg_free_13 = sum(free), agg_emission_13 = sum(emission)) |>
                  mutate(agg_shortfall_13 = (agg_emission_13 - agg_free_13) / agg_emission_13)
                  

#This is a repeat but to construct the df for year 2020
sf_2020 <- Excess_Emissions |> filter(year == 2020) |> select(country_id, id, free, emission) |> 
                            group_by(country_id) |> 
                            summarise(agg_free_20 = sum(free), agg_emission_20 = sum(emission)) |>
                            mutate(agg_shortfall_20 = (agg_emission_20 - agg_free_20) / agg_emission_20)

#Merge the country aggregate shortfall dfs
sf_agg <- left_join(sf_2013, sf_2020)

#Keep relevant columns for plot and growth rate calc
sf_agg_gr <- sf_agg |> mutate(growth_rate = (agg_shortfall_20 - agg_shortfall_13)/abs(agg_shortfall_13)) |>
                    select(country_id, agg_shortfall_13, agg_shortfall_20, growth_rate)


#plot idea
scatter_excess <- ggplot(sf_agg_gr, aes(agg_shortfall_13, growth_rate))

scatter_excess + geom_point(size = 2) + labs(x = "Shortfall in 2013", y = "Growth Rate") +
                 geom_text_repel(aes(label = country_id), vjust = -0.5) + 
                 geom_hline(yintercept = 0, colour = "Red")
              
scatter_excess_tight <- ggplot(sf_agg_gr |> filter(country_id != "SE" & country_id != "RO" & country_id != "HU"), aes(agg_shortfall_13, growth_rate))

scatter_excess_tight + geom_point(size = 2) + labs(x = "Shortfall in 2013", y = "Growth Rate") +
  geom_text_repel(aes(label = country_id), vjust = -0.5) + 
  geom_hline(yintercept = 0, colour = "Red") +
  geom_smooth(method = "lm", color = "Blue")
  

#Question 3: OLS regression
#Using the subset of countries whose agg. emissions exceed 20mil tons, regress
#EmissionGrowth on Shortfall (i, 2013) 
#Consideration is that there are large outliers that can introduce a lot of bias
#to these results.

ols <- lm(growth_rate ~ agg_shortfall_13, sf_agg_gr) 

sf_agg_sans <- sf_agg_gr |> filter(country_id != "SE" & country_id != "RO" & country_id != "HU")

ols_less <- lm(growth_rate ~ agg_shortfall_13, sf_agg_sans)

