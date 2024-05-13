#This script contains the code used to for an data analysis assignment

#Load required packages

library(tidyverse)
library(stargazer)
library(ggrepel)
library(scales)

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
#The bottom 10% shortfall ranged from -1.122235e+05 to -1.146151e+00

distribution_2020 <- ggplot(EU_2020 |> filter(shortfall > quantile(shortfall, 0.1)), aes(x = shortfall, y = after_stat(count/sum(count))))
Plot_dist_2020 <- distribution_2020 + geom_histogram(binwidth = .05) + labs(x = "Shortfall in 2020", y = "Percentage of Distribution") +
                    ggtitle("Distribution of Allowance Shortfall Across EU Firms in 2020") +
                    geom_vline(xintercept = quantile(EU_2020$shortfall,.5), colour = "blue", linewidth = .5) +
                    scale_y_continuous(labels = percent)

print(Plot_dist_2020)

#Question 2: Observe the countries with an excess of 20 million tons of emissions
#show the cross country relation between the country level

#compute the aggregate emissions for each country for 2013 and filter by those
#who exceed 20mil tons

country_excess <- EU_Emissions_Clean |> filter(year == "2013") |> 
                group_by(country_id) |> summarise(aggregate = sum(emission)) |>
                filter(aggregate >= 20000000) |> pull(country_id)

#This pulls all the countries that have meet the threshold of 20mil tons of agg emission

Excess_Emissions <- EU_Emissions_Clean |> filter(country_id %in% country_excess, year %in% c(2013, 2020))

#Here a dataframe containing the emissions data for countries in 2013 is created
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
sf_agg_gr <- sf_agg |> mutate(growth_rate = (agg_emission_20 - agg_emission_13)/agg_emission_13,) |>
                    select(country_id, agg_emission_13, agg_emission_20, growth_rate)


#plot shortfall against the growth rate
scatter_excess <- ggplot(sf_agg_gr, aes(agg_shortfall_13, growth_rate))

scatter_excess <- scatter_excess + geom_point(size = 2) + labs(x = "Shortfall in 2013", y = "Growth Rate") +
                 geom_text_repel(aes(label = country_id), vjust = -0.5) + 
                ggtitle("Relation Between Country Level Shortfall and Emission Growth")
              
#Question 3: OLS regression
#Use lm method, store the results and export using stargazer. 

ols <- lm(growth_rate ~ agg_shortfall_13, sf_agg_gr) 

#Output to Latex Code
regression_results <- stargazer(ols, title = "Regression Results", align = TRUE, dep.var.labels = "Growth Rate", covariate.labels="2013 Agg. Shortfall")

