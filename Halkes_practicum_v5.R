#Data Practicum, DSSA 5302
#Derek Halkes
#last modified date 8/3/2021
#version 5

#set the working directory
setwd("~/Desktop/Stockton/DSSA_5302")

##libraries that will be needed
library(tidyverse)
#used for heatmaps
library(usmap)
require(maps)
library(scales)
library(factoextra)


#read in the data from CSV files
factors1 <- read_csv("Contributing_to_COVID-19_Deaths.csv", col_names = TRUE)
deaths1 <- read_csv("Provisional_COVID-19_Deaths_by_Sex_and_Age.csv", 
                    col_names = TRUE)
agebystate1 <- read_csv("age_by_state_data_CSV.csv", col_names = TRUE)
statepopulation1 <- read_csv("state_population.csv", col_names = TRUE)
vaccines1 <- read_csv("covid19_vaccinations_CSV.csv", col_names = TRUE)
smoking1 <- read_csv("Cigarette_Use_Among_Adults_2018.csv", col_names = TRUE)
averageage1 <- read_csv("average_age_by_state_CSV.csv", col_names = TRUE)

################################################################################
##total deaths and factors by state

#limiting fields and results for aggregates by condition type in Factors data
factors4 <- filter(factors1, Group == "By Total", Age_Group == "All Ages",
                   State != "United States", Condition_Group != "COVID-19")
factors4 <- factors4[c(7,11,12)]

#aggregating data for all contributing factors by state
factors5 <- aggregate(x = factors4[,c("COVID19_Deaths")],
                      by = list(factors4$State),
                      FUN = sum, na.rm = TRUE)

#renaming the state field after aggregation/grouping
factors5 <- rename(factors5,State = Group.1)

#limiting fields and results for aggregated total covid deaths data
deaths4 <- filter(deaths1, Sex == "All Sexes", Group == "By Total",
                  State != "United States")
deaths4 <- deaths4[c(7,10)]

#aggregating data for total covid deaths by state
deaths5 <- aggregate(x = deaths4[,c("COVID19_Deaths")],
                     by = list(deaths4$State),
                     FUN = sum, na.rm = TRUE)

#renaming the state field after aggregation/grouping
deaths5 <- rename(deaths5,State = Group.1)
deaths5.2 <- deaths5

################################################################################
###heatmaps

##heatmap of death count by state, all ages
#changing case to lower for State column, used for "join" below so case matches
factors5$State = tolower(factors5$State)

#retrieve the states map data and merge with covid factors data
states_map <- map_data("state")
factors5map <- left_join(states_map, factors5, by = c("region" = "State"))

#changing case to lower for State column, used for "join" below so case matches
deaths5$State = tolower(deaths5$State)

#retrieve the states map data and merge with covid factors data
states_map <- map_data("state")
deaths5map <- left_join(states_map, deaths5, by = c("region" = "State"))

#create the map
ggplot(deaths5map, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = COVID19_Deaths), color = "white")+
  scale_fill_viridis_c(option = "C", labels=comma) +
  labs(title="Total covid deaths by state", subtitle = "All ages",
       x="longitude", y="latitude", fill = "Deaths")
ggsave("death_count_all_ages.jpg")
dev.off()


##heatmap for percent of total deaths by state
#limiting fields for aggregated total covid deaths data
deaths6 <- filter(deaths1, Sex == "All Sexes", Group == "By Total",
                  State != "United States")
deaths6 <- deaths6[c(7,10)]

#aggregating data for total covid deaths by state
deaths6 <- aggregate(x = deaths6[,c("COVID19_Deaths")],
                     by = list(deaths4$State),
                     FUN = sum, na.rm = TRUE)

#renaming the state field after aggregation/grouping
deaths6 <- rename(deaths6,State = Group.1)

#adding column to calculate percent of deaths by state (596740 is total deaths in US)
deaths6$Percentoftotal <- with(deaths6, COVID19_Deaths / 596740, na.rm = TRUE)


##heatmap for percent of total deaths by state
#changing case to lower for State column, used for "join" below so case matches
deaths6$State = tolower(deaths6$State)

#retrieve the states map data and merge with covid factors data
states_map <- map_data("state")
deaths6map <- left_join(states_map, deaths6, by = c("region" = "State"))
#deaths6map$Percentoftotal <- as.numeric(deaths6map$Percentoftotal)

#map for percentage of covid deaths by state
ggplot(deaths6map, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Percentoftotal), color = "white")+
  scale_fill_viridis_c(option = "C", labels=percent) +
  labs(title="Percent of total covid deaths by state", subtitle = "All ages",
       x="longitude", y="latitude", fill = "Percentage of total")
ggsave("percent_of_deaths.jpg")
dev.off()


##heatmap of population over 65 years old by state
#changing case to lower for State column, used for "join" below so case matches
agebystate1$State = tolower(agebystate1$State)

#retrieve the states map data and merge with covid age by state data
states_map <- map_data("state")
agebystatemap <- left_join(states_map, agebystate1, by = c("region" = "State"))
#deaths6map$Percentoftotal <- as.numeric(deaths6map$Percentoftotal)

#map for percentage of population over 65yo
ggplot(agebystatemap, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Population_Ages_65_or_older_percent), color = "white")+
  scale_fill_viridis_c(option = "C", labels=percent) +
  labs(title="Residents 65 years old or older", subtitle = "Percentage of state population",
       x="longitude", y="latitude", fill = "Percentage of total")
ggsave("percent_of_population_65yo.jpg")
dev.off()


##population by state heatmap
#limiting fields for state pop data
statepopulation2 <- filter(statepopulation1, State != "United States", 
                           State != "Northeast", State != "Midwest",
                           State != "South", State != "West")
statepopulation2 <- statepopulation2[c(1,13)]

#adding column to calculate percent of deaths by state (328,239,523 is total population as of 7/1/2019)
statepopulation2$Percentoftotal <- with(statepopulation2, Year_2019 / 328239523,
                                        na.rm = TRUE)

#changing case to lower for State column, used for "join" below so case matches
statepopulation2$State = tolower(statepopulation2$State)

#retrieve the states map data and merge with covid age by state data
states_map <- map_data("state")
statepopulationmap <- left_join(states_map, statepopulation2,
                                by = c("region" = "State"))

#map for percentage of population by state
ggplot(statepopulationmap, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Percentoftotal), color = "white")+
  scale_fill_viridis_c(option = "C", labels=percent) +
  labs(title="Percent of total population by state", subtitle = "All ages",
       x="longitude", y="latitude", fill = "Percentage of total")
ggsave("percent_of_total_population.jpg")
dev.off()


##heatmap of average age by state
#changing case to lower for State column, used for "join" below so case matches
averageage1$State = tolower(averageage1$State)

#retrieve the states map data and merge with average age data
states_map <- map_data("state")
averageage1map <- left_join(states_map, averageage1, by = c("region" = "State"))

#map for average age by state
ggplot(averageage1map, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Population), color = "white")+
  scale_fill_viridis_c(option = "C", labels=number) +
  labs(title="Average age by state",,
       x="longitude", y="latitude", fill = "Age")
ggsave("average_age.jpg")
dev.off()


##heatmap of percent of contributing factors compared to total US deaths, by state

#limiting fields for aggregates by condition type in Factors data
factors7 <- filter(factors1, Group == "By Total", Age_Group == "All Ages",
                   State != "United States", Condition_Group != "COVID-19")
factors7 <- factors7[c(7,11,12)]

#aggregating data for all contributing factors by state
factors7 <- aggregate(x = factors7[,c("COVID19_Deaths")],
                      by = list(factors4$State),
                      FUN = sum, na.rm = TRUE)

#renaming the state field after aggregation/grouping
factors7 <- rename(factors7,State = Group.1)

#changing case to lower for State column, used for "join" below so case matches
factors7$State = tolower(factors7$State)

#sum of all contributing factors
sum(factors7$COVID19_Deaths)

#adding column to calculate percent of deaths by state (1660556 is total deaths
#with contributing factors in US)
factors7$Percentoftotal <- with(factors7, COVID19_Deaths / 1660556, na.rm = TRUE)

#retrieve the states map data and merge with covid factors data
states_map <- map_data("state")
factors7map <- left_join(states_map, factors7, by = c("region" = "State"))

#map for percentage of covid deaths by state
ggplot(factors7map, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Percentoftotal), color = "white")+
  scale_fill_viridis_c(option = "C", labels=percent) +
  labs(title="Percent of total deaths with contributing factors", subtitle = "All ages",
       x="longitude", y="latitude", fill = "Percentage of total")
ggsave("percent_with_factors.jpg")
dev.off()

################################################################################
###scatter and bar plots

##scatterplot, deaths by state
#limiting fields for aggregates by condition type in Factors data
factors8 <- filter(factors1, Group == "By Total", Age_Group == "All Ages",
                   State == "United States", Condition_Group != "COVID-19")
factors8 <- factors8[c(7,8,12)]

#aggregating data for all contributing factors by state
factors8 <- aggregate(x = factors8[,c("COVID19_Deaths")],
                      by = list(factors8$State, factors8$Condition_Group),
                      FUN = sum, na.rm = TRUE)

#renaming the state field after aggregation/grouping
factors8 <- rename(factors8,State = Group.1, Condition_Group = Group.2)

#NOT USED
#scatterplot of deaths by state
# ggplot(deaths5.2, aes(x=State, y=COVID19_Deaths)) +
#   geom_point(color="blue") +
#   labs(title = "Total Covid-19 deaths by state", subtitle = "All ages", 
#        x = "State", y = "Deaths") +
#   theme(axis.text.x = element_text(angle = 90))
# ggsave("deaths_scatterplot.jpg")
# dev.off()


#barplot of condition groups for comorbidities
ggplot(data=factors8, aes(x=Condition_Group, y=COVID19_Deaths)) +
  geom_bar(stat="identity", color = "Purple", fill = "Blue", position=position_dodge()) +
  labs(title = "Total of comorbidities for Covid-19 deaths", subtitle = "All states, all ages",
       x="Condition groups",y="Total number of factors") +
  theme(axis.text.x = element_text(angle = 90))
ggsave("factors_barplot.jpg")
dev.off()


##dataset for stacked barplot of contributing factor deaths by state
#filtering out unwanted data
factors9 <- filter(factors1, Group == "By Total", Age_Group == "All Ages",
                   State != "United States", Condition_Group != "COVID-19")
#limiting fields
factors9 <- factors9[c(7,8,12)]

#aggregating data for all contributing factors by state
factors9 <- aggregate(x = factors9[,c("COVID19_Deaths")],
                      by = list(factors9$State, factors9$Condition_Group),
                      FUN = sum, na.rm = TRUE)

#renaming the state field after aggregation/grouping
factors9 <- rename(factors9,State = Group.1, Condition_Group = Group.2)

#NOT USED
#stacked barplot, by state, withing conditional factors
# ggplot(data=factors9, aes(x=State, y=COVID19_Deaths, fill=Condition_Group)) +
#   geom_bar(stat="identity") +
#   labs(title = "Contributing factors for Covid-19 deaths by state", subtitle = "All states, all ages",
#        x="State",y="Total factors", fill="Contributing factors") +
#   theme(axis.text.x = element_text(angle = 90))
# ggsave("factors_by_state.jpg")
# dev.off()


##stacked barplot of condition groups with a fill of subgroups
#filtering out unwanted data
factors10 <- filter(factors1, Group == "By Total", Age_Group == "All Ages",
                    State != "United States", Condition_Group != "COVID-19")
#limiting fields
factors10 <- factors10[c(8,9,12)]

#aggregating data for all conditions within condition groups
factors10 <- aggregate(x = factors10[,c("COVID19_Deaths")],
                       by = list(factors10$Condition_Group, factors10$Condition),
                       FUN = sum, na.rm = TRUE)

#renaming the state field after aggregation/grouping
factors10 <- rename(factors10,Condition_Group = Group.1,Condition = Group.2)

#stacked barplot of conditions within condition groups, no legend
ggplot(data=factors10, aes(x=Condition_Group, y=COVID19_Deaths, fill=Condition)) +
  geom_bar(stat="identity") +
  labs(title = "Individial conditions leading to Covid-19 fatalties", subtitle = "All states, all ages",
       x="Conditions",y="Total deaths", fill="Individual Conditions") +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none")
ggsave("individual_conditions_no_legend.jpg")
dev.off()

#same stacked barplot of conditions within condition groups, with legend
ggplot(data=factors10, aes(x=Condition_Group, y=COVID19_Deaths, fill=Condition)) +
  geom_bar(stat="identity") +
  labs(title = "Individial conditions leading to Covid-19 fatalties",
       x="Conditions",y="Total deaths", fill="Individual Conditions")
ggsave("individual_conditions_with_legend.jpg")
dev.off()


################################################################################
##vaccine rates by state

#limiting fields in Vaccines by state data
vaccines2 <- vaccines1[c(1,9,13,14)]

#renaming the remaining columns
vaccines2 <- rename(vaccines2,State = "State/Territory/Federal Entity",
                    People_fully_vaccinated = "People Fully Vaccinated by State of Residence",
                    People_with_atleast_one_vaccination = "People with at least One Dose by State of Residence",
                    Percent_of_population_vaccinated = "Percent of Total Pop Fully Vaccinated by State of Residence")

#renaming the NY row in State field, as it's called New York State in raw data
#causing an error in heatmap
vaccines2$State[43] = "New York"

#changing case to lower for State column, used for "join" below so case matches
vaccines2$State = tolower(vaccines2$State)

#retrieve the states map data and merge with vaccine factors data
states_map <- map_data("state")
vaccines2map <- left_join(states_map, vaccines2, by = c("region" = "State"))

#converting percent column to a number
vaccines2map$Percent_of_population_vaccinated <- as.numeric(vaccines2map$Percent_of_population_vaccinated)

#dividing percent column by 100 to give accurate percentage reading
vaccines2map$Percent_of_population_vaccinated <- (vaccines2map$Percent_of_population_vaccinated / 100)

#map for fully vaccinated people by state
ggplot(vaccines2map, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = People_fully_vaccinated), color = "white")+
  scale_fill_viridis_c(option = "C", labels=comma) +
  labs(title="Number of people who are fully vaccinated", subtitle = "All ages",
       x="longitude", y="latitude", fill = "Total vaccinations")
ggsave("fully_vaccinated.jpg")
dev.off()

#map of percentage of state population who are vaccinated
ggplot(vaccines2map, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Percent_of_population_vaccinated), color = "white")+
  scale_fill_viridis_c(option = "C", labels=percent) +
  labs(title="Percent of state population who are fully vaccinated", subtitle = "All ages",
       x="longitude", y="latitude", fill = "Total percent")
ggsave("percent_vaccinated.jpg")
dev.off()

################################################################################
##smoking rates by state

#renaming Location column to show State
smoking2 <- rename(smoking1,State = Location)

#dividing percent column by 100 to give accurate percentage reading
smoking2$Data_Value <- (smoking2$Data_Value / 100)

#changing case to lower for State column, used for "join" below so case matches
smoking2$State = tolower(smoking2$State)

#retrieve the states map data and merge with smoking data
states_map <- map_data("state")
smoking2map <- left_join(states_map, smoking2, by = c("region" = "State"))

#map of percentage of state population who smoke
ggplot(smoking2map, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Data_Value), color = "white")+
  scale_fill_viridis_c(option = "C", labels=percent) +
  labs(title="Percent of state population who smoke", subtitle = "All ages",
       x="longitude", y="latitude", fill = "Total percent")
ggsave("percent_smokers.jpg")
dev.off()

################################################################################
##combining multiple dataframes into 1, joined by state

#creating a single dataframe with total covid deaths and state population
allfields <- inner_join(deaths6, statepopulation2, by = c("State" = "State"))
allfields1 <- inner_join(deaths6, statepopulation2, by = c("State" = "State"))

#combining previous new dataframe with smoking rates
allfields2 <- inner_join(allfields, smoking2, by = c("State" = "State"))

#combining previous 2 new dataframes with vaccination rates
allfields3 <- inner_join(allfields2, vaccines2, by = c("State" = "State"))

#combining with average age
allfields4 <- inner_join(allfields3, averageage1, by = c("State" = "State"))

#removing unneeded columns
allfields5 <- allfields4[c(1:6,11:12,15)]

#renaming columns to avoid confusion
allfields5 <- rename(allfields5, PercentoftotalCoviddeaths = Percentoftotal.x, 
                     Percentoftotalpopulation = Percentoftotal.y,
                     Totalpopulation = Year_2019,
                     Percentwhosmoke = Data_Value,
                     Average_age = Population)

#calculating that percent of each state's population who died of Covid-19 in 
#larger dataset of all fields
allfields5$Pctofpopthatdied <- (allfields5$COVID19_Deaths / allfields5$Totalpopulation)

#calculating that percent of each state's population who died of Covid-19 in 
#dataset of only state and fatality percentage
allfields1$Pctofpopthatdied <- (allfields5$COVID19_Deaths / allfields5$Totalpopulation)

#removing unneeded columns
allfields1 <- allfields1[c(1,6)]

#ranking states by death percentage
allfields1$Rank<-rank(allfields1$Pctofpopthatdied)

################################################################################
#heatmap of percentages of state population who died

#duplicating dataset
allfieldsmap <- allfields5

#changing case to lower for State column, used for "join" below so case matches
allfieldsmap$State = tolower(allfieldsmap$State)

#retrieve the states map data and merge with all fields data
states_map <- map_data("state")
allfieldsmap <- left_join(states_map, allfieldsmap, by = c("region" = "State"))

#map of percentage of state population that died
ggplot(allfieldsmap, aes(long, lat, group = group)) +
  geom_polygon(aes(fill = Pctofpopthatdied), color = "white")+
  scale_fill_viridis_c(option = "C", labels=percent) +
  labs(title="Percent of state population that died", subtitle = "All ages",
       x="longitude", y="latitude", fill = "Total percent")
ggsave("percent_of_pop_that_died.jpg")
dev.off()


