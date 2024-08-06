# Setting Up Data
library(tidyverse)
library(haven)
library(readr)
library("readxl") #Select Excel file of data

my_data <- select(my_data,
                  state,
                  Region,
                  county,
                  measure_43_value,
                  measure_63_value,
                  measure_145_value,
                  measure_51_value)

my_new_data <- rename(my_data,
                      State = state,
                      County = county,
                      Region = Region,
                      Violent_Crimes = measure_43_value,
                      Median_Household_Income = measure_63_value,
                      Frequent_Mental_Distress = measure_145_value,
                      Population = measure_51_value)

my_new_data <- my_new_data %>% 
  mutate(Log_Median_Household_Income = log(Median_Household_Income))

my_new_data <- my_new_data %>% 
  mutate(Violent_Crimes_per1000 = Violent_Crimes/(Population/1000)) %>% 
  filter(Violent_Crimes_per1000 < 2000) %>% 
  mutate(Log_Violent_Crimes_per1000 = log(1 + Violent_Crimes_per1000))
--------------------------------------------------------------------------------
  
#PROVIDE STATISTICS OF DATA (1)
#Data Summaries
summary(my_new_data$Violent_Crimes)
summary(my_new_data$Frequent_Mental_Distress)

--------------------------------------------------------------------------------

#PROVIDE DATA ON GEOGRAPHICAL REGIONS (2)
#Graph Showing Mean of Violent Crimes Per Region
my_new_data %>%
  ggplot(aes(x = Region, y = Violent_Crimes)) +
  geom_bar(stat = "Summary",
           color = "blue",
           fill = "pink") +
  labs(
    y = "Number Violent Crimes",
    x = "Regions of the United States",
    title = "Figure 3:",
    subtitle = "Mean Violent Crimes and Regions of the U.S.",
    caption = "Source: County Health Rankings & Roadmaps, 2017") +
  theme_bw()

#Graph Showing Mean of Mental Distress Per Region
my_new_data %>%
  ggplot(aes(x = Region, y = Frequent_Mental_Distress)) +
  geom_bar(stat = "Summary",
           color = "blue",
           fill = "purple") +
  labs(
    y = "Percentage of County Population Who Reported Experiencing Mental Distress",
    x = "Regions of the United States",
    title = "Figure 4:",
    subtitle = "Percentage of County Population Experiencing Mental Distress and Regions of the U.S.",
    caption = "Source: County Health Rankings & Roadmaps, 2017"
  ) +
  theme_bw()

--------------------------------------------------------------------------------
  
#SHOW HISTOGRAMS OF DATA (2) 
#Histogram for Violent Crimes
ggplot(data = my_new_data, aes(x = Violent_Crimes)) + 
  geom_histogram(breaks = seq(0, 1750, 25), fill = "red", color = "black") +
  scale_x_continuous(breaks = seq(0, 1750, 100)) +
  labs(title = "Figure 5:",
       subtitle = "Violent Crimes Per County in the United States",
       caption = "Source: County Health Rankings & Roadmaps, 2017",
       x = "Violent Crimes Per County",
       y = "Frequency") 

#Histogram for Frequent Mental Distress Precentage
ggplot(data = my_new_data, aes(x = Frequent_Mental_Distress)) + 
  geom_histogram(breaks = seq(0, 0.25, 0.005), fill = "green", color = "black") +
  scale_x_continuous(breaks = seq(0, 0.25, 0.05)) +
  labs(title = "Figure 6:",
       subtitle = "Frequent Mental Distress Percentage Per County in the United States",
       caption = "Source: County Health Rankings & Roadmaps, 2017",
       x = "Frequent Mental Health Distress Percentage Per County",
       y = "Frequency")

--------------------------------------------------------------------------------
  
#INTRODUCE DATA WITHOUT THE LOG TAKEN (1)
#Regression of Violent Crimes vs. Frequent Mental Distress Percentage
ggplot(data = my_new_data, aes(x = Frequent_Mental_Distress, y = Violent_Crimes_per1000)) +
  geom_point() +
  labs(y = "Number of Violent Crimes Per County Per 1000 People",
       x = "Percentage of Self Reported Frequent Mental Distress",
       title = "Figure 7:",
       subtitle = "Violent Crimes and Frequent Mental Distress by County",
       caption = "Source: County Health Rankings & Roadmaps, 2017")

p2 <- ggplot(data = my_new_data, aes(x = Frequent_Mental_Distress, y = Violent_Crimes_per1000)) +
  geom_point() +
  labs(y = "Number of Violent Crimes Per County Per 1000 People",
       x = "Percentage of Reported Frequent Mental Distress",
       title = "Figure 7:",
       subtitle = "Violent Crimes and Frequent Mental Distress by County",
       caption = "Source: County Health Rankings & Roadmaps, 2017")

p2 + geom_smooth(method = "lm")

--------------------------------------------------------------------------------
  
#INTRODUCE DATA WITH THE LOG TAKEN (2)
#Log(Mental Distress) 
ggplot(data = my_new_data, aes(x = Frequent_Mental_Distress, y = Log_Violent_Crimes_per1000)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 0.25, 0.01)) +
  scale_y_continuous(breaks = seq(0, 6.5, 0.25)) +
  labs(title = "Figure 8:",
       subtitle = "Log of Violent Crimes and Frequent Mental Distress by County",
       caption = "Source: County Health Rankings & Roadmaps, 2017",
       x = "Frequent Mental Distress per County",
       y = "Log of Violent Crimes per County") +
  theme_bw()

p3 <- ggplot(data = my_new_data, aes(x = Frequent_Mental_Distress, y = Log_Violent_Crimes_per1000)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_continuous(breaks = seq(0, 0.25, 0.01)) +
  scale_y_continuous(breaks = seq(0, 6.5, 0.25)) +
  labs(title = "Figure 8:",
       subtitle = "Log of Violent Crimes and Frequent Mental Distress by County",
       caption = "Source: County Health Rankings & Roadmaps, 2017",
       x = "Frequent Mental Distress per County",
       y = "Log of Violent Crimes per County") +
  theme_bw()

p3 + geom_smooth(method = "lm")

#Correlation Coefficient 
my_new_data <- my_new_data %>% 
  mutate(Log_Frequent_Mental_Distress = log(Frequent_Mental_Distress))

cor(my_new_data$Log_Frequent_Mental_Distress, my_new_data$Violent_Crimes, use = "complete.obs")
[1] 0.3633334 #Console Output

--------------------------------------------------------------------------------

my_reg_crimes <- lm(Log_Violent_Crimes_per1000 ~ Frequent_Mental_Distress, data = my_new_data)  

confint(my_reg_crimes,"Frequent_Mental_Distress",level = 0.95)

summary(my_reg_crimes)

--------------------------------------------------------------------------------

#PROVIDE HISTOGRAM FOR INCOME (1)
#Histogram for Income
ggplot(data = my_new_data, aes(x = Median_Household_Income)) + 
  geom_histogram(breaks = seq(20000, 135000, 5000), fill = "red", color = "black") +
  scale_x_continuous(breaks = seq(20000, 135000, 10000)) +
  labs(title = "Figure 10:",
       subtitle = "Median Household Income Per County in the United States",
       caption = "Source: County Health Rankings & Roadmaps, 2017",
       x = "Median Household Income Per County",
       y = "Frequency")

#Histogram for Log of Income
ggplot(data = my_new_data, aes(x = Log_Median_Household_Income)) + 
  geom_histogram(breaks = seq(10, 12, 0.1), fill = "red", color = "black") +
  scale_x_continuous(breaks = seq(10, 12, 0.25)) +
  labs(title = "Figure 11:",
       subtitle = "Log of Median Household Income Per County in the United States",
       caption = "Source: County Health Rankings & Roadmaps, 2017",
       x = "Log of Median Household Income Per County",
       y = "Frequency") 

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
  
#SHOW REGRESSION GRAPH FOR INCOME AND VIOLENT CRIMES
ggplot(data = my_new_data, aes(x = Log_Median_Household_Income, y = Log_Violent_Crimes_per1000)) +
  geom_point() +
  labs(y = "Log of Violent Crimes Per County",
       x = "Log of Median Household Income",
       title = "Figure 12:",
       subtitle = "Log of Violent Crimes and Log of Median Household Income by County",
       caption = "Source: County Health Rankings & Roadmaps, 2017")

p4 <- ggplot(data = my_new_data, aes(x = Log_Median_Household_Income, y = Log_Violent_Crimes_per1000)) +
  geom_point() +
  labs(y = "Log of Violent Crimes Per County",
       x = "Log of Median Household Income",
       title = "Figure 12:",
       subtitle = "Log of Violent Crimes and Log of Median Household Income by County",
       caption = "Source: County Health Rankings & Roadmaps, 2017")

p4 + geom_smooth(method = "lm")

--------------------------------------------------------------------------------
  
#SHOW REGRESSION GRAPH FOR INCOME AND MENTAL DISTRESS
ggplot(data = my_new_data, aes(x = Log_Median_Household_Income, y = Frequent_Mental_Distress)) +
  geom_point() +
  labs(y = "Frequent Mental Distress",
       x = "Log of Median Household Income",
       title = "Figure 13:",
       subtitle = "Frequent Mental Distress and Log of Median Household Income",
       caption = "Source: County Health Rankings & Roadmaps, 2017")

p5 <- ggplot(data = my_new_data, aes(x = Log_Median_Household_Income, y = Frequent_Mental_Distress)) +
  geom_point() +
  labs(y = "Frequent Mental Distress",
       x = "Log of Median Household Income",
       title = "Figure 13:",
       subtitle = "Frequent Mental Distress and Log of Median Household Income",
       caption = "Source: County Health Rankings & Roadmaps, 2017")

p5 + geom_smooth(method = "lm")

--------------------------------------------------------------------------------
  
#SHOW MULTIVARIATE WITH INCOME, VIOLENT CRIMES AND MENTAL DISTRESS (HOLD VIOLENT CRIMES CONSTANT)
#Multivariate Table Comparing Income, Violent Crimes and Mental Distress
Multi1 <- lm(Log_Violent_Crimes_per1000 ~ Frequent_Mental_Distress, data = my_new_data)
Multi2 <- lm(Log_Violent_Crimes_per1000 ~ Log_Median_Household_Income, data = my_new_data)
Multi_Both <- lm(Log_Violent_Crimes_per1000 ~ Frequent_Mental_Distress + Log_Median_Household_Income, data = my_new_data)

stargazer(Multi1, Multi2, Multi_Both,
          type = "html",
          out = "my_new_data_models.html")