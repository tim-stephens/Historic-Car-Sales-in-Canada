#####################################################






#####################################################
#```{r RDDtrimmed, echo = FALSE, message = FALSE, include = FALSE}

# RDD from 1970:1990
RDD_data <-
  yearly_vehicle_sales %>% 
  filter(Year <= 1990 & Year >= 1970)


#chose 1982 because it had the shock unemployment rate from 7.6 - 11%
RDD_data %>% 
  ggplot(aes(x = Year,
             y = Total)) +
  geom_point(alpha = 0.2) +
  geom_smooth(data = RDD_data %>% filter(Year < 1982), 
              method='lm',
              color = "black") + 
  geom_smooth(data = RDD_data %>% filter(Year >= 1982), 
              method='lm',
              color = "black") +
  theme_minimal() +
  labs(x = "Year",
       y = "Number of Vehicles Sold") 


#``` 


#####################################################
#```{r More RDD, echo = FALSE, include = FALSE, message = FALSE, include = FALSE}
###MORE RDD 

### RDD Based on Unemployment cutoff of 10% 

total_data_set_1970_1990 %>% 
  ggplot(aes(x = unemployment_rate,
             y = Total)) +
  geom_point(alpha = 0.2) +
  geom_smooth(data = total_data_set_1970_1990 %>% filter(unemployment_rate < 10), 
              method='lm',
              color = "black") +
  geom_smooth(data = total_data_set_1970_1990 %>% filter(unemployment_rate >= 10), 
              method='lm',
              color = "black") +
  theme_minimal() +
  labs(x = "unemployment rate",
       y = "Number of Vehicles Sold")


### RDD based on oil prices 

total_data_set_1970_1990 %>% 
  ggplot(aes(x = price_in_usd,
             y = Total)) +
  geom_point(alpha = 0.2) +
  geom_smooth(data = total_data_set_1970_1990 %>% filter(price_in_usd < 29.19), 
              method='lm',
              color = "black") +
  geom_smooth(data = total_data_set_1970_1990 %>% filter(price_in_usd >= 29.19), 
              method='lm',
              color = "black") +
  theme_minimal() +
  labs(x = "unemployment rate",
       y = "Number of Vehicles Sold")


#```

#####################################################
oil_prices_lm <- 
  oil_trimmed %>% 
  filter(Year >= 1970 & Year <= 1990)

lm_data <- 
  inner_join(RDD_data, oil_prices_lm, by = 'Year')

ggplot(lm_data, aes(x = price_in_usd, y = Total)) +
  geom_point() +
  theme_minimal()


linear_model <- 
  lm(Total ~ price_in_usd, lm_data)


summary(linear_model)

#####################################################
total_data_set_1970_1990 <- 
  inner_join(unemployment_trimmed, lm_data, by = "Year")

ggplot(total_data_set_1970_1990, aes(unemployment_rate, Total)) + 
  geom_point() + 
  theme_minimal()

unemployment_rate_lm <- 
  lm(Total ~ unemployment_rate, total_data_set_1970_1990 )

summary(unemployment_rate_lm)


