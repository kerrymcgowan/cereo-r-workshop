# Explore bike data to see if there is a relationship between weather and ridership

library(tidyverse)

#### Load the data to work with 

df <- read_csv("data/daily_bike_data.csv")
df
  # cnt is the number of riders each day, outcome variable


#### Exploration od data relationships

# Time trend of ridership
p <- ggplot(data = df) +
  geom_line(aes(x = as.Date(dteday), y = cnt))
p

# Relationship between ridership and temperature
# Only applies df to the geom_point layer, allows you to add different datasets to the same graph
ggplot() +
  geom_point(data = df, aes(x = temp, y = cnt))
# This way applies df to all layers
  # geom_smooth creates a regression line, shaded area is the standard error
ggplot(data = df, aes(x = temp, y = cnt)) +
  geom_point() +
  geom_smooth()

# What is weathersit?
summary(df$weathersit) # realize this is a categorical variable
unique(df$weathersit) # better for categorical variable

# Dplyr verbs (some of them)
# mutate: adds new columns to your data frame (adds new variables to your data set)
# transmute: keeps only the new columns
# select: selects columns form the data set
# filter: filters the rows according to some logical specification

# dplyr::mutate, add the column weather_fac
df2 <- df %>%
  dplyr::mutate(
    weather_fac = factor(weathersit,
                         levels = c(1,2,3,4),
                         labels = c("Clear", "Cloudy", "Rain", "Heavy Rain"))
  )

# dplyr::select, view only a few columns
df2 %>% dplyr::select(dteday, weathersit, weather_fac)

# dplyr:filter, only look at clear days
  # == turns the variable assignment = into "find a direct match"
df2 %>% 
  dplyr::filter(weather_fac == "Clear") %>% 
  ggplot(aes(x = temp, y =cnt)) +
    geom_point() +
    geom_smooth()

# Look at rainy
df2 %>% 
  dplyr::filter(weather_fac == "Rain") %>% 
  ggplot(aes(x = temp, y =cnt)) +
  geom_point() +
  geom_smooth()

# dplyr::select, you can also drop variables
df3 <- df2 %>% 
  dplyr::select(-weathersit)

# Can also use character lists
keep_vars <- c("dteday", "weather_fac", "temp", "cnt")
df4 <- df2 %>% select(all_of(keep_vars))

# Other ways of filtering
  # | is "or"
df2 %>% dplyr::filter(weather_fac == "Rain" | weather_fac =="Cloudy")
  # Another way
weather_factors_we_like <- c("Rainy", "Cloudy")
df2 %>% dplyr::filter(weather_fac %in% c(weather_factors_we_like))

df2 %>% dplyr::filter(weather_fac != "Rain")
df2 %>% dplyr::filter(!(weather_fac %in% weather_factors_we_like))

#3 dplyr::summarize
df2 %>% 
  dplyr::group_by(weather_fac) %>%
  dplyr::summarize(
    cnt_mean = mean(cnt)
  )

df2 %>% 
  dplyr::group_by(season, weather_fac) %>%
  dplyr::summarize(
    cnt_mean = mean(cnt)
  )

### Transforming data format from long to wide or vice-versa (try to keep data long not wide in most cases)

# Transform to create separate temp variables for each month
months <- c("January", "February", "March", "April", "May", "June", "July", "September", "October", "November", "December")
df_wide <- df2 %>% 
  dplyr::mutate(mnth = factor(mnth, levels = months, labels = months)) %>%
  dplyr::rename(year = yr) %>%
  dplyr::select(year, mnth, temp) %>% 
  dplyr::group_by(year, mnth) %>% 
  dplyr::summarize(temp_mean = mean(temp)) %>% 
  tidyr::pivot_wider(names_prefix = "temp_", names_from = mnth, values_from = temp_mean) %>% 
  dplyr::rename_with(tolower) # rename to lower case

# Double curly brackets selects a list called vars in the data frame
{{ vars }}

# Alternative syntax to:
# tidyr::pivot_wider(names_prefix = "temp_", names_from = mnth, values_from = temp_mean) %>% 
# tidyr::spread(key = mnth, values = temp_mean) is the old way of writing things

## Pivoting longer
  # Column names need to be in quotes, but variable names don't
  # Creates 4 new rows from what were once columns
df_long <- df2 %>% 
  tidyr::pivot_longer(cols = c(temp, atemp, hum, windspeed), 
                      values_to = "value", names_to = "variable")

## Pivoting wider
df_wide2 <- df_long %>% 
  tidyr::pivot_wider(names_prefix = "v_", names_from = variable, values_from = value)

## Another pivot wider example
df %>% 
  group_by(weekday) %>% 
  summarize(mean_temp = mean(temp))

df %>% 
  group_by(weekday) %>% 
  summarize(mean_temp = mean(temp)) %>% 
  pivot_wider(names_from = weekday,
              values_from = mean_temp)

## Facetting (make multiple graphs per variable)

ggplot(data = df2, aes(x = temp, y = cnt)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ weather_fac)

# Allow scales to vary on y-axis
ggplot(data = df2, aes(x = temp, y = cnt)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ weather_fac, scales = "free_y")

# Add a theme and labels
ggplot(data = df2, aes(x = temp, y = cnt)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ weather_fac, scales = "free_y") +
  labs(x = "Temperature", y = "Ridership County") +
  ggtitle("Relationship between temperature and ridership")
  theme_linedraw()
  
# Change points
ggplot(data = df2, aes(x = temp, y = cnt)) +
  geom_point(shape = 21, color = "orangered") +
  geom_smooth() +
  facet_wrap(~ weather_fac, scales = "free_y") +
  labs(x = "Temperature", y = "Ridership County") +
  ggtitle("Relationship between temperature and ridership")
  theme_linedraw()
  
# Linear smooth instead of loess
ggplot(data = df2, aes(x = temp, y = cnt)) +
  geom_point(shape = 21, color = "orangered") +
  geom_smooth(method = "lm") +
  facet_wrap(~ weather_fac, scales = "free_y") +
  labs(x = "Temperature", y = "Ridership County") +
  ggtitle("Relationship between temperature and ridership")
  theme_linedraw()

# Quadratic, turn of standard error 
p <- ggplot(data = df2, aes(x = temp, y = cnt)) +
  geom_point(shape = 21, color = "orangered") +
  geom_smooth(method = "lm", fromula = y ~ poly(x, 2), color = "steelblue", se = FALSE) +
  facet_wrap(~ weather_fac, scales = "free_y") +
  labs(x = "Temperature", y = "Ridership County") +
  ggtitle("Relationship between temperature and ridership") +
  theme_linedraw() + 
  theme(strip.background = element_rect(fill = NA),
                                        strip.text = element_text(color = "black"))
ggsave(plot = p, filename = "temp_count_scatter.png")

ggplot(data = df_long, aes(x = value, y = cnt, color = variable)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~ weather_fac)
















