#setwd("~/cereo-r-workshop/drake_template/R")

#bike_data <- read.csv(file = "../data/daily_bike_data.csv",
#                      stringsAsFactors = FALSE)

## Define the functions used in my analysis

# Test code
#bike_data %>% 
#  filter(holiday == "not holiday") %>% 
#  rename(total_riders = cnt)

# Put into function
subset_data <- function(data){
  
  data %>% 
    filter(holiday == "not holiday") %>% 
    rename(total_riders = cnt)
  
}

#bike_data <- subset_data(bike_data)

#head(bike_data)

# alpha adds transparency
#bike_model_plot <- ggplot(data = bike_data, mapping = aes(x = temp, y = total_riders)) +
#  geom_point(alpha = 0.3) +
#  geom_smooth(method = "lm") +
#  facet_grid(~season)
#ggsave(bike_model_plot)

create_model_plot <- function(data){
  
  bike_model_plot <- ggplot(data = data, mapping = aes(x = temp, y = total_riders)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm") +
    facet_grid(~season)
  
  ggsave(filename = "figures/bike_model_plot.png",
         plot = bike_model_plot,
         width = 8, height = 8, units = "in")
  
}

#create_model_plot(data = bike_data)

# Define this model: total_riders ~ temperature*season
  # Add paste and ,

# To test:
#resp_var <-  "total_riders"
#exp_var1 <- "temp"
#exp_var2 <- "season"
#paste(resp_var, "~", exp_var1, "*", exp_var2)

run_model <- function(data, resp_var, exp_var1, exp_var2){
  
  lm(formula = as.formula(paste(resp_var, "~", exp_var1, "*", exp_var2)),
     data = data)
  
}












