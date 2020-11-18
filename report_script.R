# Rscript for report

setwd('~/cereo-r-workshop/')

library(tidyverse)


# data import -------------------------------------------------------------

df_all <- read_csv('data/daily_bike_data.csv')

dftemp <- df_all %>% select(cnt, temp)


# data summary ------------------------------------------------------------

ss_dftemp <- sapply(dftemp,
                    function(x) c(mean(x), min(x), max(x), sd(x))) %>% 
  data.frame() %>% 
  round(digits = 2)

row.names(ss_dftemp) <- c('mean', 'min', 'max', 'sd')

ggplot(dftemp, aes(temp, cnt)) +
  theme_minimal() +
  geom_point() +
  labs(title = 'Daily Bike Rental and Temperature',
       x = 'Temperature (F, normalized)',
       y = 'Bycicle Rentals') +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5))


# models ------------------------------------------------------------------

dftemp <- dftemp %>% 
  mutate(temp2 = temp^2)

mod1 <- lm(formula = cnt ~ temp,
           data = dftemp)

mod2 <- lm(formula = cnt ~ temp + temp2,
           data = dftemp)

pred_mod1 <- predict(mod1, dftemp['temp'])
pred_mod2 <- predict(mod2, dftemp[c('temp', 'temp2')])

dftemp <- dftemp %>% 
  mutate(cnt_mod1 = pred_mod1,
         cnt_mod2 = pred_mod2)

# or #

dftemp <- bind_cols(dftemp, list(cnt_mod1 = pred_mod1,
                                 cnt_mod2 = pred_mod2))


# model vis --------------------------------------------------------------

ggplot(dftemp, aes(temp, cnt)) +
  theme_minimal() +
  geom_point() +
  geom_line(aes(temp, pred_mod1), colour = 'red', size = 1) +
  geom_line(aes(temp, pred_mod2), colour = 'blue', size = 1) +
  labs(title = 'Daily Bike Rental and Temperature',
       x = 'Temperature (F, normalized)',
       y = 'Bycicle Rentals') +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5))

endplot <- ggplot(dftemp, aes(temp, cnt)) +
  theme_minimal() +
  geom_point() +
  geom_line(aes(temp, pred_mod1, color = 'f(temp)'), size = 1) +
  geom_line(aes(temp, pred_mod2, color = 'f(temp, temp^2)'), size = 1) +
  labs(title = 'Daily Bike Rental and Temperature',
       x = 'Temperature (F, normalized)',
       y = 'Bycicle Rentals',
       color = 'Functions' ) +
  theme(plot.title = element_text(face = 'bold', hjust = 0.5)) +
  scale_color_manual(values = c('f(temp)' = 'red',
                                'f(temp, temp^2)' = 'blue'))

