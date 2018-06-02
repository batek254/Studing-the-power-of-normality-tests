library(dplyr)
library(tidyr)
library(ggplot2)
library(normtest)
library(tsoutliers)
library(tseries)

#Ilość symulacji
N <- 1000
#Rozpatrywane stopnie swobody
dfs <- seq(5, 50, by = 5)#seq(10, 100, by = 10)
#Rozpatrywane poziomy istotności
alphas <- c(.001, .01, .05, .1)
#Długości próby
sample_l <- c(10,20,30,50,100,200,300,400,500,1000,2000)#seq(20, 1000, by = 100)

params <- expand.grid(dfs, alphas, sample_l)
str(params)
names(params) <- c("df", "alpha", "length")
head(params)
View(params)

set.seed(100)
now <- Sys.time()
powers <- sapply(1:nrow(params), function(p){
  df <- params[p, 1]
  alpha <- params[p, 2]
  l <- params[p, 3]
  p_sim <-sapply(rep(df, N), function(x){
    my_sample <- rt(l, df)
    shapiro.test(my_sample)$p.value
  })
  mean(p_sim < alpha)
})
Sys.time() - now

power_df <- bind_cols(params, power = powers)
View(power_df)

power_df %>% ggplot(aes(x = length, 
                        y = power, 
                        col = factor(df))) +
  geom_line() +
  facet_wrap(~alpha, ncol = 2)

power_wide <- 
  power_df %>%
  spread(key = df, value = power)
View(power_wide)

power_wide %>%
  ggplot(aes(x = length, 
             y = `25`,
             col = factor(alpha))) +
  geom_line()

power_long <- 
  power_wide %>%
  gather(key = df, value = power, -alpha, -length) %>%
  mutate(df = as.numeric(df))
View(power_long)
View(power_df)
glimpse(power_long)

#Kołmogorow-Smirnow
#Ilość symulacji
N <- 1000
#Rozpatrywane stopnie swobody
dfs <- seq(10, 100, by = 10) #seq(2, 20, by = 2)
#Rozpatrywane poziomy istotności
alphas <- c(.001, .01, .05, .1)
#Długości próby
sample_l <- c(10,20,30,50,100,200,300,400,500,1000,2000)#seq(20, 1000, by = 100)

params <- expand.grid(dfs, alphas, sample_l)
str(params)
names(params) <- c("df", "alpha", "length")
head(params)
View(params)

set.seed(100)
now <- Sys.time()
powers <- sapply(1:nrow(params), function(p){
  df <- params[p, 1]
  alpha <- params[p, 2]
  l <- params[p, 3]
  p_sim <-sapply(rep(df, N), function(x){
    my_sample <- rt(l, df)
    ks.test(my_sample, "pnorm")$p.value
  })
  mean(p_sim < alpha)
})
Sys.time() - now

power_df <- bind_cols(params, power = powers)
View(power_df)

power_df %>% ggplot(aes(x = length, 
                        y = power, 
                        col = factor(df))) +
  geom_line() +
  facet_wrap(~alpha, ncol = 2)

power_wide <- 
  power_df %>%
  spread(key = df, value = power)
View(power_wide)

power_wide %>%
  ggplot(aes(x = length, 
             y = `10`,
             col = factor(alpha))) +
  geom_line()

power_long <- 
  power_wide %>%
  gather(key = df, value = power, -alpha, -length) %>%
  mutate(df = as.numeric(df))
View(power_long)
View(power_df)
glimpse(power_long)

#Jarque-Bera
#Ilość symulacji
N <- 1000
#Rozpatrywane stopnie swobody
dfs <- seq(5, 50, by = 5)#seq(10, 100, by = 10)
#Rozpatrywane poziomy istotności
alphas <- c(.001, .01, .05, .1)
#Długości próby
sample_l <- c(10,20,30,50,100,200,300,400,500,1000,2000)#seq(20, 1000, by = 100)

params <- expand.grid(dfs, alphas, sample_l)
str(params)
names(params) <- c("df", "alpha", "length")
head(params)
View(params)

set.seed(100)
now <- Sys.time()
powers <- sapply(1:nrow(params), function(p){
  df <- params[p, 1]
  alpha <- params[p, 2]
  l <- params[p, 3]
  p_sim <-sapply(rep(df, N), function(x){
    my_sample <- rt(l, df)
    jarque.bera.test(my_sample)$p.value
  })
  mean(p_sim < alpha)
})
Sys.time() - now

power_df <- bind_cols(params, power = powers)
View(power_df)

power_df %>% ggplot(aes(x = length, 
                        y = power, 
                        col = factor(df))) +
  geom_line() +
  facet_wrap(~alpha, ncol = 2)

power_wide <- 
  power_df %>%
  spread(key = df, value = power)
View(power_wide)

power_wide %>%
  ggplot(aes(x = length, 
             y = `25`,
             col = factor(alpha))) +
  geom_line()

power_long <- 
  power_wide %>%
  gather(key = df, value = power, -alpha, -length) %>%
  mutate(df = as.numeric(df))
View(power_long)
View(power_df)
glimpse(power_long)

power_long %>%
  ggplot(aes(x = power,
             y = df,
             col = factor(length))) +
  geom_line()
