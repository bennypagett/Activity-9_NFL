## NFL data analysis

library(readr)
library(tidyverse)
library(dplyr)
library(broom)
install.packages("car")
install.packages("psych")
library(psych)

## read in data NFL
NFLdf <- read_csv("2019_nfl_qb_data.csv")
View(NFLdf)

##structure and description data

str(NFLdf)
describe(NFLdf)

head(NFLdf, 10)
tail(NFLdf, 20)

## filter QB's with minimum 10 games
new_df <- NFLdf %>%
  filter(games_started >= 10) %>%
  mutate(win_percentage = wins / games_started * 100)

## Explore relationship between win_percentage and passer_rating. With linear regression lm, with dashed horizontal reference line at 50%
ggplot(data = new_df, aes(x = passer_rating, y = win_percentage)) +
  geom_point(colour = "dodgerblue") +
  geom_smooth(method = "lm", colour = "magenta", se = FALSE) + ## se = FAlSE removes error bars from smooth
  geom_hline(yintercept = 50, colour = "black", linetype = "dashed")

with(new_df, cor(x = passer_rating, y = win_percentage)) ## [1] 0.6863687 moderate to strong relationship

library(broom)
fit <- lm(win_percentage ~ passer_rating, data = new_df)
tidy(fit, conf.int = TRUE)

summary(fit) #### percentage error is calculated by estimate divided by residual standard error
## i.e -50.7307

13.68/-50.7307 ### -.2696592
-.2696592*100 ## -26.96% = %of error that any prediction will be off 

## Step 7 = looking at independence of observations - looking at assumption value of 2.

car::durbinWatsonTest(fit)
###  lag Autocorrelation D-W Statistic p-value
###   1      -0.2092827      2.372391   0.348
### Alternative hypothesis: rho != 0 rejecting null hypothesis

## Outliers - Step 8 and plot
std_res <- rstandard(fit)
points <- 1:length(std_res)
## no outliers are identified as all standard residuals are less than 3
ggplot(data = NULL, aes(x = points, y = std_res)) +
  geom_point() +
  ylim(c(-4,4)) +
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

## Leverage points - Step 9
hats <- hatvalues(fit)

##plot
ggplot(data = NULL, aes(x = points, y = hats)) +
  geom_point()
## there are no hatvalues >1, however >0.1 shows some noticeable values, 
hat_labels <- if_else(hats >- 0.10, paste(points), "")

##plot hats, points >0.1 have been labeled 
ggplot(data = NULL, aes(x = points, y = hats)) +
  geom_point() +
  geom_text (aes(label = hat_labels), nudge_y = 0.005)

## regression again with labeled values
ggplot(data = new_df, aes(x = passer_rating, y = win_percentage)) +
  geom_point(colour = "dodgerblue") +
  geom_smooth(method = "lm", colour = "magenta", se = FALSE) +
  geom_hline(yintercept = 50, colour = "black", linetype = "dashed") +
  geom_text(aes(label = hat_labels), nudge_x = 1)



##Influential points - Step 10
cook <- cooks.distance(fit)

##plot - determine if any points could be considered as points of high influence
ggplot(data = NULL, aes(x = points, y = cook)) +
  geom_point()

## new labels looking at > .015
cook_labels <- if_else(cook >= 0.015, paste(points), "")

cook_labels <- if_else(cook >= 0.05, paste(points), "")

## idetifying outliers
ggplot(data = NULL, aes(x = points, y = cook)) +
  geom_point() +
  geom_text(aes(label = cook_labels), nudge_x = 1)

##regression with outliers/leverage points labelled

ggplot(data = new_df, aes(x = passer_rating, y = win_percentage)) +
  geom_point(colour = "dodgerblue") +
  geom_smooth(method = "lm", colour = "magenta", se = FALSE) +
  geom_hline(yintercept = 50, colour = "black", linetype = "dashed") +
  geom_text(aes(label = cook_labels), nudge_x = 1)

### as we chose 10games starting value, there are 8 athletes who can influence the model
## therefore, we need to remove 8 athletes and run regression again.

outliers <- c(4, 8, 11, 14, 21, 26, 28) ## removing athletes and storing in outliers df
filtered_df <- new_df %>%
  filter(!case_no %in% outliers) ## filter if case_no is not (!) %in% outliers

## re-run linear regression lm() with filtered_df
fit2 <- lm(win_percentage ~ passer_rating, data = filtered_df)
tidy(fit2, conf.int = TRUE)

summary(fit2)

### plot without the high influence points
ggplot(data = filtered_df, aes(x = passer_rating, y = win_percentage)) +
  geom_point(colour = "dodgerblue") +
  geom_smooth(method = "lm", colour = "magenta", se = FALSE) +
  geom_hline(yintercept = 50, colour = "black", linetype = "dashed")

## slope and intercept haven't changed heaps, so may not need to have removed data points

## step 11 - Homoscedasticity 
res <- residuals(fit)
fitted <- predict(fit)

## plotting fitted vs residual

ggplot(data = NULL, aes(x = fitted, y = res)) +
  geom_point(colour = "dodgerblue") +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

## doesn't appear to be evidence of heteroscedasticity

## Step 13 Normality of residual values

ggplot(data = NULL, aes(x = res)) +
  geom_histogram(colour = "black", fill = "dodgerblue", binwidth = 5)

ggplot(data = NULL, aes(sample = res)) +
  stat_qq() + stat_qq_line ()
## Perhaps some skewness, likely from the points that we investigated for influence.
# We already established they weren't doing too much damage to the overall results of the model, 
# However to fix this possible non-normal distribution we should collect more data which will most likely solve this problem. 








