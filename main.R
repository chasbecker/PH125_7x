# main module for HarvardX PH125_7x

#if(is.numeric("3")){
#  print("That's numeric")
#  } else { print("That's not numeric") }

library(Lahman)
library(dslabs)
ds_theme_set()
library(tidyverse)


Teams %>% filter(yearID %in% 1961:2001) %>%
          mutate( HR_per_game = HR/G, R_per_game=R/G ) %>%
          ggplot(aes(HR_per_game, R_per_game)) +
          geom_point(alpha=0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
          mutate( SB_per_game = SB/G, R_per_game=R/G ) %>%
          ggplot(aes(SB_per_game, R_per_game)) +
          geom_point(alpha=0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
          mutate( BB_per_game = BB/G, R_per_game=R/G ) %>%
          ggplot(aes(BB_per_game, R_per_game)) +
          geom_point(alpha=0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
          mutate( AB_per_game = AB/G, R_per_game=R/G ) %>%
          ggplot(aes(AB_per_game, R_per_game)) +
          geom_point(alpha=0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
          mutate( W_per_game = W/G, E_per_game=E/G ) %>%
          ggplot(aes(W_per_game, E_per_game)) +
          geom_point(alpha=0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
          mutate( X3B_per_game = X3B/G, X2B_per_game=X2B/G ) %>%
          ggplot(aes(X3B_per_game, X2B_per_game)) +
          geom_point(alpha=0.5)

# assessment 1.2 Q7

corSet <- Teams %>%
          filter(yearID %in% 1961:2001 ) %>%
          mutate( RunsGame = R/G ) %>%
          mutate( BatsGame = AB/G ) %>%
          mutate( WinRate = W/G ) %>%
          mutate( ErrorsGame = E/G ) %>%
          mutate( x2bg = X2B/G ) %>%
          mutate( x3bg = X3B/G )
       
cor( corSet$RunsGame, corSet$BatsGame )
cor( corSet$WinRate, corSet$ErrorsGame )
cor( corSet$x2bg, corSet$x3bg )



# 1.2: Correlation

#install.packages("HistData")  # not shown in sample code
rm(list=ls())
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983)
str(GaltonFamilies)   # also not shown in sample code

galton_heights <- GaltonFamilies %>%
                  filter( gender == "male" ) %>%
                  group_by( family ) %>%
                  sample_n( 1 ) %>%
                  ungroup() %>%
                  select( father, childHeight ) %>%
                  rename( son = childHeight )
View( galton_heights )

galton_heights %>% summarize( mean(father), sd(father), mean(son), sd(son) )

galton_heights %>% ggplot(aes(father,son)) +
                   geom_point(alpha=0.5)

# rho <- mean(scale(x)*scale(y))  this sample code is missing its preface
galton_heights %>% summarize(r=cor(father, son)) %>% pull(r)

R <- sample_n( galton_heights, 25, replace=TRUE ) %>%
     summarize( r = cor(father,son))

R

B <- 1000
N <- 25

R <- replicate( B, {sample_n(galton_heights, N, replace = TRUE) %>%
                    summarize(r=cor(father,son)) %>%
                    pull(r)
                    })
View(as.list(R))

qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))
mean(R)
sd(R)

data.frame(R) %>% ggplot(aes(sample=R)) +
                  stat_qq() +
                  geom_abline( intercept=mean(R), slope = sqrt((1-mean(R)^2/N-2)))

# Anscombe's quartet ================================
rm(list=ls())
library(tidyverse)
library(HistData)
data("GaltonFamilies")

set.seed(1983)

galton_heights <- GaltonFamilies %>%
  filter( gender == "male" ) %>%
  group_by( family ) %>%
  sample_n(1) %>%
  ungroup() %>%
  select( father, childHeight ) %>%
  rename( son = childHeight )

conditional_avg <- galton_heights %>%
  filter( round( father ) == 72 )%>%
  summarize( avg = mean( son ) ) %>%
  .$avg
conditional_avg

sum( galton_heights$father == 72 )
sum( galton_heights$father == 72.5 )


conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>%
  pull(avg)
conditional_avg

galton_heights %>%
  mutate( father_strata = factor( round( father ) ) ) %>%
  ggplot( aes( father_strata, son ) ) +
  geom_boxplot() +
  geom_point()

galton_heights %>%
  mutate( father = round( father ) ) %>% 
  group_by( father ) %>%
  summarize( son_conditional_avg = mean( son ) ) %>%
  ggplot( aes( father, son_conditional_avg ) ) +
  geom_point()

r <- galton_heights%>% summarize(r=cor(father, son)) %>% .$r
  
galton_heights %>%
   mutate(father = round(father)) %>%
   group_by(father) %>%
   summarize(son = mean(son)) %>%
   mutate(z_father = scale(father), z_son = scale(son)) %>%
   ggplot(aes(z_father, z_son)) +
     geom_point() +
     geom_abline(intercept=0, slope = r)


# rosetta stone for all of this that follows >>>>>>>>>
# slope     <- correlation * sd_y / sd_x
# intercept <- mean_y - slope * mean_x

mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y/s_x
b <- mu_y - m*mu_x

galton_heights %>%
  ggplot(aes(scale(father), scale(son))) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r)

#Bivariate Normal Distribution

galton_heights %>%
     mutate(z_father = round( (father - mean(father))/sd(father))) %>%
     filter(z_father %in% -2:2) %>%
     ggplot() +
       stat_qq(aes(sample=son)) +
       facet_wrap(~z_father)


# 11/9/2021
rm(list=ls())
library(tidyverse)

set.seed(1989, sample.kind="Rounding")
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies %>%
                  filter( gender == "female" ) %>%
                  group_by( family ) %>%
                  sample_n(1) %>%
                  ungroup() %>%
                  select( mother, childHeight ) %>%
                  rename( daughter = childHeight )
mother_mean <- mean(female_heights$mother)
mother_sd <- sd( female_heights$mother )

daughter_mean <- mean(female_heights$daughter)
daughter_sd <- sd( female_heights$daughter )
modau_cor <- cor( female_heights$mother, female_heights$daughter)

moda_slope <- modau_cor * ( daughter_sd/mother_sd )
moda_intercept <- daughter_mean - ( moda_slope * mother_mean )

r_squared <- modau_cor^2

if_mother <- 60
expected_daughter <- moda_intercept + ( if_mother * moda_slope )

# 11/10/2021 Linear Models Overview
rm(list=ls())
library(tidyverse)
