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

