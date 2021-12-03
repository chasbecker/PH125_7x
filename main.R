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
library(Lahman)
# View(Teams)

bb_slope <- Teams %>%
           filter( yearID %in% 1961:2001 ) %>%
           mutate( BB_per_game = BB/G, R_per_game = R/G ) %>%
           lm( R_per_game ~ BB_per_game, data = . ) %>%
           .$coef %>%
           .[2]
bb_slope

singles_slope <- Teams %>%
                 filter( yearID %in% 1961:2001 ) %>%
                 mutate( Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G ) %>%
                 lm( R_per_game ~ Singles_per_game, data = . ) %>%
                 .$coef %>%
                 .[2]
singles_slope

Teams %>%
  filter( yearID %in% 1961:2001 ) %>%
  mutate( Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G ) %>%
  summarize( cor(BB, HR), cor(Singles, HR), cor(BB, Singles) )

dat <- Teams %>%
       filter( yearID %in% 1961:2001 ) %>%
       mutate( HR_strata = round( HR/G, 1),
               BB_per_game = BB/G,
               R_per_game = R/G ) %>%
       filter( HR_strata >= 0.4 & HR_strata <= 1.2 )

hr_strat_plot <- dat %>% ggplot( aes(BB_per_game, R_per_game) ) +
        geom_point( alpha = 0.5 ) +
        geom_smooth( method = "lm" ) +
        facet_wrap( ~ HR_strata )
hr_strat_plot

dat %>% group_by( HR_strata ) %>%
        summarize( slope = cor(BB_per_game, R_per_game )*(sd(R_per_game)/sd(BB_per_game)))

###===========================================================

dat <- Teams %>%
        filter( yearID %in% 1961:2001 ) %>%
        mutate( BB_strata = round(BB/G, 1),
                HR_per_game = HR/G,
                R_per_game = R/G ) %>%
        filter( BB_strata >= 2.8 & BB_strata <= 3.9 )

lm_plot <- dat %>% ggplot( aes( HR_per_game, R_per_game ) ) +
         geom_point( alpha = 0.5 ) +
         geom_smooth( method = "lm" ) +
         facet_wrap( ~ BB_strata )
lm_plot

dat %>% group_by(BB_strata) %>%
        summarize( slope = cor(HR_per_game, R_per_game)*(sd(R_per_game)/sd(HR_per_game)))
### 2.2 ==========================

rm(list=ls())
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
                    filter(gender == "male" ) %>%
                    group_by(family) %>%
                    sample_n(1) %>%
                    ungroup() %>%
                    select(father, childHeight) %>%
                    rename( son = childHeight )

rss <- function(beta0, beta1, data){
  resid <-  galton_heights$son - (beta0 + beta1*galton_heights$father)
  return( sum(resid^2) )
  }

beta1 = seq(0, 1, len=nrow(galton_heights))
results <-  data.frame(beta1 = beta1,
                       rss = sapply(beta1, rss, beta0 = 25))


results %>% ggplot(aes(beta1, rss)) +
              geom_line() +
              geom_line(aes(beta1, rss))

fit <- lm( son ~ father, data = galton_heights )
fit

summary(fit)

B <- 1000
N <- 50

lse <- replicate( B, { sample_n( galton_heights, N, replace = TRUE )%>%
                       lm( son ~ father, data = .) %>%
                      .$coef 
                     }
                 )

lse <- data.frame( beta_0 = lse[1,], beta_1 = lse[2,])

library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black")
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth=0.1, color = "black")
grid.arrange(p1,p2,ncol=2)

sample_n(galton_heights, N, replace = TRUE) %>%
  lm(son ~ father, data = .) %>%
  summary %>%
  .$coef

lse %>% summarize( se_0 = sd(beta_0), se_1 = sd(beta_1))


lse %>% summarize(cor(beta_0, beta_1))

B <- 1000
N <- 50
lse <- replicate( B, {sample_n(galton_heights, N, replace = TRUE) %>%
                                 mutate( father = father - mean(father)) %>%
                                 lm( son ~ father, data = .) %>%
                                 .$coef
                      }
                )

cor(lse[1,], lse[2,])

galton_heights %>% ggplot(aes(father, son)) +
                     geom_point() +
                     geom_smooth(method="lm")

fit <- galton_heights %>%
         lm(son ~ father, data = .)

Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)
galton_heights %>%
        mutate(Y_hat = predict(lm(son~father, data = .))) %>%
        ggplot(aes(father, Y_hat)) +
          geom_line()

#++++++++++++++++++++++++++++
beta1 = seq(0, 1, len=nrow(galton_heights))
results <-  data.frame(beta1 = beta1,
                       rss = sapply(beta1, rss, beta0 = 36))


results %>% ggplot(aes(beta1, rss)) +
  geom_line() +
  geom_line(aes(beta1, rss))

# LSE assessentm Q3 ++++++++++++++++++++++++++++++++++++++

rm(list=ls())
library(tidyverse)
library(Lahman)

# View(Teams)
str(Teams)

p1 <- Teams %>%
        filter( yearID %in% 1961:2001 )

rg <- p1$R / p1$G
bbg <- p1$BB / p1$G
hrg <- p1$HR / p1$G

reg_p1 <- lm( rg ~ bbg + hrg )
summary( reg_p1 )

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list=ls())
set.seed(1989, sample.kind = "Rounding")
library(tidyverse)
library(HistData)
data("GaltonFamilies")
options(digits = 3)

female_heights <-GaltonFamilies %>%
                    filter( gender == "female" ) %>%
                    group_by(family) %>%
                    sample_n(1) %>%
                    ungroup() %>%
                    select( mother, childHeight ) %>%
                    rename( daughter = childHeight )

r1 <- lm( female_heights$mother ~ female_heights$daughter )
summary( r1 )

mh <- data.frame(female_heights$mother)

predict( r1, newdata = mh )

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

rm(list=ls())
library(tidyverse)
library(Lahman)
data("Batting")

bat_02 <- Batting %>%
            filter( yearID == 2002 ) %>%
            mutate( pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa ) %>%
            filter( pa >= 100 ) %>%
            select( playerID, singles, bb )

averages <- Batting %>%
  filter( yearID %in% 1999:2001 ) %>%
  mutate( pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa ) %>%
  filter( pa >= 100 ) %>%
  group_by( playerID ) %>%
  summarize(mean_singles = mean( singles ), mean_bb = mean(bb)) %>%
  select(playerID, mean_singles, mean_bb)
  
s <- averages %>% filter(mean_singles > 0.2) %>% nrow(.)
print(paste("singles", s))
b <- averages %>% filter(mean_bb > 0.2) %>% nrow(.)
print(paste("bb", b))

bat_cor <- inner_join( bat_02, averages, on = playerID)
cor(bat_cor$singles, bat_cor$mean_singles)
cor(bat_cor$bb, bat_cor$mean_bb)

gg_singles <- ggplot( bat_cor, aes( mean_singles, singles)) +
          geom_point()
gg_singles

gg_bb <- ggplot( bat_cor, aes( mean_bb, bb )) +
         geom_point()
gg_bb

lm_singles <- lm( data = bat_cor, singles ~ mean_singles )
lm_singles$coefficients

lm_bb <- lm( data = bat_cor, formula = bb ~ mean_bb )
lm_bb$coefficient

# Section 2: Linear Models 2.3: Tibbles, do, and broom

rm(list=ls())
library(tidyverse)
library(Lahman)
data("Teams")

dat <- Teams %>% filter( yearID %in% 1961:2001 ) %>%
                 mutate( HR = round(HR/G, 1),
                         BB = BB/G,
                         R = R/G ) %>%
                 select( HR, BB, R ) %>%
                 filter( HR >= 0.4 & HR <= 1.2 )

dat %>%
  group_by(HR) %>%
  summarize( slope = cor( BB,R )*( sd(R)/sd(BB) ) )

dat %>%
  group_by( HR ) %>%
  lm( formual = R ~ BB, data = . ) %>%
  .$coef

dat %>%
  group_by( HR ) %>%
  head()

dat %>%
  group_by( HR ) %>%
  class()

Teams
as_tibble(Teams)

class(Teams[,20])
class(as_tibble(Teams[,20]))
class(as_tibble(Teams)$HR)
Teams$hr
as_tibble(Teams)$hr

tibble( id=c(1,2,3), func = c(mean, median, sd) )

# con't

str(Teams)

#assigns result to column 'fit'
dat %>%
  group_by( HR ) %>%
  do( fit = lm( formula = R ~ BB, data = . ) )

#no column assignment, throws error
dat %>%
  group_by( hr ) %>%
  do( lm( formula = R ~ BB, data = . ) )

get_slope <- function(data_in){
  fit <- lm( formula = R ~ BB, data = data_in )
  data.frame( slope = fit$coefficient[2],
              se = summary( fit)$coefficient[2,2] )
}

a_df <- dat %>%
        group_by( HR ) %>%
        do(get_slope(.))
a_df

a_broken_df <- dat %>%
               group_by( HR ) %>%
               do( slope = get_slope(.) )

a_broken_df

get_lse <- function(data_in){
           fit <- lm( formula = R ~ BB, data = data_in )
           data.frame( term = names( fit$coefficients ),
                      estimate = fit$coefficients,
                      se = summary( fit )$coefficient[,2] )
}

an_lse <- dat %>%
          group_by( HR ) %>%
          do(get_lse(.))
# con't

library( broom )
fit_1 <- lm( formula = R ~ BB, data = dat )
tidy( fit_1 )
tidy( fit_1, conf.int = TRUE )

plt1_dat <- dat %>%
  group_by( HR ) %>%
  do( tidy( lm( formula = R ~ BB, data = . ), conf.int = TRUE ) ) %>%
  filter( term == "BB" ) %>%
  select( HR, estimate, conf.low, conf.high )

plt1_dat %>%
  ggplot( aes( HR, y = estimate, ymin = conf.low, ymax = conf.high ) ) +
    geom_errorbar() +
    geom_point()

glance( fit_1 )
