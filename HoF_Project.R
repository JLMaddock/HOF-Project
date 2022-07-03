library(tidyverse)
library(dplyr)
options(scipen = 999)
setwd("C:/Users/l_mad/Downloads/wdr/Other")
getwd()




df <- read_csv("CareerStats6.csv")
df


########## FILTERING & CLEANING ##########
df <- df %>% filter(G > 400)                                                   # More than rookie contract / 5 years in league
df_ret <- df %>% filter(Active == 0)                                           # Make retired players list
df_act <- df %>% filter(Active == 1)                                           # Make current players list





HOF_Prob_Model <- glm(HOF ~ G + PPG + RPG + APG + SPG + BPG + FGpct + TPpct + FTpct + Champ + MVP + F_MVP + AllNBA + AllDefense + AllStar + ASG_MVP + DPOY + LeadScorer + PER + TS + USG + WS + BPM + VORP,
                      data = df_ret, family = "binomial")

summary(HOF_Prob_Model)
coefs <- summary(HOF_Prob_Model)$coefficients
df_res <- mutate(df_ret,
                 Probability = fitted(HOF_Prob_Model), .after = HOF_Likely,
                 Residual = 1 - Probability, 
)

df_res %>% arrange(Residual)



df_act_hof <- df_act %>% mutate(Pre = (-1.981 + -0.002*G  + 0.172*PPG + 0.051*RPG + 0.048*APG + 1.344*SPG + 0.483*BPG + 4.836*FGpct + 3.720*TPpct + -6.675*FTpct
                                       + 0.077*Champ + 9.950*MVP + -0.746*F_MVP + 0.163*AllNBA + 0.099*AllDefense + 0.556*AllStar + 0.987*ASG_MVP + 0.946*DPOY + 1.195*LeadScorer 
                                       + -0.461*PER + 0.135*TS + 0.080*USG + 1.646*WS + 1.941*BPM + -4.245*VORP),
                                Prob = (1/(1+exp(-(Pre)))), 
                                Res = 1 - Prob, 
                                .after = HOF_Likely)
df_act_hof %>% arrange(Res)
