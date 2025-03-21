#Group 9 Project

#analysing differences in obesity by different factors
#Q.O.I: Obesity ~ age, gender, socio-economic status, lifestyle factors

df <- read.csv("DAProject1.csv")
library(tidyverse)
library(ggplot2)
library(tidyr)
library(tidymodels)
library(sjPlot)
library(performance)
library(kableExtra)
library(gt)
library(magrittr)

library(moderndive)
library(gapminder)
library(ISLR)
library(gridExtra) #grid.arrange()
library(janitor) #tabyl, adorn_ 

for (i in c(2:5, 7)) {
  df[,i] <- factor(df[,i])
}

glimpse(df)
GGally::ggpairs(df)
skimr::skim(df)


n <- df %>%
  by(., .$Year, nrow) %>%
  c()
n.Obese.Year <- df %>%
  filter(df$Obese == "Yes") %>%
  by(., .$Year, nrow) %>%
  c()
n.Obese.Year / n

#Is there a change in obesity levels by...
#Gender:    No
#Age:       ...
#Lifestyle: ...
#Soc-ec:    ...




ggplot(df, aes(x = fct_infreq(Education),
                  y = Obese, 
                  fill = Education)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Sex)





GLMattempt1 <- function(){
  model_Sat <- glm(df$Obese ~ factor(1:length(df$Obese)), family = binomial)
model_Nul <- glm(Obese ~ 1, data = df, family = binomial)
model_1 <- glm(Obese ~ Age + Education + Veg, 
               data = df, 
               family = binomial) #Sig.predictors according to model_Sat


summary(model_Sat)
summary(model_Nul)
summary(model_1) #maybe try with interaction term

tidy(model_Sat, exponentiate = TRUE)
tidy(model_1)

glance(model_Sat)
glance(model_Nul)

check_model(model_1, check = "binned_residuals")
check_model(model_Sat)

binned_residuals(model_Sat)
}





library(leaps)
models <- regsubsets(Obese ~ . , data = df)
summary(models)

plot(models, scale = "Cp") #says not to include sex, fruit or year




plot1 <- ggplot(df, aes(y = (Age), x = Obese, fill = Obese)) +
  scale_fill_manual(values = c("dodgerblue4", "firebrick3")) +
  geom_boxplot()

plot2 <- ggplot(df, aes(x = (Age), fill = Obese, alpha = 0.1)) +
  scale_fill_manual(values = c("dodgerblue4", "firebrick3")) +
  geom_density()

gridExtra::grid.arrange(plot1, plot2, ncol = 2)


##Bargraphs of obesity proportions in each education level
ggplot(df, aes(x = Education, group = Obese)) +
  geom_bar(aes(y = after_stat(prop), 
               fill = Obese), 
           stat = "count") +
  scale_fill_manual(values = c("dodgerblue4", "firebrick3"))


ggplot(df, aes(x = df[Education == "Degree of higher",])) +
  geom_bar()




#Bargraph of obesity by gender (not helpful)
ggplot(df, aes(x = Sex, group = Obese)) +
  geom_bar(aes(y = after_stat(prop), 
               fill = Obese), 
           stat = "count", 
           position = "dodge") +
  scale_fill_manual(values = c("dodgerblue4", "firebrick3"))


ggplot(df, aes(x = fct_infreq(Education),
               y = Obese, 
               fill = Education)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Sex)


ggplot(df, aes(x = (Veg),
               y = Obese, 
               fill = Veg)) +
  geom_bar(stat = "identity")

ggplot(df, aes(x = Veg, group = Obese)) +
  geom_bar(aes(y = after_stat(prop), 
               fill = Obese), 
           stat = "count", 
           position = "dodge") +
  scale_fill_manual(values = c("dodgerblue4", "firebrick3"))

df2 <- df %>%
  mutate(Obese = ifelse(Obese == "Yes", 1, 0))

model_Full <- glm(Obese~., family = "binomial", data = df2)
model_Null <- glm(Obese~1, data = df2, family = "binomial")

MASS::stepAIC(model_Null,
        scope = formula(model_Full),
        direction = "both")
#confirms what was thought all along
#Age, Education, Veg

model_1 <- glm(Obese ~ Age + Education + Veg,
               family = binomial, 
               data = df)

model_1 %>%
  tidy(conf.int = T)
