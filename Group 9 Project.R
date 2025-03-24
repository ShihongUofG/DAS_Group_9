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

for (i in c(2:7)) {
  df[,i] <- factor(df[,i])
}

gt::glimpse(df)
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

binned_residuals(model_Sat)
}





library(leaps)
models <- regsubsets(Obese ~ . , data = df)
summary(models)

plot(models, scale = "Cp") #says not to include sex, fruit or year
plot(models, scale = "bic") 
plot(models, scale = "adjr2") 


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
#In obese group, about 55% are female
#in non-obese group, about 55% are female


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

A <- MASS::stepAIC(model_Null,
        scope = formula(model_Full),
        direction = "both")
#confirms what was thought all along
#Age, Education, Veg

A$formula


model_1 <- glm(Obese ~ Age + Education + Veg,
               family = binomial, 
               data = df)

model_1 %>%
  tidy(conf.int = T)

model_1 %>%
  summary()

performance::check_model(model_1, panel = T)
binned_residuals(model_1)


#different models
#threshold = 0.5
#split 70/30 to train model
#mention training / testing, cross validation in discussion for full marks

model_1_pred <- model_1 %>%
  augment(type.predict = c("response"))

model_pred_0.5 <- model_1_pred %>%
  mutate(predicted_class = 
           factor(ifelse(.fitted > 0.5, "Yes", "No")))

model_pred_0.464 <- model_1_pred %>%
  mutate(predicted_class = 
           factor(ifelse(.fitted > 0.464, "Yes", "No")))

model_pred_0.269 <- model_1_pred %>%
  mutate(predicted_class = 
           factor(ifelse(.fitted > 0.269, "Yes", "No")))


#Let's try and find threshold of highest accuracy
mostAccurate <- function(model) {
  I <- 0
  a0 <- 0
  model_pred <- model %>%
    augment(type.predict = c("response"))
  eval_metric <- metric_set(accuracy,sensitivity,specificity,ppv,npv)
  
  model_list <- list()
  evals_list <- list()
  for (i in (1:1000)) {
    model_list[[i]] <- model_pred %>%
      mutate(predicted_class = 
               factor(ifelse(.fitted > (i/1000), "Yes", "No")))
    
    if (length(levels(model_list[[i]]$Obese)) == 2 & 
               length(levels(model_list[[i]]$predicted_class)) == 2) {
      print(paste("Trial good, ", i))
      evals_list[[i]] <- eval_metric(model_list[[i]],
                                     truth = Obese,
                                     estimate = predicted_class,  
                                     event_level = "second")
      if (evals_list[[i]][1,3] > a0) {
        print(paste("New highest accuracy found at threshold ", i))
        a0 <- evals_list[[i]][1,3]
        I <- i
      }
    } else {
      evals_list[[i]] <- "Bad fit"
    }
  }
  return(list(I, model_list[[I]], evals_list[[I]]))
}

mostAccurate(model_1) #give 0.464


roc_model_1 <- roc_curve(model_pred_0.5,
                         truth = Obese,
                         .fitted,
                         event_level = "second")  %>% 
  mutate(youden_j = sensitivity + specificity - 1)

plot(x = 0:1, y = 0:1, type = "n")
lines(x = 1 - roc_model_1$sensitivity, y = roc_model_1$specificity,
      type = "l", lwd = 2)
abline(a = 0, b = 1)
abline(v = c(0.464, 0.269))

roc_model_1 %>%
  filter(youden_j == max(youden_j))


#conf_mat(model_pred_0.5, truth = Obese, estimate = predicted_class) no 'yes' predicted
conf_mat(model_pred_0.464, truth = Obese, estimate = predicted_class)
conf_mat(model_pred_0.269, truth = Obese, estimate = predicted_class)


eval_metric <- metric_set(accuracy,sensitivity,specificity,ppv,npv)

eval_metric(model_pred_0.269,
            truth = Obese,
            estimate = predicted_class,  
            event_level = "second")

eval_metric(model_pred_0.464,
            truth = Obese,
            estimate = predicted_class,  
            event_level = "second")

?check_model()

print("Hello world")


#realign latex code
#add hyperlinks
