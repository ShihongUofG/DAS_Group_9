url <- "https://raw.githubusercontent.com/ShihongUofG/DAS_Group_9/refs/heads/main/DAProject1.csv"
data <- read.csv(url)

library(tidyverse)

data <- data %>%
  mutate(across(c(Sex, Education, Veg, Fruit, Obese), as.factor))

summary(data)

ggplot(data, aes(x = Obese)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Obesity Distribution", x = "Obese (Yes/No)", y = "Count")

ggplot(data, aes(x = Obese, y = Age, fill = Obese)) +
  geom_boxplot() +
  labs(title = "Obesity by Age", x = "Obesity", y = "Age")

ggplot(data, aes(x = Sex, fill = Obese)) +
  geom_bar(position = "fill") +
  labs(title = "Obesity Prevalence by Gender", x = "Gender", y = "Proportion") +
  scale_y_continuous(labels = scales::percent)

ggplot(data, aes(x = Education, fill = Obese)) +
  geom_bar(position = "fill") +
  labs(title = "Obesity by Education Level", x = "Education Level", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data, aes(x = Veg, fill = Obese)) +
  geom_bar(position = "fill") +
  labs(title = "Obesity and Vegetable Intake", x = "Vegetable Intake", y = "Proportion")

ggplot(data, aes(x = Fruit, fill = Obese)) +
  geom_bar(position = "fill") +
  labs(title = "Obesity and Fruit Intake", x = "Fruit Intake", y = "Proportion")

chisq.test(table(data$Sex, data$Obese))  
chisq.test(table(data$Education, data$Obese))  
chisq.test(table(data$Veg, data$Obese))  
chisq.test(table(data$Fruit, data$Obese))  

t.test(Age ~ Obese, data = data)

model <- glm(Obese ~ Age + Sex + Education + Veg + Fruit, 
             data = data, family = binomial)
summary(model)

ggplot(model, aes(x=model$residuals))+
  geom_histogram(colour = "white")+
  labs(x="Residuals", y="Count")

ggplot(model, aes(x=Obese, y=model$residual))+
  geom_point()+
  labs(y="Residuals")+
  geom_hline(yintercept = 0, colour="blue")
