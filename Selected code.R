ggplot(data, aes(x = Education, fill = Obese)) +
  geom_bar(position = "fill") +
  labs(title = "Obesity by Education Level", x = "Education Level", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values=c("dodgerblue4", "firebrick3"))

ggplot(data, aes(x = Veg, fill = Obese)) +
  geom_bar(position = "fill") +
  labs(title = "Obesity and Vegetable Intake", x = "Vegetable Intake", y = "Proportion")+
  scale_fill_manual(values=c("dodgerblue4", "firebrick3"))

ggplot(data, aes(x = Fruit, fill = Obese)) +
  geom_bar(position = "fill") +
  labs(title = "Obesity and Fruit Intake", x = "Fruit Intake", y = "Proportion")+
  scale_fill_manual(values=c("dodgerblue4", "firebrick3"))