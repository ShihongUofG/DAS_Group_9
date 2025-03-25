plot.edu <- ggplot(data, aes(x = Education, fill = Obese)) +
  geom_bar(position = "fill") +
  labs(title = "Obesity by Education Level", x = "Education Level", y = "Proportion") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values=c("dodgerblue4", "firebrick3"))

plot.veg <- ggplot(data, aes(x = Veg, fill = Obese)) +
  geom_bar(position = "fill") +
  labs(title = "Obesity and Vegetable Intake", x = "Vegetable Intake", y = "Proportion")+
  scale_fill_manual(values=c("dodgerblue4", "firebrick3"))

plot.fruit <- ggplot(data, aes(x = Fruit, fill = Obese)) +
  geom_bar(position = "fill") +
  labs(title = "Obesity and Fruit Intake", x = "Fruit Intake", y = "Proportion")+
  scale_fill_manual(values=c("dodgerblue4", "firebrick3"))

plot.age1 <- ggplot(data, aes(y = (Age), x = Obese, fill = Obese)) +
  scale_fill_manual(values = c("dodgerblue4", "firebrick3")) +
  geom_boxplot()

plot.age2 <- ggplot(data, aes(x = (Age), fill = Obese, alpha = 0.1)) +
  scale_fill_manual(values = c("dodgerblue4", "firebrick3")) +
  geom_density()

plot.year <- ggplot(data, aes(x=Year, fill=Obese))+
  geom_bar(position="fill")+
  scale_fill_manual(values = c("dodgerblue4", "firebrick3"))+
  labs(y="Proportion")

plot.sex <- ggplot(data, aes(x=Sex, fill=Obese))+
  geom_bar(position="fill")+
  scale_fill_manual(values = c("dodgerblue4", "firebrick3"))+
  labs(y="Proportion")

gridExtra::grid.arrange(plot.age1, plot.age2, ncol = 2)

gridExtra::grid.arrange(plot.year, plot.sex, ncol = 2)

gridExtra::grid.arrange(plot.fruit, plot.veg, ncol = 2)

gridExtra::grid.arrange(plot.edu, ncol = 1)
