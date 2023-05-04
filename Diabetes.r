# Import the Diet.csv file into R studio
dataset <- read.csv("Diet.csv", header = TRUE, sep = ",")
nrow(dataset)
str(dataset)

# Data cleaning: Clean all missing data
# 1. Omit NA values
dataset <- na.omit(dataset) # Omit the null values
nrow(dataset)

# 2. Omit duplicated data
if(nrow(dataset) == length(unique(dataset$Person))) {#Omit the repeated values
  print("No person is recorded more than 1 time.")
} else {dataset <- dataset[!duplicated(dataset),]}
nrow(dataset)

# 3. Adding new columns
dataset <- cbind(dataset, weightLoss=dataset$pre.weight - dataset$weight6weeks) #Adding weightLoss column

# 4. Factorizing data labels
dataset$gender <- factor(dataset$gender,levels=c(0,1), labels=c("F","M"))
dataset$Diet <- factor(dataset$Diet,levels=c(1,2,3), labels=c("Diet 1", "Diet 2", "Diet 3"))
dataset[1:5,]

#Data visualization
# 1. For the whole dataset
summary(dataset)
sd(dataset$pre.weight)
sd(dataset$weight6weeks)
sd(dataset$Age)
sd(dataset$weightLoss)
sd(dataset$Height)

# 2. Partition the dataset into 3 subgroups
diet1 = dataset[(dataset$Diet=="Diet 1"),]
diet2 = dataset[(dataset$Diet=="Diet 2"),]
diet3 = dataset[(dataset$Diet=="Diet 3"),]

# 3. Study the diet 1, diet 2, diet 3
summary(diet1)
sd(diet1$pre.weight)
sd(diet1$weight6weeks)
sd(diet1$Age)
sd(diet1$weightLoss)
sd(diet1$Height)

summary(diet2)
sd(diet2$pre.weight)
sd(diet2$weight6weeks)
sd(diet2$Age)
sd(diet2$weightLoss)
sd(diet2$Height)


summary(diet3)
sd(diet3$pre.weight)
sd(diet3$weight6weeks)
sd(diet3$Age)
sd(diet3$weightLoss)
sd(diet3$Height)

# Drawing boxplot
boxplot(weightLoss~Diet, data=dataset, horizontal = TRUE, main = "Boxplot weight loss after 6 weeks for each diet", xlab="Weightloss after 6 weeks (kg)", ylab = " ", col=c("red","yellow","green"), las = 1)
boxplot(weightLoss~gender, data=diet1, horizontal = TRUE, main = "Boxplot weight loss of Diet 1 after 6 weeks", xlab="Weight loss after 6 weeks (kg)", ylab="Gender", las = 1, col = c("pink", "skyblue"))
boxplot(weightLoss~gender, data=diet2, horizontal = TRUE, main = "Boxplot weight loss of Diet 2 after 6 weeks", xlab="Weight loss after 6 weeks (kg)", ylab="Gender", las = 1, col = c("pink", "skyblue"))
boxplot(weightLoss~gender, data=diet3, horizontal = TRUE, main = "Boxplot weight loss of Diet 3 after 6 weeks", xlab="Weight loss after 6 weeks (kg)", ylab="Gender", las = 1, col = c("pink", "skyblue"))

# Drawing QQ-plot
qqnorm(dataset$weightLoss, main = "Normal Q-Q plot of dataset")
qqline(dataset$weightLoss)

#T-test: pre_weight and weight6weeks 
  #Checking normality
shapiro.test(dataset$weightLoss)
  # Using paired t-test
t.test(dataset$pre.weight, dataset$weight6weeks, paired=TRUE)

#One-way ANOVA:
  # One way ANOVA and test each pair
one_way_anova = aov(weightLoss~Diet, data = dataset)
summary(one_way_anova)
TukeyHSD(one_way_anova)
boxplot(weightLoss~Diet, data=dataset, horizontal = TRUE, main = "Boxplot weight loss after 6 weeks for each diet", xlab="Weightloss after 6 weeks", ylab="Diet", col=c("red","yellow","green"), las = 1)
plot(TukeyHSD(aov(weightLoss~Diet, data=dataset), conf.level=.95), las = 1, cex.axis=0.6)

  # Checking normality and equal variance
shapiro.test(x = residuals(object = one_way_anova))
bartlett.test(weightLoss ~ Diet, data = dataset)

#Two-way ANOVA:
  #1. Study the interaction between Gender and Diet
table(dataset$gender,dataset$Diet)
boxplot(weightLoss~gender*Diet, data=dataset, main="Boxplot weight loss after 6 weeks for each gender and each diet", ylab="WeightLoss after 6 weeks", xlab = "", las = 1, col=c("pink", "lightblue", "pink", "lightblue", "pink", "lightblue"))
interaction.plot(dataset$Diet, dataset$gender, dataset$weightLoss)
interaction.plot(dataset$gender, dataset$Diet, dataset$weightLoss)

  #2. Study how Diet and gender affect weightLoss by 2way ANOVA
two_way_anova = aov(weightLoss~gender*Diet,data=dataset)
summary(two_way_anova)
TukeyHSD(two_way_anova)

install.packages("car")
library(car)
shapiro.test(x = residuals(object = two_way_anova))
leveneTest(weightLoss ~ gender*Diet, data = dataset)

plot(TukeyHSD(two_way_anova,conf.level=.95),las = 1,cex.axis=0.75)
