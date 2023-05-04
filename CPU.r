##### Import libraries #####
library(dplyr)
library(ggplot2)
library(GGally)


##### Import data #####
dataset = read.csv("machine_data.csv", header = FALSE)
dataset[1:5,]

column_names = c("NAME", "MODEL", "MYCT", "MMIN", "MMAX", "CACH", "CHMIN", "CHMAX", "PRP", "ERP")
names(dataset) = column_names
dataset[1:5,]


##### Data cleaning #####
cat("The count of NA values in the dataset is", sum(is.na(dataset)))
complete.cases(dataset)
length(unique(dataset$MODEL))
nrow(dataset)


##### Data visualization #####
# Descriptive statistics for all attributes
summary(dataset)
sd(dataset$MYCT)
sd(dataset$MMIN)
sd(dataset$MMAX)
sd(dataset$CACH)
sd(dataset$CHMIN)
sd(dataset$CHMAX)
sd(dataset$PRP)
sd(dataset$ERP)

# Graph: Published CPU Performance By Vendors
vendor_data <- dataset %>% 
  group_by(NAME) %>% 
  summarise(lower = min(PRP), upper = max(PRP), p = median(PRP))

ggplot(data = vendor_data,  mapping = aes(x = reorder(NAME, -upper), y = p, color = factor(NAME))) + 
  ggtitle("Published CPU Performance By Vendors") +
  xlab("Vendors") + 
  ylab("Performance") + 
  theme(legend.position="none") +
  geom_pointrange(size = 0.3, mapping = aes(ymin = lower, ymax = upper)) + 
  theme(axis.text.x=element_text(angle=90, vjust = 0.5, hjust=1))

# Graph: Preparation for One-way ANOVA
dataset = cbind(dataset, LOGPRP = log(dataset$PRP))
plot(density(dataset$PRP), main = "Distribution of CPU performances", xlab = "")
plot(density(dataset$LOGPRP), main = "Distribution of CPU performances", xlab = "")


##### One-way ANOVA #####
sort(table(dataset$NAME), decreasing = TRUE)
chosen_vendors <- names(sort(table(dataset$NAME), decreasing = TRUE)[1:6])
anova_dataset <- dataset[dataset$NAME %in% chosen_vendors,]

one_way_anova <- aov(LOGPRP ~ NAME, data = anova_dataset)
summary(one_way_anova)
TukeyHSD(one_way_anova)
plot(TukeyHSD(one_way_anova, conf.level =.95), las = 1, cex.axis=0.4)

shapiro.test(x = residuals(object = one_way_anova))
bartlett.test(LOGPRP ~ NAME, data = anova_dataset)

dataset <- dataset[-c(11)]


##### Prediction model #####
# Data transformation 
dataset$CH_average <-(dataset$CHMIN+dataset$CHMAX)/2
dataset$F <- 1/dataset$MYCT
dataset$M_average <- (dataset$MMIN+dataset$MMAX)/2
dataset <- dataset[-c(1:5,7:8,10)]
str(dataset)

# Plot correlation
ggpairs(data=dataset,columns=1:5,title="Correlation of data")

# Split train and test data
seed <- sample(c(rep(0,0.7*nrow(dataset)),rep(1,0.3*nrow(dataset))))
seed
data.train <- dataset[seed==0,]
data.test <- dataset[seed==1,]

# Prediction model
model <- lm(PRP~F+M_average+CACH+CH_average,data=data.train)
summary(model)
data.test$Predicted <- predict(model,data.test)
plot(abs(data.test$Predicted-data.test$PRP),pch=19,xlab="i^(th) testcase",ylab="Error",main="Absolute error in predictions of model")
ggplot(data.test,aes(x=PRP,y=Predicted))+geom_point()+stat_smooth(method="lm",col="red")
