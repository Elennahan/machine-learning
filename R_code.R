# set the path, read data
setwd("D:/project/logistic_model")
data <- read.csv("D:/project/logistic_model/heloc_dataset_v1.csv")

# 提取自变量与因变量，并将因变量转为数值型因子变量类型
y <- ifelse(as.integer(data$RiskPerformance) == 1, 0, 1)
x <- data[,-1]
num <- length(y)

# buid the randon sub sample as the training set
split <- sample(num, num*(2/3))
x_train <- x[split,]
y_train <- y[split]
train_set <- cbind(x_train, y_train)

# build the test set
test_set <- data[-split,]
x_test <- x[-split,]
y_original <- y[-split]

# build our model by the training set
logistic <- glm(y_train  ~ . , data = train_set, family = binomial(link="logit"))
summary(logistic)

# predict with our model
y_test <- predict(logistic, x_test)
y_test1 <- ifelse(y_test <= 0.5, 0, 1)
