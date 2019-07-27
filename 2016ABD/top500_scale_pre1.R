## 2016年厦门大数据大赛
## 第二题：基于大数据的商品销售预测及关联销售挖掘
## author:Daitu
## 2016-7-20
## 工作：读取预处理后的数据进行探索分析;
## 进行特征工程的构建
## 分析top500预处理后的数据进行销量的预测



## 更改工作文件夹------------------------------------
setwd("/Users/daitu/数据分析/2016ABD")
getwd()

## 加载所需要的包-----------------------------------
library(dplyr)
library(ggplot2)
library(readr)
library(dummies)
library(randomForest)
library(caret)
library(psych)
library(car)
library(MASS)


## 导入数据集####--------------------------------------------
## 销售前500数据
top500group <- read_csv("第二题数据/top500group.csv")
head(top500group)
str(top500group)

# top500item <- read_csv("第二题数据/top500item.csv")
# head(top500item)
# str(top500item)



## 特征工程的构建####-----------------------------------
## 1:去除时间变量,因为我们预测的是月销售量，而时间是每天数据，
## 并且只有18天的数据，所以剔除
top500group$time <- NULL


## 2:将shop_type数据转化为哑变量####-----------------------------
top500group$shop_type <- as.factor(top500group$shop_type)  ## 转化为因子变量
top500group$brand_name <- as.factor(top500group$brand_name)
# dum1 <- as.data.frame(dummy(shop_type))
# colnames(dum1) <- c("TB_JISHI","TB_TMALL")
# top500group2 <- cbind(top500group,dum1)
# ## 剔除shop_type数据
# top500group2$shop_type <- NULL

## 3: 我们的目标是建立预测销售量的模型，
## 在预测的时候，我们应该是得不到销售额数据的，所以该变量不能参与模型的构建
top500group$monthly_sales <- NULL
## 4:针对平均价格这个变量，理论上不难的得到，可以保留

## 5:针对与牌数据，在这里定义为因子变量
# top500group2$brand_name <- as.factor(top500group2$brand_name)
# top500group2$item_number <- as.factor(top500group2$item_number)
# str(top500group2)
# ## 将因变量和自变量分离
# top500group2 <- as.data.frame(top500group2)
# top500group2$monthly_sales_num <- as.numeric(top500group2$monthly_sales_num)
# x_data <- as.matrix(top500group2[,-4])
# y_data <- as.vector(top500group2[,4])
# ## 查看需要预测的变量的分布情况
# hist(y_data,breaks = 200,freq = FALSE)
# VIM::aggr(top500group2)
## 6:构建"assessment_num","shop_num","avg_price","like_num"变量取对数的自变量
## 首先 ＋1 然后在取log
logadd1 <- function(x) log(x+1)
logdata <- apply(top500group[c("assessment_num","shop_num","avg_price",
                               "like_num")], 2,logadd1)
colnames(logdata) <- c("assessment_num_log","shop_num_log","avg_price_log",
                   "like_num_log")

top500group <- cbind(top500group,logdata)

## 7:构建"assessment_num","shop_num","avg_price","like_num"变量平方的自变量
squarefun <- function(x) x*x
squdata <- apply(top500group[c("assessment_num","shop_num","avg_price",
                               "like_num")], 2,squarefun)
colnames(squdata) <- c("assessment_num_2","shop_num_2","avg_price_2",
                    "like_num_2")

top500group <- cbind(top500group,squdata)

## 构建多元线性回归模型一####-----------------------
## 该模型使用c("assessment_num","monthly_sales_num","shop_num",
## "avg_price","monthly_sales","like_num") 5个自变量构建
lm1data <- top500group[,4:16]
lm1.modle <- lm(formula = monthly_sales_num~.,data = lm1data)
summary(lm1.modle)
## 该模型的方程和可决定系数都是显著的但是 Adjusted R-squared:  0.2286 
## 调整的可决定系数偏小
par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0),family = "STKaiti")
plot(lm1.modle)
## 可以看出，残差并不是服从正态分布
anova(lm1.modle)  
car::vif(lm1.modle)
## 可以认为不存在多重共线性


## 模型二引入虚拟变量销售方式进行多元线性回归####-------------------
lm2data <- top500group[,3:16]
lm2data$shop_type <- as.factor(lm2data$shop_type)
lm2.model <- lm(formula = monthly_sales_num~shop_type*(
  assessment_num + shop_num + avg_price + like_num) + 
    assessment_num_log + shop_num_log + avg_price_log + like_num_log +
    assessment_num_2 + shop_num_2 + avg_price_2 + like_num_2,
  data = lm2data)
summary(lm2.model)
lm2.model.s <- step(lm2.model)
summary(lm2.model.s)
## Adjusted R-squared:  0.2322 
par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0),family = "STKaiti")
plot(lm2.model)

## top500group2 :数据的销售量进行预测####----------------------------
## 先使用随机森林查看变量的重要性####--------------
set.seed(131)
rf_data <- top500group[,3:16]
scal_rf <- randomForest(formula = monthly_sales_num ~.,mtry = 5,
                        data = rf_data, ntree=100,importance = TRUE)

scal_rf

## 查看变量的重要性
importance(scal_rf)
varImpPlot(scal_rf,type = 1) # mean decrease in accuracy

varImpPlot(scal_rf,type = 2) # mean decrease in node impuriz



## 商品销量预测回归模型3
selcol <- c("assessment_num_log","assessment_num_2","like_num","like_num_2",
            "like_num_log","shop_num","shop_num_2","shop_num_log","shop_type",
            "monthly_sales_num")
lm3data <- top500group[selcol]
lm3.model <- lm(formula = monthly_sales_num~ . ,data = lm3data)
summary(lm3.model)



