## 2016年厦门大数据大赛
## 第二题：基于大数据的商品销售预测及关联销售挖掘
## author:Daitu
## 2016-7-12
## 从数据集中针对店铺，进行关联销售挖掘
## 店铺的相似性


## 更改工作文件夹------------------------------------
setwd("/Users/daitu/数据分析/2016ABD")
getwd()
rm(list = ls());gc()


## 加载所需要的包-----------------------------------
library(dplyr)
library(ggplot2)
library(arules)
library(gridExtra)
library(stringr)


# sessionInfo()

## 第一步：读取数据####-------------------------------------------
# item_id 每一个商品链接的独有的ID   ----字符串-------
# item_number 商品的款号          ----字符串-------  
# shop_id 销售这件商品的店铺ID        ----字符串-------
# shop_type 销售这件商品的店铺类型，分为TB_TMALL与TB_JISHI两种   ----字符串-------
# brand_name 商品的品牌名                    ----字符串-------
# item_name 商品名(商品标题)               ----字符串-------
# price 该商品的销售价格(此处对各sku取均值)
# tag_price 商品的标签价
# monthly_sales_num 商品月销量
# assessment_num 商品评价数
# monthly_sales 月销售额  -----价格乘以销量-----

## 商品销售
load("第二题数据/item_fact.RData")
head(item_fact)
summary(item_fact)

## 查看多少商品有销量
sum(item_fact$monthly_sales_num > 0)  #约有305218商品有销售出去的纪录
## 只对有销售纪录的商品进行关联分析
item_fact <- item_fact[which(item_fact$monthly_sales_num > 0),]

## 商品的数目
iten_number <- as.data.frame(table(item_fact$item_number))  
dim(iten_number)  ## 127137 种商品编号

# ## 只分析出现频次大于1的商品
# iten_number <- iten_number[iten_number$Freq>1,]
# dim(iten_number)   ## 39932 种商品编号

bb <- item_fact[item_fact$item_number == "00001",]
## 商品编号无法唯一定义商品，所以要剔除商品编号异常的数据，它们的存在没有帮组
## 提出商品编号带有：“－”，“＊”，“＃”，“。”等，
## 即只保留开始和结尾都是数字或者为字母的商品
iten_number <- iten_number[grepl("^[[:alnum:]]+[A-Za-z0-9\\+-]+[[:alnum:]]$",iten_number$Var1),]
## 将商品编号只有0、1的数据删除，因为不能唯一定位
iten_number <- iten_number[!grepl("^[0-1]+[0]+[0-1]$",iten_number$Var1),]

item_fact <- item_fact[item_fact$item_number %in% iten_number$Var1,]
dim(item_fact)   # 还有283597 条纪录

n_distinct(item_fact$shop_id) #  16858个店铺 

n_distinct(item_fact$brand_name) #  3939 种品牌


## 查看每个商铺大约有多少件商品出售####-------------------
shop <- item_fact %>%
  dplyr::group_by(shop_id) %>%   ## 将数据按照店铺的ID进行分组
  dplyr::summarise(n_item = n_distinct(item_number)) ## 该店铺共销售了多少种商品
summary(shop)

# shop <- as.data.frame(table(item_fact$shop_id))
# summary(shop$Freq)
## 销售商品数目的分布
p1 <- ggplot(shop,aes(n_item)) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(binwidth = 20,color = "firebrick",fill = "red",alpha = 0.5)+
  labs(x = "销售商品数目(件)",y = "店铺的数目(家)",title = "店铺销售商品数目分布")


p2 <- ggplot(shop[shop$n_item<=500,],aes(n_item)) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(binwidth = 5,color = "firebrick",fill = "red",alpha = 0.5)+
  labs(x = "销售商品数目(<=500件)",y = "店铺的数目(家)",title = "店铺销售商品数目分布")

gridExtra::grid.arrange(p1,p2,nrow = 2)

## 大多数上架的销售商品数目小于100件


## 将数据转化为适合关联规则的形式####----------------------------------


## 店铺－商品数据
arule_data <- item_fact[c("shop_id","item_number")]
str(arule_data)

## 将数据保存为便于csv文件
write.csv(arule_data,file = "第二题数据/店铺_商品.txt",row.names = FALSE)

## 店铺－品牌数据
arule_brand <- item_fact[c("shop_id","brand_name")]
str(arule_brand)
aa <- as.data.frame(table(arule_brand$brand_name))
bb <- item_fact[item_fact$brand_name == "0",]
## 虽然有些品牌的名称是不正确的，但是我们能最后的到在规则中不考虑这些错误的品牌
## 将数据保存为便于csv文件
write.csv(arule_brand,file = "第二题数据/店铺_品牌.txt",row.names = FALSE)



## 对店铺和销售的商品数据进行关联规则分析####-----------------------------
## 读取关联分析数据
arudata <- read.transactions(file = "第二题数据/店铺_商品.txt",format = "single",
                             sep = ",",cols = c(1,2),rm.duplicates = TRUE)

## 查看数据
summary(arudata)
inspect(arudata[1:2])

## 可视化商品的频率图
## itemFrequency(item_arules)
par(family = "STKaiti",cex = 0.75)
itemFrequencyPlot(arudata,type = "absolute",topN = 50,main = "商品频率图top50",
                  xlab = "商品编号",ylab = "频数",col = "LightSkyBlue")
par(family = "STKaiti",cex = 0.75)
itemFrequencyPlot(arudata,type = "relative",topN = 50,main = "商品频率图top50",
                  xlab = "商品编号",ylab = "频率",col = "LightSkyBlue")



## 可以看出出现次数最多的商品大约出现了200多次，并且top50的商品出现次数均大于50次，
## 但是商品出现的频率均偏低。

## 查看店铺和商品的稀疏矩阵图像
par(family = "STKaiti",cex = 0.75,mai = c(0,1,0,1)) 
arules::image(arudata,xlab = "item number",ylab = "shop number",main = "shops--items")


# ## 计算各个项集的频率
# fsets <- eclat(item_arules,parameter = list(support = 0.005,maxlen = 10))
# 
# inspect(sort(fsets,by = "support"))

## 训练模型
myrule <- apriori(data = arudata,
                 parameter = list(support = 0.001,
                                  confidence = 0.9,
                                   minlen = 2))


summary(myrule)

inspect(myrule[1:5])

## 将规则按照支持度排序
sortsupport <- arules::sort(myrule,decreasing = TRUE,by = "support")
inspect(sortsupport[1:10])

## 将规则按照置信度排序
sortconf <- arules::sort(myrule,decreasing = TRUE,by = "confidence")
inspect(sortconf[1:10])

## 将规则按照提升度排序
sortlift <- arules::sort(myrule,decreasing = TRUE,by = "lift")
inspect(sortlift[1:10])

## 将关联规则数据转化为数据表格的形式
item_shop_ruledf <- as(myrule,"data.frame")

## 我门可以发现，我门发现的2000多条的规则中，置信度非常的接近1
## 并且提升度也很大，这时因为很多店铺销售的东西都很相似

utils::write.csv(item_shop_ruledf,file = "第二题数据/店铺_商品_关联结果.txt",
                 row.names = FALSE,quote = FALSE,fileEncoding = "UTF-8")



## 对店铺销售的品牌进行关联规则的挖掘####--------------------
## 读取关联分析数据
arudata <- read.transactions(file = "第二题数据/店铺_品牌.txt",format = "single",
                             sep = ",",cols = c(1,2),rm.duplicates = TRUE)

## 查看数据
summary(arudata)
inspect(arudata[1:5])

## 可视化商品的频率图
## itemFrequency(item_arules)
par(family = "STKaiti",cex = 0.75)
itemFrequencyPlot(arudata,type = "absolute",topN = 30,main = "品牌频率图top30",
                  xlab = "品牌名",ylab = "频数",col = "LightSkyBlue")
par(family = "STKaiti",cex = 0.75)
itemFrequencyPlot(arudata,type = "relative",topN = 30,main = "品牌频率图top30",
                  xlab = "品牌名",ylab = "频率",col = "LightSkyBlue")



## 可以看出出现次数最多的商品大约出现了4000多次，说明有很多店铺，销售这个品牌的产品，
## 并且top7的品牌出现次数均大于1000次，
## 品牌出现的频率偏高。

## 查看店铺和商品的稀疏矩阵图像
par(family = "STKaiti",cex = 0.75) 
arules::image(arudata,xlab = "brand number",ylab = "shop number",main = "shops--brands")


## 训练模型
myrule <- apriori(data = arudata,
                  parameter = list(support = 0.005,
                                   confidence = 0.5,
                                   minlen = 1))


summary(myrule)

inspect(myrule[1:10])

## 将规则按照支持度排序
sortsupport <- arules::sort(myrule,decreasing = TRUE,by = "support")
inspect(sortsupport[1:15])

## 将规则按照置信度排序
sortconf <- arules::sort(myrule,decreasing = TRUE,by = "confidence")
inspect(sortconf[1:15])

## 将规则按照提升度排序
sortlift <- arules::sort(myrule,decreasing = TRUE,by = "lift")
inspect(sortlift[1:15])

## 将关联规则数据转化为数据表格的形式
brand_shop_ruledf <- as(myrule,"data.frame")

utils::write.csv(brand_shop_ruledf,file = "第二题数据/店铺_品牌_关联结果.txt",
          row.names = FALSE,quote = FALSE,fileEncoding = "UTF-8")

