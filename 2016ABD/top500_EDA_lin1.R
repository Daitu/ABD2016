## 2016年厦门大数据大赛
## 第二题：基于大数据的商品销售预测及关联销售挖掘
## author:Daitu
## 2016-7-20
## 工作：读取预处理后的数据进行探索分析;
## 分析top500的数据有什么趋势等，探索性可视化分析


## 更改工作文件夹------------------------------------
setwd("/Users/daitu/数据分析/2016ABD")
getwd()



## 加载所需要的包-----------------------------------
library(VIM)
library(zoo)
library(dplyr)
library(ggplot2)
library(GGally)
library(gridExtra)
library(treemap)
library(d3treeR)
library(readr)


## 导入数据集####--------------------------------------------
## 销售前500数据
load("第二题数据/top500df.RData")
dim(top500df)
table(top500df$day)
top500df <- tbl_df(top500df)
top500df$time <- paste(top500df$year,top500df$month,top500df$day,sep = "/")
## 对数据进行分组后查看
n_distinct(top500df$brand_name)  ## 一共209个品牌
n_distinct(top500df$item_number)  ## 一共出现了949个商品


top500gu <- top500df %>%
  dplyr::group_by(brand_name,item_number,time,shop_type) %>%
  summarise(assessment_num = mean(assessment_num),
            monthly_sales_num = mean(monthly_sales_num),
            shop_num = mean(shop_num),
            avg_price = mean(avg_price),
            monthly_sales = mean(monthly_sales),
            like_num = mean(like_num)) 



## 对变量的缺失值探索性分析 ####--------------------------------
VIM::aggr(top500gu,prop = TRUE)

## 1:可以看出大约有10%的店铺销售方式数据缺失,与之相对应的销售店铺的数目均为0
## 可以发现店铺销售方式缺失的数据的缺失值主要集中在18，19，20这三天
## 对于店铺销售方式可以使用它后面的数据来填补缺失值，这样能减少错误的概率
## 对于的销售店铺的数目，我们也可以使用他后面的数据来填补0值
## 2:平均单价数据和月销售额数据大约有15%的数据缺失，并且还是同时缺失

## 首先将缺失值对应的0元素转化为缺失值
top500gu$shop_num[is.na(top500gu$shop_type)] <- NA
## 使用后面的数值来填补这两列的缺失值
top500gu[,c(4,7)] <- apply(top500gu[,c(4,7)], 2, na.locf)

## 查看缺失值的情况
VIM::aggr(top500gu,prop = TRUE)
## 只剩下平均价格和月销售额存在缺失值，
## 并且该类性的缺失值是针对某一种商品的整体数据缺失,
## 对于这种类型的缺失值,几乎无法进行缺失值的插补处理
## 所以决定删除这些数据
top500gu <- top500gu %>%
  dplyr::filter(!is.na(avg_price))
dim(top500gu)

## 查看缺失值的情况
VIM::aggr(top500gu)
## 可以发现已经不存在缺失值了


## top500数据的可视化探索####------------------------------
## 添加数据：月销量 ＝ 月销售额 ／ 平均价格
top500gu <- top500gu %>%
  mutate(number = round(monthly_sales / avg_price)) %>%
  arrange(desc(monthly_sales))

## 查看原始数据中的销量 和 计算出来的月销量之间的差异
ggplot(data = top500gu, aes(x = monthly_sales_num , y = number)) +
  theme_grey(base_family = "STKaiti") +
  geom_point(aes(colour = shop_type),size = 1) +
  theme(legend.position = "top") +
  geom_abline(intercept = 1,size = 0.2) +
  labs(x = "原始月销售量",y = "计算月销售量",title = "月销售量检查")

## 从上面的图像可以看出两个销量并不完全一样的，但是总体趋势还是在一条直线上的
## 并且：（1）：TB_TMALL类型的店铺的月销售量更高，
## (2) :TB_JISHI的销量普遍偏低的
## 还是无法验证数据中的销量数据是否正确，假设销量数据是正确的
top500gu$number <- NULL   #删除生成的销售数据



#  top500gu中抽取50种商品的销量的盒形图
item <- sample(unique(top500gu$item_number),size = 50)  # 从所有的商品中选择50个
ggplot(top500gu[top500gu$item_number %in% item,]) +
  theme_bw(base_family = "STKaiti") +
  geom_boxplot(aes(x = item_number,y = monthly_sales_num,fill = shop_type))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "商品",y = "月销量",title = "商品销量盒形图")

## 可以查看商品销量的差别


#  top500gu中抽取50个品牌的销量的盒形图
brand <- sample(unique(top500gu$brand_name),size = 50)

ggplot(top500gu[top500gu$brand_name %in% brand,]) +
  theme_bw(base_family = "STKaiti") +
  geom_boxplot(aes(x = brand_name,y = monthly_sales_num,fill = shop_type))+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "品牌",y = "月销量",title = "品牌销量盒形图")

## 跨度较大，是由于不同的商品引起的

##  将数据按照商品分组，对数据进行可视化#### ----------------------------
top500gu$shop_num <- as.numeric(top500gu$shop_num)
top500_item <- top500gu %>%
  dplyr::group_by(item_number,brand_name,shop_type) %>% # 确定商品的唯一性
  summarise(assessment_num = mean(assessment_num),
            monthly_sales_num = mean(monthly_sales_num),
            shop_num = mean(shop_num),
            avg_price = mean(avg_price),
            monthly_sales = mean(monthly_sales),
            like_num = mean(like_num)) %>%
  arrange(desc(monthly_sales_num))
dim(top500_item)
as.data.frame(head(top500_item))
## 有902件产品，上榜国top500


## 绘制平行坐标图进行分析 ---------------------------------------
lab_x <- c("评价数目","月销量","销售店铺数","单价","月销售额","喜欢数目")
p1 <- ggparcoord(top500_item,columns = c(4:dim(top500_item)[2]),
           groupColumn = "shop_type") +
  theme_gray(base_family = "STKaiti") +
  theme(legend.position = "top") + 
  scale_x_discrete(labels = lab_x) +
  labs(x= "",y = "标准化数值",title = "top500平行坐标图")


p2 <- ggparcoord(top500_item,columns = c(4:dim(top500_item)[2]),
           groupColumn = "shop_type",scale = "uniminmax") +
  theme_gray(base_family = "STKaiti") +
  theme(legend.position = "top") + 
  scale_x_discrete(labels = lab_x) +
  labs(x= "",y = "单位区间数值",title = "top500平行坐标图")

grid.arrange(p1,p2,nrow = 2)

## 
## 查看散点图矩阵----------------------------------
ggscatmat(data = as.data.frame(top500_item),columns = c(4:dim(top500_item)[2]),
          color = "shop_type",corMethod = "pearson") +
  theme_bw(base_family = "STKaiti") +
  theme(legend.position = "top") + 
  ggtitle("散点图矩阵")


## 查看商品的销售额和单价的关系####-----------------
t1 <- treemap(top500_item,index = c("brand_name","item_number"),vSize = "monthly_sales",
              vColor = "avg_price",type="value",fontfamily.title = "STKaiti", 
              fontfamily.labels = "STKaiti",fontfamily.legend = "STKaiti",
              title = "产品的月销售额", title.legend = "平均售价")

d3tree( t1,rootname = "品牌～商品号＋color = 单价" )


## 查看商品的销售量和单价的关系----------------
t1 <- treemap(top500_item,index = c("brand_name","item_number"),vSize = "monthly_sales_num",
              vColor = "avg_price",type="value",fontfamily.title = "STKaiti", 
              fontfamily.labels = "STKaiti",fontfamily.legend = "STKaiti",
              title = "商品的月销售量", title.legend = "平均售价")

d3tree( t1,rootname = "品牌～商品号＋color = 单价" )




## 对商品和店铺类型分组，确定产品的唯一性#### ----------------------------------
top500brand <- top500_item %>%
  dplyr::group_by(brand_name,shop_type) %>%
  summarise(item_sum = n_distinct(item_number), #该品牌有多少热销品
            assessment_sum = sum(assessment_num),# 总评价数目
            monthly_sales_sum = sum(monthly_sales_num),# 平均月总销售总量
            shop_sum = sum(shop_num), #销售店铺总数
            monthly_sales.sum = sum(monthly_sales), #平均月销售总额
            avg_price_me = mean(avg_price),# 平均售价
            like_num_sum = sum(like_num)) %>% #喜欢的数量
  mutate(brand_type = paste(brand_name,shop_type,sep = "~")) %>%
  arrange(desc(monthly_sales.sum))
dim(top500brand)
as.data.frame(head(top500brand))
n_distinct(top500brand$brand_name)

## 查看不同品牌的平均月销售总额的树图 ####
## 销售总额与单价的关系---------------------------------
t1 <- treemap(top500brand,index = c("shop_type","brand_name"),vSize = "monthly_sales.sum",
        vColor = "avg_price_me",type="value",fontfamily.title = "STKaiti", 
        fontfamily.labels = "STKaiti",fontfamily.legend = "STKaiti",
        title = "品牌的月销售额", title.legend = "平均售价")

d3tree( t1,rootname = "品牌～销售方式＋color = 单价" )

d3tree2( t1,rootname = "品牌～销售方式" )

## 销售总额与受喜欢的程度关系---------------------------------
t2 <- treemap(top500brand,index = c("shop_type","brand_name"),vSize = "monthly_sales.sum",
              vColor = "like_num_sum",type="value",fontfamily.title = "STKaiti", 
              fontfamily.labels = "STKaiti",fontfamily.legend = "STKaiti",
              title = "品牌的月销售额", title.legend = "喜欢数量")

d3tree( t2,rootname = "品牌～销售方式＋color = 喜欢数量" )

d3tree2( t2,rootname = "品牌～销售方式" )


## 查看不同品牌的平均月销售数量的树图 ####
## 销售总额与单价的关系---------------------------------
t1 <- treemap(top500brand,index = c("shop_type","brand_name"),vSize = "monthly_sales_sum",
              vColor = "avg_price_me",type="value",fontfamily.title = "STKaiti", 
              fontfamily.labels = "STKaiti",fontfamily.legend = "STKaiti",
              title = "品牌的月销售量", title.legend = "平均售价")

d3tree( t1,rootname = "品牌～销售方式＋color = 单价" )

d3tree2( t1,rootname = "品牌～销售方式" )

## 销售总额与受喜欢的程度关系---------------------------------
t2 <- treemap(top500brand,index = c("shop_type","brand_name"),vSize = "monthly_sales_sum",
              vColor = "like_num_sum",type="value",fontfamily.title = "STKaiti", 
              fontfamily.labels = "STKaiti",fontfamily.legend = "STKaiti",
              title = "品牌的月销售额", title.legend = "喜欢数量")

d3tree( t2,rootname = "品牌～销售方式＋color = 喜欢数量" )

d3tree2( t2,rootname = "品牌～销售方式" )




## 将整理号的数据保存下来备用####-----------------------------
write.csv(top500gu,file = "第二题数据/top500group.csv",fileEncoding = "UTF-8",
          quote = FALSE,row.names = FALSE)

write_csv(top500_item,path = "第二题数据/top500item.csv")
