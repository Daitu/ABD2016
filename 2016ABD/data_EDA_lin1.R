## 2016年厦门大数据大赛
## 第二题：基于大数据的商品销售预测及关联销售挖掘
## author:Daitu
## 2016-7-12
## 工作：读取预处理后的数据进行探索分析;数据可视化

## 更改工作文件夹------------------------------------
setwd("/Users/daitu/数据分析/2016ABD")
getwd()
rm(list = ls());gc()


## 加载所需要的包-----------------------------------
library(stringr)
library(data.table)
library(dplyr)
library(VIM)
library(ggplot2)
library(gridExtra)
library(GGally)
library(psych)
library(corrplot)


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
# as.data.frame(item_fact[item_fact$price > 30000 ,])

# ## 销售前500数据
# load("第二题数据/top500df.RData")
# head(top500df)
## 读取店铺的信息数据####
filename4 <- "第二题数据/shop_dsr."
shop_dsr <- fread(filename4,header  = TRUE,sep = "\t",
                  colClasses  = rep(c("character","numeric"),times = c(2,3)))

head(shop_dsr)
dim(shop_dsr)   #只有19190个店铺名
shop_dsr <- tbl_df(shop_dsr)
## 将数据按照店铺分组---------------------------------------
shop <- item_fact %>%
  dplyr::group_by(shop_id) %>%   ## 将数据按照店铺的ID进行分组
  dplyr::summarise(n_item = n_distinct(item_id),   ## 该店铺共销售了多少种商品
            brand_number = n_distinct(brand_name), ## 该店铺共销售了几种品牌的商品
            monthly_shop_sum = sum(monthly_sales_num), ## 该店铺的月销量
            monthly_sales_sum = sum(monthly_sales),   ## 该店铺的月销售额
            assessment_sum = sum(assessment_num),     ## 该店铺的评价总数
            tag_price_mean = mean(tag_price),   # 该店铺商品的平均标签价
            shop_type = unique(shop_type)) %>%  ## 店铺的销售方式
  mutate(price_mean = monthly_sales_sum / monthly_shop_sum)  ## 该店铺商品的平均售价)
dim(shop)    # 有 23531  家店铺
summary(shop)

## 可以发现平均销售价格会出现 0/0 为缺失值的情况，针对这种情况,将平均销售价格定位0元。
shop$price_mean[is.na(shop$price_mean)] <- 0
summary(shop)
## ----------------------------------------------------------

## 将 shop_dsr和shop数据连接
sum(shop$shop_id %in% shop_dsr$shop_id)
## 我们可以发现这两个数据集中，只有14977个店铺是相同的，其他则是不同的

# aa <- left_join(shop_dsr,shop,by = "shop_id")


## 对上面的商铺数据进行可视化探索分析
## 查看两种销售方式的对比 -------------------------
table(shop$shop_type)

table(shop$shop_type) / dim(shop)[1]
## 可以发现销售方式为TB_JISHI占据百分比大于97%
## 销售方式为TB_TMALL 占据百分比不到3%


## 商铺销售商品数目的可视化  ---------------------------------
summary(shop$n_item)
length(which(shop$n_item > 1000)) 
## 可以发现有一个店铺销售的商品数目为5671件，约有65个店铺销售的商品数目多于1000
## 查看这些数据
# data.frame(shop[which(shop$n_item > 1000),])

p1 <- ggplot(shop) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(n_item),binwidth = 5,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  labs(x = "销售商品数目(件)",y = "店铺的数目(家)",title = "店铺销售商品数目分布")+
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(shop[which(shop$n_item <= 250),]) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(n_item),binwidth = 5,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  labs(x = "销售商品数目(<=250件)",y = "店铺的数目(家)",title = "店铺销售商品数目分布")+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(p1,p2,nrow = 2)

## 可以看出在两种销售方式店铺中，主要销售方式为－TB_JISHI，店铺数目占据大部分，
## 但是这种销售规模较小，大多数销售商品数目小于250
## 而对于TB_TMALL，店铺数量不多，但是销售商品数目多于250的均为这种销售方式的店铺

## 商铺销售商品的品牌数目可视化------------------------------------
summary(shop$brand_number)
length(which(shop$brand_number > 20)) 
## 可以发现有店铺销售的商品品牌数目为57个品牌，约有116个店铺销售的商品品牌数目多于20

p1 <- ggplot(shop) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(brand_number),binwidth = 1,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  labs(x = "销售商品品牌数(个)",y = "店铺的数目(家)",title = "店铺销售商品品牌数分布")+
  theme(plot.title = element_text(hjust = 0.5))

p2 <- ggplot(shop[which(shop$n_item <= 20),]) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(brand_number),binwidth = 1,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  labs(x = "销售商品品牌数(<=20个)",y = "店铺的数目(家)",title = "店铺销售商品品牌数分布")+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(p1,p2,nrow = 2)

## 在销售商品占据的品牌数目上，两种方式的店铺数量的分布大致是相同的

## 店铺的月销售额可视化分析---------------------------------
summary(shop$monthly_sales_sum)
# boxplot(shop$monthly_sales_sum)

ms1 <- ggplot(shop) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(monthly_sales_sum),bins = 100,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  scale_x_continuous(labels = function(x) paste(x/1e6,"m",sep = "")) +
  labs(x = "月销售额(元)",y = "店铺的数目(家)",title = "店铺月销售额分布")+
  theme(plot.title = element_text(hjust = 0.5))

ms2 <- ggplot(shop[shop$monthly_sales_sum <= 10000,]) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(monthly_sales_sum),bins = 100,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  #scale_x_continuous(labels = function(x) paste(x/10e3,"k",sep = "")) +
  labs(x = "月销售额(<=1e4元)",y = "店铺的数目(家)",title = "店铺月销售额分布")+
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(ms1,ms2,nrow = 2)
## 可以看出 销售方式为TB_TMALL的店铺的月销售额更大，月销售额有超过90,000,000元的
## 只有很少一部分店铺的月销售额少于10000元
##  销售方式为TB_JISHI 的店铺，月销售额大多数不超过1000元

## 店铺的商品月销量数据可视化---------------------------------
summary(shop$monthly_shop_sum)
dim(shop[shop$monthly_shop_sum <= 1000,])[1]/dim(shop)[1]
## 月96%的商铺，月销售商品数目小于1000件
summary(shop[shop$monthly_shop_sum <= 1000,]$monthly_shop_sum)

mss1 <- ggplot(shop) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(monthly_shop_sum),bins = 200,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  scale_x_continuous(labels = function(x) paste(x/1e4,"万",sep = "")) +
  labs(x = "商品月销量(个)",y = "店铺的数目(家)",title = "店铺商品月销量分布")+
  theme(plot.title = element_text(hjust = 0.5))
# mss1

mss2 <- ggplot(shop[shop$monthly_shop_sum <= 1000,]) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(monthly_shop_sum),bins = 100,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  #scale_x_continuous(labels = function(x) paste(x/10e3,"k",sep = "")) +
  labs(x = "商品月销量(<=1e3个)",y = "店铺的数目(家)",title = "店铺商品月销量分布")+
  theme(plot.title = element_text(hjust = 0.5))
# mss2
grid.arrange(mss1,mss2,nrow = 2)

## 可以发现整体的趋势是相同的

## 对店铺的评价总数数据可视化---------------------------------
summary(shop$assessment_sum)
dim(shop[shop$assessment_sum <= 2000,])[1]/dim(shop)[1]
## 月95.4%的商铺，评价总数目小于2000个
summary(shop[shop$assessment_sum <= 2000,]$assessment_sum)

as1 <- ggplot(shop) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(assessment_sum),bins = 200,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  scale_x_continuous(labels = function(x) paste(x/1e4,"万",sep = "")) +
  labs(x = "评价总数目(条)",y = "店铺的数目(家)",title = "店铺评价总数目分布")+
  theme(plot.title = element_text(hjust = 0.5))
# as1

as2 <- ggplot(shop[shop$assessment_sum <= 2000,]) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(assessment_sum),bins = 100,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  #scale_x_continuous(labels = function(x) paste(x/10e3,"k",sep = "")) +
  labs(x = "评价总数目(<=2e3条)",y = "店铺的数目(家)",title = "店铺评价总数目分布")+
  theme(plot.title = element_text(hjust = 0.5))
# as2
grid.arrange(as1,as2,nrow = 2)
## 可以发现整体的趋势是相同的


## 店铺商品的平均售价数据可视化---------------------------------
summary(shop$price_mean)
length(which(shop$price_mean > 3000))
## 平均售价大于3千的店铺有64个
mean_price_if <- left_join(shop[which(shop$price_mean > 3000),],shop_dsr,by = "shop_id")
as.data.frame(mean_price_if)

dim(shop[shop$price_mean <= 3000,])[1]/dim(shop)[1]
## 月95.5%的商铺，店铺商品的平均销售价格小于3000
summary(shop[shop$price_mean <= 3000,]$price_mean)

pm1 <- ggplot(shop) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(price_mean),bins = 200,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free_y") +
  scale_x_continuous(labels = function(x) paste(x/1e3,"k",sep = "")) +
  labs(x = "商品平均售价(元)",y = "店铺的数目(家)",title = "店铺商品平均售价分布")+
  theme(plot.title = element_text(hjust = 0.5))
# pm1

pm2 <- ggplot(shop[shop$price_mean <= 3000,]) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(price_mean),bins = 100,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free_y") +
  #scale_x_continuous(labels = function(x) paste(x/10e3,"k",sep = "")) +
  labs(x = "商品平均售价(<=3e3元)",y = "店铺的数目(家)",title = "店铺商品平均售价分布")+
  theme(plot.title = element_text(hjust = 0.5))
# pm2
grid.arrange(pm1,pm2,nrow = 2)
## 在两种累想上铺的商品平均售价上，可以发现两类上铺的分布几乎是一样的，
## 并不能说明那种类型的店铺出售的商品更高档

## 对商店商品的平均标价 进行可视化分析---------------------------
summary(shop$tag_price_mean)
length(which(shop$tag_price_mean > 3000))
length(which(shop$tag_price_mean > 100000))
## 平均售价大于3千的店铺有236个 ,标价大于10万的有3个
mean_price_if <- left_join(shop[which(shop$tag_price_mean > 100000),],shop_dsr,by = "shop_id")
as.data.frame(mean_price_if)
## 这些类型的店铺军事TB_JISHI，并且约销售额要么没有要么很低
## 说明这些商品的标价很高，但是并没有人去购买，说明这些商品是博人眼球的物品


dim(shop[shop$tag_price_mean <= 3000,])[1]/dim(shop)[1]
## 约98.9%的商铺，商铺商品的平均标价小于3000
summary(shop[shop$tag_price_mean <= 3000,]$tag_price_mean)

pm1 <- ggplot(shop[shop$tag_price_mean > 3000 & shop$tag_price_mean < 100000,]) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(tag_price_mean),bins = 200,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  scale_x_continuous(labels = function(x) paste(x/1e3,"k",sep = "")) +
  labs(x = "商品平均标价(>3e3&<1e5元)",y = "店铺的数目(家)",title = "店铺商品平均标价分布")
# pm1

pm2 <- ggplot(shop[shop$tag_price_mean <= 3000,]) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(tag_price_mean),bins = 100,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free_y") +
  #scale_x_continuous(labels = function(x) paste(x/10e3,"k",sep = "")) +
  labs(x = "商品平均标价(<=3e3元)",y = "店铺的数目(家)",title = "店铺商品平均标价分布")
# pm2
grid.arrange(pm1,pm2,nrow = 2)

## 可以看出两类商店的平均标价的分布存在很明显的差异
## 在高价区，BT_JISHI的商品平均标价较高
## 在低价区，BT_TMALL的商铺比较集中于商品平均标价高的区域

## 对商铺数据进行平行坐标图可视化####--------------------------------
lab_x <- c("销售商品数","销售品牌数","月销量","月销售额","评论数","平均标价","平均售价")
ggparcoord(shop,columns = c(2:7,9),groupColumn = 8,scale = "std") +
  theme_gray(base_family = "STKaiti") +
  theme(legend.position = "top") + 
  scale_x_discrete(labels = lab_x) +
  labs(x= "",y = "标准化后数值",title = "商铺平行坐标图")


ggparcoord(shop,columns = c(2:7,9),groupColumn = 8,scale = "robust") +
  theme_gray(base_family = "STKaiti") +
  theme(legend.position = "top") + 
  scale_x_discrete(labels = lab_x) +
  labs(x= "",y = "Robust后数值",title = "商铺平行坐标图")
## Robust :减去中位数，除以中位数的标准偏差


ggparcoord(shop,columns = c(2:7,9),groupColumn = 8 ,scale = "uniminmax") +
  theme_gray(base_family = "STKaiti") +
  theme(legend.position = "top") + 
  scale_x_discrete(labels = lab_x) +
  labs(x= "",y = "单位区间数值",title = "商铺平行坐标图")

## 从平行坐标图上我们可以看出两类变店铺的差异
## 1: TB_TMALL 的销售商品的数目更多，月销量更高，月销售额更高，评论数目更多
## 2:TB_JISHI  的销售品牌数更多，平均标价更高，平均售价更高


## 分析商铺数据中的数据的相关系数####---------------------------
## 查看散点图
ggscatmat(data = as.data.frame(shop),columns = c(2:7,9),
          color = "shop_type",corMethod = "pearson") +
  theme_bw(base_family = "STKaiti") +
  theme(legend.position = "top") + 
  ggtitle("散点图矩阵")

## 从散点图和相关系数矩阵中可以看出，不同的类别销售方式相关性有差异

## 将数据标准化，然后查看相关系数和散点矩阵图--------------------------------
shop_std <- tbl_df(as.data.frame(apply(shop[,c(2:7,9)], 2, scale)))
shop_std$shop_type <- shop$shop_type
shop_std$shop_id <- shop$shop_id   # 标准化后的商铺数据
head(shop_std)
summary(shop_std)

## 查看散点图
ggscatmat(data = as.data.frame(shop_std),columns = 1:7,
          color = "shop_type",corMethod = "pearson") +
  theme_bw(base_family = "STKaiti") +
  theme(legend.position = "top") + 
  ggtitle("散点图矩阵")
## 好像数据标准化，并不能让那个改变相关系数的大小


shop_cor <- cor(shop[,c(2:7,9)])
shop_cor
# corr.test(shop[,c(2:7,9)])
# corr.p(shop_cor,dim(shop)[1])

lab_x <- c("销售商品数","销售品牌数","月销量","月销售额","评论数","平均标价","平均售价")
colnames(shop_cor) <- lab_x
rownames(shop_cor) <- lab_x
shop_cor
par(family = "STKaiti",mfrow = c(1,1))
corrplot(shop_cor,method = "pie",type = "full",
         mar = c(0, 6, 6, 0),title = "相关系数图")
## 可以发现月销量、月销售额、评论数之间的相关系数较大
## 销售商品数与：月销量、月销售额、评论数相关夜超过了0.5



## 分析店铺的信息数据####--------------------------------------------
head(shop_dsr)
summary(shop_dsr)
shop_dsrj <- left_join(shop_dsr,shop[,c(1,8)],by = "shop_id")
## 可以发现有些店铺是无法表明销售类型的，需要把这些数据删除
shop_dsrj <- dplyr::filter(shop_dsrj,!is.na(shop_type))

## 对数据进行可视化--------------------------------------

mas <- ggplot(shop_dsrj,aes(mas,fill = shop_type)) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(y = ..density..),binwidth = 0.1,alpha = 0.6,
                 position='fill',color="firebrick") +
  theme(legend.position = "right") 

# mas


sas <- ggplot(shop_dsrj,aes(sas,fill = shop_type)) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(y = ..density..),binwidth = 0.1,alpha = 0.6,
                 position='fill',color="firebrick") +
  theme(legend.position = "right") 

# sas

cas <- ggplot(shop_dsrj,aes(cas,fill = shop_type)) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(y = ..density..),binwidth = 0.1,alpha = 0.6,
                 position='fill',color="firebrick") +
  theme(legend.position = "right") 

# cas

grid.arrange(mas,sas,cas,nrow = 3,top = 0,bottom = 0)

## 可以发现，TB_TMALL 没有低分



## ---------------------------------------------------------------------
## ---------------------------------------------------------------------
## 将两个商店的数据结合，只分析14977多家店铺的数据####----------------
shop <- dplyr::left_join(shop,shop_dsr,by = "shop_id") %>%
  dplyr::filter(!is.na(shop_name)) %>%
  dplyr::arrange(desc(monthly_sales_sum))
dim(shop)
as.data.frame(head(shop))



## 查看两种销售方式的对比 -------------------------
table(shop$shop_type)

table(shop$shop_type) / dim(shop)[1]
## 可以发现销售方式为TB_JISHI占据百分比大于96.5%
## 销售方式为TB_TMALL 占据百分比不到3.4%


## 商铺销售商品数目的可视化  ---------------------------------
summary(shop$n_item)
length(which(shop$n_item > 1000)) 
## 可以发现有一个店铺销售的商品数目为5671件，约有65个店铺销售的商品数目多于1000
## 查看这些数据
# data.frame(shop[which(shop$n_item > 1000),])

p1 <- ggplot(shop) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(n_item),binwidth = 5,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  labs(x = "销售商品数目(件)",y = "店铺的数目(家)",title = "店铺销售商品数目分布")

p2 <- ggplot(shop[which(shop$n_item <= 250),]) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(n_item),binwidth = 5,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  labs(x = "销售商品数目(<=250件)",y = "店铺的数目(家)",title = "店铺销售商品数目分布")

grid.arrange(p1,p2,nrow = 2)

## 可以看出在两种销售方式店铺中，主要销售方式为－TB_JISHI，店铺数目占据大部分，
## 但是这种销售规模较小，大多数销售商品数目小于250
## 而对于TB_TMALL，店铺数量不多，但是销售商品数目多于250的均为这种销售方式的店铺

## 商铺销售商品的品牌数目可视化------------------------------------
summary(shop$brand_number)
length(which(shop$brand_number > 20)) 
## 可以发现有店铺销售的商品品牌数目为85个品牌，约有337个店铺销售的商品品牌数目多于20

p1 <- ggplot(shop) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(brand_number),binwidth = 1,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  labs(x = "销售商品品牌数(个)",y = "店铺的数目(家)",title = "店铺销售商品品牌数分布")

p2 <- ggplot(shop[which(shop$n_item <= 20),]) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(brand_number),binwidth = 1,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  labs(x = "销售商品品牌数(<=20个)",y = "店铺的数目(家)",title = "店铺销售商品品牌数分布")

grid.arrange(p1,p2,nrow = 2)

## 在销售商品占据的品牌数目上，两种方式的店铺数量的分布大致是相同的

## 店铺的月销售额可视化分析---------------------------------
summary(shop$monthly_sales_sum)
# boxplot(shop$monthly_sales_sum)

ms1 <- ggplot(shop) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(monthly_sales_sum),bins = 100,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  scale_x_continuous(labels = function(x) paste(x/1e6,"m",sep = "")) +
  labs(x = "月销售额(元)",y = "店铺的数目(家)",title = "店铺月销售额分布")

ms2 <- ggplot(shop[shop$monthly_sales_sum <= 10000,]) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(monthly_sales_sum),bins = 100,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  #scale_x_continuous(labels = function(x) paste(x/10e3,"k",sep = "")) +
  labs(x = "月销售额(<=1e4元)",y = "店铺的数目(家)",title = "店铺月销售额分布")

grid.arrange(ms1,ms2,nrow = 2)
## 可以看出 销售方式为TB_TMALL的店铺的月销售额更大，月销售额有超过90,000,000元的
## 只有很少一部分店铺的月销售额少于10000元
##  销售方式为TB_JISHI 的店铺，月销售额大多数不超过1000元

## 店铺的商品月销量数据可视化---------------------------------
summary(shop$monthly_shop_sum)
dim(shop[shop$monthly_shop_sum <= 1000,])[1]/dim(shop)[1]
## 月96%的商铺，月销售商品数目小于1000件
summary(shop[shop$monthly_shop_sum <= 1000,]$monthly_shop_sum)

mss1 <- ggplot(shop) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(monthly_shop_sum),bins = 200,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  scale_x_continuous(labels = function(x) paste(x/1e4,"万",sep = "")) +
  labs(x = "商品月销量(个)",y = "店铺的数目(家)",title = "店铺商品月销量分布")
# mss1

mss2 <- ggplot(shop[shop$monthly_shop_sum <= 1000,]) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(monthly_shop_sum),bins = 100,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  #scale_x_continuous(labels = function(x) paste(x/10e3,"k",sep = "")) +
  labs(x = "商品月销量(<=1e3个)",y = "店铺的数目(家)",title = "店铺商品月销量分布")
# mss2
grid.arrange(mss1,mss2,nrow = 2)

## 可以发现整体的趋势是相同的

## 对店铺的评价总数数据可视化---------------------------------
summary(shop$assessment_sum)
dim(shop[shop$assessment_sum <= 2000,])[1]/dim(shop)[1]
## 月95.4%的商铺，评价总数目小于2000个
summary(shop[shop$assessment_sum <= 2000,]$assessment_sum)

as1 <- ggplot(shop) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(assessment_sum),bins = 200,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  scale_x_continuous(labels = function(x) paste(x/1e4,"万",sep = "")) +
  labs(x = "评价总数目(条)",y = "店铺的数目(家)",title = "店铺评价总数目分布")
# as1

as2 <- ggplot(shop[shop$assessment_sum <= 2000,]) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(assessment_sum),bins = 100,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  #scale_x_continuous(labels = function(x) paste(x/10e3,"k",sep = "")) +
  labs(x = "评价总数目(<=2e3条)",y = "店铺的数目(家)",title = "店铺评价总数目分布")
# as2
grid.arrange(as1,as2,nrow = 2)
## 可以发现整体的趋势是相同的


## 店铺商品的平均售价数据可视化---------------------------------
summary(shop$price_mean)
length(which(shop$price_mean > 3000))
## 平均售价大于3千的店铺有60个
mean_price_if <- left_join(shop[which(shop$price_mean > 3000),],shop_dsr,by = "shop_id")
# as.data.frame(mean_price_if)

dim(shop[shop$price_mean <= 3000,])[1]/dim(shop)[1]
## 月95.5%的商铺，店铺商品的平均销售价格小于3000
summary(shop[shop$price_mean <= 3000,]$price_mean)

pm1 <- ggplot(shop) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(price_mean),bins = 200,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free_y") +
  scale_x_continuous(labels = function(x) paste(x/1e3,"k",sep = "")) +
  labs(x = "商品平均售价(元)",y = "店铺的数目(家)",title = "店铺商品平均售价分布")
# pm1

pm2 <- ggplot(shop[shop$price_mean <= 3000,]) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(price_mean),bins = 100,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free_y") +
  #scale_x_continuous(labels = function(x) paste(x/10e3,"k",sep = "")) +
  labs(x = "商品平均售价(<=3e3元)",y = "店铺的数目(家)",title = "店铺商品平均售价分布")
# pm2
grid.arrange(pm1,pm2,nrow = 2)
## 在两种累想上铺的商品平均售价上，可以发现两类上铺的分布几乎是一样的，
## 并不能说明那种类型的店铺出售的商品更高档

## 对商店商品的平均标价 进行可视化分析---------------------------
summary(shop$tag_price_mean)
length(which(shop$tag_price_mean > 3000))
length(which(shop$tag_price_mean > 100000))
## 平均售价大于3千的店铺有260个 ,标价大于10万的有4个
mean_price_if <- left_join(shop[which(shop$tag_price_mean > 100000),],shop_dsr,by = "shop_id")
as.data.frame(mean_price_if)
## 这些类型的店铺军事TB_JISHI，并且约销售额要么没有要么很低
## 说明这些商品的标价很高，但是并没有人去购买，说明这些商品是博人眼球的物品


dim(shop[shop$tag_price_mean <= 3000,])[1]/dim(shop)[1]
## 约98.9%的商铺，商铺商品的平均标价小于3000
summary(shop[shop$tag_price_mean <= 3000,]$tag_price_mean)

pm1 <- ggplot(shop[shop$tag_price_mean > 3000 & shop$tag_price_mean < 100000,]) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(tag_price_mean),bins = 200,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free") +
  scale_x_continuous(labels = function(x) paste(x/1e3,"k",sep = "")) +
  labs(x = "商品平均标价(>3e3&<1e5元)",y = "店铺的数目(家)",title = "店铺商品平均标价分布")
# pm1

pm2 <- ggplot(shop[shop$tag_price_mean <= 3000,]) +
  theme_bw(base_family = "STKaiti") +
  geom_histogram(aes(tag_price_mean),bins = 100,color="firebrick",fill = "red",
                 alpha = 0.6) + 
  facet_wrap(~shop_type,scales = "free_y") +
  #scale_x_continuous(labels = function(x) paste(x/10e3,"k",sep = "")) +
  labs(x = "商品平均标价(<=3e3元)",y = "店铺的数目(家)",title = "店铺商品平均标价分布")
# pm2
grid.arrange(pm1,pm2,nrow = 2)

## 可以看出两类商店的平均标价的分布存在很明显的差异
## 在高价区，BT_JISHI的商品平均标价较高
## 在低价区，BT_TMALL的商铺比较集中于商品平均标价高的区域


## 对商铺数据进行平行坐标图可视化####--------------------------------
lab_x <- c("销售商品数","销售品牌数","月销量","月销售额","评论数","平均标价",
           "平均售价","mas","sas","cas")
ggparcoord(shop,columns = c(2:7,9,11:13),groupColumn = 8,scale = "std") +
  theme_gray(base_family = "STKaiti") +
  theme(legend.position = "top") + 
  scale_x_discrete(labels = lab_x) +
  labs(x= "",y = "标准化后数值",title = "商铺平行坐标图")


ggparcoord(shop,columns = c(2:7,9,11:13),groupColumn = 8,scale = "robust") +
  theme_gray(base_family = "STKaiti") +
  theme(legend.position = "top") + 
  scale_x_discrete(labels = lab_x) +
  labs(x= "",y = "Robust后数值",title = "商铺平行坐标图")
## Robust :减去中位数，除以中位数的标准偏差


ggparcoord(shop,columns = c(2:7,9,11:13),groupColumn = 8 ,scale = "uniminmax") +
  theme_gray(base_family = "STKaiti") +
  theme(legend.position = "top") + 
  scale_x_discrete(labels = lab_x) +
  labs(x= "",y = "单位区间数值",title = "商铺平行坐标图")

## 从平行坐标图上我们可以看出两类变店铺的差异
## 1: TB_TMALL 的销售商品的数目更多，月销量更高，月销售额更高，评论数目更多
## 2:TB_JISHI  的销售品牌数更多，平均标价更高，平均售价更高


## 分析商铺数据中的数据的相关系数####---------------------------
## 查看散点图
ggscatmat(data = as.data.frame(shop),columns = c(2:7,9,11:13),
          color = "shop_type",corMethod = "pearson") +
  theme_bw(base_family = "STKaiti") +
  theme(legend.position = "top") + 
  ggtitle("散点图矩阵")

## 从散点图和相关系数矩阵中可以看出，不同的类别销售方式相关性有差异

## 将数据标准化，然后查看相关系数和散点矩阵图--------------------------------
shop_std <- tbl_df(as.data.frame(apply(shop[,c(2:7,9,11:13)], 2, scale)))
shop_std$shop_type <- shop$shop_type
shop_std$shop_id <- shop$shop_id   # 标准化后的商铺数据
head(shop_std)
summary(shop_std)

## 查看散点图
ggscatmat(data = as.data.frame(shop_std),columns = 1:7,
          color = "shop_type",corMethod = "pearson") +
  theme_bw(base_family = "STKaiti") +
  theme(legend.position = "top") + 
  ggtitle("散点图矩阵")


## 查看店铺的累积月销售分布####--------------------------------
month_all <- sum(shop$monthly_sales_sum)
print(month_all)  ## 约12.5亿元
month_pro <-cumsum(shop$monthly_sales_sum / month_all)
ggplot() +
  theme_grey(base_family = "STKaiti") +
  geom_line(aes(x = seq(1,length(month_pro)),y = month_pro)) + 
  geom_vline(xintercept = length(month_pro[month_pro != 1]),color = "red") +
  labs(x = "店铺数目",y = "累积百分比",title = "店铺月销售额") +
  geom_text(aes(length(month_pro[month_pro != 1])-1000,0.5),label = "月销售＝0",
            family = "STKaiti") +
  geom_vline(xintercept = length(month_pro[month_pro <= 0.9]),color = "red") +
  geom_text(aes(length(month_pro[month_pro <= 0.9])-600,0.5),label = "月销售\n占所有\n90%",
            family = "STKaiti") +
  scale_x_continuous(breaks = seq(1,length(month_pro),by = 1000)) +
  geom_text(aes(length(month_pro[month_pro <= 0.9])+3000,0.5),label = "月销售占所有10%",
            family = "STKaiti") 


## 绘制直方图查看销量售额的分布
data1 <- data.frame(month_pro = month_pro,shop_type = shop$shop_type)
data1 <- data1[data1$month_pro<1,]  # 只查看销量部位0的数据
ggplot(data1) + theme_bw(base_family = "STKaiti") +
  geom_bar(aes(x = seq(1,length(month_pro)),y = month_pro,
               color = shop_type),stat = "identity",width = 1) +
  theme(legend.position = "top") +
  labs(x = "店铺数目",y = "累积百分比",title = "店铺月销售额") 

## 可以发现虽然店铺为TB_TMALL类型的店铺数量少，但是对总体月销售额的贡献却占据主题部分

