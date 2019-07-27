## 2016年厦门大数据大赛
## 第二题：基于大数据的商品销售预测及关联销售挖掘
## author:Daitu
## 2016-7-12
## 工作：读取数据进行数据与处理，并保存


## 更改工作文件夹------------------------------------
setwd("/Users/daitu/数据分析/2016ABD")
getwd()



## 加载所需要的包-----------------------------------
library(stringr)
library(data.table)
library(dplyr)
library(VIM)
library(ggplot2)
library(gridExtra)
library(readr)
library(tm)

## 第一步：读取数据####-------------------------------------------
# 查看工作文件夹下的文件名
flodername <- "数据/第2题 商品销量预测及关联销售挖掘/热销款销售数据"
filename <- dir(flodername)
filename  ## 18个数据文件
filename <- str_c(flodername,filename,sep = "/")
filename
## 读取所有文件
top500 <- sapply(filename, fread,sep = "\t" ,na.strings="NULL",simplify = FALSE,USE.NAMES = FALSE)
length(top500)  # 一共18天的数据

##  top500[[1]]   ...... top500[[18]]
##  20160617      ......  20160706

head(top500[[1]])
top500df <- rbindlist(top500)
## 对该数据进行缺失值分析
VIM::aggr(top500df,prop = TRUE)

## 可以发现有一列数据的缺失值有70%左右，所以我们可以将这列数据剔除
## 该列数据为库销比
top500df$stock_sales_ratio <- NULL

VIM::aggr(top500df,prop = FALSE)

## 读取第二个数据文件 -------------------------------------------
## 该数据文件是对原始数据进行与处理后的文件，去除了错误的行，只保留了正确的行

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

filename2 <- "第二题数据/res.tsv"   # 提取出来的正确的数据
filename3 <- "第二题数据/errgai.csv"  # 修改后的错误的数据
selects <- c("item_id","item_number","shop_id","shop_type","brand_name","item_name",
             "price","tag_price","monthly_sales_num","assessment_num")
colClasses <- rep(c("character","numeric"),times = c(6,5))
item_fact <- fread(filename2,header = TRUE,sep = "\t",na.strings="NULL",
                   select = selects,colClasses = colClasses)
## itemgai <- read.csv(filename3,header = FALSE,sep = ",",stringsAsFactors = FALSE)
itemgai <- fread(filename3,header = FALSE,sep = ",",na.strings="NULL",
                 select = 1:10,colClasses = colClasses)
## 将两个数据集合并
item_fact <- data.table::rbindlist(list(item_fact,itemgai))

str(item_fact)
colnames(item_fact)

## 对变量的缺失值探索性分析
VIM::aggr(item_fact,prop = FALSE)

length(which(is.na(item_fact$monthly_sales_num)))


item_fact[which(is.na(item_fact$monthly_sales_num)),]
## 可以发现有23产品的月销售量是缺失的，因为只有少数的缺失值，所以将这些数据删除
item_fact <- item_fact[which(!is.na(item_fact$monthly_sales_num)),]

VIM::aggr(item_fact,prop = FALSE)
## 可以发现已经没有缺失值

## 计算商品的销售额
item_fact$monthly_sales <- item_fact$price * item_fact$monthly_sales_num

## 查看集中一共有多少个店铺 ，并进行可视化 #### ---------------------------------
## 将数据集转化为tbl数据
item_fact <- tbl_df(item_fact)
head(item_fact)
n_distinct(item_fact$shop_id)   ## 24493家店铺

## 查看商品的款号情况
item_num <- tbl_df(as.data.frame(table(item_fact$item_number))) %>%
  arrange(desc(Freq))
## 款好共有约32万个，说明有些商品是相同的，而且该列数据也是很乱的，
## 很多不能理解为是真正的款号

## 对商品款号的进一步处理####----------------------------------------------
## 1：剔除所有款号中含有中文的商品
check_chinese <- function(string){ ## 该函数用来检查字符串中是否含有中文
  str <-unlist(str_extract_all(string,"[\u4e00-\u9fff]+"))
  ifelse(length(str)>0,TRUE,FALSE)
}
indexnum <- sapply(item_fact$item_number, 
                   check_chinese,simplify = TRUE, USE.NAMES = FALSE)

item_fact <- item_fact[!indexnum,] ## 剔除数据

item_num <- tbl_df(as.data.frame(table(item_fact$item_number))) %>%
  arrange(desc(Freq))
dim(item_num)[1]
## 调整后的款号共有约304024个，

## 数据中一共有多少个品牌
brand <- tbl_df(as.data.frame(table(item_fact$brand_name))) %>%
  arrange(desc(Freq))
dim(brand)[1]
## 可以看出大约一共有21783个品牌，但是仔细查看数据，会发现这一列数据是很杂乱无章的


## 对商品的品牌名字进行处理####--------------------------------------
## 1:如果品牌名有中英文，只保留英文品牌名
## 2:将所有的英文品牌名转化为小写字母
check_brand <- function(string){## 处理品牌名
  strnew <- unlist(str_split(string,"/")) #切分字符串
  if(length(strnew)>1){  ## 说明含有中英文名
    # 提取英文名
    strnew <- unlist(str_extract_all(strnew,"[^[\u4e00-\u9fff]+]+"))[1]
  }
  ## 转化为小写
  strnew <- tolower(strnew)
  return(strnew)
}

check_brand("sherpa/夏尔巴")

item_fact$brand_name <- unlist(sapply(item_fact$brand_name,
       check_brand,simplify = TRUE,USE.NAMES = FALSE))

brand <- tbl_df(as.data.frame(table(item_fact$brand_name))) %>%
  arrange(desc(Freq))
dim(brand)[1]
## 调整后约19415个品牌

## 剔除只出现次数小于3的品牌数据
item_fact <- item_fact[item_fact$brand_name %in% brand$Var1[brand$Freq > 2],]
brand <- tbl_df(as.data.frame(table(item_fact$brand_name))) %>%
  arrange(desc(Freq))
dim(brand)[1]
## 还剩下约 6121个品牌




## 读取店铺的信息数据####
option <- options(digits = 5)
filename4 <- "第二题数据/shop_dsr." 
shop_dsr <- fread(filename4,header  = TRUE,sep = "\t",
                     colClasses  = rep(c("character","numeric"),times = c(2,3)))

head(shop_dsr)
## 进行缺失值分析
VIM::aggr(shop_dsr)
## 可以发现不存在缺失值
summary(shop_dsr)

## 将需要的数据保存下来，便于后面的分析#### -----------------------

save(item_fact,file = "第二题数据/item_fact.RData")
save(top500df,file = "第二题数据/top500df.RData")












