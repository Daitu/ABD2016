# -*- coding: utf-8 -*-
"""
Created on Fri Jul 15 21:16:28 2016

@author: daitu
"""

""""
该程序用来清洗原始数据，将符合要求（10）个数据的行重新保存为新的文件夹
将不符合的行，保存到另一文件夹
原作者：陈粮
改编者：孙玉林
"""
#%% 更改工作文件夹
import os
os.getcwd()
os.chdir("/Users/daitu/数据分析/2016ABD/第二题数据")

#%% 数据预处理

f=open('items_fact.txt','r')
g=open('res.tsv','a')
h=open('err.csv','a')
line=f.readline()
try:
    while line:
        r=line.split('\t')
        if len(r)==10:  ##将正确的行写入文件
            for i in r:
                try:
                    g.write(i.split('\n')[0]+'\t')
                except:
                    print('write err')
            g.write('\n')
        else:        ## 将错误的行写入另一个文件
            for i in r:
                try:
                    h.write(i.split('\n')[0]+',')
                except:
                    print('else')
            h.write('\n')
        try:
            line=f.readline()
        except:
            y=1
            while  y:
                try:
                    line=f.readline()
                    y=0
                except:
                    y=1
            print('A err')
except:
    print('all err')
finally:
    f.close()
    g.close()
    h.close()


