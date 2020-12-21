library(dplyr)
library(tidyr)

## For loop for 2000-2003 data
setwd("/Users/daiyichao/Desktop/宏观数据库2000-2002/2000-2003")
nameee1 = as.data.frame(matrix(NA,nrow = 35 , ncol = 39))
for(i in 1:length(dir())){
        if(i == 20){
                nameee1[i, ] = NA
        }
        else{
                dat = read.csv(dir()[i],fileEncoding = "GBK",stringsAsFactors=FALSE,header = FALSE, skip =4,nrow = 39)
                IInd = strsplit(dat$V1,"规模")
                for(g in 1:39){
                        nameee1[i, g] =  sapply(IInd, function(x) x[1])[g]
        }
       
        }
}
g = character(0)

## 20号表格缺失

nameee1 = nameee1[-20,]
for(i in 1:39){
        g[i] = as.character(any(nameee1[,i] != nameee1[1,i]))
}
table(g) 

## 特殊表格15号 27号

nameee1 = nameee1[-15,]
nameee1 = nameee1[-25,]
for(i in 1:39){
        g[i] = as.character(any(nameee1[,i] != nameee1[1,i]))
}
table(g) 


## 其余正常，原因是15号和27号表格缺失“木材及竹材采运业”

Index = as.character(nameee1[1,])
data1 = data.frame(ind = rep(1:length(Index),3), Names_of_Index = rep(Index,3),
                   year = c(rep(2002,39),rep(2001,length(Index)),rep(2000,length(Index))))
namee = data.frame(x = rep(NA,length(dir())))
for(i in 1:length(dir())){
        if(i == 20){
                namee[i, 1] = "应付帐款(亿元)"
                data1 = cbind(data1,NA)
        }
        else if(i == 15){
                dat = read.csv(dir()[i],fileEncoding = "GBK",stringsAsFactors=FALSE,header = FALSE, skip =4,nrow = 38)
                part1 = dat[c(1:6),];part2 = dat[c(7:38),];
                middle = data.frame(V1 = "木材及竹材采运业规模以上工业企业外商资本金(亿元)",V2 = NA, V3 = NA, V4 = NA)
                dat = rbind(part1,middle,part2)
                Ind = strsplit(dat$V1,"企业")
                namee[i, 1] = sapply(Ind, function(x) x[2])[1]
                x = dat %>% 
                        gather(data = dat[,2:4],)
                x = x[,-1]
                x = as.data.frame(x)
                data1 = cbind(data1,x)
        }
        else if(i == 27){
                dat = read.csv(dir()[i],fileEncoding = "GBK",stringsAsFactors=FALSE,header = FALSE, skip =4,nrow = 38)
                part1 = dat[c(1:6),];part2 = dat[c(7:38),];
                middle = data.frame(V1 = "木材及竹材采运业规模以上工业企业港澳台资本金(亿元)",V2 = NA, V3 = NA, V4 = NA)
                dat = rbind(part1,middle,part2)
                Ind = strsplit(dat$V1,"企业")
                namee[i, 1] = sapply(Ind, function(x) x[2])[1]
                x = dat %>% 
                        gather(data = dat[,2:4],)
                x = x[,-1]
                x = as.data.frame(x)
                data1 = cbind(data1,x)
        }
        else{
                dat = read.csv(dir()[i],fileEncoding = "GBK",stringsAsFactors=FALSE,header = FALSE, skip =4,nrow = 39)
                Ind = strsplit(dat$V1,"企业")
                namee[i, 1] = sapply(Ind, function(x) x[2])[1]
                x = dat %>% 
                        gather(data = dat[,2:4],)
                x = x[,-1]
                x = as.data.frame(x)
                data1 = cbind(data1,x)
        }
        
}
namee[6,1] = "亏损企业单位数(个)"
namee[8,1] = "企业单位数(个)"
colnames(data1)[4:38] = namee$x




## For loop for 2004-2011 data
setwd("~/Desktop/宏观数据库2003-2011")
nameee1 = as.data.frame(matrix(NA,nrow = 35 , ncol = 39))
for(i in 1:length(dir())){
        dat = read.csv(dir()[i],fileEncoding = "GBK",stringsAsFactors=FALSE,header = FALSE, skip =4,nrow = 39)
        IInd = strsplit(dat$V1,"规模")
        for(g in 1:39){
                nameee1[i, g] =  sapply(IInd, function(x) x[1])[g]
        }
}
g = character(0)
for(i in 1:39){
        g[i] = as.character(any(nameee1[,i] != nameee1[1,i]))
}
table(g) 
Index = as.character(nameee1[1,])
data11 = data.frame(ind = rep(1:length(Index),9), Names_of_Index = rep(Index,9),
                    year = c(rep(2011,39),rep(2010,length(Index)),rep(2009,length(Index)),rep(2008,length(Index)),
                             rep(2007,length(Index)),rep(2006,length(Index)),rep(2005,length(Index)),rep(2004,length(Index)),
                             rep(2003,length(Index))))


namee1 = data.frame(x = rep(NA,length(dir())))


for(i in 1:length(dir())){
        dat = read.csv(dir()[i],fileEncoding = "GBK",stringsAsFactors=FALSE,header = FALSE, skip =4,nrow = 39)
        Ind = strsplit(dat$V1,"企业")
        namee1[i, 1] = sapply(Ind, function(x) x[2])[1]
        x1 = dat %>% 
                gather(data = dat[,2:10])
        x1 = x1[,-1]
        x1 = as.data.frame(x1)
        data11 = cbind(data11,x1)
}
namee1[6,1] = "亏损企业单位数(个)"
namee1[8,1] = "企业单位数(个)"
colnames(data11)[4:38] = namee1$x





## For loop for 2012-2019 data
setwd("~/Desktop/宏观数据库2012-2019")
nameee1 = as.data.frame(matrix(NA,nrow = 35 , ncol = 41))
for(i in 1:length(dir())){
        dat = read.csv(dir()[i],fileEncoding = "GBK",stringsAsFactors=FALSE,header = FALSE, skip =4,nrow = 41)
        IInd = strsplit(dat$V1,"规模")
        for(g in 1:41){
                nameee1[i, g] =  sapply(IInd, function(x) x[1])[g]
        }
}
g = character(0)
for(i in 1:41){
        g[i] = as.character(any(nameee1[,i] != nameee1[1,i]))
}
table(g) 
g

## 8 号表格，“开采专业及辅助性活动”不同于其他“开采辅助活动”
## 另外 2012 年到2019年存在41 个行业，与其他两个时间段不相同


Index = as.character(nameee1[1,])
data111 = data.frame(ind = rep(1:length(Index),8), Names_of_Index = rep(Index,8),
                    year = c(rep(2019,41),rep(2018,length(Index)),rep(2017,length(Index)),rep(2016,length(Index)),
                             rep(2015,length(Index)),rep(2014,length(Index)),rep(2013,length(Index)),rep(2012,length(Index))))
namee11 = data.frame(x = rep(NA,length(dir())))
for(i in 1:length(dir())){
        dat = read.csv(dir()[i],fileEncoding = "GBK",stringsAsFactors=FALSE,header = FALSE, skip =4,nrow = 41)
        Ind = strsplit(dat$V1,"企业")
        namee11[i, 1] = sapply(Ind, function(x) x[2])[1]
        x1 = dat %>% 
                gather(data = dat[,2:length(dat)])
        x1 = x1[,-1]
        if(328 - length(x1)==0){
                data111 = cbind(data111,x1)
        }
        else{
                n = (328 - length(x1))/41
                x1 = as.data.frame(x1)
                y1 = data.frame(x1 = rep(NA, n*41))
                x1 = rbind(y1,x1)
                data111 = cbind(data111,x1)
        }
}
namee11[6,1] = "亏损企业单位数(个)"
namee11[8,1] = "企业单位数(个)"
colnames(data111)[4:38] = namee11$x


## 比较行业指标
table(as.character(data1[c(1:39),2]) == as.character(data11[c(1:39),2]))
table(as.character(data1[c(1:39),2]) == as.character(data111[c(1:39),2]))
table(as.character(data11[c(1:39),2]) == as.character(data111[c(1:39),2]))

name1 = as.character(data1[c(1:39),2])
name2 = as.character(data11[c(1:39),2])
name3 = as.character(data111[c(1:41),2])
Name = data.frame(Data1 = c(name1,"",""),Data2 = c(name2,"",""),Data3 = c(name3))


## Fixing:
data1$Names_of_Index = as.character(data1$Names_of_Index)
data11$Names_of_Index = as.character(data11$Names_of_Index)
data111$Names_of_Index = as.character(data111$Names_of_Index)

## data1 "自来水的生产和供应业" 改名为"水的生产和供应业"
in1 = data1$Names_of_Index == "自来水的生产和供应业"
data1$Names_of_Index[in1] = "水的生产和供应业"

## data 1 "煤炭采选业" 更名为 “煤炭开采和洗选业”
in1 = data1$Names_of_Index == "煤炭采选业"
table(in1)
data1$Names_of_Index[in1] = "煤炭开采和洗选业"

### data111 “酒、饮料和精制茶制造业” 改为“饮料制造业”
data11[data11$Names_of_Index =="饮料制造业",4:9 ]
data111[data111$Names_of_Index == "酒、饮料和精制茶制造业",4:9 ]
in1 = data111$Names_of_Index == "酒、饮料和精制茶制造业"
data111$Names_of_Index[in1] = "饮料制造业"

## data 111 "金属制品业" 和 "金属制品、机械和设备修理业" 合并
a = colSums(is.na(data111[data111$Names_of_Index == "金属制品业",]))
b = colSums(is.na(data111[data111$Names_of_Index == "金属制品、机械和设备修理业",]))
table(a ==b )
data111[data111$Names_of_Index == "金属制品业",4:38] = data111[data111$Names_of_Index == "金属制品业",4:38]+
        data111[data111$Names_of_Index == "金属制品、机械和设备修理业",4:38]
data111[data111$Names_of_Index == "金属制品业",4:9]

c = colSums(is.na(data111[data111$Names_of_Index == "金属制品业",]))
table(a == c)
data111 = data111[data111$Names_of_Index != "金属制品、机械和设备修理业",]

### data 111 "有色金属冶炼和压延加工业“ 改为 “有色金属冶炼及压延加工业”

in1 = data111$Names_of_Index == "有色金属冶炼和压延加工业"
data111$Names_of_Index[in1] = "有色金属冶炼及压延加工业"
in1 = data111$Names_of_Index == "有色金属冶炼和压延加工业"
table(in1)

### data 111 "化学原料和化学制品制造业“ 改为 “化学原料及化学制品制造业”

in1 = data111$Names_of_Index == "化学原料和化学制品制造"
data111$Names_of_Index[in1] = "化学原料和化学制品制造业"
in1 = data111$Names_of_Index == "有色金属冶炼和压延加工业"
table(in1)

### data111 的 "仪器仪表制造业" 讲 data11 "仪器仪表及文化、办公用机械制造业", data1"仪器仪表及文化办公用机械制造业"
data1[data1$Names_of_Index =="仪器仪表及文化办公用机械制造业",4:9 ]
data11[data11$Names_of_Index =="仪器仪表及文化、办公用机械制造业",4:9 ]
data111[data111$Names_of_Index == "仪器仪表制造业",4:9 ]

in1 = data11$Names_of_Index == "仪器仪表及文化、办公用机械制造业"
table(in1)
data11$Names_of_Index[in1] = "仪器仪表制造业"
in1 = data11$Names_of_Index == "仪器仪表及文化、办公用机械制造业"
table(in1)


in1 = data1$Names_of_Index == "仪器仪表及文化办公用机械制造业"
table(in1)
data1$Names_of_Index[in1] = "仪器仪表制造业"
in1 = data1$Names_of_Index == "仪器仪表及文化办公用机械制造业"
table(in1)

### data 111 “铁路、船舶、航空航天和其他运输设备” 改为 “交通运输设备制造业”   ###不能合并

data11[data11$Names_of_Index =="交通运输设备制造业",4:9 ]
data111[data111$Names_of_Index == "铁路、船舶、航空航天和其他运输设备",4:9 ]


## data1 的"石油加工及炼焦业" data11 的“石油加工、炼焦及核燃料加工业” [data111 的 “石油加工、炼焦和核燃料加工业”]
data1[data1$Names_of_Index =="石油加工及炼焦业",4:9 ]
data11[data11$Names_of_Index =="石油加工、炼焦及核燃料加工业",4:9 ]
data111[data111$Names_of_Index == "石油加工、炼焦和核燃料加工业",4:9 ]


in1 = data11$Names_of_Index == "石油加工、炼焦及核燃料加工业"
table(in1)
data11$Names_of_Index[in1] = "石油加工、炼焦和核燃料加工业"
in1 = data11$Names_of_Index == "石油加工、炼焦及核燃料加工业"
table(in1)

in1 = data1$Names_of_Index == "石油加工及炼焦业"
table(in1)
data1$Names_of_Index[in1] = "石油加工、炼焦和核燃料加工业"
in1 = data1$Names_of_Index == "石油加工及炼焦业"
table(in1)

### data 111 "电气机械和器材制造业" 改为 “电气机械及器材制造业”
in1 = data111$Names_of_Index == "电气机械和器材制造业"
table(in1)
data111$Names_of_Index[in1] = "电气机械及器材制造业"
in1 = data111$Names_of_Index == "电气机械和器材制造业"
table(in1)

### data1 的“烟草加工业” 和 data11 和 data111 的 “烟草制品业”

data1[data1$Names_of_Index =="烟草加工业",4:9 ]
data11[data11$Names_of_Index =="烟草制品业",4:9 ]
data111[data111$Names_of_Index == "烟草制品业",4:9 ]
in1 = data1$Names_of_Index == "烟草加工业"
table(in1)
data1$Names_of_Index[in1] = "烟草制品业"
in1 = data1$Names_of_Index == "烟草加工业"
table(in1)

### data1 橡胶制品业/塑料制品业 合并

a = colSums(is.na(data1[data1$Names_of_Index == "橡胶制品业",]))
b = colSums(is.na(data1[data1$Names_of_Index == "塑料制品业",]))
table(a ==b )
data1[data1$Names_of_Index == "橡胶制品业",4:9]
data1[data1$Names_of_Index == "塑料制品业",4:9]
data1[data1$Names_of_Index == "橡胶制品业",4:38] = data1[data1$Names_of_Index == "橡胶制品业",4:38]+
        data1[data1$Names_of_Index == "塑料制品业",4:38]
data1[data1$Names_of_Index == "橡胶制品业",4:9]

c = colSums(is.na(data1[data1$Names_of_Index == "橡胶制品业",]))
table(a == c)
data1 = data1[data1$Names_of_Index != "塑料制品业",]

in1 = data1$Names_of_Index == "橡胶制品业"
table(in1)
data1$Names_of_Index[in1] = "橡胶和塑料制品业"
in1 = data1$Names_of_Index == "橡胶制品业"
table(in1)

## data1 橡胶制品业/塑料制品业 合并
a = colSums(is.na(data11[data11$Names_of_Index == "橡胶制品业",]))
b = colSums(is.na(data11[data11$Names_of_Index == "塑料制品业",]))
table(a ==b )
data11[data11$Names_of_Index == "橡胶制品业",4:9]
data11[data11$Names_of_Index == "塑料制品业",4:9]
data11[data11$Names_of_Index == "橡胶制品业",4:38] = data11[data11$Names_of_Index == "橡胶制品业",4:38]+
        data11[data11$Names_of_Index == "塑料制品业",4:38]
data11[data11$Names_of_Index == "橡胶制品业",4:9]

c = colSums(is.na(data11[data11$Names_of_Index == "橡胶制品业",]))
table(a == c)
data11 = data11[data11$Names_of_Index != "塑料制品业",]

in1 = data11$Names_of_Index == "橡胶制品业"
table(in1)
data11$Names_of_Index[in1] = "橡胶和塑料制品业"
in1 = data11$Names_of_Index == "橡胶制品业"
table(in1)

### data 1 木材加工及竹、藤、棕、草制品业/木材及竹材采运业 合并 data11 “木材加工及木、竹、藤、棕、草制品业” data111 “木材加工和木、竹、藤、棕、草制品业”

data1[data1$Names_of_Index == "木材加工及竹、藤、棕、草制品业",4:9]
data1[data1$Names_of_Index == "木材及竹材采运业",4:9]
data11[data11$Names_of_Index == "木材加工及木、竹、藤、棕、草制品业",4:9]   ## 可以合并

a = colSums(is.na(data1[data1$Names_of_Index == "木材加工及竹、藤、棕、草制品业",]))
b = colSums(is.na(data1[data1$Names_of_Index == "木材及竹材采运业",]))
table(a ==b )

data1[data1$Names_of_Index == "木材及竹材采运业","外商资本金(亿元)"] = 0 ## 无前后数据只能为 0
data1[data1$Names_of_Index == "木材及竹材采运业","港澳台资本金(亿元)"] = 0

data1[data1$Names_of_Index == "木材加工及竹、藤、棕、草制品业",4:38] = data1[data1$Names_of_Index == "木材加工及竹、藤、棕、草制品业",4:38]+
        data1[data1$Names_of_Index == "木材及竹材采运业",4:38]
data1[data1$Names_of_Index == "木材加工及竹、藤、棕、草制品业",4:9]

c = colSums(is.na(data1[data1$Names_of_Index == "木材加工及竹、藤、棕、草制品业",]))
table(a == c)
data1 = data1[data1$Names_of_Index != "木材及竹材采运业",]

in1 = data1$Names_of_Index == "木材加工及竹、藤、棕、草制品业"
table(in1)
data1$Names_of_Index[in1] = "木材加工和木、竹、藤、棕、草制品业"
in1 = data1$Names_of_Index == "木材加工及竹、藤、棕、草制品业"
table(in1)

in1 = data11$Names_of_Index == "木材加工及木、竹、藤、棕、草制品业"
table(in1)
data11$Names_of_Index[in1] = "木材加工和木、竹、藤、棕、草制品业"
in1 = data11$Names_of_Index == "木材加工及木、竹、藤、棕、草制品业"
table(in1)

###data1 印刷业、记录媒介的复制 data11 印刷业和记录媒介的复制 [data 111 印刷和记录媒介复制业]

in1 = data1$Names_of_Index == "印刷业、记录媒介的复制"
table(in1)
data1$Names_of_Index[in1] = "印刷和记录媒介复制业"
in1 = data1$Names_of_Index == "印刷业、记录媒介的复制"
table(in1)

in1 = data11$Names_of_Index == "印刷业和记录媒介的复制"
table(in1)
data11$Names_of_Index[in1] = "印刷和记录媒介复制业"
in1 = data11$Names_of_Index == "印刷业和记录媒介的复制"
table(in1)


### data 111 文教、工美、体育和娱乐用品制造业 data1 data 11 文教体育用品制造业

data1[data1$Names_of_Index == "文教体育用品制造业",4:9]
data11[data11$Names_of_Index == "文教体育用品制造业",4:9]
data111[data111$Names_of_Index == "文教、工美、体育和娱乐用品制造业",4:9]  ### 差距过于悬殊

### data1 其他矿采选业 data11 data 111 其他采矿业

data1[data1$Names_of_Index == "其他矿采选业",4:9]
data11[data11$Names_of_Index == "其他采矿业",4:9]
data111[data111$Names_of_Index == "其他采矿业",4:9] 


in1 = data1$Names_of_Index == "其他矿采选业"
table(in1)
data1$Names_of_Index[in1] = "其他采矿业"
in1 = data1$Names_of_Index == "其他矿采选业"
table(in1)

### data 1 电子及通信设备制造业 data 11通信设备、计算机及其他电子设备制造 data111 计算机、通信和其他电子设备制造业

data1[data1$Names_of_Index == "电子及通信设备制造业",4:9]
data11[data11$Names_of_Index == "通信设备、计算机及其他电子设备制造",4:9]
data111[data111$Names_of_Index == "计算机、通信和其他电子设备制造业",4:9] 

in1 = data1$Names_of_Index == "电子及通信设备制造业"
table(in1)
data1$Names_of_Index[in1] = "计算机、通信和其他电子设备制造业"
in1 = data1$Names_of_Index == "电子及通信设备制造业"
table(in1)

in1 = data11$Names_of_Index == "通信设备、计算机及其他电子设备制造"
table(in1)
data11$Names_of_Index[in1] = "计算机、通信和其他电子设备制造业"
in1 = data11$Names_of_Index == "通信设备、计算机及其他电子设备制造"
table(in1)

### data1 食品加工业 data11 data111 【农副食品加工业】

data1[data1$Names_of_Index == "食品加工业",4:9]
data11[data11$Names_of_Index == "农副食品加工业",4:9]
data111[data111$Names_of_Index == "农副食品加工业",4:9] 

in1 = data1$Names_of_Index == "食品加工业"
table(in1)
data1$Names_of_Index[in1] = "农副食品加工业"
in1 = data1$Names_of_Index == "食品加工业"
table(in1)

### data1 data111其它制造业 data11 工艺品及其他制造业

data1[data1$Names_of_Index == "其它制造业",4:9]
data11[data11$Names_of_Index == "工艺品及其他制造业",4:9]
data111[data111$Names_of_Index == "其他制造业",4:9]   ## 完全不一个东西

in1 = data111$Names_of_Index == "其他制造业"
table(in1)
data111$Names_of_Index[in1] = "其它制造业"
in1 = data11$Names_of_Index == "其他制造业"
table(in1)

## data11 废弃资源和废旧材料回收加工业 data 111 [废弃资源综合利用业]

data11[data11$Names_of_Index == "废弃资源和废旧材料回收加工业",4:9]
data111[data111$Names_of_Index == "废弃资源综合利用业",4:9] 

in1 = data11$Names_of_Index == "废弃资源和废旧材料回收加工业"
table(in1)
data11$Names_of_Index[in1] = "废弃资源综合利用业"
in1 = data11$Names_of_Index == "废弃资源和废旧材料回收加工业"
table(in1)


## data1 皮革、毛皮、羽绒及其制品业 data11 皮革、毛皮、羽毛(绒)及其制品业 data111 皮革、毛皮、羽毛及其制品和制鞋业

data1[data1$Names_of_Index == "皮革、毛皮、羽绒及其制品业",4:9]
data11[data11$Names_of_Index == "皮革、毛皮、羽毛(绒)及其制品业",4:9]
data111[data111$Names_of_Index == "皮革、毛皮、羽毛及其制品和制鞋业",4:9] 



## data1 服装及其他纤维制品制造业 data11 纺织服装、鞋、帽制造业 data 111 纺织服装、服饰业

data1[data1$Names_of_Index == "服装及其他纤维制品制造业",4:9]
data11[data11$Names_of_Index == "纺织服装、鞋、帽制造业",4:9]
data111[data111$Names_of_Index == "纺织服装、服饰业",4:9] 

## 可以合并

data1[data1$Names_of_Index == "服装及其他纤维制品制造业",4:9]+data1[data1$Names_of_Index == "皮革、毛皮、羽绒及其制品业",4:9]
data11[data11$Names_of_Index == "纺织服装、鞋、帽制造业",4:9]+data11[data11$Names_of_Index == "皮革、毛皮、羽毛(绒)及其制品业",4:9]
data111[data111$Names_of_Index == "纺织服装、服饰业",4:9] +data111[data111$Names_of_Index == "皮革、毛皮、羽毛及其制品和制鞋业",4:9] 



##data1 [皮革、毛皮、羽毛及其制品,制鞋,服饰,纺织服装业] 皮革、毛皮、羽绒及其制品业 和 服装及其他纤维制品制造业 合并


a = colSums(is.na(data1[data1$Names_of_Index == "皮革、毛皮、羽绒及其制品业",]))
b = colSums(is.na(data1[data1$Names_of_Index == "服装及其他纤维制品制造业",]))
table(a ==b )
data1[data1$Names_of_Index == "皮革、毛皮、羽绒及其制品业",4:9]
data1[data1$Names_of_Index == "服装及其他纤维制品制造业",4:9]
data1[data1$Names_of_Index == "皮革、毛皮、羽绒及其制品业",4:38] = data1[data1$Names_of_Index == "皮革、毛皮、羽绒及其制品业",4:38]+
        data1[data1$Names_of_Index == "服装及其他纤维制品制造业",4:38]
data1[data1$Names_of_Index == "皮革、毛皮、羽绒及其制品业",4:9]

c = colSums(is.na(data1[data1$Names_of_Index == "皮革、毛皮、羽绒及其制品业",]))
table(a == c)
data1 = data1[data1$Names_of_Index != "服装及其他纤维制品制造业",]

in1 = data1$Names_of_Index == "皮革、毛皮、羽绒及其制品业"
table(in1)
data1$Names_of_Index[in1] = "皮革、毛皮、羽毛及其制品,制鞋,服饰,纺织服装业"
in1 = data1$Names_of_Index == "皮革、毛皮、羽绒及其制品业"
table(in1)

##data11 [皮革、毛皮、羽毛及其制品,制鞋,服饰,纺织服装业] 皮革、毛皮、羽毛(绒)及其制品业 和 纺织服装、鞋、帽制造业 合并


a = colSums(is.na(data11[data11$Names_of_Index == "皮革、毛皮、羽毛(绒)及其制品业",]))
b = colSums(is.na(data11[data11$Names_of_Index == "纺织服装、鞋、帽制造业",]))
table(a ==b )
data11[data11$Names_of_Index == "皮革、毛皮、羽毛(绒)及其制品业",4:9]
data11[data11$Names_of_Index == "纺织服装、鞋、帽制造业",4:9]
data11[data11$Names_of_Index == "皮革、毛皮、羽毛(绒)及其制品业",4:38] = data11[data11$Names_of_Index == "皮革、毛皮、羽毛(绒)及其制品业",4:38]+
        data11[data11$Names_of_Index == "纺织服装、鞋、帽制造业",4:38]
data11[data11$Names_of_Index == "皮革、毛皮、羽毛(绒)及其制品业",4:9]

c = colSums(is.na(data11[data11$Names_of_Index == "皮革、毛皮、羽毛(绒)及其制品",]))
table(a == c)
data11 = data11[data11$Names_of_Index != "纺织服装、鞋、帽制造业",]

in1 = data11$Names_of_Index == "皮革、毛皮、羽毛(绒)及其制品业"
table(in1)
data11$Names_of_Index[in1] = "皮革、毛皮、羽毛及其制品,制鞋,服饰,纺织服装业"
in1 = data11$Names_of_Index == "皮革、毛皮、羽毛(绒)及其制品业"
table(in1)

##data111 [皮革、毛皮、羽毛及其制品,制鞋,服饰,纺织服装业] 皮革、毛皮、羽毛及其制品和制鞋业 和 纺织服装、服饰业 合并


a = colSums(is.na(data111[data111$Names_of_Index == "皮革、毛皮、羽毛及其制品和制鞋业",]))
b = colSums(is.na(data111[data111$Names_of_Index == "纺织服装、服饰业",]))
table(a ==b )
data111[data111$Names_of_Index == "皮革、毛皮、羽毛及其制品和制鞋业",4:9]
data111[data111$Names_of_Index == "纺织服装、服饰业",4:9]
data111[data111$Names_of_Index == "皮革、毛皮、羽毛及其制品和制鞋业",4:38] = data111[data111$Names_of_Index == "皮革、毛皮、羽毛及其制品和制鞋业",4:38]+
        data111[data111$Names_of_Index == "纺织服装、服饰业",4:38]
data111[data111$Names_of_Index == "皮革、毛皮、羽毛及其制品和制鞋业",4:9]

c = colSums(is.na(data111[data111$Names_of_Index == "皮革、毛皮、羽毛及其制品和制鞋业",]))
table(a == c)
data111 = data111[data111$Names_of_Index != "纺织服装、服饰业",]

in1 = data111$Names_of_Index == "皮革、毛皮、羽毛及其制品和制鞋业"
table(in1)
data111$Names_of_Index[in1] = "皮革、毛皮、羽毛及其制品,制鞋,服饰,纺织服装业"
in1 = data111$Names_of_Index == "皮革、毛皮、羽毛及其制品和制鞋业"
table(in1)

## data1非金属矿物制造业	data11	非金属矿物制品业	data111非金属矿物制品业

data1[data1$Names_of_Index == "非金属矿物制造业",4:9]
data11[data11$Names_of_Index == "非金属矿物制品业",4:9]
data111[data111$Names_of_Index == "非金属矿物制品业",4:9] 

in1 = data1$Names_of_Index == "非金属矿物制造业"
table(in1)
data1$Names_of_Index[in1] = "非金属矿物制品业"
in1 = data1$Names_of_Index == "非金属矿物制品业"
table(in1)

## 电力、蒸汽、热水的生产和供应业	电力、热力的生产和供应业	电力、热力生产和供应业
in1 = data1$Names_of_Index == "电力、蒸汽、热水的生产和供应业"
table(in1)
data1$Names_of_Index[in1] = "电力、热力的生产和供应业"
in1 = data1$Names_of_Index == "电力、热力的生产和供应业"
table(in1)

in1 = data111$Names_of_Index == "电力、热力生产和供应业"
table(in1)
data111$Names_of_Index[in1] = "电力、热力的生产和供应业"
in1 = data111$Names_of_Index == "电力、热力的生产和供应业"
table(in1)

##黑色金属冶炼及压延加工业	黑色金属冶炼及压延加工业	黑色金属冶炼和压延加工业

in1 = data111$Names_of_Index == "黑色金属冶炼和压延加工业"
table(in1)
data111$Names_of_Index[in1] = "黑色金属冶炼及压延加工业"
in1 = data111$Names_of_Index == "黑色金属冶炼及压延加工业"
table(in1)

## 化学原料及化学制品制造业	化学原料及化学制品制造业	化学原料和化学制品制造业

in1 = data111$Names_of_Index == "化学原料和化学制品制造业"
table(in1)
data111$Names_of_Index[in1] = "化学原料及化学制品制造业"
in1 = data111$Names_of_Index == "化学原料及化学制品制造业"
table(in1)

## 造纸及纸制品业	造纸及纸制品业	造纸和纸制品业

in1 = data111$Names_of_Index == "造纸和纸制品业"
table(in1)
data111$Names_of_Index[in1] = "造纸及纸制品业"
in1 = data111$Names_of_Index == "造纸及纸制品业"
table(in1)

## 部分没有的数据
### data 1
 
nust_data1 = data1[data1$Names_of_Index == "其它制造业" | data1$Names_of_Index =="普通机械制造业" |
                           data1$Names_of_Index == "煤气生产和供应业" |data1$Names_of_Index =="交通运输设备制造业" | 
                           data1$Names_of_Index =="文教体育用品制造业", ]


data1 = data1[data1$Names_of_Index != "其它制造业" & data1$Names_of_Index !="普通机械制造业" &
        data1$Names_of_Index != "煤气生产和供应业" & data1$Names_of_Index !="交通运输设备制造业" & 
        data1$Names_of_Index !="文教体育用品制造业", ]


### data 11

nust_data11 = data11[data11$Names_of_Index == "燃气生产和供应业" | data11$Names_of_Index =="通用设备制造业" |
                           data11$Names_of_Index == "废弃资源综合利用业" |data11$Names_of_Index =="工艺品及其他制造业" | 
                           data11$Names_of_Index =="交通运输设备制造业" | data11$Names_of_Index =="文教体育用品制造业", ]


data11 = data11[data11$Names_of_Index != "燃气生产和供应业" & data11$Names_of_Index !="通用设备制造业" &
                             data11$Names_of_Index != "废弃资源综合利用业" & data11$Names_of_Index !="工艺品及其他制造业" & 
                             data11$Names_of_Index !="交通运输设备制造业" & data11$Names_of_Index !="文教体育用品制造业", ]


### data 111

nust_data111 = data111[data111$Names_of_Index == "其它制造业" | data111$Names_of_Index =="燃气生产和供应业" |
                             data111$Names_of_Index == "通用设备制造业" |data111$Names_of_Index =="废弃资源综合利用业" | 
                             data111$Names_of_Index =="开采辅助活动" | data111$Names_of_Index =="汽车制造业" |
                               data111$Names_of_Index == "铁路、船舶、航空航天和其他运输设备" |
                               data111$Names_of_Index == "文教、工美、体育和娱乐用品制造业", ]


data111 = data111[data111$Names_of_Index != "其它制造业" & data111$Names_of_Index !="燃气生产和供应业" &
                               data111$Names_of_Index != "通用设备制造业" &data111$Names_of_Index !="废弃资源综合利用业" &
                               data111$Names_of_Index !="开采辅助活动" & data111$Names_of_Index !="汽车制造业" &
                               data111$Names_of_Index != "铁路、船舶、航空航天和其他运输设备" &
                               data111$Names_of_Index != "文教、工美、体育和娱乐用品制造业", ]


name1 = as.character(data1[c(1:31),2])
name2 = as.character(data11[c(1:31),2])
name3 = as.character(data111[c(1:31),2])

Name = data.frame(Data1 = name1,Data2 =name2,Data3 =name3)

name1 == name2
name2 == name3


data1$ind = rep(1:31,3)
data11$ind = rep(1:31, 9)
data111$ind = rep(1:31, 8)


##col 7 名字不一样"主营业务税金及附加(亿元)" "主营业务费用及附加(亿元)" 感觉没什么区别
colnames(data111)[7]  = colnames(data1)[7]

## create the same data set:
same_data = rbind(data1, data11, data111)

## nust_data1

nust_data1[nust_data1$Names_of_Index == "其它制造业" | nust_data1$Names_of_Index =="普通机械制造业" |
                   nust_data1$Names_of_Index == "煤气生产和供应业" |nust_data1$Names_of_Index =="交通运输设备制造业" | 
                   nust_data1$Names_of_Index =="文教体育用品制造业", ]$ind =c(45, 38,44,32,39)


nust_data11[nust_data11$Names_of_Index == "燃气生产和供应业" | nust_data11$Names_of_Index =="通用设备制造业" |
                                  nust_data11$Names_of_Index == "废弃资源综合利用业" |nust_data11$Names_of_Index =="工艺品及其他制造业" | 
                                  nust_data11$Names_of_Index =="交通运输设备制造业" | 
                                  nust_data11$Names_of_Index =="文教体育用品制造业", ]$ind = c(45, 34,44,43,35,33)

nust_data111[nust_data111$Names_of_Index == "其它制造业" | nust_data111$Names_of_Index =="燃气生产和供应业" |
                     nust_data111$Names_of_Index == "通用设备制造业" |nust_data111$Names_of_Index =="废弃资源综合利用业" | 
                     nust_data111$Names_of_Index =="开采辅助活动" | nust_data111$Names_of_Index =="汽车制造业" |
                     nust_data111$Names_of_Index == "铁路、船舶、航空航天和其他运输设备" |
                     nust_data111$Names_of_Index == "文教、工美、体育和娱乐用品制造业", ]$ind = c(36,41,34,37,40,32,35,33)

colnames(nust_data1) == colnames(nust_data11) 
colnames(nust_data111)[7]  = colnames(nust_data11)[7]
colnames(nust_data11) == colnames(nust_data111) 
nust_data = rbind(nust_data1,nust_data11,nust_data111)

## 用NA 补全缺失年份

b1 = as.data.frame(matrix(data = NA, nrow = 9, ncol = 38))
colnames(b1) = colnames(nust_data)
b1[,1] = nust_data[nust_data$Names_of_Index == 	"其它制造业", 1][1]
b1[,2] = nust_data[nust_data$Names_of_Index == 	"其它制造业", 2][1]
b1[,3] = rep(2003:2011,1)
nust_data =  rbind(nust_data,b1)


b1 = as.data.frame(matrix(data = NA, nrow = 3, ncol = 38))
colnames(b1) = colnames(nust_data)
b1[,1] = nust_data[nust_data$Names_of_Index == 	"燃气生产和供应业", 1][1]
b1[,2] = nust_data[nust_data$Names_of_Index == 	"燃气生产和供应业", 2][1]
b1[,3] = rep(2000:2002,1)
nust_data =  rbind(nust_data,b1)

b1 = as.data.frame(matrix(data = NA, nrow = 3, ncol = 38))
colnames(b1) = colnames(nust_data)
b1[,1] = nust_data[nust_data$Names_of_Index == 	"通用设备制造业", 1][1]
b1[,2] = nust_data[nust_data$Names_of_Index == 	"通用设备制造业", 2][1]
b1[,3] = rep(2000:2002,1)
nust_data =  rbind(nust_data,b1)


b1 = as.data.frame(matrix(data = NA, nrow = 3, ncol = 38))
colnames(b1) = colnames(nust_data)
b1[,1] = nust_data[nust_data$Names_of_Index == 	"废弃资源综合利用业", 1][1]
b1[,2] = nust_data[nust_data$Names_of_Index == 	"废弃资源综合利用业", 2][1]
b1[,3] = rep(2000:2002,1)
nust_data =  rbind(nust_data,b1)

b1 = as.data.frame(matrix(data = NA, nrow = 12, ncol = 38))
colnames(b1) = colnames(nust_data)
b1[,1] = nust_data[nust_data$Names_of_Index == 	"开采辅助活动", 1][1]
b1[,2] = nust_data[nust_data$Names_of_Index == 	"开采辅助活动", 2][1]
b1[,3] = rep(2000:2011,1)
nust_data =  rbind(nust_data,b1)

b1 = as.data.frame(matrix(data = NA, nrow = 12, ncol = 38))
colnames(b1) = colnames(nust_data)
b1[,1] = nust_data[nust_data$Names_of_Index == 	"汽车制造业", 1][1]
b1[,2] = nust_data[nust_data$Names_of_Index == 	"汽车制造业", 2][1]
b1[,3] = rep(2000:2011,1)
nust_data =  rbind(nust_data,b1)

b1 = as.data.frame(matrix(data = NA, nrow = 17, ncol = 38))
colnames(b1) = colnames(nust_data)
b1[,1] = nust_data[nust_data$Names_of_Index == 	"普通机械制造业", 1][1]
b1[,2] = nust_data[nust_data$Names_of_Index == 	"普通机械制造业", 2][1]
b1[,3] = rep(2003:2019,1)
nust_data =  rbind(nust_data,b1)

b1 = as.data.frame(matrix(data = NA, nrow = 17, ncol = 38))
colnames(b1) = colnames(nust_data)
b1[,1] = nust_data[nust_data$Names_of_Index == 	"煤气生产和供应业", 1][1]
b1[,2] = nust_data[nust_data$Names_of_Index == 	"煤气生产和供应业", 2][1]
b1[,3] = rep(2003:2019,1)
nust_data =  rbind(nust_data,b1)


b1 = as.data.frame(matrix(data = NA, nrow = 12, ncol = 38))
colnames(b1) = colnames(nust_data)
b1[,1] = nust_data[nust_data$Names_of_Index == 	"铁路、船舶、航空航天和其他运输设备", 1][1]
b1[,2] = nust_data[nust_data$Names_of_Index == 	"铁路、船舶、航空航天和其他运输设备", 2][1]
b1[,3] = rep(2000:2011,1)
nust_data =  rbind(nust_data,b1)

b1 = as.data.frame(matrix(data = NA, nrow = 12, ncol = 38))
colnames(b1) = colnames(nust_data)
b1[,1] = nust_data[nust_data$Names_of_Index == 	"文教、工美、体育和娱乐用品制造业", 1][1]
b1[,2] = nust_data[nust_data$Names_of_Index == 	"文教、工美、体育和娱乐用品制造业", 2][1]
b1[,3] = rep(2000:2011,1)
nust_data =  rbind(nust_data,b1)

b1 = as.data.frame(matrix(data = NA, nrow = 11, ncol = 38))
colnames(b1) = colnames(nust_data)
b1[,1] = nust_data[nust_data$Names_of_Index == 	"工艺品及其他制造业", 1][1]
b1[,2] = nust_data[nust_data$Names_of_Index == 	"工艺品及其他制造业", 2][1]
b1[,3] = rep(c(2000:2002, 2012:2019),1)
nust_data =  rbind(nust_data,b1)

b1 = as.data.frame(matrix(data = NA, nrow = 8, ncol = 38))
colnames(b1) = colnames(nust_data)
b1[,1] = nust_data[nust_data$Names_of_Index == 	"交通运输设备制造业", 1][1]
b1[,2] = nust_data[nust_data$Names_of_Index == 	"交通运输设备制造业", 2][1]
b1[,3] = rep(2012:2019,1)
nust_data =  rbind(nust_data,b1)

b1 = as.data.frame(matrix(data = NA, nrow = 8, ncol = 38))
colnames(b1) = colnames(nust_data)
b1[,1] = nust_data[nust_data$Names_of_Index == 	"文教体育用品制造业", 1][1]
b1[,2] = nust_data[nust_data$Names_of_Index == 	"文教体育用品制造业", 2][1]
b1[,3] = rep(2012:2019,1)
nust_data =  rbind(nust_data,b1)



## FINAL / FIX

#### Table 1
setwd( "/Users/daiyichao/Desktop")
FINAL_DATA = rbind(same_data,nust_data)
FINAL_DATA[FINAL_DATA$ind == 43, ]$ind = 42
FINAL_DATA[FINAL_DATA$ind == 44, ]$ind = 43
FINAL_DATA[FINAL_DATA$ind == 45, ]$ind = 44
FINAL_DATA = FINAL_DATA[order(FINAL_DATA$ind,FINAL_DATA$year),]


#### Table 2
zz = character(0)
g = 1
for(i in 1:44){
        zz[i] = FINAL_DATA[g,2]
        g = g+20
        
}

Index = as.data.frame(zz)
colnames(Index) = "INDEX"
CODE = as.data.frame(x = 1:44)
Index = cbind(CODE, Index)
colnames(Index)[1] = "CODE"


#### FIX PART waiting...




##### Table 3

Unit = as.data.frame(colnames(FINAL_DATA))
write.csv(FINAL_DATA,"FINALDATA.csv")
write.csv(Index,"INDEX.csv")
write.csv(Unit, "Unit.csv")







