rm(list=ls())
source("source.R")

## Data1, 회계데이터터
path = "H:/내 드라이브/R/Factor investing/investing/Raw_data"
data1 = fread(paste0(path, "/Data1.csv"), encoding = "UTF-8", header =T)
data1 = as.data.frame(data1)

##
for(i in 3:length(data1)){
    data1[,i] = str_remove_all(data1[,i],",")
    data1[,i] = as.numeric(data1[,i])
}

##
item_list = data1$`Item Name` %>% unique()


for(i in 1:length(item_list)){
    
    temp = data1[data1$`Item Name` %in% item_list[i], ]
    temp = pivot_longer(temp, cols = 3:length(data1), values_to = item_list[i], names_to = "Date")
    temp = temp %>% select(-`Item Name`) %>% relocate(Date, .before = Symbol)

    assign(item_list[i],temp)
    
}

delete_list = ls()[!ls() %in% item_list]; 
delete_list = delete_list[!delete_list %in% c("item_list", "path")]
rm(list = delete_list); rm(delete); item_list = item_list[!item_list %in% "delete"]

##
data_set = Asset

for(i in 2:length(item_list)){
    
    temp = get(item_list[i])
    temp = as.data.frame(temp)
    
    data_set = merge(data_set, temp, by = c("Date", "Symbol"))
    
}

##
path_result = "H:/내 드라이브/R/Factor investing/investing/preprocessed"
fwrite(data_set, paste0(path_result, "/accounting_data.csv"))

##
rm(list= ls()[!ls() %in% c("path", "path_result")])
gc()

## data2, 시장데이터
path = "H:/내 드라이브/R/Factor investing/investing/Raw_data"
path_result = "H:/내 드라이브/R/Factor investing/investing/preprocessed"

data2 = fread(paste0(path, "/data2.csv"), header = F, encoding ="UTF-8")
data2 = as.data.frame(data2)

item_list = data2[2,1:5] %>% as.character(); item_list = item_list[-1]

## 개별 item을 변수로 item별로 가져오기기
for(i in 1:length(item_list)){
    
    ## item별 index 찾아오기
    ind = which(data2[2, ] == item_list[i])
    temp = data2[, c(1,ind)]
    
    ## 저장했다 skip하여 header처리하여 가져오기
    assign(item_list[i], temp)
    fwrite(get(item_list[i]), paste0(path_result, "/data", i, ".csv") )
    rm(temp)
    
    temp = fread(paste0(path_result, "/data", i, ".csv"), header = T, encoding ="UTF-8", skip = 1)
    temp = temp[-1, ]; temp = as.data.frame(temp)
    
    ## 쉼표 제거
    for(j in 2:length(temp)){
        temp[,j] = as.numeric(str_remove_all(temp[,j], ","))
    }
    
    ## 형변환
    temp = pivot_longer(temp, cols = 2:length(temp), values_to = item_list[i])
    temp = temp %>% rename(ticker = name, Date = Symbol)
    
    ## 변수 저장
    assign(item_list[i], temp)
    rm(temp)
}

rm(data2)
gc()

## 병합
temp = adj_price

for(i in 1:4){
    temp = merge(temp, get(item_list[i]), by = c("Date", "ticker"))
}

##
temp = temp[,-length(temp)]
temp = temp %>% rename(adj_price = adj_price.x)
##
data2 <- temp
rm(list=ls()[ls() %in% item_list])
rm(temp)
gc()

## Data3 : rm-rf
path = "H:/내 드라이브/R/Factor investing/investing/Raw_data"
path_result = "H:/내 드라이브/R/Factor investing/investing/preprocessed"
library(TTR)

data3 = fread(paste0(path, "/Data3.csv"), header = T, encoding = "UTF-8")
data3 = as.data.frame(data3)
data3[,2] = as.numeric(str_remove_all(data3[,2], ","))

data3 = data3 %>% arrange(Frequency) %>% 
    mutate(Rm = ROC(KOSPI, 1, "discrete")) %>% 
    mutate(Rm_Rf = Rm-`1yr_int`) %>% select(Frequency, Rm_Rf) %>% 
    rename(Date = Frequency)

data3$Date = as.character(data3$Date)

##Data2와 Dat3 결합
data2 = left_join(data2, data3, by = "Date")

##data4: 업종구분
data4 = fread(paste0(path, "/Data4.csv"), header = T, encoding = "UTF-8")
data4 = pivot_longer(data4, col =1:length(data4)); data4 = as.data.frame(data4)
data4 = data4 %>% rename(ticekr = value)

data4 = data4 %>% filter(ticekr == "제조업")
## data2에서 제조업만 남기기기
data2 = data2[data2$ticker %in% data4$name, ]


rm(data3, data4)

data2 = data2 %>% group_by(ticker) %>% 
    arrange(Date) %>% mutate(Size = price * issuees) %>% 
    mutate(RET = ROC(adj_price, 1, "discrete")) %>% ungroup()

fwrite(data2, paste0(path_result, "/MKT_data.csv"))