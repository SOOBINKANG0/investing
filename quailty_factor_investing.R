##
source("source.R")

##
library(PortfolioAnalytics)
library(TTR)

##data loading 
path = "H:/내 드라이브/R/Factor investing/investing/investment"

data1 = fread(paste0(path, "/accounting.csv"), header= T, encoding = "UTF-8")
data1 = as.data.frame(data1)

data2 = fread(paste0(path, "/data.csv"), header= F, encoding = "UTF-8")
data2 = as.data.frame(data2)

## data1 preprocessing
for(i in 4:length(data1)){
    data1[,i] = as.numeric(str_remove_all(data1[,i], ","))
}

data1 = data1 %>% rename(month =`결산월`, year = `회계년`) %>% 
    mutate(Date = paste0(year, month)) %>% relocate(Date, .before = Symbol)

data1 = data1 %>% group_by(Symbol) %>% arrange(Date) %>% 
    mutate(asset_growth = ROC(Asset, 1, 'discrete')) %>%
    mutate(Gross_profit = Margin/Asset) %>% 
    mutate(acc_temp = 
               (CA-(cash+ST_fin_asset))-(CL-ST_fin_liab)) %>% ungroup()
data1 = data1 %>% 
    select(Date, Symbol, asset_growth, Gross_profit, acc_temp, Asset, equity, DEP) %>% 
    group_by(Symbol) %>% mutate(lag_acc = lag(acc_temp, 1)) %>% 
    mutate(Accrual = (acc_temp - lag_acc - DEP)/Asset) %>% ungroup()
data1 = data1 %>% filter(Date == "202312") ## infomation available for 2024. 03

## data2
item_list = c("price", "issue")

price_col = grep(data2[2,], pattern = "price")
issue_col = grep(data2[2,], pattern = "issue")

price = data2[,c(1, price_col)]
issue = data2[,c(1, issue_col)]

for(i in 1:length(item_list)){
    temp = get(item_list[i])
    fwrite(temp, paste0(item_list[i], ".csv"))
    
    tempy = fread(paste0(item_list[i], ".csv"), header = T, skip = 1, encoding = "UTF-8")
    tempy = tempy[-1,]; tempy = as.data.frame(tempy)
    
    for(j in 2:length(tempy)){
        tempy[,j] = as.numeric(str_remove_all(tempy[,j], ","))
    }
    
    assign(item_list[i], tempy)
} 

##
delist = c(ls()[ls() %in% item_list], "data1")
rm(list=ls()[!ls() %in% delist])

## longer data
issue = issue %>% pivot_longer(cols = 2:length(issue), names_to = "ticker")
issue = issue %>% rename(issue = value)
    
price = price %>% pivot_longer(cols = 2:length(price), names_to = "ticker")
price = price %>% rename(price = value)

## merge
data_set = merge(issue, price, by =c("Symbol", "ticker")) ## data_set_for_mkt
rm(issue, price)

##for data_set
data_set = data_set %>% 
    mutate(month = month(Symbol)) %>% 
    mutate(year = year(Symbol)) %>% 
    mutate(month = 
               ifelse(str_count(month) == 1, paste0("0", month), month))

data_set = data_set %>% mutate(Date = paste0(year, month)) %>% select(-month, -year) %>% 
    relocate(Date, .before = Symbol)

#data_set = data_set %>% select(-Symbol)

##filtering yearly
portfolio_performance <- data_set

data_set = data_set %>% filter(Date == "202403" | Date == "202303") %>% 
    group_by(ticker) %>% 
    mutate(issue_factor = ROC(issue, 1, "discrete"))

data_set = data_set %>% filter(Date == "202403")

## accounting info to available timing, 2024. 03
data1$Date = as.numeric(data1$Date); data1$Date = data1$Date+91; data1$Date = as.character(data1$Date)

## merge
data1 = data1 %>% rename(ticker = Symbol)
merged_dat = merge(data1, data_set, by = c("Date", "ticker"))

rm(data_set, data1); gc()

## select factor
merged_dat = merged_dat %>% mutate(PBR = price*issue/(equity*1000)) %>% 
     select(Date, ticker, asset_growth, Gross_profit, Accrual, issue_factor, PBR)

## manufacturing industry sorting
ind_try = fread("Industry.csv", header = T, encoding = "UTF-8") ## only 제조업
ind_try = ind_try %>% rename(ticker = Symbol)
merged_dat = merged_dat[merged_dat$ticker %in%  ind_try$ticker, ] # 3124개

rm(ind_try)

##
factor_list <- colnames(merged_dat)
factor_list = factor_list[!factor_list %in% c("Date", "ticker")]

merged_dat = as.data.frame(merged_dat)

## winsorize, z-score, sorting, 
for(i in 3:(length(merged_dat)-1)){
    merged_dat[,i] = ifelse(merged_dat[,i] > quantile(merged_dat[,i], 0.99,na.rm = T), 
                            quantile(merged_dat[,i], 0.99, na.rm = T),
                            
                            ifelse(merged_dat[,i] < quantile(merged_dat[,i], 0.01,na.rm = T), 
                                   quantile(merged_dat[,i], 0.01, na.rm = T), merged_dat[,i]))
    merged_dat[,i] = (merged_dat[,i] - mean(merged_dat[,i], na.rm =T))/sd(merged_dat[,i], na.rm = T)
}

## composite factor z score
merged_dat = merged_dat %>% rowwise() %>% 
    mutate(composite = sum(-asset_growth + Gross_profit + -Accrual + (-issue_factor))) %>% 
    ungroup() %>% 
    filter(PBR > 1) %>% 
    mutate(PBR_tile = ntile(PBR, 2)) %>% mutate(composite_tile = ntile(composite, 5))

## select stock investment Universe
Universe = merged_dat %>% filter(PBR_tile == 1 & composite_tile == 5 & 
                                     Gross_profit > 0 & issue_factor < 0 & asset_growth < 0 & Accrual < 0)
## Evalueate
port_perform<- portfolio_performance
rm(portfolio_performance)

port_perform = port_perform[port_perform$ticker %in% Universe$ticker, ]
port_perform = port_perform %>% select(-Symbol, -issue) %>% pivot_wider(names_from = ticker, values_from = price)
port_perform = as.data.frame(port_perform)

for(i in 2:length(port_perform)){
    port_perform[, i] = ROC(port_perform[,i], 1, "discrete")    
}

## sd
port_perform = port_perform[-1,]
bin = port_perform[-c(1:11),]

for(i in 2:length(port_perform)){
    bin[,i] = rollapply(port_perform[,i], FUN = sd, width = 12)
    bin[,i] = bin[,i]*sqrt(12)
}

## Equally weighted naive risk parity
## harmonic mean of sd
bin = bin[,-1]
weight = bin[4,]/sum(bin[4,])

amount = 50000000
should_buy = weight*amount/

weight
# A002880   A005610    A009450    A014990    A030190    A030350    A034940    A037270    A041460    A051900
# 16 0.1761461 0.0100592 0.02329726 0.01527252 0.01345028 0.03543803 0.01454001 0.02641139 0.01641172 0.02806453
# A052220    A052460    A052860    A060260    A065170    A065770    A067000 A068940   A070590    A071950    A073190
# 16 0.01465512 0.03740452 0.02519324 0.01959951 0.03081878 0.01575919 0.02347663       0 0.0319594 0.02004446 0.01178696
# A078520    A080720    A086710    A090430    A105630     A130580    A150900   A203450    A214420    A237820
# 16 0.03575283 0.01543004 0.02855356 0.02716319 0.02760235 0.006912998 0.02948977 0.0166976 0.04764762 0.04765821
# A263700    A289010    A293490    A331380    A339950    A348030    A367000
# 16 0.03205365 0.01146589 0.01481204 0.01768567 0.02243159 0.01342033 0.01543382

