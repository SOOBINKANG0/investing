##
source("source.R")

## Rm-Rf
path = "H:/내 드라이브/R/Factor investing/investing/Raw_data"
data2 = fread(paste0(path, "/data2.csv"), header = T, encoding = 'UTF-8')

data2$KOSPI = str_remove_all(data2$KOSPI, ",")
data2$KOSPI = as.numeric(data2$KOSPI); data2 = as.data.frame(data2)

data2 = data2 %>% mutate(Rm = ROC(KOSPI, 1, 'discrete')) %>% 
    mutate(Rm_Rf = Rm-Rf) %>% rename(Date = Frequency) %>% select(-KOSPI, -Rf, -Rm)

## Growth factor
data4 = fread(paste0(path, "/data4.csv"), header = T, encoding = 'UTF-8')
data4 = as.data.frame(data4)

for(i in 2:length(data4)){
    data4[,i] = as.numeric(str_remove_all(data4[,i], ","))
}

data4 = data4 %>% pivot_longer(cols = -ticker, names_to = "Date", values_to = "sales") %>% 
    relocate(Date, .before = ticker)
data4 = data4 %>% group_by(ticker) %>% 
    arrange(Date) %>% mutate(growth_factor = ROC(sales, 1, "discrete"))
data4 = as.data.frame(data4); data4$Date = as.IDate(data4$Date)

data4 = data4 %>% filter(Date != "1999-12-28")

## data3(accounting_data)
data3 = fread(paste0(path, "/data3.csv"), header = T, encoding = 'UTF-8')
data3 = as.data.frame(data3)

for(i in 3:length(data3)){
    data3[,i] = as.numeric(str_remove_all(data3[,i], ","))
}

item_list = c("Asset", "Liab", "Equity")

## separating_item
for(i in 1:3){
    assign(item_list[i], data3[data3$`Item Name` %in% item_list[i],] )    
}

## wide to long format 
for(i in 1:3){

    temp = get(item_list[i])
    temp = temp[,-2]
    
    temp = temp %>% pivot_longer(cols = -Symbol)
    temp = as.data.frame(temp)
    
    temp = temp %>% rename(Date = name,
                           ticker = Symbol) %>% 
        relocate(Date, .before = ticker)
    names(temp)[names(temp) == "value"] <- item_list[i]
    
    assign(item_list[i], temp)
    rm(temp)

}

## Data3, data4 merge
acc_data = merge(Asset, Liab, by = c("Date", "ticker"))
acc_data = merge(acc_data, Equity, by = c("Date", "ticker"))

rm(Asset, Liab, Equity, data3)

## merge acc(asset, liab, equity, growth factor)
acc_data$Date = as.IDate(acc_data$Date)
acc_data = merge(acc_data, data4, all.x = T, by = c("Date", "ticker"))

rm(data4)
gc()

## data1_preprocssing
data1 = fread(paste0(path, "/data1.csv"), header =T, encoding = "UTF-8")
data1 = as.data.frame(data1)

## remove ","
for(i in 3:length(data1)){
    data1[,i] = as.numeric(str_remove_all(data1[,i],","))
}

item_list2 = data1$Item_name %>% unique()

## separating_item
for(i in 1:length(item_list2)){
    assign(item_list2[i], data1[data1$Item_name %in% item_list2[i],] )    
}

## wide to long format
for(i in 1:length(item_list2)){
    
    temp = get(item_list2[i])
    temp = temp[,-2]
    
    temp = temp %>% pivot_longer(cols = -Ticker)
    temp = as.data.frame(temp)
    
    temp = temp %>% rename(Date = name,
                           ticker = Ticker) %>% 
        relocate(Date, .before = ticker)
    names(temp)[names(temp) == "value"] <- item_list2[i]
    
    assign(item_list2[i], temp)
    rm(temp)
    
}

## merge
merged_dat = get(item_list2[1])

for(i in 2:length(item_list2)){
    temp = get(item_list2[i])
    merged_dat = merge(merged_dat, temp, all.x = T, by = c("Date",'ticker'))
    
    rm(temp)
}

##
rm_list = c("acc_data", "merged_dat", "data2")
rm(list = ls()[!ls() %in% rm_list])

gc()

## acc_data: 회계데이터 이용 가능시점에 일치시키기
## data2 -> rm-rf
## merged_dat: mkt 데이터


## merged_dat %>% filter(ticker == "A000110") %>% View() -> 거래정지
## 수익률 상장폐지 고려, 상장폐지되면 수익률 -100%
## 단, 수익률은 adj_price로 구하기
merged_dat = merged_dat %>% 
    group_by(ticker) %>% arrange(Date) %>% 
    mutate(size = price*stock_issues) %>% 
    mutate(ret =  ROC(adj_price, 1, "discrete")) %>% 
    mutate(lag_ret = lag(ret, 1)) %>% 
    mutate(ret = ifelse(is.na(price) & is.na(lag_ret) == F, -1, ret)) %>% 
    select(-lag_ret) %>% ungroup()

## momentum <- 확인필요
merged_dat = merged_dat %>% group_by(ticker) %>% arrange(Date) %>% 
    mutate(mom_temp = ROC(adj_price, 10, "discrete")) %>% 
    mutate(momentum = lag(mom_temp, 1)) %>% 
    select(-mom_temp) %>% ungroup()

#merged_dat %>% filter(ticker == "A005930") %>% select(Date, ticker, adj_price, ret, momentum) %>% View()

## volatility
vol = merged_dat %>% select(Date, ticker, ret)
vol = pivot_wider(vol, values_from = ret, names_from = ticker); vol = as.data.frame(vol)
vol = vol[-1,]

## bin
vol_data = data.frame(vol[,1])
ticker_list = colnames(vol)

## 12month vol
for(i in 2:length(vol)){
    temp = rollapply(data = vol[, i], FUN = sd, align = "right", width = 12, fill = NA)    
    vol_data = cbind(vol_data, temp)
}

colnames(vol_data) <- ticker_list
vol_data_temp = pivot_longer(data = vol_data, cols=2:length(vol_data), names_to = "ticker", values_to = "12mon_vol")
vol_data_temp = as.data.frame(vol_data_temp)

merged_dat = merge(merged_dat, vol_data_temp, by = c("Date", "ticker"), all.x = T)

##
rm(vol, vol_data, vol_data_temp, temp)
gc()

## acc_data 이용가능시점으로 만들기

## 1.mkt data's last day
day = merged_dat %>% ungroup() %>% select(Date) %>% unique() %>% 
    mutate(year = year(Date)) %>% 
    mutate(month = month(Date)) %>% mutate(day = day(Date))

day$month = as.character(day$month)
day = day %>% mutate(month = ifelse(str_count(month) == 1, paste0("0", month), month))

## 2. acc data's separating Date
acc_data = acc_data %>% 
    mutate(year = year(ymd(Date))) %>% mutate(month = month(ymd(Date)))
acc_data$month = as.character(acc_data$month)

acc_data = acc_data %>% mutate(month = ifelse(str_count(month) == 1, paste0("0", month), month))

##
temp = acc_data

temp = temp %>% filter(month == "03" | month == "06" | month == "09" | month == "12")

temp <- temp %>%
    mutate(month = case_when(
        month == "03" ~ "05",
        month == "06" ~ "08",
        month == "09" ~ "11",
        month == "12" ~ "03",
        TRUE ~ month  # Default case, if none of the above conditions are met
    ))

## add year + 1
temp$year = as.numeric(temp$year)
temp = temp %>% mutate(year = ifelse(month == "03", year+1, year))

# day %>% View() ##last day of the month
day = day %>% select(-Date)
temp = merge(temp, day, by = c("year", "month"))
temp = temp %>% mutate(Date2 = format(ymd(paste(year, month, day, sep = "-")), "%Y-%m-%d"))

temp = temp %>% 
    select(-year, -month, -day, -Date) %>% 
    relocate(Date2, .before = ticker) %>% 
    rename(Date = Date2)

acc_data <- temp

## all_data %>% filter(ticker == "A005930") %>% View()
## all_data
all_data = merge(merged_dat, acc_data, by = c("Date", "ticker"), all.x = T)
rm(temp,day, merged_dat, acc_data); gc()


temp <- all_data

temp <- temp %>% group_by(ticker) %>% arrange(Date) %>% 
    fill(Asset, Liab, Equity, .direction = "down") %>% 
    mutate(PBR = size/Equity/1000) %>% ungroup()

temp = merge(temp, data2, by ="Date")

##
fwrite(temp, "all_data.csv")
