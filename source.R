#source 저장하기
rm(list=ls())
pkg = c("tidyverse", "data.table", "reshape2", "forecast", 'TTR', "RSelenium", "rvest")

##
for(i in 1:length(pkg)){
    if(require(pkg[i], character.only = T) == F){
        install.packages(pkg[i])
    } else {
        library(pkg[i], character.only = T)
    }
}