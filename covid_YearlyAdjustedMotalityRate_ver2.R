################################################################################################
#
# covid_YearlyAdjustedMotalityRate_ver2.R
#
#   Yearly Adjusted Mortality Rate
#
#   ①　厚労省　データからわかる－新型コロナウイルス感染症情報－
#       https://covid19.mhlw.go.jp/?lang=ja
#         性別・年代別死亡者数（累積）open data:
#         https://covid19.mhlw.go.jp/public/opendata/deaths_detail_cumulative_weekly.csv
#         
#
################################################################################################

library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(tidyr)
library(ggrepel)
library(slider)
library(stringr)


mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}


URL <- "https://covid19.mhlw.go.jp/public/opendata/deaths_detail_cumulative_weekly.csv"
sht <- read.csv(URL, header = F)

StartD <- as.Date("2022/01/05")
LastD <- as.Date(unlist(strsplit(sht[nrow(sht), 1], "~"))[2])

#
# Japan
#

sht %>%
  slice(-1, -2) %>%
  separate(V1, into = c("begin", "end"), sep = "~", remove = FALSE) %>%
  mutate(DATE = as.Date(begin)) %>%
  filter(DATE >= StartD - 7) %>%
  select(DATE, V2:V21) %>%
  filter(DATE %in% c(min(DATE),max(DATE))) -> ALL

diffALL <- as.numeric(ALL[2, -1]) - as.numeric(ALL[1, -1])
deathALL <- diffALL[1:10] + diffALL[11:20]


#
# Saitama
#

sht %>%
  slice(-1, -2) %>%
  separate(V1, into = c("begin", "end"), sep = "~", remove = FALSE) %>%
  mutate(DATE = as.Date(begin)) %>%
  filter(DATE >= StartD - 7) %>%
  select(DATE, V222:V241) %>%
  filter(DATE %in% c(min(DATE),max(DATE))) -> SAITAMA

diffSAITAMA <- as.numeric(SAITAMA[2, -1]) - as.numeric(SAITAMA[1, -1])
diffSAITAMA[is.na(diffSAITAMA)] <- 0
deathSAITAMA <- diffSAITAMA[1:10] + diffSAITAMA[11:20]


#
# Chiba
#

URL <- "https://raw.githubusercontent.com/igproj-fusion/covid19_data/main/covid_chiba_deaths.csv"
#URL <- "/home/my/data/Chiba_pref_covid/covid_chiba_deaths.csv"
read.csv(URL) %>%
  select(Age, Date) %>%
  mutate(Date = as.Date(Date, format = "%Y年%m月%d日")) %>%
  filter(Date >= StartD) %>%
  filter(Date <= LastD) %>%
  filter(Age != is.na(Age)) %>%
  mutate_cond(Age == 100, Age = 90) %>%
  count(Age) %>%
  right_join(data.frame(Age = seq(0, 90, by = 10)), by = c("Age" = "Age")) %>%
  arrange(Age) %>%
  mutate_cond(is.na(n), n = 0) -> CHIBA

deathCHIBA <- CHIBA$n


#
# Calculation
#

p <- read.csv("~/data/Covid_demography/Jpop47.csv")
popALL <- as.numeric(p[1,-1])
popCHIBA <- as.numeric(p[13,-1])
popSAITAMA <- as.numeric(p[12,-1])

q <- read.csv("~/data/Covid_demography/H27ModelPop.csv", header = FALSE)
modelPop <- q$V2 * 1000

ALLD <- sum(deathALL / popALL * modelPop) / sum(modelPop) * 100000
CHIBAD <- sum(deathCHIBA / popCHIBA * modelPop) / sum(modelPop) * 100000
SAITAMAD <- sum(deathSAITAMA / popSAITAMA * modelPop) / sum(modelPop) * 100000
