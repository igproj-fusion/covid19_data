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

FirstD <- as.Date("2022/01/05")

URL1 <- "https://covid19.mhlw.go.jp/public/opendata/deaths_detail_cumulative_weekly.csv"
read.csv(URL1, header = F) %>%
  slice(-1, -2) %>%
  separate(V1, into = c("begin", "end"), sep = "~", remove = FALSE) %>%
  mutate(DATE = as.Date(begin)) %>%
  filter(DATE >= FirstD - 7) -> tmp

LastD <- as.Date(unlist(strsplit(tmp[nrow(tmp), 1], "~"))[2])

#
# Japan
#

tmp %>%
  select(DATE, V2:V21) %>%
  filter(DATE %in% c(min(DATE),max(DATE))) -> JAPAN

diffJAPAN <- as.numeric(JAPAN[2, -1]) - as.numeric(JAPAN[1, -1])
deathJAPAN <- diffJAPAN[1:10] + diffJAPAN[11:20]

#
# Saitama
#

tmp %>%
  select(DATE, V222:V241) %>%
  filter(DATE %in% c(min(DATE),max(DATE))) -> SAITAMA

diffSAITAMA <- as.numeric(SAITAMA[2, -1]) - as.numeric(SAITAMA[1, -1])
diffSAITAMA[is.na(diffSAITAMA)] <- 0
deathSAITAMA <- diffSAITAMA[1:10] + diffSAITAMA[11:20]

#
# Chiba
#

URL2 <- "https://raw.githubusercontent.com/igproj-fusion/covid19_data/main/covid_chiba_deaths.csv"
read.csv(URL2) %>%
  select(Age, Date) %>%
  mutate(Date = as.Date(Date, format = "%Y年%m月%d日")) %>%
  filter(Date >= FirstD) %>%
  filter(Date <= LastD) %>%
  filter(Age != is.na(Age)) %>%
  mutate_cond(Age == 100, Age = 90) %>%
  count(Age) %>%
  right_join(data.frame(Age = seq(0, 90, by = 10)), by = c("Age" = "Age")) %>%
  arrange(Age) %>%
  mutate_cond(is.na(n), n = 0) -> CHIBA

deathCHIBA <- CHIBA$n


p <- read.csv("https://raw.githubusercontent.com/igproj-fusion/covid19_data/main/Jpop47_20220101.csv")
popJAPAN <- as.numeric(p[1,-1])
popCHIBA <- as.numeric(p[13,-1])
popSAITAMA <- as.numeric(p[12,-1])

q <- read.csv("https://raw.githubusercontent.com/igproj-fusion/covid19_data/main/H27ModelPop.csv", 
              header = FALSE)
modelPop <- q$V2 * 1000

JapanD <- sum(deathJAPAN / popJAPAN * modelPop) / sum(modelPop) * 100000
ChibaD <- sum(deathCHIBA / popCHIBA * modelPop) / sum(modelPop) * 100000
SaitamaD <- sum(deathSAITAMA / popSAITAMA * modelPop) / sum(modelPop) * 100000

cat(paste0("\n\n", FirstD, "〜", LastD, "\n\n",
           "千葉県　", round(ChibaD, digits = 2), "\n",
           "全　国　 ", round(JapanD, digits = 2), "\n",
           "埼玉県　 ", round(SaitamaD, digits = 2), "\n\n"))
