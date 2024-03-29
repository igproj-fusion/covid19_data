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


library(dplyr)
library(tidyr)


mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

FirstD <- as.Date("2022/01/05")

#
# Deaths of Japan & Saitama
#

URL1 <- "https://covid19.mhlw.go.jp/public/opendata/deaths_detail_cumulative_weekly.csv"
read.csv(URL1, header = F) %>%
  slice(-1, -2) %>%
  separate(V1, into = c("begin", "end"), sep = "~", remove = FALSE) %>%
  mutate(DATE = as.Date(begin)) %>%
  filter(DATE >= FirstD - 7) %>%
  filter(DATE %in% c(min(DATE),max(DATE))) %>%
  select(DATE, V2:V21, V222:V241) -> tmp

LastD <- as.Date(tmp[nrow(tmp), 1]) + 6
Diff <- as.numeric(tmp[2, -1]) - as.numeric(tmp[1, -1])
Diff[is.na(Diff)] <- 0

JapanDeath <- Diff[1:10] + Diff[11:20]
SaitamaDeath <- Diff[21:30] + Diff[31:40]

#
# Deaths of Chiba
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
  mutate_cond(is.na(n), n = 0) %>%
  select(n) -> ChibaDeath


#
# Calculation of Age-adjusted Mortality
#

p <- read.csv("https://raw.githubusercontent.com/igproj-fusion/covid19_data/main/Jpop47_20220101.csv")
JapanPop <- as.numeric(p[1,-1])
ChibaPop <- as.numeric(p[13,-1])
SaitamaPop <- as.numeric(p[12,-1])


q <- read.csv("https://raw.githubusercontent.com/igproj-fusion/covid19_data/main/H27ModelPop.csv", 
              header = FALSE)
modelPop <- q$V2 * 1000


JapanMR <- sum(JapanDeath / JapanPop * modelPop) / sum(modelPop) * 100000
ChibaMR <- sum(ChibaDeath / ChibaPop * modelPop) / sum(modelPop) * 100000
SaitamaMR <- sum(SaitamaDeath / SaitamaPop * modelPop) / sum(modelPop) * 100000

cat(paste0("\n\n", FirstD, "〜", LastD, "\n\n",
           "千葉県　", round(ChibaMR, digits = 2), "\n",
           "全　国　 ", round(JapanMR, digits = 2), "\n",
           "埼玉県　 ", round(SaitamaMR, digits = 2), "\n\n"))


