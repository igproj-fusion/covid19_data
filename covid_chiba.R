library(sf)
library(dplyr)
library(ggplot2)
library(ggthemes)

Days <- 7

chiba <- st_read("https://raw.githubusercontent.com/igproj-fusion/covid19_data/main/chiba_admin.geojson")
df <- read.csv("https://raw.githubusercontent.com/igproj-fusion/covid19_data/main/covid_chiba_2022.csv", check.names = FALSE)

cDate <- tail(colnames(df), 1)
pDate <- tail(colnames(df), 2)[1]
sDate <- as.character(as.Date(cDate) - Days)

df %>% select(sichoson, all_of(cDate), all_of(pDate), all_of(sDate)) %>% 
  rename(Today = all_of(cDate), Yesterday = all_of(pDate), Begin = all_of(sDate)) %>% 
  mutate(Today = replace(Today, is.na(Today), 0),
         Yesterday = replace(Yesterday, is.na(Yesterday), 0),
         Begin = replace(Begin, is.na(Begin), 0)) %>%
  mutate(total = Today - Begin, new = Today - Yesterday) %>%
  mutate(total = replace(total, total == 0, NA),
         new = replace(new, new == 0, NA)) %>% 
  select(sichoson, new, total) -> dt

pf <- read.csv("https://raw.githubusercontent.com/igproj-fusion/covid19_data/main/chiba_pop_20211201.csv", 
               check.names = FALSE)
pop <- pf[-(2:7), c(1,2)]
dt$total2 <- round(dt$total * 100000 / pop$T, digits = 0)

map <- left_join(chiba, dt, by = c("SIKUCHOSON" = "sichoson"))
cap <- "Data: https://github.com/igproj-fusion/covid19_data/blob/main/covid_chiba_2022.csv"

nx <- c( 0.00,  0.02,  0.00,  0.00, -0.01,  0.00, -0.02,  0.03,  0.00,  0.00,
         -0.01,  0.00,  0.00,  0.00,  0.03,  0.015, 0.00,  0.00,  0.00,  0.00,
         0.01,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00, -0.01,
         0.01, -0.01,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
         -0.01,  0.00,  0.00,  0.00,  0.00,  0.00,  -0.02,  0.0,  0.00,  0.00,
         -0.01,  0.00,  0.00, -0.01)
ny <- c(-0.01, -0.02,  0.00, -0.015,-0.02,  0.00,  0.00, -0.05,  0.00,  0.00,
        0.00,  0.00,  0.00,  0.00, -0.03,  0.00,  0.00,  0.00,  0.00,  0.00,
        0.00,  0.00,  0.00, -0.03,  0.00,  0.00, -0.01,  0.00,  0.00,  0.01,
        -0.01,  0.045, 0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  0.00,
        0.00,  0.00,  0.00,  0.00,  0.00,  0.00,  -0.02,  0.0,  0.00,  0.00,
        0.00,  0.00,  0.00,  0.00)

ggplot(data = map) +
  geom_sf(aes(fill = new),
          alpha = 0.8, colour = 'grey5', size = 0.1) +
  scale_fill_gradient(low = "#fef9f9", high = "#dd8585", 
                      na.value = "white", name = "No. of Cases") + #, breaks=c(0,2,4,6,8,10,12)) +
  geom_sf_text(aes(label = new), nudge_x = nx, nudge_y = ny) +
  theme_map() +
  theme(legend.position = c(0.80, 0.05)) +
  labs(title = "COVID-19 Cases in Chiba",
       subtitle = paste("on ", cDate),
       caption = cap) +
  theme(plot.title = element_text(size = rel(1.6)),
        plot.subtitle = element_text(size = rel(1.4)),
        plot.caption = element_text(size = rel(1.2)))



ggplot(data = map) +
  geom_sf(aes(fill = total),
          alpha = 0.8, colour = 'grey5', size = 0.1) +
  scale_fill_gradient(low = "#fef9f9", high = "#cd0505", 
                      na.value = "white", name = "No. of Cases") + #, breaks=c(0,2,4,6,8,10)) +
  geom_sf_text(aes(label = total), nudge_x = nx, nudge_y = ny) +
  theme_map() +
  theme(legend.position = c(0.80, 0.05)) +
  labs(title = "COVID-19 Cases in Chiba",
       subtitle = paste("from ", as.character(as.Date(sDate) + 1), " to ", cDate),
       caption = cap) +
  theme(plot.title = element_text(size = rel(1.6)),
        plot.subtitle = element_text(size = rel(1.4)),
        plot.caption = element_text(size = rel(1.2)))



ggplot(data = map) +
  geom_sf(aes(fill = total2),
          alpha = 0.8, colour = 'grey5', size = 0.1) +
  scale_fill_gradient(low = "#fef9f9", high = "#cd0505", 
                      na.value = "white", name = "per 100,000") +
  geom_sf_text(aes(label = total2), nudge_x = nx, nudge_y = ny) +
  theme_map() +
  theme(legend.position = c(0.80, 0.05)) +
  labs(title = "COVID-19 Cases in Chiba",
       subtitle = paste("from ", as.character(as.Date(sDate) + 1), " to ", cDate),
       caption = cap) +
  theme(plot.title = element_text(size = rel(1.6)),
        plot.subtitle = element_text(size = rel(1.4)),
        plot.caption = element_text(size = rel(1.2)))


df %>%
  select(sichoson, ncol(.) - 14, ncol(.) - 7, ncol(.)) %>%
  mutate(ratio = unlist((.[4] - .[3]) / (.[3] - .[2]))) %>%
  select(sichoson, ratio) %>% 
  left_join(chiba, ., by = c("SIKUCHOSON" = "sichoson")) -> map


breaks <- c(min(map$ratio), 1.,  max(map$ratio))

ggplot(data = map) +
  geom_sf(aes(fill = ratio),
          alpha = 0.8, colour = 'grey5', size = 0.1) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "darkorange", 
                       midpoint = 1, 
                       breaks = breaks, 
                       labels = format(breaks, digits = 3)) +
  theme_map() +
  theme(legend.position = c(0.80, 0.05)) +
  labs(title = "千葉県各市町村の新規感染者数 前週比",
       subtitle = paste("on ", tail(colnames(df), 1)),
       caption = cap) +
  theme(plot.title = element_text(size = rel(2)),
        plot.subtitle = element_text(size = rel(1.6)),
        plot.caption = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.0)),
        legend.title = element_blank())
