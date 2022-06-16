################################################################################
################################################################################
######                        DATA CHALLENGE                             #######
######                 DATE: 26.11.2021 - 03.12.2021                     #######
######                           Zhi Zhou                                #######
################################################################################
################################################################################



################################################################################
######                         Install Package                           #######                         
################################################################################

# RSQLite embeds the SQLite database engine in R.
install.packages("RSQLite")   
# This will also install several packages that are needed to interact with SQLite
# databases from R such as blob, DBI, memories and plogr.

# dplyr is a grammar of data manipulation 
install.packages("tidyverse")

# Some easy to use functions for creating and customizing "ggplot2"
install.packages("ggpubr")

# Combine separate ggplots into the same graphic
install.packages("patchwork")

# Convert country name and country code
install.packages("countrycode")

# Country code lists
install.packages("ISOcodes")

# Generate wordclods
install.packages("wordcloud2")



################################################################################
######                       Load/Attach Package                         #######                         
################################################################################

library("DBI")
library("RSQLite")
library("tidyverse")
library("stringr")
library("lubridate")
library("ggpubr")
library("patchwork")
library("countrycode")
library("ISOcodes")
library("wordcloud2")



################################################################################
######                       Connect to a Database                       #######                         
################################################################################  

## Connect to the sqlite file
sample <- dbConnect(RSQLite::SQLite(), 
                    "C:/Users/Administrator/Desktop/Supercell/sample.sqlite")

## Get a list of all tables
alltables <- dbListTables(sample)

## Get tables
# Get table "account"
account <- dbReadTable(sample, "account")
# Get table "account_date_session"
account_date_session <- dbReadTable(sample, "account_date_session")
# Get table "iap_purchase"
iap_purchase <- dbReadTable(sample, "iap_purchase")

## Data fetched, disconnect
dbDisconnect(sample)


################################ TASK 1, Q1 ####################################


# Check if there exists account_id in "account" but not in "account_date_session
NFid <- account$account_id[!account$account_id %in% account_date_session$account_id]
# There are 341 players not in session table. Those players should also be counted
# when calculating Daily Active User. 

# Full Join account and account_date_session
fullJoinDF <- full_join(account, account_date_session, by = "account_id") 
# Fill missing unit in date
fullJoinDF <- fullJoinDF %>% 
  mutate(date = case_when(is.na(date) ~ ymd(str_extract_all(created_time, "\\d{4}-\\d{2}-\\d{2}")),
                          TRUE ~ as.Date(date, "%Y-%m-%d")))

# Daily New User (DNU) vs DAU of the 2016-01-01 
DNU_1st <- account %>%  ymd(str_extract_all(created_time, "\\d{4}-\\d{2}-\\d{2}"))) %>% 
  filter(created_date == "2016-01-01")
DAU_1st <- fullJoinDF %>% 
  filter(date == "2016-01-01")
# Notice that DAU of 2016-01-01 = 1084 and DNU of 2016-01-01 = 1091, suppose 2016-01-01
# is the date when the game launched. Active user should be the same be the same as 
# new users. 
# DNU_1st[!DNU_1st$account_id %in% DAU_1st$account_id,]
# Gives which account id is not in DAU_1st

# Daily Active Users
DAU <- fullJoinDF %>% 
  mutate(month = month(date)) %>% 
  group_by(date, month) %>% 
  count() 
# Average Daily Active users
avgDAU <- mean(DAU$n)

# By findings above, substitute the fist n with 1091
DAU[1, 3] <- 1091

# Plot DAU
pDAU <- ggplot(DAU, aes(x = date, y = n/1000)) +
  geom_line() +
  geom_hline(yintercept = avgDAU/1000, 
             linetype = "dashed", 
             size = 1, 
             color = "red", 
             alpha = 0.5) +
  theme_bw() + 
  scale_x_date(name = " ",
               date_breaks = "1 month", 
               minor_breaks = "1 week", 
               date_labels = "%b %d") + 
  scale_y_continuous(name = "DAU in thousand (K)", limits = c(0, 6.5)) + 
  ggtitle(" ") + 
theme(axis.text.x = element_text(angle = 45, size = 11, vjust = 0.5),
      axis.title.x = element_text(size = 18, margin = margin(10, 0, 0, 0)),
      axis.text.y = element_text(size = 11),
      axis.title.y = element_text(size = 13, margin = margin(0, 10, 0, 0)),
      aspect.ratio = 4/16)

# Add brackets & annotations 
pDAU <- pDAU + geom_bracket(xmin = as.Date("2016-06-01", "%Y-%m-%d"), 
                    xmax = as.Date("2016-09-01", "%Y-%m-%d"), 
                    y.position = 6, 
                    tip.length = 0.03, 
                    label = "Summer holidays",
                    vjust = -0.5) +
  geom_bracket(xmin = as.Date("2016-01-01", "%Y-%m-%d"), 
               xmax = as.Date("2016-03-01", "%Y-%m-%d"), 
               y.position = 6, 
               tip.length = 0.03, 
               label = "Expanding period",
               vjust = -0.5) +
  annotate(
    geom = "curve", 
    x = as.Date("2016-09-27", "%Y-%m-%d"), 
    y = 5.25, 
    xend = as.Date("2016-09-05", "%Y-%m-%d"), 
    yend = 4.539, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  ) +
  annotate(geom = "text", 
           x = as.Date("2016-09-27", "%Y-%m-%d"), 
           y = 5.25, label = "Holiday season ends", hjust = "left")

# DAU of March
Mar.DAU <- DAU %>% 
  filter(month == 3)

# Plot
pMar <- ggplot(Mar.DAU, aes(x = date, y = n/1000)) +
  geom_line() +
  geom_hline(yintercept = avgDAU/1000, 
             linetype = "dashed", 
             size = 1, 
             color = "red", 
             alpha = 0.5) +
  theme_bw() + 
  scale_x_date(date_breaks = "2 day", 
               date_minor_breaks = "1 day", 
               date_labels = "%d %a") + 
  scale_y_continuous(name = "DAU (March) in thousand (K)", limits = c(0, 6.5)) + 
  ggtitle(" ") + 
  theme(axis.text.x = element_text(angle = 45, size = 11, vjust = 0.5),
        axis.title.x = element_text(size = 18, margin = margin(10, 0, 20, 0)),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 13, margin = margin(0, 10, 0, 0)),
        aspect.ratio = 4/16)
# Add brackets & annotations
pMar <- pMar + geom_bracket(xmin = as.Date("2016-03-05", "%Y-%m-%d"), 
                    xmax = as.Date("2016-03-06", "%Y-%m-%d"), 
                    y.position = 6, 
                    tip.length = 0.03, 
                    label = "1st Weekend",
                    vjust = -0.5) +
  geom_bracket(xmin = as.Date("2016-03-12", "%Y-%m-%d"), 
               xmax = as.Date("2016-03-13", "%Y-%m-%d"), 
               y.position = 6, 
               tip.length = 0.03, 
               label = "2nd Weekend",
               vjust = -0.5) +
  geom_bracket(xmin = as.Date("2016-03-19", "%Y-%m-%d"), 
               xmax = as.Date("2016-03-20", "%Y-%m-%d"), 
               y.position = 6, 
               tip.length = 0.03, 
               label = "3rd Weekend",
               vjust = -0.5) +
  geom_bracket(xmin = as.Date("2016-03-26", "%Y-%m-%d"), 
               xmax = as.Date("2016-03-27", "%Y-%m-%d"), 
               y.position = 6, 
               tip.length = 0.03, 
               label = "4th Weekend",
               vjust = -0.5)
# Combine pDAR & pMar into the same graphic
pDAU/pMar



################################ APPENDIX I ####################################

# Monthly Active User
MAU <- fullJoinDF %>% 
  mutate(month = month(date)) %>% 
  group_by(month) %>% 
  summarise(n = n_distinct(account_id))
# Average Monthly Active Users
avgMAU <- mean(MAU$n)
# add date to MAU for visualization
MAU$date <- as.Date(c("2016-01-16", "2016-02-15", "2016-03-16", "2016-04-15", "2016-05-16", "2016-06-15",
                      "2016-07-16", "2016-08-16", "2016-09-15", "2016-10-16", "2016-11-15", "2016-12-16"),
                    "%Y-%m-%d")

# Plot MAU
pMAU <- ggplot(MAU, aes(x = date, y = n/1000)) +
  geom_line() + 
  geom_hline(yintercept = avgMAU/1000, 
             linetype = "dashed", 
             size = 1, 
             color = "red", 
             alpha = 0.5) +
  theme_bw() + 
  scale_x_date(name = " ",
               date_breaks = "1 month", 
               minor_breaks = "1 week", 
               date_labels = "%b %d") + 
  scale_y_continuous(name = "MAU in thousand (K)", limits = c(10, 25)) + 
  ggtitle(" ") +
  theme(axis.text.x = element_text(angle = 45, size = 11, vjust = 0.5),
        axis.title.x = element_text(size = 18, margin = margin(10, 0, 0, 0)),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 13, margin = margin(0, 10, 0, 0)),
        aspect.ratio = 4/16)



# Stickiness in %
stickiness <- full_join(DAU, MAU, by = "month") %>% 
  mutate(stickiness = n.x/n.y * 100)
avgStickiness <- mean(stickiness$stickiness)

# Plot stickiness
pStickiness <- ggplot(stickiness, aes(x = date.x, y = stickiness)) +
  geom_line() + 
  geom_hline(yintercept = avgStickiness, 
             linetype = "dashed", 
             size = 1, 
             color = "red", 
             alpha = 0.5) +
  theme_bw() + 
  scale_x_date(name = "date",
               date_breaks = "1 month", 
               minor_breaks = "1 week", 
               date_labels = "%b %d") + 
  scale_y_continuous(name = "Stickiness in %", limits = c(0, 50)) + 
  ggtitle(" ") +
  theme(axis.text.x = element_text(angle = 45, size = 11, vjust = 0.5),
        axis.title.x = element_text(size = 18, margin = margin(10, 0, 0, 0)),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 13, margin = margin(0, 10, 0, 0)),
        aspect.ratio = 4/16)

# Combine pDAU, pMAU & pStickiness
pDAU/pMAU/pStickiness


############################### APPENDIX II ####################################

# Daily Revenue
DR <- iap_purchase %>% 
  mutate(created_date = ymd(str_extract_all(created_time, "\\d{4}-\\d{2}-\\d{2}")),
         month = month(created_date)) %>% 
  group_by(created_date, month) %>% 
  summarise(sum = sum(iap_price_usd_cents)/100) 
avgDR <- mean(DR$sum)

# Plot DR
ggplot(DR, aes(x = created_date, y = sum)) +
  geom_line() + 
  geom_hline(yintercept = avgDR, 
             linetype = "dashed", 
             size = 1, 
             color = "red", 
             alpha = 0.5) +
  theme_bw() + 
  scale_x_date(name = "date",
               date_breaks = "1 month", 
               minor_breaks = "1 week", 
               date_labels = "%b %d") + 
  scale_y_continuous(name = "DR in USD dollar", limits = c(0, 1300)) + 
  ggtitle(" ") +
  theme(axis.text.x = element_text(angle = 45, size = 11, vjust = 0.5),
        axis.title.x = element_text(size = 18, margin = margin(10, 0, 0, 0)),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 13, margin = margin(0, 10, 0, 0)),
        aspect.ratio = 9/16)


 
################################ TASK 1, Q2 ####################################

# Users
# Registration country counts
nRegistration <- fullJoinDF %>% 
  group_by(country_code) %>% 
  summarise(n = n_distinct(account_id)) %>% 
  arrange(desc(n))

# Turn country code to country name 
# NOTICE: Kosovo (XK) is not in the list, And I have manually added it to the user dictionary,
# so it denoted by NA by country code function. As there is only 1 registered user in 
# Kosovo, it does not affect the result much.
nRegistration$country_code <- countrycode(nRegistration$country_code, 
                                          "Alpha_2",
                                          "Name",
                                          custom_dict = ISOcodes::ISO_3166_1)

# Show the first six lines in the date frame
head(nRegistration)

# Word Cloud
wordcloud2(nRegistration, color = "random-light", backgroundColor = "white") 


# Calculate DAU of top 4 countries
DAU_top4 <- fullJoinDF %>% 
  group_by(country_code, date) %>% 
  summarise(n = n_distinct(account_id)) %>% 
  filter(country_code %in% c("CN", "US", "TR", "FR"))

# Convert country code to country name
DAU_top4$country_code <- factor(countrycode(DAU_top4$country_code, 
                                     "Alpha_2",
                                     "Name",
                                     custom_dict = ISOcodes::ISO_3166_1),
                                levels = c("China", "United States", "Turkey", "France"))
# Calculate average DAU for each of top 4 country
avgDAU_top4 <- DAU_top4 %>% 
  group_by(country_code) %>% 
  summarise(mean = mean(n))

# Plot DAU of top 4 countries
pDAU_top4 <- ggplot(DAU_top4, aes(x = as.Date(date, "%Y-%m-%d"), y = n/1000)) +
  geom_line() +
  theme_bw() + 
  scale_x_date(name = "date",
               date_breaks = "1 month", 
               minor_breaks = "1 week", 
               date_labels = "%b %d") + 
  scale_y_continuous(name = "DAU in thousand (K)", limits = c(0, 3)) + 
  ggtitle(" ") + 
  facet_wrap(~ country_code, ncol = 2) +
  geom_hline(data = avgDAU_top4,
             aes(yintercept = mean/1000), 
             linetype = "dashed", 
             size = 1, 
             color = "red", 
             alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 45, size = 11, vjust = 0.5),
        axis.title.x = element_text(size = 18, margin = margin(10, 0, 0, 0)),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 13, margin = margin(0, 10, 0, 0)),
        strip.background = element_rect(fill = "gray95"),
        strip.text = element_text(size = 12),
        aspect.ratio = 1/3)


# Revenue 
# Full join table account and iap_purchase
rfullJoinDF <- full_join(account, iap_purchase, by = "account_id")

# Revenue for each date based on country
rCountry_Date <- rfullJoinDF %>% 
  mutate(created_date = ymd(str_extract_all(created_time.y, "\\d{4}-\\d{2}-\\d{2}"))) %>% 
  group_by(country_code, created_date) %>% 
  summarise(amount = sum(iap_price_usd_cents)) %>% 
  na.omit()

# Purchase Frequency for each country
rCountry <- rCountry_Date %>% 
  group_by(country_code) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

# Convert code to name
rCountry$country_code <- countrycode(rCountry$country_code, 
                                     "Alpha_2",
                                      "Name",
                                      custom_dict = ISOcodes::ISO_3166_1)

# Word Cloud to see the purchase frequency
wordcloud2(rCountry, color = "random-dark", backgroundColor = "white", size = 0.5)

# Total revenue based on country
totalRC <- rCountry_Date %>% 
  group_by(country_code) %>% 
  summarise(total = sum(amount)) %>% 
  arrange(desc(total))

# Convert code to name
totalRC$country_code <- countrycode(totalRC$country_code, 
                                    "Alpha_2",
                                    "Name",
                                    custom_dict = ISOcodes::ISO_3166_1)

# Word Cloud to see the total revenue for each country
wordcloud2(totalRC, color = "random-dark", backgroundColor = "white", size = 0.5)

# Revenue by date of top 3 country
rCountry_top3 <- rCountry_Date %>% 
  filter(country_code %in% c("CN", "US", "KR"))

# Convert code to name
rCountry_top3$country_code <- countrycode(rCountry_top3$country_code, 
                                          "Alpha_2",
                                          "Name",
                                          custom_dict = ISOcodes::ISO_3166_1)


# Average revenue of top 3 country
avgRevenue_top3 <- rCountry_top3 %>% 
  group_by(country_code) %>% 
  summarise(mean = mean(amount/100))

# Plot revenue for top 3 country
pRevenue_top3 <- ggplot(rCountry_top3, aes(x = created_date, y = amount/100)) +
  geom_line() +
  theme_bw() + 
  scale_x_date(name = "date",
               date_breaks = "1 month", 
               minor_breaks = "1 week", 
               date_labels = "%b %d") + 
  scale_y_continuous(name = "Revenue in USD", limits = c(0, 1100)) + 
  ggtitle(" ") + 
  facet_wrap(~ country_code, ncol = 2) +
  geom_hline(data = avgRevenue_top3,
             aes(yintercept = mean), 
             linetype = "dashed", 
             size = 1, 
             color = "red", 
             alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 45, size = 11, vjust = 0.5),
        axis.title.x = element_text(size = 18, margin = margin(10, 0, 0, 0)),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 13, margin = margin(0, 10, 0, 0)),
        strip.background = element_rect(fill = "gray95"),
        strip.text = element_text(size = 12),
        aspect.ratio = 1/3)

# Revenue by continent
rContinent <- rCountry_Date 
rContinent$country_code <- countrycode(rContinent$country_code,
                                       origin = "iso2c",
                                       destination = "continent")

# Average revenue of continents
avgRevenue_continent <- rContinent %>% 
  group_by(country_code) %>% 
  summarise(mean = mean(amount/100))

# Plot revenue by continent 
pRevenue_continent <- ggplot(rContinent, aes(x = created_date, y = amount/100)) +
  geom_line() +
  theme_bw() + 
  scale_x_date(name = "date",
               date_breaks = "1 month", 
               minor_breaks = "1 week", 
               date_labels = "%b %d") + 
  scale_y_continuous(name = "Revenue in USD", limits = c(0, 1100)) + 
  ggtitle(" ") + 
  facet_wrap(~ country_code, ncol = 2) +
  geom_hline(data = avgRevenue_continent,
             aes(yintercept = mean), 
             linetype = "dashed", 
             size = 1, 
             color = "red", 
             alpha = 0.5) +
  theme(axis.text.x = element_text(angle = 45, size = 11, vjust = 0.5),
        axis.title.x = element_text(size = 18, margin = margin(10, 0, 0, 0)),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 13, margin = margin(0, 10, 0, 0)),
        strip.background = element_rect(fill = "gray95"),
        strip.text = element_text(size = 12),
        aspect.ratio = 1/3)



################################ TASK 1, Q3 ####################################

# Weekly Active Users
WAU.full <- account_date_session %>% 
  mutate(week = week(date)) %>% 
  group_by(week) %>% 
  summarise(WAU = n_distinct(account_id))

# Weekly Revenue
WR <- iap_purchase %>% 
  mutate(week = week(ymd(str_extract_all(created_time, "\\d{4}-\\d{2}-\\d{2}")))) %>% 
  group_by(week) %>% 
  summarise(WR = sum(iap_price_usd_cents))

# Weekly New Users
WNU.full <- fullJoinDF %>% 
  mutate(created_week = week(ymd(str_extract_all(created_time, "\\d{4}-\\d{2}-\\d{2}")))) %>% 
  group_by(created_week) %>% 
  summarise(WNU = n_distinct(account_id)) 

# Weekly Average Revenue Per User
ARPU <- full_join(WAU.full, WR, by = "week")  %>% 
  mutate(ARPU = WR/WAU)

# Get mean
mean(ARPU$ARPU)

# Retention 
re <- vector(length(1:53), mode="list")
for(i in 1:52){
  WNU <- fullJoinDF %>% 
    mutate(created_week = week(ymd(str_extract_all(created_time, "\\d{4}-\\d{2}-\\d{2}")))) %>% 
    group_by(created_week) %>% 
    mutate(WNU = n_distinct(account_id)) %>% 
    ungroup() %>% 
    subset(created_week == i)
  for (j in (i+1):53) {
    WAU <- fullJoinDF %>% 
      mutate(week = week(date)) %>% 
      group_by(week) %>% 
      mutate(WAU = n_distinct(account_id)) %>% 
      subset(week == j)
    
    re[[i]][j] <- sum(unique(WAU$account_id) %in% unique(WNU$account_id))
  }
}
# Remove missing values
r <- re %>% 
  lapply(na.omit)

install.packages("data.table")
install.packages("plyr")
library(data.table)
# Convert list to data frame and ombine WNU.full to r.data
r.data <- as.data.frame(t(plyr::rbind.fill(as.data.frame(t(WNU.full$WNU)), as.data.frame(t(plyr::ldply(r, rbind))))))
# Save it and edit in EXCEL
# save(r.data, file = "retention.csv")

# Derived a model from r.data
# Calculate summation Retention rate for week 53
sum = 0
for (i in 1:53) {
  y <- 0.3715 * i^(-0.632)
  sum = y + sum
}
sum

################################ TASK 2, Q2 ####################################

# Platform
platform <- as.data.frame(table(account$created_platform))
colnames(platform) <- c("platform", "value")

platform <- platform %>% 
  arrange(desc(platform)) %>%
  mutate(prop = value / sum(value)*100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

pie_platform <- ggplot(platform, aes(x = "", y = value, fill = platform)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=4) +
  scale_fill_brewer("Platform") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=14, face="bold"),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20)) +  
  theme(axis.text.x = element_blank()) +
  geom_text(aes(y = value/2 + c(0, cumsum(value)[-length(value)]), 
                label = paste(round(prop, 2), "%", sep="")), size=5)

# Device android
device <- account %>% 
  filter(created_platform == "Android") %>% 
  group_by(created_device) %>% 
  count() %>% 
  arrange(desc(n))
# Take first 20 rows 
device_top20 <- device[1:20, ]
# Plot 
pDevice <- ggplot(data = device_top20, aes(x = as.factor(created_device), y = n)) +
  geom_bar(stat="identity", fill = "steelblue") +
  theme_bw() +
  xlab("Device Model") +
  ggtitle(" ") + 
  theme(axis.text.x = element_text(angle = 90, size = 17),
        axis.title.x = element_text(size = 20, margin = margin(10, 0, 20, 0)),
        axis.text.y = element_text(size = 17),
        axis.title.y = element_text(size = 20, margin = margin(0, 10, 0, 20)),
        plot.title = element_text(margin = margin(20, 0, 10, 0))) + 
  scale_x_discrete(limits=c("SM-G900F", "GT-I9500", "GT-I9300", "GT-I9060I", "SM-T113", 
                            "GT-N7100", "SM-T110", "SAMSUNG-SM-N900A", "HM 2A", "Redmi Note 3",
                            "A31", "ASUS_T00J", "SM-G920F", "MI 4LTE", "SM-G531H",        
                            "GT-I9505", "HM NOTE 1S", "HM NOTE 1LTE", "GT-I9301I", "Redmi Note 2"))

# app store
app_store <- account %>% 
  filter(created_platform == "Android") %>% 
  group_by(created_app_store_id) %>% 
  count() %>% 
  arrange(desc(n))
# Plot 
pAppstore <- ggplot(data = app_store, aes(x = as.character(created_app_store_id), y = n)) +
  geom_bar(stat="identity", fill="steelblue") +
  theme_bw() +
  xlab("App Store") +
  ggtitle(" ") + 
  theme(axis.text.x = element_text(angle = 0, size = 17),
        axis.title.x = element_text(size = 20, margin = margin(10, 0, 20, 0)),
        axis.text.y = element_text(size = 17),
        axis.title.y = element_text(size = 20, margin = margin(0, 10, 0, 20)),
        plot.title = element_text(margin = margin(20, 0, 10, 0))) + 
  scale_x_discrete(limits=c("2", "4", "19", "3", "7", "10", "16", "6", "8", "15", "11" ,"14", "9", "0"))

# Combine 
(pie_platform + pDevice)/ (pAppstore)



# Device IOS
device_IOS <- account %>% 
  filter(created_platform == "iOS") %>% 
  group_by(created_device) %>% 
  count() %>% 
  arrange(desc(n))
# Take first 20 rows 
device_IOS_top20 <- device_IOS[1:20, ]
# Plot 
pDeviceiOS <- ggplot(data = device_IOS_top20, aes(x = as.factor(created_device), y = n)) +
  geom_bar(stat="identity", fill = "steelblue") +
  theme_bw() +
  xlab("Device Model") +
  ggtitle(" ") + 
  theme(axis.text.x = element_text(angle = 90, size = 17),
        axis.title.x = element_text(size = 20, margin = margin(10, 0, 20, 0)),
        axis.text.y = element_text(size = 17),
        axis.title.y = element_text(size = 20, margin = margin(0, 10, 0, 20)),
        plot.title = element_text(margin = margin(20, 0, 10, 0)))

# DAU Android
DAU_Android <- fullJoinDF %>% 
  filter(created_platform == "Android") %>% 
  mutate(month = month(date)) %>% 
  group_by(date, month) %>% 
  count() 
# Plot
pDAU_Android <- ggplot(DAU_Android, aes(x = as.Date(date, "%Y-%m-%d"), y = n/1000)) +
  geom_line() +
  geom_hline(yintercept = mean(DAU_Android$n)/1000, 
             linetype = "dashed", 
             size = 1, 
             color = "red", 
             alpha = 0.5) +
  theme_bw() + 
  scale_x_date(name = " ",
               date_breaks = "1 month", 
               minor_breaks = "1 week", 
               date_labels = "%b %d") + 
  scale_y_continuous(name = "DAU in thousand (K)", limits = c(0, 6.5)) + 
  ggtitle(" ") + 
  theme(axis.text.x = element_text(angle = 45, size = 11, vjust = 0.5),
        axis.title.x = element_text(size = 18, margin = margin(10, 0, 0, 0)),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 13, margin = margin(0, 10, 0, 0)),
        aspect.ratio = 4/16)


# MAU Android
MAU_Android <- fullJoinDF %>% 
  filter(created_platform == "Android") %>% 
  mutate(month = month(date)) %>% 
  group_by(month) %>% 
  summarise(n = n_distinct(account_id))
# add date to MAU for visualization
MAU_Android$date <- as.Date(c("2016-01-16", "2016-02-15", "2016-03-16", "2016-04-15", "2016-05-16", "2016-06-15",
                      "2016-07-16", "2016-08-16", "2016-09-15", "2016-10-16", "2016-11-15", "2016-12-16"),
                    "%Y-%m-%d")

pMAU_Android <- ggplot(MAU_Android, aes(x = date, y = n/1000)) +
  geom_line() + 
  geom_hline(yintercept = mean(MAU_Android$n)/1000, 
             linetype = "dashed", 
             size = 1, 
             color = "red", 
             alpha = 0.5) +
  theme_bw() + 
  scale_x_date(name = " ",
               date_breaks = "1 month", 
               minor_breaks = "1 week", 
               date_labels = "%b %d") + 
  scale_y_continuous(name = "MAU in thousand (K)", limits = c(10, 25)) + 
  ggtitle(" ") +
  theme(axis.text.x = element_text(angle = 45, size = 11, vjust = 0.5),
        axis.title.x = element_text(size = 18, margin = margin(10, 0, 0, 0)),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 13, margin = margin(0, 10, 0, 0)),
        aspect.ratio = 4/16)

# Stickiness in % - Android
stickiness_Android <- full_join(DAU_Android, MAU_Android, by = "month") %>% 
  mutate(stickiness = n.x/n.y * 100)
avgStickiness_Android <- mean(stickiness_Android$stickiness)

# Plot stickiness
pStickiness_Android <- ggplot(stickiness_Android, aes(x = date.x, y = stickiness)) +
  geom_line() + 
  geom_hline(yintercept = avgStickiness_Android, 
             linetype = "dashed", 
             size = 1, 
             color = "red", 
             alpha = 0.5) +
  theme_bw() + 
  scale_x_date(name = "date",
               date_breaks = "1 month", 
               minor_breaks = "1 week", 
               date_labels = "%b %d") + 
  scale_y_continuous(name = "Stickiness in %", limits = c(0, 50)) + 
  ggtitle(" ") +
  theme(axis.text.x = element_text(angle = 45, size = 11, vjust = 0.5),
        axis.title.x = element_text(size = 18, margin = margin(10, 0, 0, 0)),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 13, margin = margin(0, 10, 0, 0)),
        aspect.ratio = 4/16)




# DAU iOS
DAU_iOS <- fullJoinDF %>% 
  filter(created_platform == "iOS") %>% 
  mutate(month = month(date)) %>% 
  group_by(date, month) %>% 
  count() 
# Plot
pDAU_iOS <- ggplot(DAU_iOS, aes(x = as.Date(date, "%Y-%m-%d"), y = n/1000)) +
  geom_line() +
  geom_hline(yintercept = mean(DAU_iOS$n)/1000, 
             linetype = "dashed", 
             size = 1, 
             color = "red", 
             alpha = 0.5) +
  theme_bw() + 
  scale_x_date(name = " ",
               date_breaks = "1 month", 
               minor_breaks = "1 week", 
               date_labels = "%b %d") + 
  scale_y_continuous(name = "DAU in thousand (K)", limits = c(0, 2)) + 
  ggtitle(" ") + 
  theme(axis.text.x = element_text(angle = 45, size = 11, vjust = 0.5),
        axis.title.x = element_text(size = 18, margin = margin(10, 0, 0, 0)),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 13, margin = margin(0, 10, 0, 0)),
        aspect.ratio = 4/16)


# MAU iOS
MAU_iOS <- fullJoinDF %>% 
  filter(created_platform == "iOS") %>% 
  mutate(month = month(date)) %>% 
  group_by(month) %>% 
  summarise(n = n_distinct(account_id))
# add date to MAU for visualization
MAU_iOS$date <- as.Date(c("2016-01-16", "2016-02-15", "2016-03-16", "2016-04-15", "2016-05-16", "2016-06-15",
                              "2016-07-16", "2016-08-16", "2016-09-15", "2016-10-16", "2016-11-15", "2016-12-16"),
                            "%Y-%m-%d")

pMAU_iOS <- ggplot(MAU_iOS, aes(x = date, y = n/1000)) +
  geom_line() + 
  geom_hline(yintercept = mean(MAU_iOS$n)/1000, 
             linetype = "dashed", 
             size = 1, 
             color = "red", 
             alpha = 0.5) +
  theme_bw() + 
  scale_x_date(name = " ",
               date_breaks = "1 month", 
               minor_breaks = "1 week", 
               date_labels = "%b %d") + 
  scale_y_continuous(name = "MAU in thousand (K)", limits = c(1, 7)) + 
  ggtitle(" ") +
  theme(axis.text.x = element_text(angle = 45, size = 11, vjust = 0.5),
        axis.title.x = element_text(size = 18, margin = margin(10, 0, 0, 0)),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 13, margin = margin(0, 10, 0, 0)),
        aspect.ratio = 4/16)

# Stickiness in % - iOS
stickiness_iOS <- full_join(DAU_iOS, MAU_iOS, by = "month") %>% 
  mutate(stickiness = n.x/n.y * 100)
avgStickiness_iOS <- mean(stickiness_iOS$stickiness)

# Plot stickiness
pStickiness_iOS <- ggplot(stickiness_iOS, aes(x = date.x, y = stickiness)) +
  geom_line() + 
  geom_hline(yintercept = avgStickiness_iOS, 
             linetype = "dashed", 
             size = 1, 
             color = "red", 
             alpha = 0.5) +
  theme_bw() + 
  scale_x_date(name = "date",
               date_breaks = "1 month", 
               minor_breaks = "1 week", 
               date_labels = "%b %d") + 
  scale_y_continuous(name = "Stickiness in %", limits = c(0, 50)) + 
  ggtitle(" ") +
  theme(axis.text.x = element_text(angle = 45, size = 11, vjust = 0.5),
        axis.title.x = element_text(size = 18, margin = margin(10, 0, 0, 0)),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(size = 13, margin = margin(0, 10, 0, 0)),
        aspect.ratio = 4/16)

# Combine
(pDAU_Android + pDAU_iOS)/(pMAU_Android + pMAU_iOS)/(pStickiness_Android + pStickiness_iOS)
