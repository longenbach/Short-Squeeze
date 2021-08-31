## SETUP: ----------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(stringr)

my_path <- "/Users/shlong/Documents/GitHubProjects_git/"
folder <- "Short-Squeeze/output"
setwd(paste0(my_path, folder))
getwd()

## PLOTS: ----------------------------------------------------------------------------------------

system("ls")
current <- "ShortInterest_(05-21-21)_06-03-21.csv"
lag <- "ShortInterest_(02-12-21)_02-25-21.csv"

current_date <- stringr::str_split(current, pattern = '_')[[1]][2]
lag_date <- stringr::str_split(lag, pattern = '_')[[1]][2]

df_total1 = read.csv(current)
df_total0 = read.csv(lag) 

df_total1 %>% 
  mutate( vun = Public_Float / Shares_Outstanding) %>% 
  filter(vun < 5) %>% 
  ggplot(aes(x= Public_Float / Shares_Outstanding, y = Shares_Sold_Short / Public_Float, label = Ticker)) + 
  geom_text() 

## Plot 1: Short Interest vs. Public Availability
df_total1 %>% 
  mutate( vun = Public_Float / Shares_Outstanding) %>% 
  #filter(vun < 1) %>% 
  ggplot(aes(y= (Public_Float / Shares_Outstanding)*100, x = (Shares_Sold_Short / Public_Float) *100, label = Ticker, color = "rosybrown")) + 
  geom_text(size = 5) +
  labs(x = "Short Interest = (Shares Sold Short / Public Float) %", y = "(Public Float / Shares Outstanding) %") +
  scale_colour_manual(values=c("navyblue")) + 
  theme(legend.position = "none") 

## Plot 2: Percentage Chg Short Interest vs. Short Interest 
df_total1 %>% 
  mutate( vun = Public_Float / Shares_Outstanding) %>% 
  #filter(ChangefromLast < 750) %>% 
  #filter(vun < 1) %>% 
  ggplot(aes(x= ChangefromLast, y = (Shares_Sold_Short / Public_Float) *100, label = Ticker, color = ChangefromLast)) + 
  geom_text(size = 5) +
  labs(y = "Short Interest = (Shares Sold Short / Public Float) %", x = "Percentage change in Short Interest from the Previous Report")  + 
  theme(legend.position = "none")

## Table: Short Interest Chg
data_par <- df_total1 %>% 
  merge(df_total0, by="Ticker") %>%
  mutate(  Short_Interest_Current = ((Shares_Sold_Short.x / Public_Float.x) *100) ) %>%
  mutate(  Short_Interest_Lag = ((Shares_Sold_Short.y / Public_Float.y) *100) ) %>% 
  select(c("Ticker","Short_Interest_Lag","Short_Interest_Current")) %>% 
  arrange(desc(Short_Interest_Current)) %>% 
  head(50)

# (to do)

## EXTRA: ----------------------------------------------------------------------------------------

# Check for data anomalies
df_total1$PFdivSO = df_total1$Public_Float / df_total1$Shares_Outstanding

df_total1 %>% 
  mutate(cutoff = PFdivSO < 1.0000001) %>% 
  group_by(cutoff) %>% 
  summarise(n())

length(na.omit(df_total1$PFdivSO))

df_total1 %>% 
  mutate( vun = Public_Float / Shares_Outstanding) %>% 
  filter(vun < 5) %>% 
  ggplot() + 
  geom_histogram(aes(x= Public_Float / Shares_Outstanding),bins=100)
