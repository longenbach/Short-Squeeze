## PACKAGES:
library(rvest)
library(dplyr)
library(stringr)

## EXAMAPLE - SPWR:
url <- "https://www.wsj.com/market-data/quotes/SPWR" #SPWR"
sel <- ".WSJTheme--cr_data_field--2DrXlJgS"

dat <- url %>%
  read_html() %>%
  html_nodes(sel)%>%
  html_text()

dat_split = str_split(dat, "   ")
dat_split

SharesOutstanding = str_split(str_split(dat_split[[4]], "Shares Outstanding")[[1]]," ")[[2]]
PublicFloat = str_split(str_split(dat_split[[5]], "Public Float")[[1]]," ")[[2]]
SharesSoldShort = str_split(str_split(dat_split[[9]], "Shares Sold Short")[[1]]," ")[[2]]
ChangefromLast = str_split(str_split(dat_split[[10]], "Change from Last")[[1]],"%")[[2]][1]
PercentofFloat = str_split(str_split(dat_split[[11]], "Percent of Float")[[1]],"%")[[2]][1]

## TICKER SYMBOLS:

# https://stackoverflow.com/questions/25338608/download-all-stock-symbol-list-of-a-market
# http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NASDAQ&render=download
# http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NYSE&render=download

setwd("/Users/shlong/desktop/Short-Squeeze")

NYSE = read.csv("nasdaq_screener_1611857427822.csv")
NASDAQ = read.csv("nasdaq_screener_1611857411887.csv")

num_of_stocks = nrow(NYSE) + nrow(NASDAQ)
TICKERS = c(levels(NYSE$Symbol),levels(NASDAQ$Symbol))

# https://shortsqueeze.com/?symbol=GME&submit=Short+Quote%E2%84%A2
# http://www.nasdaqtrader.com/Trader.aspx?id=ShortInterest

## SEE IF TICKER HAS DATA ON WSJ.com:
raw_pull = sapply(TICKERS,function(TICKER){
  try(
    dat <- paste("https://www.wsj.com/market-data/quotes/",TICKER,sep="") %>%
      read_html() %>%
      html_nodes(".WSJTheme--cr_data_field--2DrXlJgS")%>%
      html_text()
  )
})

## FORMAT RAW DATA:
df_total2 = data.frame()
for (i in 1:length(raw_pull)){
  symbol = names(raw_pull[i])
  raw_i = raw_pull[[i]]
  if (length(raw_i) < 9) next  ## Only if has data
  print(symbol )
  SharesOutstanding = str_split(str_split(raw_i[4], "Shares Outstanding")[[1]]," ")[[2]]
  PublicFloat = str_split(str_split(raw_i[5], "Public Float")[[1]]," ")[[2]]
  SharesSoldShort = str_split(str_split(raw_i[9], "Shares Sold Short")[[1]]," ")[[2]]
  ChangefromLast = str_split(str_split(raw_i[10], "Change from Last")[[1]],"%")[[2]][1]
  PercentofFloat = str_split(str_split(raw_i[11], "Percent of Float")[[1]],"%")[[2]][1]
  
  output = data.frame(Ticker = as.character(symbol),
                      SharesOutstanding = as.numeric(gsub(",", "", SharesOutstanding[1])),
                      SO_Units = as.character(SharesOutstanding[2]),
                      PublicFloat = as.numeric(gsub(",", "", PublicFloat[1])),
                      PF_Units = as.character(PublicFloat[2]),
                      SharesSoldShort = as.numeric(gsub(",", "", SharesSoldShort[1])),
                      SSS_Units = as.character(SharesSoldShort[2]),
                      ChangefromLast = as.numeric(ChangefromLast),
                      PercentofFloat = as.numeric(PercentofFloat))
  
  
  df_total2 <- rbind(df_total2,output)  
  
}

# Units = NA, M, B
unique(df_total2$SO_Units)
unique(df_total2$PF_Units)
unique(df_total2$SSS_Units)

SO = ifelse(df_total2$SO_Units == "B", 10^9,10^6)
PF = ifelse(df_total2$PF_Units == "B", 10^9,10^6)
SSS = ifelse(df_total2$SSS_Units == "B", 10^9,10^6)

SO[is.na(SO)] <- 1
PF[is.na(PF)] <- 1
SSS[is.na(SSS)] <- 1

df_total2$Shares_Outstanding = SO * df_total2$SharesOutstanding
df_total2$Public_Float =  PF * df_total2$PublicFloat
df_total2$Shares_Sold_Short = SSS * df_total2$SharesSoldShort

## WRITE CSV:
# write.csv(df_total2,file = "ShortInterest_Feb25th.csv")

## LOAD CSV & PLOT:
setwd("/Users/shlong/desktop/Short-Squeeze")
df_total2 = read.csv("ShortInterest_Feb25th.csv")

library(ggplot2)
library(dplyr)

df_total2 %>% 
  mutate( vun = Public_Float / Shares_Outstanding) %>% 
  filter(vun < 5) %>% 
  ggplot(aes(x= Public_Float / Shares_Outstanding, y = Shares_Sold_Short / Public_Float, label = Ticker)) + 
  geom_text() 

df_total2 %>% 
  mutate( vun = Public_Float / Shares_Outstanding) %>% 
  filter(vun < 5) %>% 
  ggplot(aes(x= Public_Float / Shares_Outstanding, y = Shares_Sold_Short / Public_Float, label = Ticker)) + 
  geom_text() 

## Plot 1:
df_total2 %>% 
  mutate( vun = Public_Float / Shares_Outstanding) %>% 
  filter(vun < 1) %>% 
  ggplot(aes(y= (Public_Float / Shares_Outstanding)*100, x = (Shares_Sold_Short / Public_Float) *100, label = Ticker, color = "rosybrown")) + 
  geom_text(size = 5) +
  labs(x = "Short Interest = (Shares Sold Short / Public Float) %", y = "(Public Float / Shares Outstanding) %") +
  scale_colour_manual(values=c("navyblue")) + 
  theme(legend.position = "none") 

## Plot 2:
df_total2 %>% 
  mutate( vun = Public_Float / Shares_Outstanding) %>% 
  filter(ChangefromLast < 750) %>% 
  #filter(vun < 1) %>% 
  ggplot(aes(x= ChangefromLast, y = (Shares_Sold_Short / Public_Float) *100, label = Ticker, color = ChangefromLast)) + 
  geom_text(size = 5) +
  labs(y = "Short Interest = (Shares Sold Short / Public Float) %", x = "Percentage change in Short Interest from the Previous Report")  + 
  theme(legend.position = "none")
 
## Plot3 
library(GGally)
setwd("/Users/shlong/desktop/Short-Squeeze")
df_total0 = read.csv("ShortInterest.csv") #1/15/2021
df_total2 = read.csv("ShortInterest_Feb25th.csv") #2/12/2021

data_par = df_total2 %>% 
  merge(df_total0,by="Ticker") %>%
  mutate(  Short_Interest_Feb = ((Shares_Sold_Short.x / Public_Float.x) *100) ) %>%
  mutate(  Short_Interest_Jan = ((Shares_Sold_Short.y / Public_Float.y) *100) ) %>% 
  select(c("Ticker","Short_Interest_Jan","Short_Interest_Feb")) %>% 
  arrange(desc(Short_Interest_Feb)) %>% 
  head(500)

write.csv(data_par,file = "ShortInterest_Chg_JantoFeb.csv")


ggparcoord(data_par,
           columns = 2:3,groupColumn = 1,
           showPoints = TRUE, 
           title = "Parallel Coordinate Plot for the Iris Data",
           alphaLines = 0.3
)


## Checks

df_total2$PFdivSO = df_total2$Public_Float / df_total2$Shares_Outstanding

df_total2 %>% 
  mutate(cutoff = PFdivSO < 1.0000001) %>% 
  group_by(cutoff) %>% 
  summarise(n())

df_total2 %>% 
  mutate(cutoff = ch< 1.0000001) %>% 
  group_by(cutoff) %>% 
  summarise(n())

length(na.omit(df_total2$PFdivSO))

df_total2 %>% 
  mutate( vun = Public_Float / Shares_Outstanding) %>% 
  filter(vun < 5) %>% 
  ggplot() + 
  geom_histogram(aes(x= Public_Float / Shares_Outstanding),bins=100)
