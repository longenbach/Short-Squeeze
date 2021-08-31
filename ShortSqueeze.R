## PACKAGES: ----------------------------------------------------------------------------------------
library(rvest)
library(dplyr)
library(stringr)


## EXAMAPLE - SPWR: ---------------------------------------------------------------------------------
url <- "https://www.wsj.com/market-data/quotes/SPWR"
sel <- ".WSJTheme--cr_data_field--2DrXlJgS"

dat <- url %>%
  read_html() %>%
  html_nodes(sel)%>%
  html_text()

dat_split <- str_split(dat, "   ")
dat_split

SharesOutstanding <- str_split(str_split(dat_split[[4]], "Shares Outstanding")[[1]]," ")[[2]]
PublicFloat <- str_split(str_split(dat_split[[5]], "Public Float")[[1]]," ")[[2]]
SharesSoldShort <- str_split(str_split(dat_split[[9]], "Shares Sold Short")[[1]]," ")[[2]]
ChangefromLast <- str_split(str_split(dat_split[[10]], "Change from Last")[[1]],"%")[[2]][1]
PercentofFloat <- str_split(str_split(dat_split[[11]], "Percent of Float")[[1]],"%")[[2]][1]


## GET TICKER SYMBOLS (input/): ---------------------------------------------------------------------

## Resources: 
# https://stackoverflow.com/questions/25338608/download-all-stock-symbol-list-of-a-market
# http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NASDAQ&render=download
# http://www.nasdaq.com/screening/companies-by-industry.aspx?exchange=NYSE&render=download

my_path <- "/Users/shlong/Documents/GitHubProjects_git/"
git_folder <- "Short-Squeeze/"
setwd(paste0(my_path, git_folder))

NYSE <- read.csv(paste0(my_path, git_folder, "input/", "nasdaq_screener_1611857427822.csv"))
NASDAQ <- read.csv(paste0(my_path, git_folder, "input/", "nasdaq_screener_1611857411887.csv"))

num_of_stocks <- nrow(NYSE) + nrow(NASDAQ)
TICKERS <- c(levels(NYSE$Symbol),levels(NASDAQ$Symbol))


## GET SHORT POSITIONS: -----------------------------------------------------------------------------

## Resources: 
# https://shortsqueeze.com/?symbol=GME&submit=Short+Quote%E2%84%A2
# http://www.nasdaqtrader.com/Trader.aspx?id=ShortInterest
# https://www.wsj.com/market-data/quotes/ 

## If available, pull from www.wsj.com
raw_pull <- sapply(TICKERS,function(TICKER){
  print(TICKER)
  try(
    dat <- paste("https://www.wsj.com/market-data/quotes/",TICKER,sep="") %>%
      read_html() %>%
      html_nodes(".WSJTheme--cr_data_field--2DrXlJgS")%>%
      html_text()
  )
})


## FORMAT PULL DATA: --------------------------------------------------------------------------------

# Format into table
df_total2 <- data.frame()
for (i in 1:length(raw_pull)){
  symbol <- names(raw_pull[i])
  raw_i <- raw_pull[[i]]
  if (length(raw_i) < 9) next  ## Skip if ticker doesn't have short-interest
  print(symbol)
  SharesOutstanding <- str_split(str_split(raw_i[4], "Shares Outstanding")[[1]]," ")[[2]]
  PublicFloat <- str_split(str_split(raw_i[5], "Public Float")[[1]]," ")[[2]]
  SharesSoldShort <- str_split(str_split(raw_i[9], "Shares Sold Short")[[1]]," ")[[2]]
  ChangefromLast <- str_split(str_split(raw_i[10], "Change from Last")[[1]],"%")[[2]][1]
  PercentofFloat <- str_split(str_split(raw_i[11], "Percent of Float")[[1]],"%")[[2]][1]
  
  output <- data.frame(Ticker = as.character(symbol),
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

# Check units = NA, M, B
unique(df_total2$SO_Units)
unique(df_total2$PF_Units)
unique(df_total2$SSS_Units)

# Convert to units to numbers
SO <- ifelse(df_total2$SO_Units == "B", 10^9,10^6)
PF <- ifelse(df_total2$PF_Units == "B", 10^9,10^6)
SSS <- ifelse(df_total2$SSS_Units == "B", 10^9,10^6)

SO[is.na(SO)] <- 1
PF[is.na(PF)] <- 1
SSS[is.na(SSS)] <- 1

df_total2$Shares_Outstanding <- SO * df_total2$SharesOutstanding
df_total2$Public_Float <-  PF * df_total2$PublicFloat
df_total2$Shares_Sold_Short <- SSS * df_total2$SharesSoldShort


## WRITE TO CSV: -----------------------------------------------------------------------------------

cur_date <- as.character(format(Sys.Date(),format = "%m-%d-%y"))

data_date <- url %>%
  read_html() %>%
  html_nodes("h3 span")%>%
  html_text()
data_date <- stringr::str_replace_all(data_date, pattern = "/", replace = "-")

output_path <- "output/"
output_file <- paste0("ShortInterest_",data_date, "_", cur_date, ".csv")

write.csv(df_total2,file = paste0(output_path, output_file))


