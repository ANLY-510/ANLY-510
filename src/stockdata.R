library(quantmod)
library(tidyverse)
library(tibbletime)

marketcap <- read_csv("data/marketcap.csv")

indsum <- marketcap %>%
  group_by(Sector) %>%
  summarise(sum_index = sum(`Market Cap`, na.rm = T))

# market cap sum by sector
datamerge <- marketcap %>%
  left_join(indsum, by = "Sector")

# create weight by marketcap for each sector
weight <-
  datamerge %>%
  mutate(weight = `Market Cap`/sum_index) %>%
  select(Sector, Symbol, weight)

sector <- levels(as.factor(weight$Sector))

# Total 11 sectors

for (j in seq_along(sector)) {
  # get symbols for each sector
  symbol <-
    weight %>%
    filter(Sector == sector[j]) %>%
    pull(Symbol)

  # wight for each symbol
  symbolweight <-
    weight %>%
    filter(Sector == sector[j]) %>%
    pull(weight)

  # quantmod package to get stock data from Yahoo
  for (i in seq_along(symbol)) {
    tryit <-
      try(getSymbols(
        symbol[i],
        from = "2009-01-01",
        to = "2017-09-30",
        src =  "yahoo",
        adjust =  TRUE
      ))
    if (inherits(tryit, "try-error")) {
      i <- i + 1
    } else {
      getSymbols(
        symbol[i],
        from = "2009-01-01",
        to = "2017-09-30",
        src =  "yahoo",
        adjust =  TRUE
      )
      return <- dailyReturn(get(symbol[i]))
      weightedreturn <- return * symbolweight[i]
      names(return) <- symbol[i]
      if ((i == 1) & (j == 1)) {
        dailyreturndata <- return
        weighteddata <- weightedreturn
      } else {
        dailyreturndata <- merge(dailyreturndata, return)
        weighteddata <- merge(weighteddata, weightedreturn)
      }
    }
  }
  sectorreturn <- rowSums(weighteddata, na.rm = TRUE)
  names(sectorreturn) <- sector[j]
  if (j == 1) {
    tempdata <- cbind(weighteddata, sectorreturn)
    sectordata <- tempdata[, ncol(tempdata)]
  } else {
    sectordata <- merge(sectordata, sectorreturn)
  }
}

# transform to time series data
names(sectordata) <- sector

# get daily return for each symbol and each sector
xtsdailyreturn<-as.xts(dailyreturndata)
# write.zoo(xtsdailyreturn,"dailyreturndata.csv", sep=",")
xtssector<-as.xts(sectordata)
# write.zoo(xtssector,"sectordata.csv", sep=",")

# data that didn't retrieve successfully
different.names <- weight$Symbol[!(weight$Symbol %in% names(dailyreturndata))]
# weight[different.names,2]

# daily return for each sector
row_date <- sectordata %>%
  as.data.frame() %>%
  row.names() %>%
  as.Date()

sectordata <- as_tibble(sectordata) %>%
  mutate(date = row_date)

sectordata <-
  sectordata %>%
  mutate(date = as.Date(date)) %>%
  as_tbl_time(index = date)

sectordata %>%
  gather(key = "sector", value = "return", -date) %>%
  ggplot(aes(x = as.Date(date), y = return)) +
  geom_line() +
  # geom_smooth() +
  facet_grid(name ~ .)

# everyday's culumative change
