library(tidyverse)


sector_colors <- c("Basic Materials"="brown",
                   "Communication Services"="blue",
                   "Consumer Cyclical"='red',
                   "Consumer Defensive"='green',
                   "Energy"='yellow',
                   "Financial Services"='black',
                   "Healthcare"='darkgreen',
                   "Industrials"='purple',
                   "Real Estate"='grey',
                   "Technology"='pink',
                   "Utilities"='cyan')

#Making a sector breakdown for the 1561 equities

ggplot(SF1_tibble_w_tickersANDLabels, aes(x = sector, fill = sector)) +
  geom_bar() +
  scale_fill_manual(values = sector_colors)+
  labs(x="Sector", y="n", title="1561 Equity Sector Rep") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(fill = "none")

#11 ticker sector representation

The11_w_sector <- The11_SF1_tbl_w_tickers %>% select(ticker)
The11_w_sector <- left_join(The11_w_sector, raw_TICKERS)
The11_w_sector <- The11_w_sector %>% select(ticker, sector) %>% distinct()

ggplot(The11_w_sector, aes(x = sector, fill = sector)) +
  geom_bar() +
  scale_fill_manual(values = sector_colors)+
  labs(x="Sector", y="n", title="11 Selected Equities Sector Rep") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  guides(fill = "none")
  













