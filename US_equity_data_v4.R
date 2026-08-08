# ____ NOTES ____ NOTES ____ NOTES ____ NOTES ____ NOTES ____ NOTES ____ NOTES ____ NOTES ____
# You should only have to mess with the first block of code, i.e. the
# "(7/9)THIS WORKS: Getting the raw data in and then making them into tibbles" section to end with 
# what I believe will be the data we need (the SF1_tibble, SF1_tibble_w_tickers, returns_tibble, and returns_tibble_w_tickers).
# 
# For the first section, 
# just point the read.csv() calls to wherever you downloaded the data to on your PC (data is in google drive)
#
# Once the first section is set you should be able to run the whole thing fine. The first {} block will
# take some time most likely though
# 
# If you get through this whole thing successfully the important stuff will be: 
# 1. returns_tibble_w_tickers will have the daily returns (values), trading days (features), and tickers (observations)
# 2. returns_tibble will be the same thing but without the ticker column/feature/variable
# 3. SF1_tibble_w_tickers will have the SFS metric name/values (features/values respectively) and tickers (observations)
# 4. SF1_tibble will be the same idea as the returns_table i.e. the ticker column being gone
#
# *** when doing this don't save any changes to the .csv files and I didnt convert the numbers (excel might ask to do this)
# *** Feel free to use the group chat or email for any questions
# *** Note I did not sclae or center anything but I am pretty sure for everything we plan on doing we will want to do both

  


##### (7/9)THIS WORKS: Getting the raw data in and then making them into tibbles
  
{
    ### Load package(s) needed
    library(tidyverse)
    
    ### Get raw data in as data frames
    
    # SHARADAR_DAILY (valuation metrics)
    raw_DAILY <- read.csv("C:\\market_data\\sharadar\\SHARADAR_DAILY_3_10809cb7704a0782be3ca22e507971cc\\SHARADAR_DAILY_3_10809cb7704a0782be3ca22e507971cc.csv")
    
    # SHARADAR_METRICS (statistics based on actual market data/ equity data)
    raw_METRICS <- read.csv("C:\\market_data\\sharadar\\SHARADAR_METRICS_a5c0f19b4a6d6a0eb7d8a2c89c55071b\\SHARADAR_METRICS_a5c0f19b4a6d6a0eb7d8a2c89c55071b.csv")
    
    # SHARADAR_SEP (Equity price data)
    raw_SEP <- read.csv("C:\\market_data\\sharadar\\SHARADAR_SEP_2_f1b7d3c75f69bea6716145181fe035e0\\SHARADAR_SEP_2_f1b7d3c75f69bea6716145181fe035e0.csv")
    
    # SHARADAR_SF1 (Standardized financial statement data)
    raw_SF1 <- read.csv("C:\\market_data\\sharadar\\SHARADAR_SF1_3_f9b5c423610c7ae36b31c4b8129e694c\\SHARADAR_SF1_3_f9b5c423610c7ae36b31c4b8129e694c.csv")
    
    # SHARADAR_SFP (Fund price data)
    raw_SFP <- read.csv("C:\\market_data\\sharadar\\SHARADAR_SFP_2_526c8897597cf32d798a2d45833e977e\\SHARADAR_SFP_2_526c8897597cf32d798a2d45833e977e.csv")
    
    # SHARADAR_SP500 (History of SP500 constituency changes i.e. whos in or out of the SP500)
    raw_SP500 <- read.csv("C:\\market_data\\sharadar\\SHARADAR_SP500_2_fc77b8024e67111bd709cacef0141836\\SHARADAR_SP500_2_fc77b8024e67111bd709cacef0141836.csv")
    
    # SHARADAR_TICKERS (Metadata for every public US company i.e. data about each company that doesnt change often)
    raw_TICKERS <- read.csv("C:\\market_data\\sharadar\\SHARADAR_TICKERS_2_7ef16c1ffc4112bb028c13f3e3743955\\SHARADAR_TICKERS_2_7ef16c1ffc4112bb028c13f3e3743955.csv")
  
    ### Turn the data frames into tibbles since they are easier to work with
    
    raw_DAILY <- as_tibble(raw_DAILY)
    raw_METRICS <- as_tibble(raw_METRICS)
    raw_SEP <- as_tibble(raw_SEP)
    raw_SF1 <- as_tibble(raw_SF1)
    raw_SFP <- as_tibble(raw_SFP)
    raw_SP500 <- as_tibble(raw_SP500)
    raw_TICKERS <- as_tibble(raw_TICKERS)
    
    #I would save the "workspace image" here so you dont have to redo the above just to get started everytime.
    #Usually Rstudio will ask if you want to do this when you close the app
    
}
  
##### THIS WORKS: Making a ticker (rows), daily returns over 10 years (columns) matrix with NO NA's
  
{
  #Dropping NA's and grouping by date (need to group by date to see the number of tickers for each date)
  library(tidyverse)
  returns_tibble <- raw_SEP %>%
    drop_na() %>%
    filter(date >= min(date)) %>%
    arrange(date,ticker) %>%
    as_tibble() %>%
    group_by(date)
  
  #Seeing the number of tickers for each date
  num_tickers_per_date <- returns_tibble %>% summarise(n_distinct(ticker))
  
  #Seeing how many days we have in the data
  length(unique(returns_tibble$date)) #=2599
  
  #Making the returns tibble hold only tickers that have observations for all days
  returns_tibble <- returns_tibble %>%
    ungroup()
  
  returns_tibble <- returns_tibble %>%
    group_by(ticker) %>%
    filter(n() == 2599)
  
  #Seeing how many unique tickers are left
  length(unique(returns_tibble$ticker)) #=2906 tickers
  
  ### Getting it into ticker by daily log return form
  
  #Need a lagged (yesterday's) adjusted close feature in order to make the log returns feature
  returns_tibble <- returns_tibble %>% mutate(lag_adj_close = lag(closeadj)) %>% drop_na()
  
  #Making the log returns feature
  returns_tibble <- returns_tibble %>% mutate(log_return = log(closeadj/lag_adj_close))
  
  # Dropping all the stuff we don't need i.e. anything not ticker, date, or log_return
  returns_tibble <- returns_tibble %>% select(date, ticker, log_return)
  
  # Pivot wider so tickers are the observations
  returns_tibble <- returns_tibble %>% pivot_wider(id_cols = ticker, names_from = date, values_from = log_return)
}
  
  
##### THIS WORKS:
##### Making the ticker (rows), by SF1 i.e. standardized financial statement data (columns) matrix 
##### WITH THE SAME TICKERS FROM THE RETURNS_TIBBLE, NO NA'S, AND THE 2 MOST RECENT $CALENDARDATE'S
{
  
  other_tibble <- raw_SF1 %>%
    filter()
  
  ### Filtering by dimension to see which dimension to use. We should use identical dimension for the
  ### financial statements because we want the features to be comparable to each other
  
  #Making 1 tibble each for the 6 different dimensions
  other_tibble_ARQ <- other_tibble %>% filter(dimension == 'ARQ')
  other_tibble_ART <- other_tibble %>% filter(dimension == 'ART')
  other_tibble_ARY <- other_tibble %>% filter(dimension == 'ARY')
  other_tibble_MRQ <- other_tibble %>% filter(dimension == 'MRQ')
  other_tibble_MRT <- other_tibble %>% filter(dimension == 'MRT')
  other_tibble_MRY <- other_tibble %>% filter(dimension == 'MRY')
  
  #Seeing how many tickers from the returns_tibble are in each of those other_tibble dimensions
  length(intersect(unique(returns_tibble$ticker), unique(other_tibble_ARQ$ticker))) #=2682
  length(intersect(unique(returns_tibble$ticker), unique(other_tibble_ART$ticker))) #=2787
  length(intersect(unique(returns_tibble$ticker), unique(other_tibble_ARY$ticker))) #=2787
  length(intersect(unique(returns_tibble$ticker), unique(other_tibble_MRQ$ticker))) #=2682
  length(intersect(unique(returns_tibble$ticker), unique(other_tibble_MRT$ticker))) #=2787
  length(intersect(unique(returns_tibble$ticker), unique(other_tibble_MRY$ticker))) #=2787
  
  #Not a huge difference between them so we will use MRT since it is the data reflecting
  # the most recent rolling 12-month window of data 
  # i.e. most recent = better than old, rolling 12-month = reduces effect of a potential wierd quarter for company x
  
  #Freeing up PC memory by getting rid of the other stuff
  remove(other_tibble_ARQ, other_tibble_ART, other_tibble_ARY, other_tibble_MRQ, other_tibble_MRY)
  
  # Filtering other_tibble_MRT to only include the tickers that are in returns_tibble
  vector_of_returns_tibble_tickers <- as.vector(returns_tibble$ticker)
  other_tibble_MRT <- other_tibble_MRT %>% filter(ticker %in% vector_of_returns_tibble_tickers) 
  
  ### Keeping only the most recent MRT for each ticker. We dont want super old stuff
  
  # Filtering out the non most recent data for each ticker
  other_tibble_MRT <- other_tibble_MRT %>% 
    group_by(ticker) %>% 
    filter(calendardate == max(calendardate))
  
  #Making a bar plot to see the counts of most recent calendar dates
  ggplot(other_tibble_MRT, aes(x=calendardate))+geom_bar()
  # Looks like just keeping the calendardate's >= 2024-12-31 will work great (keeps almost all of them)
  
  #Filtering out the stuff with calendardate < 2024-12-31
  other_tibble_MRT <- other_tibble_MRT %>% filter(calendardate > "2024-12-30")
  
  #Getting rid of rows with NA's since I forgot to do that earlier
  other_tibble_MRT <- other_tibble_MRT %>% drop_na() %>% ungroup()
  
  ### Finalizing the features we will use in the SF1 matrix
  
  #Making some more informative features
  other_tibble_MRT <- other_tibble_MRT %>% 
    mutate(rel_assetsc=assetsc/assetsavg,
           rel_debtc=debtc/debt,
           rel_inventory=inventory/assets,
           rel_investments=investments/assets,
           rel_investmentsc=investmentsc/investments,
           rel_ncfo=ncfo/ncf,
           rel_payables=payables/liabilities,
           rel_ppnenett=ppnenet/assets,
           rel_receivables=receivables/assets,
           rel_retearn=retearn/equity,
           rel_rnd=rnd/revenue,
           rel_sgna=sgna/revenue,
           rel_tangibles=tangibles/assets)
  
  #Keep only the features we are using
  other_tibble_MRT <- other_tibble_MRT %>%
    select(ticker, assetsavg, rel_assetsc, assetturnover, bvps,
           capex, cashnequsd, currentratio, de, rel_debtc, divyield,
           ebitdamargin, evebitda, grossmargin, rel_inventory,
           rel_investments, rel_investmentsc, marketcap,
           rel_ncfo, netmargin, rel_payables, payoutratio, pb, pe,
           rel_ppnenett, ps, rel_receivables, rel_retearn, rel_rnd,
           roa, roe, roic, rel_sgna, rel_tangibles, cor, fcfps)
  
  #make vector of tickers, so we can remove it from the tibble (so that scale() works),
  #then rejoin it later
  
  tickers_temp_home <- as.vector(other_tibble_MRT$ticker)
  
  #making sure they are the same so we dont assign observations to the wrong feature
  all.equal(tickers_temp_home, other_tibble_MRT$ticker)
  
  #remove the ticker column from other_tibble_MRT since scale() wont like the ticker character variable
  other_tibble_MRT <- other_tibble_MRT %>% select(-ticker)
  
  #Scale and center. There are lots of different units of measurement. Im sure the variances vary wildly as well
  scaled_other_tibble_MRT <- other_tibble_MRT %>% scale()
  
  #make it a tibble again, since scale() turned it into a matrix
  scaled_other_tibble_MRT <- as_tibble(scaled_other_tibble_MRT)
  #Put the tickers back in
  SF1_tibble <- scaled_other_tibble_MRT %>% mutate(ticker = tickers_temp_home)
  #Keeping an unscaled SF1 tibble
  unscaled_SF1_tibble <- other_tibble_MRT
    
}
  
  
##### We need to reconcile the returns_tibble by dropping the tickers we lost in the other_tibble_MRT creation
  
{
  
  #Making a vector of the remaining tickers
  final_tickers <- as.vector(SF1_tibble$ticker)
  
  #Filtering the returns_tibble so that it has the same tickers as other_tibble_MRT
  returns_tibble <- returns_tibble %>% filter(ticker %in% final_tickers)
    
}

##### Cleaning everything up so its less confusing

{
  returns_tibble <- ungroup(returns_tibble)
  
  returns_tibble_w_tickers <- returns_tibble
  returns_tibble <- returns_tibble %>% select(-ticker)
  
  SF_1_tibble_w_tickers <- SF1_tibble
  SF1_tibble <- SF1_tibble %>% select(-ticker)
}

##### Figuring out the NA/NaN situation in the SF1 dataset
{
  colSums(is.na(SF1_tibble)) #number of NA's or NaN's in each feature
  
  #Only 3 features have NA/NaN's in them, just going to remove them
  SF1_tibble <- SF1_tibble %>% select(-rel_debtc, -rel_investmentsc, -rel_ncfo)
  SF1_tibble_w_tickers <- SF_1_tibble_w_tickers %>% select(-rel_debtc, -rel_investmentsc, -rel_ncfo)
  unscaled_SF1_tibble <- unscaled_SF1_tibble %>% select(-rel_debtc, -rel_investmentsc, -rel_ncfo)
}

##### Removing seemingly unnecessary objects
{
  remove(final_tickers) 
  remove(num_tickers_per_date) 
  remove(other_tibble)
  remove(other_tibble_MRT)
  remove(tickers_temp_home)
  remove(vector_of_returns_tibble_tickers)
}

#Me double checking we had 10 years of returns data

{
  checking_dates_tibble <- returns_tibble %>%
    t()
  #Good
}

##### Making the same returns_tibble for the 11 proposal tickers

{
  #Log returns tibble for the 11 tickers
  The11_returns_tbl <- filter(raw_SEP, ticker %in% c("AAPL",
                                                         "GOOG",
                                                         "BLK",
                                                         "AXP",
                                                         "BAC",
                                                         "CI",
                                                         "JNJ",
                                                         "MDT",
                                                         "BA",
                                                         "MMM",
                                                         "FDX"))
  The11_RT_grp_tbl <- group_by(The11_returns_tbl, ticker)
  The11_RT_grp_tbl %>% dplyr::count(ticker) #Seeing how many observations for each of the 11
  The11_returns_tbl <- The11_returns_tbl %>% select(ticker, date, closeadj)
  The11_returns_tbl <- The11_returns_tbl %>% group_by(ticker) %>% arrange(date)
  The11_returns_tbl <- The11_returns_tbl %>%  mutate(lag_closeadj = lag(closeadj))
  The11_returns_tbl <- The11_returns_tbl %>% mutate(log_ret = log(closeadj/lag_closeadj))
  The11_returns_tbl <- The11_returns_tbl %>% select(ticker, date, log_ret)
  The11_returns_tbl <- The11_returns_tbl %>% drop_na()
  The11_returns_tbl <- The11_returns_tbl %>% pivot_wider(names_from = date, id_cols = ticker, values_from = log_ret)
  #Looks good

}

##### Making the same returns_tibble for the 11 proposal tickers

{
  The11_tickers <- c("AAPL",
                    "GOOGL",  #GOOGL is alphabet's SF1 ticker name, also their preferred stock ticker
                    "BLK",
                    "AXP",
                    "BAC",
                    "CI",
                    "JNJ",
                    "MDT",
                    "BA",
                    "MMM",
                    "FDX")
  The11_SF1_tbl <- raw_SF1 %>% filter(ticker %in% The11_tickers)
  The11_SF1_tbl <- The11_SF1_tbl %>% filter(calendardate > "2024-12-30")
  #Group by ticker and see each'es most recent calendardate
  The11_SF1_tbl <- The11_SF1_tbl %>% group_by(ticker)
  The11_SF1_tbl %>% summarise(latest_date = max(calendardate))
  #Keeping only the observations with calendardate == 2024-12-31 (hopefully all 11 have one)
  The11_SF1_tbl <- The11_SF1_tbl %>% filter(calendardate == "2024-12-31")
  #Seeing if all 11 have are still in there AND have the same kind of SFS (i.e. the same "dimension" SF1 feature value)
  The11_SF1_tbl %>% summarise(num_unique_tick = unique(ticker)) #they're all there
  The11s_FS_dims <- The11_SF1_tbl %>% reframe(num_unique_dims = unique(dimension))
  #Theya ll have the 6 different "versions" of SFS's
  The11_SF1_tbl <- The11_SF1_tbl %>% filter(dimension == "MRT")
  The11_SF1_tbl <- The11_SF1_tbl %>% mutate(rel_assetsc=assetsc/assetsavg,
                                            rel_debtc=debtc/debt,
                                            rel_inventory=inventory/assets,
                                            rel_investments=investments/assets,
                                            rel_investmentsc=investmentsc/investments,
                                            rel_ncfo=ncfo/ncf,
                                            rel_payables=payables/liabilities,
                                            rel_ppnenett=ppnenet/assets,
                                            rel_receivables=receivables/assets,
                                            rel_retearn=retearn/equity,
                                            rel_rnd=rnd/revenue,
                                            rel_sgna=sgna/revenue,
                                            rel_tangibles=tangibles/assets)
  The11_SF1_tbl_w_tickers <- The11_SF1_tbl %>% 
    select(ticker, assetsavg, rel_assetsc, assetturnover, bvps,
           capex, cashnequsd, currentratio, de, rel_debtc, divyield,
           ebitdamargin, evebitda, grossmargin, rel_inventory,
           rel_investments, rel_investmentsc, marketcap,
           rel_ncfo, netmargin, rel_payables, payoutratio, pb, pe,
           rel_ppnenett, ps, rel_receivables, rel_retearn, rel_rnd,
           roa, roe, roic, rel_sgna, rel_tangibles, cor, fcfps) %>%
    arrange(ticker)
  The11_SF1_tbl <-  ungroup(The11_SF1_tbl)
  The11_SF1_tbl_w_tickers <- ungroup(The11_SF1_tbl_w_tickers)
  The11_SF1_tbl <- The11_SF1_tbl_w_tickers %>% select(-ticker)
  #Checking for NA's
  NAcheck1 <- The11_SF1_tbl %>% summarize(across(everything(), ~sum(is.na(.))))
  #Need to remove rel_assetsc, currentratio, rel_debtc, & rel_investmentsc
  The11_SF1_tbl <- The11_SF1_tbl %>% select(-rel_assetsc, -currentratio,
                                            -rel_debtc, -rel_investmentsc)
  The11_SF1_tbl_w_tickers <- The11_SF1_tbl_w_tickers %>% select(-rel_assetsc, -currentratio,
                                            -rel_debtc, -rel_investmentsc)
  # Think we good. Note we had to drop one additional feature when compared to the 1561 ticker SF1 tibbles
  # i.e. SF1_tibble and SF1_tibble_w_tickers

} 

# Making an SF1_tibble with industry and sector features for cluster comparisons

tickersIWant <- SF1_tibble_w_tickers %>% select(ticker)
tickersIWant <- as.vector(tickersIWant$ticker)
features_from_TICKERS_I_want <- raw_TICKERS %>% select(ticker, industry, sector, table)

SecInd_labels_tibble <- features_from_TICKERS_I_want %>% filter(ticker %in% tickersIWant)
SecInd_labels_tibble <- filter(SecInd_labels_tibble, table == "SF1")

SF1_tibble_w_tickersANDLabels <-
  SF1_tibble_w_tickers %>%
  left_join(
    SecInd_labels_tibble %>%
      select(ticker, industry, sector) %>%
      distinct(),
    by = "ticker"
  )

SF1_tibble_w_labels <- SF1_tibble_w_tickersANDLabels %>% select(ticker, sector, industry)
