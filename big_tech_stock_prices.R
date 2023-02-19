##my first tidy tuesday

library(tidyverse)
library(ggplot2)
library(lubridate)

#get data
big_tech_stock_prices <- read.csv("data/2023/2023-02-07/big_tech_stock_prices.csv")

#filter to date range of interest and calculate value of shares based on initial
#investment of £100 of each of the shares listed
year_close <- big_tech_stock_prices |>
  filter(date < "2023-01-01" & date > "2018-12-31") |>
  group_by(stock_symbol) |> 
  mutate(date = ymd(date),
         year = year(date)) |> 
 group_by(year, stock_symbol) |> 
  filter(date == max(date)) |>
  group_by(stock_symbol) |> 
  mutate(
         v = close * 100/first(close),
         stock_symbol = as.factor(stock_symbol)) |> 
  arrange(year, desc(v))
  
#get label info for annotation
label_info <- filter(year_close, year > 2018) |> 
  group_by(stock_symbol,year) |> 
  arrange(desc(close)) |> 
  slice_head(n = 1) |> 
  select(year, v, stock_symbol) |> 
  arrange(desc(year), desc(v))

# plot
ggplot(
  filter(year_close, year > 2018), aes(x = year, y = v, fill = fct_reorder2(stock_symbol, year, v))) + 
    geom_col(position = "dodge", show.legend = FALSE) +
    geom_text(data = label_info, aes(x = year, y = 40, label = stock_symbol, group = fct_reorder2(stock_symbol, year, v)),
    position = position_dodge(width = 0.9), angle = 90, size = 2, hjust = "center"
    ) +
  labs(
    title = "Big tech covid boom or bust?",
    caption = "https://www.kaggle.com/datasets/evangower/big-tech-stock-prices",
    subtitle = "The value of £100 worth of shares bought at the end of 2019 after  1, 2 and 3 years",
    y = "value (£)"
  ) +
  geom_hline(yintercept = 100, linetype = 2) +
  annotate(
    geom = "label", x = 2021.503, y = 1230, fill = "grey95",
    label = "TSLA - Tesla, Inc
NVDA - NVIDIA Corporation
AAPL - Apple Inc.
ORCL - Oracle Corporation
MSFT - Microsoft Corporation
GOOG - Alphabet Inc.
IBM - International Business Machines Corporation
ADBE - Adobe Inc.
CSCO - Cisco Systems, Inc.
AMZN - Amazon.com, Inc.
NFLX - Netflix, Inc.
CRM, -alesforce, Inc.
META - Meta Platforms, Inc.
INTC - Intel Corporation",
    vjust = "top", hjust = "left",
    size = 7/.pt
  )
