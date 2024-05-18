# Clear environment variables
rm(list=ls())

# Load necessary libraries
library(quantmod)
library(TTR)
library(ggplot2)
library(gtrendsR)
library(zoo)
library(dplyr)
library(tidyr)
library(purrr)
library(plm)
library(lme4)
library(stargazer)


# Define the ticker symbols you want to analyze
all_tickers <- c("NVDA", "GOOGL", "MSFT", "IBM", "META", "XOM", "KO", "MMM", "PG", "HD")
ai_tickers <- c("NVDA", "GOOGL", "MSFT", "IBM", "META")
non_ai_tickers <- c("XOM", "KO", "MMM", "PG", "HD")

# Define the date ranges
release_date <- as.Date("2023-03-14")
start_date <- as.Date("2022-10-01")
end_date <- as.Date("2023-10-01")
event_start <- as.Date(release_date) - 30
event_end <- as.Date(release_date) + 60
returns_dates <- paste(start_date, end_date, sep = "/")


gtrends_string <- paste(format(event_start, "%Y-%m-%d"), format(event_end, "%Y-%m-%d"))


# Initialize lists to store AR and CAR for each ticker
ar_list <- list()
car_list <- list()

ar_plots <- list()
car_plots <- list()

# Loop through each ticker
for (ticker in all_tickers) {
  # Download stock data for the ticker and SPY (market index)
  getSymbols(c(ticker, "SPY"), src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)
  
  # Compute daily log returns for the ticker and SPY
  ticker_returns <- dailyReturn(get(ticker), type = 'log')
  spy_returns <- dailyReturn(SPY, type = 'log')
  
  # Calculate beta of the ticker relative to SPY over the previous year
  beta_ticker <- cov(ticker_returns[returns_dates], spy_returns[returns_dates]) / cov(spy_returns[returns_dates])
  #beta_nvda <- cov(nvda_returns['2019-06-01/2020-06-01'], spy_returns['2019-06-01/2020-06-01']) /cov(spy_returns['2019-06-01/2020-06-01'])
  
  # Ensure dates are within the bounds of our data
  event_dates <- index(ticker_returns[(index(ticker_returns) >= event_start) & (index(ticker_returns) <= event_end)])
  
  # Compute abnormal returns 30 days before and after ChatGPT-4 introduction
  ar_ticker <- ticker_returns[event_dates] - (beta_ticker[1,1] * spy_returns[event_dates])
  
  # Compute Cumulative Abnormal Returns (CAR)
  car_ticker <- cumsum(ar_ticker)
  
  # Store AR and CAR in their respective lists
  ar_list[[ticker]] <- data.frame(Date = event_dates, AR = ar_ticker)
  car_list[[ticker]] <- data.frame(Date = event_dates, CAR = car_ticker)
  
  if (length(ar_ticker) > 0 && length(car_ticker) > 0) {
    # Create a ggplot object for abnormal returns
    ar_plot <- ggplot(ar_list[[ticker]], aes(x = Date, y = daily.returns)) +
      geom_line() +
      labs(title = paste("Abnormal Returns (AR) of", ticker), x = "Date", y = "AR")
    ar_plots[[ticker]] <- ar_plot
    
    # Create a ggplot object for cumulative abnormal returns
    car_plot <- ggplot(car_list[[ticker]], aes(x = Date, y = daily.returns)) +
      geom_line() +
      labs(title = paste("Cumulative Abnormal Returns (CAR) of", ticker), x = "Date", y = "CAR")
    car_plots[[ticker]] <- car_plot
  } else {
    cat(paste("No data to plot for ticker:", ticker, "\n"))
  }
}


# Combine AR data from list into a single data frame
combined_ar_ai <- do.call(rbind, lapply(ai_tickers, function(ticker) {
  data <- ar_list[[ticker]]
  data$ticker <- ticker  # Add a new column for ticker
  return(data)
}))

ar_plot_gg_ai <- ggplot(combined_ar_ai, aes(x = Date, y = daily.returns, color = ticker)) + 
  geom_line(aes(group = ticker), linewidth = 1, alpha = 0.8) +
  labs(title = "Abnormal Returns (AR) for AI Tickers", 
       x = "Date", y = "Abnormal Returns", 
       subtitle = "Using the release of ChatGPT4 (03/14/2023) as the line of demarcation") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set1") +
  geom_vline(xintercept = as.Date("2023-03-14"), linetype = "dashed", color = "red", linewidth = 1) +
  guides(color = guide_legend(override.aes = list(linewidth = 3))) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank())

#Non-AI tickers
combined_ar_nonai <- do.call(rbind, lapply(non_ai_tickers, function(ticker) {
  data <- ar_list[[ticker]]
  data$ticker <- ticker  # Add a new column for ticker
  return(data)
}))

ar_plot_gg_nonai <- ggplot(combined_ar_nonai, aes(x = Date, y = daily.returns, color = ticker)) + 
  geom_line(aes(group = ticker), linewidth = 1, alpha = 0.8) +
  labs(title = "Abnormal Returns (AR) for Non-AI Tickers", 
       x = "Date", y = "Abnormal Returns", 
       subtitle = "Using the release of ChatGPT4 (03/14/2023) as the line of demarcation") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set1") +
  geom_vline(xintercept = as.Date("2023-03-14"), linetype = "dashed", color = "red", linewidth = 1) +
  theme(legend.key.width = unit(1,"cm")) +
  guides(color = guide_legend(override.aes = list(linewidth = 3))) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank())


# Combine CAR data from list into a single data frame
combined_car_ai <- do.call(rbind, lapply(ai_tickers, function(ticker) {
  data <- car_list[[ticker]]
  data$ticker <- ticker  # Add a new column for ticker
  return(data)
}))

car_plot_gg_ai <- ggplot(combined_car_ai, aes(x = Date, y = daily.returns, color = ticker)) + 
  geom_line(aes(group = ticker), linewidth = 1, alpha = 0.8) +
  labs(title = "Cumulative Abnormal Returns (CAR) for AI Tickers", 
       x = "Date", y = "Cumulative Abnormal Returns", 
       subtitle = "Using the release of ChatGPT4 (03/14/2023) as the line of demarcation") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set1") +
  geom_vline(xintercept = as.Date("2023-03-14"), linetype = "dashed", color = "red", linewidth = 1) +
  theme(legend.key.width = unit(1,"cm")) +
  guides(color = guide_legend(override.aes = list(linewidth = 3))) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank())

# Non-AI tickers
combined_car_nonai <- do.call(rbind, lapply(non_ai_tickers, function(ticker) {
  data <- car_list[[ticker]]
  data$ticker <- ticker  # Add a new column for ticker
  return(data)
}))

car_plot_gg_nonai <- ggplot(combined_car_nonai, aes(x = Date, y = daily.returns, color = ticker)) + 
  geom_line(aes(group = ticker), linewidth = 1, alpha = 0.8) +
  labs(title = "Cumulative Abnormal Returns (CAR) for Non-AI Tickers", 
       x = "Date", y = "Cumulative Abnormal Returns", 
       subtitle = "Using the release of ChatGPT4 (03/14/2023) as the line of demarcation") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set1") +
  geom_vline(xintercept = as.Date("2023-03-14"), linetype = "dashed", color = "red", linewidth = 1) +
  theme(legend.key.width = unit(1,"cm")) +
  guides(color = guide_legend(override.aes = list(linewidth = 3))) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank())


ar_plot_gg_ai
ar_plot_gg_nonai
car_plot_gg_ai
car_plot_gg_nonai

#############
# Google Trends Analysis

keywords <- c("OpenAI", "AI chatbot", "GPT 4", "Bing", "Gemini") # Replace or add more keywords as needed

# Make trends API call to gather data
trends_data <- gtrends(keywords, time = gtrends_string, geo = "US")

# Subset just the interest over time
interest_over_time <- trends_data$interest_over_time

# Replace "<1" with "0" and convert the hits column to numeric
interest_over_time$hits <- as.integer(gsub("<", "", interest_over_time$hits))

# Change data type of "date" to Date datatype
interest_over_time$date <- as.Date(interest_over_time$date)


# Plot findings
gtrends_plot <- ggplot(interest_over_time, aes(x = date, y = hits, color = keyword)) +
  geom_line(aes(group = keyword), linewidth = 1, alpha = 0.8) +
  labs(title = "Google Search Interests Over Time",
       x = "Date",
       y = "Search Interest") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set1") +
  geom_vline(xintercept = as.Date("2023-03-14"), linetype = "dashed", color = "red", linewidth = 1) +
  theme(legend.key.width = unit(1,"cm")) +
  guides(color = guide_legend(override.aes = list(linewidth = 3))) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "lines"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank())

gtrends_plot


#############
# Hierarchical Time Series Model

## Creating findings (non-lag) for model
car_plm <- data.frame(Ticker = character(), stringsAsFactors = FALSE)

# Loop through car_list
for (ticker in names(car_list)) {
  #Get the data from current ticker
  df <- car_list[[ticker]]
  
  #Pivot data to wide format
  wide_df <- pivot_wider(df, names_from = Date, values_from = daily.returns)
  
  #Add ticker name as column
  wide_df$Ticker <- ticker
  
  #Bind wide data frame to result
  car_plm <- bind_rows(car_plm, wide_df)
}



## Creating findings (lag) for model
# Initialize an empty data frame to store the final results
car_lag_plm <- data.frame(Ticker = character(), stringsAsFactors = FALSE)

# Loop through each element in the list
for (ticker in names(car_list)) {
  # Get the data frame for the current ticker
  df <- car_list[[ticker]]
  
  # Create a lagged version of daily returns
  df <- df %>% 
    arrange(Date) %>% # Ensure data is in date order before lagging
    mutate(daily.returns = lag(daily.returns, n = 1, default = NA)) # Lag the daily.returns column
  
  # Pivot the data frame to wide format
  wide_df <- pivot_wider(df, names_from = Date, values_from = daily.returns)
  
  # Add the ticker name as a column
  wide_df$Ticker <- ticker
  
  # Bind this wide data frame to the lagged result
  car_lag_plm <- bind_rows(car_lag_plm, wide_df)
}


## Creating finds for trends analytics
# Use pivot_wider to transform the data frame
wide_interest_over_time <- interest_over_time %>%
  select(keyword, date, hits) %>%  # Select only the relevant columns
  group_by(keyword) %>%            # Group by keyword to ensure uniqueness
  pivot_wider(
    names_from = date,             # Use 'date' as the new column names
    values_from = hits             # Fill the cells with 'hits' values
  ) %>%
  ungroup()                        # Remove the grouping



### Creating long data frame including both returns and lag
long_car <- car_plm %>%
  pivot_longer(
    cols = -Ticker,
    names_to = "date",
    values_to = "return"
  ) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  arrange(Ticker, date) %>%
  group_by(Ticker) %>%
  mutate(lagged_return = dplyr::lag(return, 1))


##Adding function for pre-post intervention
long_car <- long_car %>%
  mutate(after_release = if_else(date >= release_date, 1, 0))


##Adding GTrends to long_car
gtrends_long_df <- pivot_longer(wide_interest_over_time, 
                                cols = -keyword, 
                                names_to = "date", 
                                values_to = "hits",
                                names_transform = list(date = as.Date))

#Join gtrends to long_car
combined_df <- long_car %>%
  left_join(gtrends_long_df, by = "date")

#Converting to pdata.frame for panel data analysis
pdata <- pdata.frame(combined_df, index = c("Ticker", "date"))


##Prepare pdata for modeling
#Excluding NA data
pdata <- pdata[!is.na(pdata$lagged_return),]



#############
# Modeling

## PLM models
# Fit a fixed effects model
plm_fe_model <- plm(return ~ lagged_return + after_release 
                    + hits, data = pdata, model = "within")

# Fit a random effects model
plm_re_model <- plm(return ~ lagged_return + after_release 
                    + hits, data = pdata, model = "random")

## LMER models
lmer_model <- lmer(return ~ lagged_return + after_release + hits + 
                     (1|Ticker), data = pdata)


# Comparing plm models
plm_comparison <- stargazer(plm_fe_model, plm_re_model, type = "text")
summary(lmer_model)
all_model_comp <- stargazer(plm_fe_model, plm_re_model, lmer_model, type = "text")

fixed_effects <- fixef(plm_fe_model)
View(fixed_effects)
