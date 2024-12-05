#first step install and load packages
install.packages("httr2")
install.packages("tidyverse")
install.packages("lubridate")

library(httr2)
library(tidyverse)
library(lubridate)
library(DT)
library(ggplot2)


#setting api keys as variables
Sys.setenv(ALPHA_VANTAGE_API_KEY = "GM4W3GGRH9N64MY6")
Sys.setenv(FRED_API_KEY = "4da9514de38745a176de80c8af3d51ad")
alpha_api_key <- Sys.getenv("ALPHA_VANTAGE_API_KEY")
fred_api_key <- Sys.getenv("FRED_API_KEY")

# Retrieving necessary data
# wage growth
wage_growth_response <- request("https://api.stlouisfed.org/fred/series/observations") %>%
req_url_query(
  series_id = "CES0500000003",
  api_key = fred_api_key,
  file_type = "json"
) %>%
  req_perform()

wage_growth_data <- wage_growth_response %>%
  resp_body_json() %>%
  pluck("observations") %>%
  map_dfr(as_tibble) %>%
  mutate(date = as.Date(date), value = as.numeric(value))

wage_growth_data <- wage_growth_data %>%
  arrange(date) %>%  # Ensure data is sorted by date
  mutate(
    wage_growth_rate = (value - lag(value)) / lag(value) * 100  # Percentage change
  )

# Inspect the first few rows
head(wage_growth_data)
# inflation
inflation_response <- request("https://api.stlouisfed.org/fred/series/observations") %>%
  req_url_query(
    series_id = "CPIAUCSL",
    api_key = fred_api_key,
    file_type = "json"
  ) %>%
  req_perform()

inflation_data <- inflation_response %>%
  resp_body_json() %>%
  pluck("observations") %>%
  map_dfr(as_tibble) %>%
  mutate(date = as.Date(date), value = as.numeric(value))

inflation_data <- inflation_data %>%
  arrange(date) %>%  # Ensure data is sorted by date
  mutate(
    inflation_rate = (value - lag(value)) / lag(value) * 100  # Percentage change
  )
#Bond market
bond_market_response <- request("https://api.stlouisfed.org/fred/series/observations") %>%
  req_url_query(
    series_id = "DGS10",
    api_key = fred_api_key,
    file_type = "json"
  ) %>%
  req_perform()

bond_market_data <- bond_market_response %>%
  resp_body_json() %>%
  pluck("observations") %>%
  map_dfr(as_tibble) %>%
  mutate(date = as.Date(date), value = as.numeric(value))
#short term debt
short_term_debt_response <- request("https://api.stlouisfed.org/fred/series/observations") %>%
  req_url_query(
    series_id = "TB3MS",
    api_key = fred_api_key,
    file_type = "json"
  ) %>%
  req_perform()

short_term_debt_data <- short_term_debt_response %>%
  resp_body_json() %>%
  pluck("observations") %>%
  map_dfr(as_tibble) %>%
  mutate(date = as.Date(date), value = as.numeric(value))

# US returns
us_equity_response <- request("https://www.alphavantage.co/query") %>%
req_url_query(
  `function` = "TIME_SERIES_MONTHLY",
  symbol = "SPY",
  apikey = alpha_api_key
) %>%
  req_perform()

us_equity_data <- us_equity_response %>%
  resp_body_json() %>%
  pluck("Monthly Time Series") %>%
  map_dfr(as_tibble, .id = "date") %>%
  rename(
    open = `1. open`, high = `2. high`, low = `3. low`, close = `4. close`,
    volume = `5. volume`
  ) %>%
  mutate(date = as.Date(date), adjusted_close = as.numeric(close))

#Int returns
intl_equity_response <- request("https://www.alphavantage.co/query") %>%
  req_url_query(
    `function` = "TIME_SERIES_MONTHLY",
    symbol = "EFA",
    apikey = alpha_api_key
  ) %>%
  req_perform()

intl_equity_data <- intl_equity_response %>%
  resp_body_json() %>%
  pluck("Monthly Time Series") %>%
  map_dfr(as_tibble, .id = "date") %>%
  rename(
    open = `1. open`, high = `2. high`, low = `3. low`, close = `4. close`,
    volume = `5. volume`
  ) %>%
  mutate(date = as.Date(date), adjusted_close = as.numeric(close))

#monthly validation check
# Validate monthly data
check_monthly_frequency <- function(data) {
  return(length(unique(format(data$date, "%Y-%m"))) == nrow(data))
}

list(
  Wage_Growth = check_monthly_frequency(wage_growth_data),
  Inflation = check_monthly_frequency(inflation_data),
  Bond_Market = check_monthly_frequency(bond_market_data),
  Short_Term_Debt = check_monthly_frequency(short_term_debt_data),
  US_Equity = check_monthly_frequency(us_equity_data),
  Intl_Equity = check_monthly_frequency(intl_equity_data)
)

#Part 4
# Compute Long-Run Averages and variance in a datatable
## Calculate Long-Run Averages and Variances
summary_table <- data.frame(
  Series = c("Wage Growth", "Inflation", "US Equity", "International Equity", "Bond Market", "Short-Term Debt"),
  Long_Run_Average = c(
    mean(wage_growth_data$wage_growth_rate, na.rm = TRUE),  # Use wage_growth_rate
    mean(inflation_data$inflation_rate, na.rm = TRUE),      # Use inflation_rate
    mean(us_equity_data$adjusted_close, na.rm = TRUE),
    mean(intl_equity_data$adjusted_close, na.rm = TRUE),
    mean(bond_market_data$value, na.rm = TRUE),
    mean(short_term_debt_data$value, na.rm = TRUE)
  ),
  Variance = c(
    var(wage_growth_data$wage_growth_rate, na.rm = TRUE),   # Use wage_growth_rate
    var(inflation_data$inflation_rate, na.rm = TRUE),       # Use inflation_rate
    var(us_equity_data$adjusted_close, na.rm = TRUE),
    var(intl_equity_data$adjusted_close, na.rm = TRUE),
    var(bond_market_data$value, na.rm = TRUE),
    var(short_term_debt_data$value, na.rm = TRUE)
  )
)

# Display the Summary Table with DT
library(DT)
datatable(
  summary_table,
  options = list(pageLength = 5, autoWidth = TRUE),
  caption = "Summary Table of Long-Run Averages and Variances (Inflation and Wage Growth Rate Adjusted)"
)

#task 5
# Initialize parameters
starting_salary <- 50000
years <- 20  # 20-year career
projected_salary <- numeric(years)
projected_salary[1] <- starting_salary

# Use average wage growth rate
average_wage_growth <- mean(wage_growth_data$wage_growth_rate, na.rm = TRUE) / 100

# Project salary for 20 years
for (t in 2:years) {
  projected_salary[t] <- projected_salary[t - 1] * (1 + average_wage_growth)
}

# View the projected salary
projected_salary

# Final Average Salary (FAS)
fas <- mean(tail(projected_salary, 3))

# TRS parameters
benefit_multiplier <- 0.02  # 2%
years_of_service <- years

# TRS Benefit
trs_benefit <- years_of_service * fas * benefit_multiplier

trs_benefit

# ORP parameters
employee_contribution_rate <- 0.05  # 5%
employer_contribution_rate <- 0.08  # 8%
investment_return <- 0.06  # 6% annual return

# Initialize ORP balance
orp_balance <- 0

# Calculate ORP retirement balance
for (t in 1:years) {
  yearly_contribution <- projected_salary[t] * (employee_contribution_rate + employer_contribution_rate)
  orp_balance <- orp_balance * (1 + investment_return) + yearly_contribution
}

orp_balance
comparison <- data.frame(
  Plan = c("TRS", "ORP"),
  Value = c(trs_benefit, orp_balance)
)

# Display the table using DT
library(DT)
datatable(comparison, options = list(pageLength = 5, autoWidth = TRUE),
          caption = "Comparison of TRS and ORP Retirement Benefits (20-Year Career)")
library(ggplot2)

ggplot(comparison, aes(x = Plan, y = Value, fill = Plan)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Comparison of TRS and ORP Retirement Benefits (20-Year Career)",
    x = "Retirement Plan",
    y = "Benefit Value ($)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("steelblue", "darkorange")) +
  geom_text(aes(label = round(Value, 2)), vjust = -0.5)
#Looks like the orp beats out the trs heavily

#Task6:
# TRS parameters
retirement_age <- 65
death_age <- 85
years_retired <- death_age - retirement_age
cola <- mean(inflation_data$inflation_rate, na.rm = TRUE) / 100  # Average inflation rate

# Project TRS monthly benefit over retirement
trs_monthly_benefits <- numeric(years_retired * 12)
trs_monthly_benefits[1] <- trs_benefit / 12  # Initial monthly benefit
for (t in 2:length(trs_monthly_benefits)) {
  trs_monthly_benefits[t] <- trs_monthly_benefits[t - 1] * (1 + cola)
}
# ORP parameters
initial_balance <- orp_balance
withdrawals_per_year <- years_retired
annual_withdrawal <- initial_balance / withdrawals_per_year  # Equal annual withdrawals
investment_return <- mean(wage_growth_data$wage_growth_rate, na.rm = TRUE) / 100

# Project ORP balance and withdrawals
orp_balance_projection <- numeric(years_retired)
orp_balance_projection[1] <- initial_balance
orp_withdrawals <- numeric(years_retired)

for (t in 1:years_retired) {
  if (t == 1) {
    orp_withdrawals[t] <- annual_withdrawal
    orp_balance_projection[t] <- initial_balance - annual_withdrawal
  } else {
    orp_balance_projection[t] <- orp_balance_projection[t - 1] * (1 + investment_return) - annual_withdrawal
    orp_withdrawals[t] <- annual_withdrawal
  }
}

orp_balance_projection
orp_withdrawals
# Convert ORP annual withdrawal to monthly
orp_monthly_withdrawals <- rep(orp_withdrawals / 12, each = 12)

# Calculate monthly income gaps
income_gaps <- abs(trs_monthly_benefits - orp_monthly_withdrawals)

# Summary statistics
summary <- data.frame(
  Metric = c("Average Monthly Income (TRS)", "Average Monthly Income (ORP)",
             "Maximum Income Gap", "Minimum Income Gap", "Funds Remaining (ORP)"),
  Value = c(
    mean(trs_monthly_benefits),
    mean(orp_monthly_withdrawals),
    max(income_gaps),
    min(income_gaps),
    max(orp_balance_projection)  # ORP funds left at death
  )
)

summary

# Create a time vector for plotting
time <- seq(1, length(trs_monthly_benefits), by = 1)
# TRS vs ORP Monthly Income
income_data <- data.frame(
  Time = time,
  TRS = trs_monthly_benefits,
  ORP = orp_monthly_withdrawals
)

ggplot(income_data, aes(x = Time)) +
  geom_line(aes(y = TRS, color = "TRS")) +
  geom_line(aes(y = ORP, color = "ORP")) +
  labs(
    title = "Comparison of TRS and ORP Monthly Income",
    x = "Months in Retirement",
    y = "Monthly Income ($)"
  ) +
  scale_color_manual(values = c("TRS" = "blue", "ORP" = "orange")) +
  theme_minimal()

# ORP Balance Depletion
orp_balance_data <- data.frame(
  Time = seq(1, years_retired, by = 1),
  Balance = orp_balance_projection
)

ggplot(orp_balance_data, aes(x = Time, y = Balance)) +
  geom_line(color = "red") +
  labs(
    title = "ORP Balance Over Retirement",
    x = "Years in Retirement",
    y = "Balance ($)"
  ) +
  theme_minimal()
#Task 7
set.seed(123)

# Parameters
bootstrap_samples <- 200
career_years <- 20
retirement_years <- 20

# Create bootstrap samples
bootstrap_histories <- lapply(1:bootstrap_samples, function(i) {
  list(
    wage_growth = sample(wage_growth_data$wage_growth_rate, career_years, replace = TRUE),
    inflation = sample(inflation_data$inflation_rate, retirement_years, replace = TRUE),
    investment_return = sample(wage_growth_data$wage_growth_rate, retirement_years, replace = TRUE)
  )
})

# Initialize results
trs_results <- numeric(bootstrap_samples)
orp_results <- numeric(bootstrap_samples)
orp_exhaustion <- numeric(bootstrap_samples)

# Simulate TRS and ORP
for (i in 1:bootstrap_samples) {
  sample <- bootstrap_histories[[i]]
  wage_growth <- sample$wage_growth / 100
  inflation <- sample$inflation / 100
  investment_return <- sample$investment_return / 100
  
  # TRS Simulation
  salary <- numeric(career_years)
  salary[1] <- 50000
  for (t in 2:career_years) {
    salary[t] <- salary[t - 1] * (1 + wage_growth[t])
  }
  
  fas <- mean(tail(salary, 3))
  trs_benefit <- career_years * fas * 0.02
  trs_monthly <- trs_benefit / 12
  trs_monthly_benefits <- numeric(retirement_years * 12)
  trs_monthly_benefits[1] <- trs_monthly
  inflation_monthly <- rep(inflation, each = 12)
  
  for (t in 2:length(trs_monthly_benefits)) {
    trs_monthly_benefits[t] <- trs_monthly_benefits[t - 1] * (1 + inflation_monthly[t])
  }
  
  # ORP Simulation
  orp_balance <- 0
  withdrawal_rate <- 0.04
  for (t in 1:career_years) {
    orp_balance <- orp_balance * (1 + investment_return[t]) + salary[t] * (0.05 + 0.08)
  }
  
  orp_monthly_withdrawals <- numeric(retirement_years * 12)
  orp_balance_projection <- numeric(retirement_years * 12)
  orp_balance_projection[1] <- orp_balance
  investment_return_monthly <- rep(investment_return, each = 12)
  
  exhausted <- FALSE
  for (t in 1:(retirement_years * 12 - 1)) {
    if (is.na(orp_balance_projection[t])) {
      orp_balance_projection[t] <- 0
    }
    annual_withdrawal <- orp_balance_projection[t] * withdrawal_rate / 12
    orp_monthly_withdrawals[t] <- ifelse(is.na(annual_withdrawal), 0, annual_withdrawal)
    
    if (!is.na(orp_balance_projection[t]) && !is.na(investment_return_monthly[t])) {
      orp_balance_projection[t + 1] <- orp_balance_projection[t] * (1 + investment_return_monthly[t]) - annual_withdrawal
    } else {
      orp_balance_projection[t + 1] <- 0
    }
    
    if (!is.na(orp_balance_projection[t + 1]) && orp_balance_projection[t + 1] <= 0) {
      exhausted <- TRUE
      orp_balance_projection[t + 1] <- 0
      break
    }
  }
  
  orp_exhaustion[i] <- as.numeric(exhausted)
  trs_results[i] <- mean(trs_monthly_benefits, na.rm = TRUE)
  orp_results[i] <- mean(orp_monthly_withdrawals, na.rm = TRUE)
}

orp_exhaustion_prob <- mean(orp_exhaustion, na.rm = TRUE)
orp_outperformance_prob <- mean(orp_results > trs_results, na.rm = TRUE)

summary_table <- data.frame(
  Metric = c("Probability ORP Exhausts Before Death", "Probability ORP Outperforms TRS"),
  Value = c(orp_exhaustion_prob, orp_outperformance_prob)
)

datatable(summary_table, options = list(pageLength = 5, autoWidth = TRUE))

