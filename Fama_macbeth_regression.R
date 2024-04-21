rm(list=ls())

rf = 0.01
N = 2000 # of firms in an economy
T = 2520 # length of periods
dt = 1/252 # length of a time unit (annual)
set.seed(12345)

s = 0.5 * dt^0.05 # average idiosyncratic risk

# Correctly specify the risk of a firm and market factor
beta_true = rnorm(N, 0, 1) # True beta for N firms
mkt_factor = 0.06 * dt + rnorm(T, mean = 0, sd = 1) # Market factor, return of market for each period

# Simulate market return
mkt = rf * dt + mkt_factor # Market return for T periods

# Initialize the matrix for stock returns
stock_returns = matrix(nrow = T, ncol = N)

# Populate stock_returns with simulated data
for (i in 1:T) {
    for (j in 1:N) {
        stock_returns[i, j] = rf * dt + beta_true[j] * mkt_factor[i] + s * rnorm(1, mean = 0, sd = 1)
    }
}

# Fama-MacBeth regression

# Step 1: Time series regression for all N firms
estimated_beta_bin = numeric(length = N)

for (i in 1:N) {
    result = lm(stock_returns[, i] ~ mkt - 1) # -1 removes the intercept
    estimated_beta_bin[i] <- result$coefficients[1] # Get the slope (beta estimate)
}

# Compare beta_true and estimated_beta_bin
data.frame(beta_true, estimated_beta_bin)


## step2:  cross sectional regression for all T time
## regression cross sectional of stock return on each firm's beta (time by time)
f = numeric()

for(i in 1:T){ ## time by time cross sectional regression
    model = lm(stock_returns[i,]~estimated_beta_bin) #each time entire firm
    f[i]<- model$coefficient[2]
}

summary(data.frame(f))
# Min.   :-3.86762  
# 1st Qu.:-0.63937  
# Median :-0.01301  
# Mean   :-0.01012  ##risk premium for factor return
# 3rd Qu.: 0.62131  
# Max.   : 3.26228  

