# ============================================================
# GMM + Graphical Model (GGM/glasso)
# on the real 11-ticker data.
#
# packages installed to run this
# install.packages(c("moments", "mclust", "glasso", "mvtnorm", "qgraph"))
# ============================================================

library(tidyverse)

# ============================================================
# STEP 1: 
# Load the two files actually used in this pipeline.
#
# All 10 Sharadar CSVs sit in data_csv/
# SEP  = daily prices -> what the return matrix and GMM
# features are built from
# SF1  = fundamentals -> not currently used downstream, kept
# loaded in case a future feature needs it, but nothing
# below touches it yet
# ============================================================

# change this to where your csv is
base_path <- "/Users/ahmedahmed/Desktop/Stat_4051_Project/data_csv"

# reading csv here could be different if on mac or windows etc 
# learned the hard way
raw_SEP <- read_csv(file.path(base_path, "SHARADAR_SEP_2_f1b7d3c75f69bea6716145181fe035e0.csv"))
raw_SF1 <- read_csv(file.path(base_path, "SHARADAR_SF1_3_f9b5c423610c7ae36b31c4b8129e694c.csv"))

# ============================================================
# Step 2:
# Filtered down to the 11 proposal tickers + 2025 dates,
# then compute daily log returns.
#
# Why log returns instead of raw prices: prices themselves aren't
# stationary/comparable across companies at wildly different price
# levels (AAPL vs. BLK), and log returns are what the proposal's
# planned features (avg return, volatility, drawdown, skew) are
# defined on. log(close_t / close_t-1) is apparently the standard
# convention for daily return series
#
# Why dplyr:: / tidyr:: Because why not. prefixes everywhere: 
# earlier in this project, loading MASS (for mvrnorm, during unrelated 
# practice) silently masked dplyr::select() and broke this exact pipeline
# with a cryptic "unused arguments" error. Namespacing every call
# makes the script immune to that regardless of what else gets
# loaded earlier in the session. Had to learn this the hard way.
# ------------------------------------------------------------

target_tickers <- c("AAPL", "GOOGL", "BLK", "AXP", "BAC",
                     "CI", "JNJ", "MDT", "BA", "MMM", "FDX")

returns_tibble <- raw_SEP %>%
  dplyr::filter(ticker %in% target_tickers) %>%
  dplyr::filter(date >= "2025-01-01", date <= "2025-12-31") %>%
  tidyr::drop_na() %>%
  dplyr::arrange(ticker, date) %>%
  dplyr::group_by(ticker) %>%
  dplyr::mutate(lag_adj_close = dplyr::lag(closeadj)) %>%
  tidyr::drop_na() %>%   # drops the first day per ticker, which has no lag to compute a return from
  dplyr::mutate(log_return = log(closeadj / lag_adj_close)) %>%
  dplyr::ungroup() %>%
  dplyr::select(date, ticker, log_return)

# Confirming all 11 tickers, similar row counts each.
# Result when this was run: all 11 tickers, 82 rows each. That means
# only 82 trading days of 2025 were available at the time of pulling
# this data, not a full ~252-day year. Add to report's limitations section: 
# everything downstream (features, correlations,
# glasso) is estimated on fewer days than a full year would give.
returns_tibble %>% dplyr::count(ticker)

# ============================================================
# STEP 3: 
# Build two different shapes of the same return data,
# because GMM and the graphical model need opposite orientations.
#
# feature_table / feature_matrix_scaled: one row per company,
# columns are summary features -> this is what GMM clusters.
# returns_wide: one row per day, columns are companies' returns
# -> this is what the graphical model needs, since glasso
# works on a covariance/correlation matrix computed across
# companies over time, not a matrix of summary stats.
# Getting this orientation backwards was an early mistake in my
# thinking with Fran's script (it had tickers as rows, dates as columns, raw
# unsummarized returns) ad I didn't know how to work it,
# pivot_wider(id_cols = date, ...) below
# is the fix, giving days x companies.
# ------------------------------------------------------------

max_drawdown <- function(r) {
  # Largest peak to trough decline in cumulative growth over the window.
  # Not a base R function, this was written by hand to match the proposal's
  # planned feature list (avg return, volatility, drawdown, skew).
  cum_growth   <- cumprod(1 + r)
  running_peak <- cummax(cum_growth)
  min((cum_growth - running_peak) / running_peak)
}

feature_table <- returns_tibble %>%
  dplyr::group_by(ticker) %>%
  dplyr::summarise(
    avg_return   = mean(log_return),
    volatility   = sd(log_return),
    max_drawdown = max_drawdown(log_return),
    skewness     = moments::skewness(log_return)
  ) %>%
  dplyr::ungroup()

# Scaling before GMM/distance based methods: these 4 features are on
# very different scales (avg_return is tiny, ex ~0.001; drawdown is
# a larger negative fraction), without scaling, whichever feature
# has the largest raw magnitude would dominate the clustering for no
# statistically meaningful reason.
feature_matrix_scaled <- feature_table %>%
  tibble::column_to_rownames("ticker") %>%
  scale()

returns_wide <- returns_tibble %>%  #Fran: This has observations as dates i.e. very dependent observations
  tidyr::pivot_wider(id_cols = date, names_from = ticker, values_from = log_return)

# ============================================================
# STEP 4: GMM on the feature table (11 companies x 4 features).
#
# What I was hoping to find: the proposal groups the 11 companies
# into informal sectors (tech has AAPL/GOOGL, finance has BLK/AXP/BAC,
# healthcare: CI/JNJ/MDT, industrial: BA/MMM/FDX), the hope was
# that GMM would recover something close to that grouping purely
# from return based features, which would validate the sector
# story with actual data instead of just asserting it.
#
# Why G = 1:4, not unrestricted: first tried Mclust with no G
# limit at all. Result: it found 9 separate components for 11
# data points, this made it where essentially treating almost 
# every company as its own cluster. That's overfitting, not a real 
# pattern: with only 11 observations, an unconstrained search over cluster 
# count will be noise. I figured this out from the pattern shown in
# Discussion 3's S-curve example, where letting Mclust search
# freely on a small/moderate-n dataset also over split into 8-9
# components, same failure mode, different data, which is what
# made me trust it was a real, expected issue and not a mistake in
# my code. Restricting the search to G = 1:4 forces it to consider
# only a small, defensible range of cluster counts.
# ============================================================

library(mclust)

gmm_fit_real <- Mclust(feature_matrix_scaled, G = 1:4)
summary(gmm_fit_real)
gmm_fit_real$classification #Fran: cluster membership
round(gmm_fit_real$z, 2) #Fran: more cluster membership

# Actual result: BIC picked 2 clusters (not 4), splitting as:
#   Group 1: AAPL, AXP, BA, BAC, FDX, GOOGL
#   Group 2: BLK, CI, JNJ, MDT, MMM
# This partially matches the hoped-for sector story (a tech +
# industrial leaning group vs. a more defensive group) but does not
# cleanly separate finance from healthcare ex BLK (finance) lands
# with the healthcare names instead of with AXP/BAC. Worth reporting
# honestly as a partial confirmation, not framing it as if GMM found
# exactly the 4 sector split we expected going in.

# ============================================================
# STEP 5: 
# Graphical model (glasso) on the real return matrix.
# The whole point of this section is picking rho, the glasso
# penalty, and that choice went through three attempts before
# landing somewhere defensible.
# ============================================================

library(glasso)
library(mvtnorm)
# Note: loading mvtnorm after mclust prints a message that mvtnorm's
# dmvnorm masks mclust's, code still runs regardless. mvtnorm::dmvnorm
# (vectorized over rows of a matrix) is the one this script actually
# needs, and it's the one R will use since mvtnorm was loaded last.

real_tickers <- colnames(returns_wide)[-1]
X_real <- as.matrix(returns_wide[ , -1])   # days x companies
n_days <- nrow(X_real)

# ATTEMPT 1:
# scanned rho over c(0.1, 0.2, 0.3, 0.4, 0.5)
# on a plain cor() matrix, counted edges above a 0.05 threshold at
# each (32, 29, 24, 20, 13), and picked rho = 0.3 by eyeballing what
# looked like "reasonable" sparsity. That's not a real justification
# it's just picking whichever picture looks nicest. Replaced by
# what follows.

# ATTEMPT 2: 
# rho chosen by held out predictive log-likelihood,
# following GGM and Ising Model with Validation and Cross-Validation
# The idea: instead of picking rho by how the resulting picture looks, 
# score each candidate rho by how well its fitted covariance predicts 
# days the model never saw, using dmvnorm(). Higher held out log
# likelihood = genuinely better fit to the real correlation structure, 
# not just a sparser or denser picture.

rho_grid <- c(seq(0.01, 0.05, by = 0.01), seq(0.1, 0.5, by = 0.05))
# (This grid was extended down to 0.01 after a first pass on
# seq(0.05, 0.5, by = 0.05) picked rho = 0.05, which is the smallest value
# tested, meaning the real optimum might be even lower. Wanted to
# check that before trusting a boundary value.)

# single 70/30 train/validation split
set.seed(4051)
train_idx <- sample(seq_len(n_days), size = round(0.7 * n_days))
X_train <- X_real[train_idx, ]
X_valid <- X_real[-train_idx, ]

# Ridge stabilized correlation matrix
# S <- cor(X) + diag(1e-5, ncol(X))), a tiny amount added to the
# diagonal to keep the matrix well-conditioned/invertible, standard
# practice before feeding a correlation matrix into glasso.
S_train <- cor(X_train) + diag(1e-5, ncol(X_train))

valid_loglik <- sapply(rho_grid, function(r) {
  fit <- glasso(S_train, rho = r)
  Sigma_hat <- (fit$w + t(fit$w)) / 2   # symmetrize, glasso can return tiny asymmetries from numerical rounding
  sum(dmvnorm(X_valid, mean = colMeans(X_train), sigma = Sigma_hat, log = TRUE))
})
names(valid_loglik) <- rho_grid
cat("Validation log-likelihood by rho\n")
print(round(valid_loglik, 1))
rho_validation <- rho_grid[which.max(valid_loglik)]
cat("Validation-selected rho:", rho_validation, "\n\n")

# Result: -152.7, -162.2, -170.3, -177.4, -183.7, -209.1, -228.4,
# -244.2, -257.5, -269.0, -279.1, -288.1, -296.0, -303.2 (for rho =
# 0.01, 0.02, ..., 0.5), strictly increasing as rho shrinks, best
# at rho = 0.01, the smallest value tried, still climbing. Not an
# interior optimum, the criterion just keeps wanting less penalty.

# 5-fold cross-validation, more stable than trusting one
# lucky/unlucky 70/30 split on only ~82 days. Fold assignment below
# mirrors caret::createFolds() from Discussion 6 (random, roughly
# equal sized folds) but built with base R sample() to avoid adding
# the caret dependency under time pressure.
set.seed(4051)
k <- 5
folds <- sample(rep(1:k, length.out = n_days))

cv_loglik <- sapply(rho_grid, function(r) {
  fold_scores <- sapply(1:k, function(f) {
    X_tr <- X_real[folds != f, ]
    X_te <- X_real[folds == f, ]
    S_tr <- cor(X_tr) + diag(1e-5, ncol(X_tr))
    fit <- glasso(S_tr, rho = r)
    Sigma_hat <- (fit$w + t(fit$w)) / 2
    sum(dmvnorm(X_te, mean = colMeans(X_tr), sigma = Sigma_hat, log = TRUE))
  })
  sum(fold_scores)
})
names(cv_loglik) <- rho_grid
cat("5-fold CV total log-likelihood by rho: \n")
print(round(cv_loglik, 1))
rho_cv <- rho_grid[which.max(cv_loglik)]
cat("CV-selected rho:", rho_cv, "\n\n")

# Result: same monotonic pattern (-514.3 at rho=0.01 down to -997.1
# at rho=0.5), best at rho = 0.01 again. Conclusion at this point:
# not a grid-resolution problem but a  structural. With n = 82 days
# >> p = 11 companies, there's very little overfitting risk, so pure
# predictive log-likelihood has nothing pushing it toward sparsity.
# That's a real, reportable result (these stocks are broadly, densely
# correlated, normal market wide co movement), but on its own it
# doesn't hand me a usable rho for an interpretable graph.

# ATTEMPT 3:
# BIC, which explicitly penalizes model
# complexity instead of rewarding fit alone. Formula follows the
# standard Gaussian graphical model BIC: -2*loglik + log(n)*df,
# where df = p diagonal entries + one parameter per unique edge.
# The hope here was that adding a real complexity penalty would let
# the criterion actually reward sparsity where earlier ones couldn't.

S_real <- cor(X_real) + diag(1e-5, ncol(X_real))
p <- ncol(X_real)
bic_grid <- seq(0.05, 0.6, by = 0.05)

bic_scores <- sapply(bic_grid, function(r) {
  fit <- glasso(S_real, rho = r)
  Theta_r <- fit$wi
  n_edges <- sum(abs(Theta_r) > 1e-8 & row(Theta_r) != col(Theta_r)) / 2
  df <- p + n_edges
  loglik_term <- n_days * (log(det(Theta_r)) - sum(diag(S_real %*% Theta_r)))
  -loglik_term + log(n_days) * df   # smaller BIC = better tradeoff of fit vs. complexity
})
names(bic_scores) <- bic_grid
cat("BIC by rho (smaller = better): \n")
print(round(bic_scores, 1))
rho_bic <- bic_grid[which.min(bic_scores)]
cat("BIC-selected rho:", rho_bic, "\n\n")

# Result: 530.8, 573.5, 631.6, 694.8, 741.9, 799.0, 849.8, 893.7,
# 929.4, 968.1, 1000.8, 1046.7 (rho = 0.05, 0.1, ..., 0.6), still
# strictly increasing, best at rho = 0.05, the grid floor again.
#
# Final Conclusion:
# 3 Independent, principled criteria (validation log-likelihood,
# 5-fold CV log-likelihood, and BIC) all agree, over every rho
# actually tested, that less penalization always scores better.
# That's not a failure to search hard enough, it means these 11 stocks' 
# returns are genuinely broadly correlated rather than sparsely/conditionally 
# independent, which makes sense given they all lived through the same few months 
# of 2025 market conditions.A heavily pruned "sparse graph" story would 
# misrepresent this data. rho = 0.05 is used below as the least restrictive, 
# and best supported value across all three criteria. This is not a sparsity 
# target that was reached, but the honest result of the tuning process.

chosen_rho <- rho_bic

fit_real <- glasso(S_real, rho = chosen_rho)
Theta_real <- fit_real$wi
rownames(Theta_real) <- colnames(Theta_real) <- real_tickers

# Which pairs are still connected at this rho confirms the "mostly
# connected" story: most rows below have many entries, ex most
# companies are linked to most other companies once fit at rho = 0.05.
which(abs(Theta_real) > 0.05 & row(Theta_real) != col(Theta_real), arr.ind = TRUE)

# ============================================================
# STEP 6: 
# Network diagram.
#
# Why qgraph instead of igraph (the original choice): Discussion 6
# visualizes its GGM results with qgraph specifically because it
# color codes edges by the sign of the partial correlation (positive
# vs. negative), which a plain igraph adjacency plot doesn't do by
# default. More informative (sign matters for interpretation,
# not just presence/absence of a connection.)
# ============================================================

library(qgraph)

plot_title <- paste0("Graphical Model: 11-Ticker Network (rho = ", chosen_rho, ")")

qgraph(Theta_real,
       layout = "spring",
       labels = real_tickers,
       title = plot_title)

# First attempt at saving wrapped this same qgraph() call in
# png(...); qgraph(...); dev.off(), that threw a
# "cannot open the connection" / "Graphics error: Plot rendering
# error" in RStudio on dev.off(). That turned out to be an RStudio
# graphics device snapshot thing (the on screen plot rendered fine;
# only the save step broke), not a mistake in the modeling code.
# Fixed by using qgraph's own filetype/filename save arguments
# instead, which write the file directly without going through
# RStudio's graphics device at all, confirmed working ("Output
# stored in .../network_plot.png").
qgraph(Theta_real,
       layout = "spring",
       labels = real_tickers,
       title = plot_title,
       filetype = "png",
       filename = "network_plot",
       width = 8,
       height = 6)
