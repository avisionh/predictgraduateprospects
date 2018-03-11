# --------------------- #
## Bayesian Regression ##
# --------------------- #

# ----------------------------------------------------------------
# Desc: Carry out Bayesian multi-linear regression.
# Naming convention: N/A
# Credit: None
# Script Dependencies:
# 1. '0.1 - functions.R'
# 2. '1.1 - dataLoad.R'
# 3. '1.2 - dataWrangle.R'
# 4. '1.3 - dataConsolidation.R'
# 5. '2.1 - dataExploration.R'
# 6. 'regModel.txt'
# Packages Used: runjags, coda, mcmcse
# Notes: None
# ----------------------------------------------------------------
# Set-up
 # turn variables positive to run Bayes
mat_reg <- apply(X = mat_reg, MARGIN = 2, FUN = func_transPos)
 # alter data_master$`Graduate Prospects 2017`
 # to estimate distibution parameters
 # for Bayesian model in regModel.txt
temp <- data_master[, "Graduate Prospects 2017"]
temp <- ifelse(is.na(temp), median(temp, na.rm = TRUE), temp)
temp <- (temp - min(x = temp)) / (max(temp) - min(temp))
temp <- func_transPos(temp)                    

# FORMULATE PRIORS -----------------
# Graduate Prospects 2017 distribution
plotdist(data = temp, histo = TRUE, demp = TRUE)
descdist(data = temp, discrete = FALSE)

# Fit the uniform, normal, and beta distributions
# Can't be beta because Graduate Prospect  range outside of [0,1]
rpt_dist <- list()
name_dist <- c("Uniform", "Normal")
rpt_dist[[1]] <- fitdist(data = temp, distr = "unif", method = "mme",
                       discrete = F, na.rm = T)
rpt_dist[[2]] <- fitdist(data = temp, distr = "norm", method = "mme",
                       discrete = F, na.rm = T)

# Plot the goodness-of-fit graphs
par(mfrow = c(2,2)) 
func_plotGoF(dist = rpt_dist, titles = name_dist)

# BAYES REGRESSION -----------------
# Setup
 # create vector of regressuion parameters
vec_regParameters <- "beta0"
len <- ncol(x = mat_reg) - 1
for(i in 1:len) vec_regParameters <- append(x = vec_regParameters, values = paste0("beta[", i, "]"))
numRows <- nrow(mat_reg)
 # define DV and IVs for regression
data_regBayes <- list(x = mat_reg[1:numRows, 1:len], y = mat_reg[1:numRows, len + 1])

tic <- Sys.time()

# Run model
set.seed(123)
model_regBayes <- run.jags(method = "parallel", model = "regModel.txt", monitor = vec_regParameters,
                           data = data_regBayes, n.chains = 4, summarise = FALSE, plots = FALSE, silent.jags = TRUE)
toc <- Sys.time()
toc - tic
 # convert to mcmc list
model_regBayes <- as.mcmc.list(model_regBayes)

# MCMC DIAGNOSTICS -----------------
#for (i in 1:length(vec_regParameters)) diagMCMC(codaObject = model_regBayes, parName = vec_regParameters[i])
 # turn off plotting window so can show console-generated plots
dev.off()
 # Gelman-Rubin statistic and summaries
gelman.diag(x = model_regBayes)
summary(object = model_regBayes)
 # credible interval
HPDinterval(model_regBayes, prob = 0.95)

# RMSE ----------------------------
tic <- Sys.time()
# From above plots, set burnin = 6,000
model_finalregBayes <- func_kFoldCV(x = mat_reg[1:numRows, 1:len], y = mat_reg[1:numRows, len + 1],
                     partition = 0.8, folds = 5, parameters = vec_regParameters,
                     burnin = 6000, nChains = 4)
toc <- Sys.time()
toc - tic

 # get RMSE
txt_rmseBayes <- mean(x = model_finalregBayes[[3]])
txt_rmseBayes <- round(x = txt_rmseBayes, digits = 2)
if(txt_rmseBayes < 0.3) {
  txt_rmse_conc <- c("small", "accurate")
} else {
  if (txt_rmseBayes < 0.6) {
    txt_rmse_conc <- c("relatively high", "relatively inaccurate")
  } else
    txt_rmse_conc <- c("very high", "inaccurate")
}

rm(temp, len, numRows, tic, toc, i, data_regBayes)