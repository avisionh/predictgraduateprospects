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
# Packages Used: runjags, coda
# Notes: None
# ----------------------------------------------------------------

# FORMULATE PRIORS -----------------
# Graduate Prospects 2017 distribution
plotdist(data = data_master$`Graduate Prospects 2017`, histo = TRUE, demp = TRUE)
descdist(data = data_master$`Graduate Prospects 2017`, discrete = FALSE)

# Fit the uniform, normal, and beta distributions
# Can't be beta because Graduate Prospect  range outside of [0,1]
rpt_dist <- list()
name_dist <- c("Uniform", "Normal")
rpt_dist[[1]] <- fitdist(data = data_master$`Graduate Prospects 2017`, distr = "unif", method = "mme",
                       discrete = F, na.rm = T)
rpt_dist[[2]] <- fitdist(data = data_master$`Graduate Prospects 2017`, distr = "norm", method = "mme",
                       discrete = F, na.rm = T)

# Plot the goodness-of-fit graphs
par(mfrow = c(2,2)) 
func_plotGoF(dist = rpt_dist, titles = name_dist)

# BAYES REGRESSION -----------------
# Setup
 # turn variables positive to run Bayes
mat_reg <- apply(X = mat_reg, MARGIN = 2, FUN = func_transPos)
 # create vector of regressuion parameters
vec_regParameters <- "beta0"
len <- ncol(x = mat_reg) - 1
for(i in 1:len) vec_regParameters <- append(x = vec_regParameters, values = paste0("beta[", i, "]"))
numRows <- nrow(mat_reg)
 # define DV and IVs for regression
data_regBayes <- list(x = mat_reg[1:numRows, 1:len], y = mat_reg[1:numRows])

tic <- Sys.time()

# Run model
set.seed(123)
model_regBayes <- run.jags(method = "parallel", model = "regModel.txt", monitor = vec_regParameters,
                           data = data_regBayes, n.chains = 4, summarise = FALSE, plots = FALSE)
toc <- Sys.time()
toc - tic
 # convert to mcmc list
model_regBayes <- as.mcmc.list(model_regBayes)

# MCMC DIAGNOSTICS -----------------
for (i in 1:length(vec_regParameters)) diagMCMC(codaObject = model_regBayes, parName = vec_regParameters[i])
gelman.diag(model_regBayes)


rm(len, numRows, tic, toc, i)

# -----------------
tic <- Sys.time()
test <- func_kFoldCV(x = mat_reg[1:numRows, 1:len], y = mat_reg[1:numRows],
                     partition = 0.8, folds = 2, parameters = vec_regParameters,
                     burnin = 6000, nChains = 2)
toc <- Sys.time()
toc - tic