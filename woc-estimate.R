library(rjags)

#### user options ####

options <- list(
  modeltype = "latent",  # possible values: "latent", "given"
  datatype = "all",      # possible values: "experts","USexperts","all"
  burnin = 1000,         # number of burn in samples
  nsamp = 1000,          # number of MCMC samples
  save = FALSE           # save the data?
)

#### functions to load and format data & model ####

# function for reading and preprocessing data
getEmpiricalData <- function(options) {
  
  # load data from the CSV file
  data <- read.csv(file="handwriting2.csv")
  
  # trim to relevant subset
  if(options$datatype == "experts") {
    data <- data[data$Condition %in% c("Non-US HW Expert", "US HW Expert"),]
  } else if(options$datatype == "USexperts") {
    data <- data[data$Condition %in% c("US HW Expert"),]
  }
  
  # trim out the one participant who gave "50" as the answer
  # to everything (because zero variance is problematic)
  s <- aggregate(est ~ Participant,data,sd)
  excl <- s$Participant[s$est<1]
  data <- data[!(data$Participant %in% excl),] 
  
  return(data)
  
}

# function for organising data
getJAGSData <- function(data, options) {
  
  # count stuff
  ids <- unique(data$Participant)
  nsubj <- length(ids)
  nfeat <- nlevels(data$Feature)
  
  # renumber ids strictly sequentially for JAGS
  for(s in 1:nsubj) {
    inds <- data[, "Participant"] == ids[s]
    data[inds, "Participant"] <- s 
  }
  
  # non missing cases only, and rescale probabilities to [0,1]
  ind <- !is.na(data$est) 
  est <- data$est[ind] / 100
  true <- data$true[ind] / 100
  id <- data$Participant[ind]
  cond <- as.character(data$Condition)[ind]
  stim <- as.numeric(data$Feature)[ind]
  
  # number of observations
  nobs <- length(est)
  
  # ground truth probabilities
  xt <- aggregate(true ~ stim, FUN=function(x){x[1]})[[2]]
  
  # group membership at the person level
  group <- as.numeric(
    aggregate(
      Condition~Participant,
      data,
      function(x){x[1]}
    )[[2]]
  )
  
  # return data structure that JAGS will use
  return( list(
    y = est, # participant responses
    xt = xt,  # actual frequency
    item = stim,  # which item was presented?
    nobs = nobs,   # observations
    nsubj = nsubj,
    id = id,
    group = group,
    nfeat = nfeat
  ))
  
}


getJAGSModel <- function(jagsData, options) {
  
  # load the model specification from file
  modelText <- readLines("./woc-model.bug")
  modelText <- paste(modelText,collapse = "\n")
  
  # substitute the relevant line for model type
  if(options$modeltype == "latent") {
    featureModelLine <- "x[f] ~ dbeta(1,1)"
  } else if(options$modeltype == "given") {
    featureModelLine <- "x[f] <- xt[f]"
  }
  modelText <- gsub(
    pattern = "INSERT-FEATURE-MODEL-HERE", 
    replacement = featureModelLine, 
    x = modelText, 
    fixed = TRUE
    )
  
  # return a JAGS model
  return( jags.model(
    file = textConnection(modelText),
    n.adapt = options$burnin,
    data = jagsData)
  )
  
}

#### do everything ####

# set up the model
dataset <- getEmpiricalData(options)
jagsData <- getJAGSData(dataset, options)
jagsModel <- getJAGSModel(jagsData, options)

# draw samples
samples <- jags.samples(
  model = jagsModel, 
  variable.names = c("x","calibration","precision","shift",
                     "scale","bias","noise","err_x"), 
  n.iter = options$nsamp 
)

# save the data if requested
if(options$save == TRUE) {
  filename <- paste0("hier-ld-estimates-", options$datatype, "-", 
                     options$modeltype, ".Rdata")
  save(samples, dataset, jagsModel, jagsData, options, file = filename)
}


