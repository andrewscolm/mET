########################################################################################
# This is a multipurpose script to run supra-threshold screening algorithms. See
# Turpin et al. Development of visual field screening procedures: A case study of the
# Octopus perimeter. Translational Vision Science & Technology, 5(3):3:1–9, 2016
########################################################################################
# set working directory.
# IMPORTANT: make sure to use the directory where this code is stored. And make sure that
# subfolders "setupFiles", "results", and "simulations" exist. The folder setupFiles 
setwd( "/Users/ivanmarin-franch/03.glaucoma/03.projects/11.PaulArtes/07.suprathresoldAlgorithms/01.esterman" )
# load libraries
library( OPI )
# load
source( "helperFunctions.r" )

########################################################################################
# INPUT parameters
id         <- "s001" # IMPORTANT
simulation <- TRUE # if it is a simulation
fname      <- "setupFiles/estermanNew.csv"  # file containing the spatial test locations
########################################################################################
# stimulus parameters
stimTime       <- 200  # time of stimulus presentation in seconds
interStimTime  <- 1000 # time between stimulus presentation in seconds
numberFPtrials <- 10   # number of catch trials to quantify false positives.
bgluminance    <- 10   # background luminance of the bowl
                       # THIS PARAMETER FOR opiSetBackground IS NOT USED AT THE MOMENT.
fixCentre      <- 1    # type of fixation target. NEEDS TO BE TESTED.
                       # THIS PARAMETER FOR opiSetBackground IS NOT USED AT THE MOMENT.
########################################################################################
# OPI parameters
serverPort               <- 50001
eyeSuiteSettingsLocation <- "C:/ProgramData/Haag-Streit/EyeSuite/"
eye                      <- "right"
gazeFeed                 <- 1
bigWheel                 <- FALSE
pres_buzzer              <- 0
resp_buzzer              <- 3
zero_dB_is_10000_asb     <- TRUE
########################################################################################
# set up the OPI connection we want:
if( simulation ) {
  chooseOpi( "SimHensonRT" ) # simulated "perimeter" based on Henson's FOS curves
  data( RtDbUnits )
} else {
  chooseOpi( "Octopus900" ) # the Octopus 900
}
# maximum luminance of the Octopus 900
if( zero_dB_is_10000_asb ) {
  maxInt <- 10000 # apostilb
} else {
  maxInt <-  4000 # apostilb
}

# load the files with the locations and stimulus levels
trials   <- read.csv( fname )
# setup random spatial stimulus presentation
trialseq <- setupTrialSequence( trials, numberFPtrials )

# if maximum luminance is 4000 asb, then correct the dB level for presentation
if( !zero_dB_is_10000_asb ) trialseq$stimLevel <- cdTodb( dbTocd( trialseq$stimLevel ), 4000 / pi )

# initialize the connection with the Octopus or Henson's simulation
if( simulation ) {
  opiInitialize( rtData = RtDbUnits, maxStim = maxInt / pi )
} else {
  opiInitialize( serverPort               = serverPort,
                 eyeSuiteSettingsLocation = eyeSuiteSettingsLocation,
                 eye                      = eye,
                 gazeFeed                 = gazeFeed,
                 bigWheel                 = bigWheel,
                 pres_buzzer              = pres_buzzer,
                 resp_buzzer              = resp_buzzer,
                 zero_dB_is_10000_asb     = zero_dB_is_10000_asb )
}

# record date and time
systime <- Sys.time()
sysdate  <- format( systime, "%Y-%m-%d" )
systime  <- gsub( ":", "_", format( systime, "%X" ) )
if( simulation ) {
  fnamelog <- paste0( "simulations/", id, "_", sysdate, "_", systime, "_log.csv" )
  fname    <- paste0( "simulations/", id, "_", sysdate, "_", systime, ".csv" )
} else {
  fnamelog <- paste0( "results/", id, "_", sysdate, "_", systime, "_log.csv" )
  fname    <- paste0( "results/", id, "_", sysdate, "_", systime, ".csv" )
}

# start the presentations
trialseq$seen   <- NA
trialseq$rt     <- NA
trialseq$pupilx <- NA
trialseq$pupily <- NA
trialseq$err    <- ""
for( i in 1:nrow( trialseq ) ) {
  print( paste( i, "of", nrow( trialseq ) ) )
  # build stimulus for this presentation and inform next stimulus
  stim <- makeStim( trialseq[i,], maxInt = maxInt )
  if( i < nrow( trialseq ) ) stimNext <- makeStim( trialseq[i+1,], maxInt = maxInt )
  if( simulation ) {
    resp <- opiPresent( stim = stim )
  } else {
    resp <- opiPresent( stim = stim, stimNext = stimNext )
  }
  # HERE IS WHERE WE CAN USE DIFFERENT MULTI-SAMPLING STRATEGIES.
  # so far, we only check for non false-positive trials if the location has been tested
  # in the past and was seen
  if( !trialseq$fptrial[i] ) {
    idx <- which( trialseq$x == trialseq$x[i] & trialseq$y == trialseq$y[i] )
    idx <- idx[!c( idx %in% i )]
    if( idx < i ) if( trialseq$seen[idx] ) next
  }
  if( resp$seen == 1 ) {
    trialseq$seen[i] <- TRUE
  } else {
    trialseq$seen[i] <- FALSE
  }
  trialseq$rt[i]     <- resp$time
  if( !is.null( resp$err ) ) trialseq$err[i]  <- resp$seen
  if( !simulation ) {
    trialseq$pupilx[i] <- resp$pupilX
    trialseq$pupily[i] <- resp$pupilY
  }
}

# if maximum luminance is 4000 asb, then change reference back to 10000
if( !zero_dB_is_10000_asb ) trialseq$stimLevel <- cdTodb( dbTocd( trialseq$stimLevel, 4000 / pi ) )

# remove all locations not seen
idx <- which( !is.na( trialseq$seen ) )
if( length( idx ) > 0 ) trialseq <- trialseq[idx,]
# result summary
trials      <- trials[,c(1:2)]
trials$seen <- "none"
trials$rt   <- ""
for( i in 1:nrow( trials ) ) {
  idx <- which( trialseq$x == trials$x[i] & trialseq$y == trials$y[i] & trialseq$fptrial == FALSE )
  if( length( idx ) == 1 ) {
    trials$seen[i] <- "first"
    trials$rt[i]   <- trialseq$rt[idx[1]]
  } else if( trialseq$seen[idx[2]] ) {
    trials$seen[i] <- "second"
    trials$rt[i]   <- trialseq$rt[idx[2]]
  }
}

# save results
write.csv( trials, file = fname, row.names = FALSE )
write.csv( trialseq, file = fnamelog, row.names = FALSE )