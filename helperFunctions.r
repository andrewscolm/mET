# generate the random sequence with false positive trials
setupTrialSequence <- function( trials, numberFPtrials = 10 ) {
  nreps     <- ( ncol( trials ) - 2 ) / 2 # the structure of the Excel is important to obtain the max number of repetitions
  trialseq <- trials[,c( 1:4 )]
  trials    <- trials[,-c( 3:4 )]
  names( trialseq ) <- c( "x", "y", "stimLevel", "stimSize" )
  fptrials <- data.frame( x         = rep( 50,    numberFPtrials ),
                          y         = rep( 50,    numberFPtrials ),
                          stimLevel = rep( 60,    numberFPtrials ),
                          stimSize  = rep( 0.108, numberFPtrials ) )
  trialseq  <-   rbind( trialseq, fptrials )
  # randomize first trial + false positive trials
  trialseq <- trialseq[sample( nrow( trialseq ) ),]
  for( i in 2:nreps ) {
    names( trials )[c( 1:4 )] <- names( trialseq )
    trialseq <- rbind( trialseq, trials[sample( nrow( trials ) ),c( 1:4 )] )
    trials   <- trials[,-c( 3:4 )]
  }
  trialseq$fptrial                           <- FALSE
  trialseq$fptrial[trialseq$stimLevel == 60] <- TRUE
  return( trialseq )
}

# this function just formats the stimulus info to pass onto the Octopus or the Henson simulations
makeStim <- function( st, maxInt, color = "white", duration = 200, responseWindow = 1000 ){
  s <- list( x              = st$x,
             y              = st$y,
             level          = dbTocd( st$stimLevel, maxInt / pi ),
             size           = as.numeric( st$stimSize ),
             color          = color,
             duration       = duration, 
             responseWindow = responseWindow )
  class( s ) <- "opiStaticStimulus"
  return( s )
}