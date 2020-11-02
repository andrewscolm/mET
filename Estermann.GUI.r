                setwd("C:/Users/User/Documents/esterman.GUI")
           #     .libPaths("C:/Packages")          
                #####################################################################################                    
                ################ To run code: Press CTRL+A then CTRL +R #############################
                #####################################################################################                    
                
                work<-getwd() ### To check for changes to directory
                
                if (!require(tcltk2)) install.packages('tcltk2') #### install GUI if needed
                library( tcltk2 )  ##### load GUI package
                ######## load required packages
                if (!require(OPI)) install.packages('OPI') #### install package if needed
                library( OPI )                             ##### load package
               
                if (!require(ggplot2)) install.packages('ggplot2') #### install package if needed
                library( ggplot2 )                             ##### load package
                if (!require(labeling)) install.packages('labeling') #### install package if needed
                library( labeling )                             ##### load package
                if (!require(digest)) install.packages('digest') #### install package if needed
                library( digest )                             ##### load package
                if (!require(gridExtra)) install.packages('gridExtra') #### install package if needed
                library( gridExtra )                             ##### load package
                if (!require(ggforce)) install.packages('ggforce') #### install GUI if needed
                library( ggforce )  ##### load GUI package
                if (!require(tkrplot)) install.packages('tkrplot') #### install GUI if needed
                library( tkrplot )  ##### load GUI package
                tclRequire("ttk::theme::radiance")
                tk2theme("radiance")
                
                ###### Input window set up 
                input.win <- tktoplevel() ###Create a window called input.win
                        
                        ## ID
                        idname<- tclVar("")  ##### set default id
                        input.win$env$idName <-tk2entry(input.win, width = "25", textvariable = idname) #### set up text input
                        butID<- tkbutton(input.win, text = "Patient ID")
                        tkgrid(butID,input.win$env$idName,padx = 4, pady = c(0, 15),sticky = "e") ### add line 1 of window
                        tkgrid.configure(input.win$env$idName,columnspan = 2,sticky="w") ### add line 1 of window
                        
                        ## Visit
                        visitname<- tclVar("")  ##### set default visit
                        input.win$env$visitName <-tk2entry(input.win, width = "25", textvariable = visitname) #### set up text input
                        butVisit<- tkbutton(input.win, text = "Visit")
                        tkgrid(butVisit,input.win$env$visitName,padx = 4, pady = c(0, 15),sticky = "e") ### add line 1 of window
                        tkgrid.configure(input.win$env$visitName,columnspan = 2,sticky="w") ### add line 1 of window
                        
                        ##Port
                        portname <- tclVar("50001")  ##### set default port name
                        input.win$env$entName <-tk2entry(input.win, width = "25", textvariable = portname) #### set up text input
                        butPort<- tkbutton(input.win, text = "Port Number")
                        tkgrid(butPort,input.win$env$entName,padx = 2, pady = c(0, 15),sticky = "e") ### add line 1 of window
                        tkgrid.configure(input.win$env$entName,columnspan = 4,sticky="w") ### add line 1 of window
                        
                        ### Simulation / Octopus mode
                        input.win$env$rbSim <- tk2radiobutton(input.win) ### set up button 1 
                        input.win$env$rbOct <- tk2radiobutton(input.win) ### set up button 2
                        rbValueType <- tclVar(FALSE) ### set default button value
                        tkconfigure(input.win$env$rbSim, variable = rbValueType, value = "Simulation") ### set value if button 1 selected
                        tkconfigure(input.win$env$rbOct, variable = rbValueType, value = "Octopus900") ### set value if button 2 selected
                        butType <- tkbutton(input.win, text = "Type")
                        tkgrid(butType,tk2label(input.win, text = "Simulation"), input.win$env$rbSim,tk2label(input.win,text = "Octopus900"), input.win$env$rbOct,tk2label(input.win,text = "                          "),padx = 2, pady = c(0, 15),sticky = "e")
                        tkgrid.configure(input.win$env$rbSim,input.win$env$rbOct,sticky = "w") 
                        
                        ##### Graphing option
                        input.win$env$rbY <- tk2radiobutton(input.win) ### set up button 5
                        input.win$env$rbN <- tk2radiobutton(input.win) ### set up button 6
                        rbValueGraph<- tclVar(FALSE) ### set default button value
                        tkconfigure(input.win$env$rbY, variable = rbValueGraph, value = "yes") ### set value if button 1 selected
                        tkconfigure(input.win$env$rbN, variable = rbValueGraph, value = "no") ### set value if button 2 selected
                        butGraph <- tkbutton(input.win, text = "Live Graph")
                        tkgrid(butGraph,tk2label(input.win, text = "On"), input.win$env$rbY,tk2label(input.win,text = "Off"), input.win$env$rbN,padx = 4, pady = c(0, 15),sticky = "e")
                        tkgrid.configure(input.win$env$rbY,input.win$env$rbN,sticky="w") ## align grid
                        
                        ##### directory
                        pathA <- function() {  setwd(tk_choose.dir(getwd()));tclvalue(labelText) <- getwd()} ### set function for selecting work directory
                        input.win$env$dir <- ttkbutton(input.win,text = "Directory", command = pathA)
                        labelText <- tclVar(getwd())
                        input.win$env$direct<-tk2label(input.win,textvariable = labelText)
                        tkgrid(input.win$env$dir,input.win$env$direct , padx = 4, pady = c(0, 15),sticky = "w")
                        tkgrid.configure(input.win$env$direct, columnspan = 8)
                        
                        ## Cancel and continue
                        onCancel <- function() {tkdestroy(input.win) }  ## set function to close window when cancel is selected
                        input.win$env$butCanc <- ttkbutton(input.win, text = "Cancel",command = onCancel) ### set up ok button
                        
                        cont<- function(){ tclvalue(continue) <- 1;tkdestroy(input.win)}
                        continue <- tclVar(0)
                        input.win$env$butCont<-ttkbutton(input.win, text = "Continue",command=cont)
                        
                        tkgrid( tk2label(input.win, text = ""),input.win$env$butCanc,input.win$env$butCont, padx = 15, pady = c(0, 15)) 
                        tkgrid.configure(input.win$env$butCanc,  columnspan =2, sticky="w")
                        tkgrid.configure(input.win$env$butCont,columnspan =3, sticky="e")
                      
                        tkwait.window(input.win)  #### stop reading code until input.win window is closed
                        
                        ###Input Values  
                        ifelse(as.character(tclvalue(rbValueType))=="Simulation",simulation<-TRUE,simulation<-FALSE) # set simulation status
                        serverPort <- as.numeric(tclvalue(portname)) ### set server port
                        id <- gsub("[[:space:]]", ".",as.character(tclvalue(idname))) ### set id (removes spaces)
                        visit <- paste0("visit.",gsub("[[:space:]]", ".",as.character(tclvalue(visitname)))) ### set visit (removes spaces)
                        graph<-as.character(tclvalue(rbValueGraph)) ### set graph
                        contins<-as.character(tclvalue(continue))
                   
          #######################################################################################################
              
                  if(contins=="1"){   #### only run code if continue is selected
  
  
source( "helperFunctions.r" )   ##### load package
                    source( "tablefunction.r" )            
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

            df <- cbind.data.frame(c(90,90),c(90,90),c(T,F)) #to help set up the graph
            colnames(df)<-c("x","y","seen") #to help set up the graph

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
systime  <- gsub( ":", ".", format( systime, "%R" ) ) ### Time format changed
if( simulation ) {
  fnamelog <- paste0( "simulations/", id, "_",visit,"_", sysdate, "_", systime, "_log.csv" )
  fname    <- paste0( "simulations/", id, "_",visit,"_", sysdate, "_", systime, ".csv" )
  fname2 <- paste0( "simulations/", id, "_",visit,"_", sysdate, "_", systime, "_summary.csv" )
  
} else {
  fnamelog <- paste0( "results/", id, "_",visit,"_", sysdate, "_", systime, "_log.csv" )
  fname    <- paste0( "results/", id, "_",visit,"_", sysdate, "_", systime, ".csv" )
  fname2    <- paste0( "results/", id, "_",visit,"_", sysdate, "_", systime, "_summary.csv" )
}

                  # set up name for saving tiff of graph
                  if( !simulation ) {
                    fname.tiff    <- paste0( "results/", id, "_",visit,"_", sysdate, "_", systime, ".tiff" )
                  }


# start the presentations
trialseq$seen   <- NA
trialseq$rt     <- NA
trialseq$pupilx <- NA
trialseq$pupily <- NA
trialseq$err    <- ""

                fp=0 ### set false positives
                fp.fail=0 ### set false positives failed
                percent<-0  #### set % of test complete
                i=0
               
                fontTextLabel <- tkfont.create(family = "A", size = 14, weight = "bold") ## set font
                
                ## set a dataframe of response times
                df.resp<-NULL  #  for testing if resp is working DELETE
                
                ######################### PLOT OPTIONS
                hscale <- 1    # Horizontal scaling
                vscale <- 1    # Vertical scaling
                colour <- c("red", "black")
              
                wait<-tclVar(0) ##### set up for pausing
                continue2<-tclVar(1) ### set up for continue after graphing
                stopped<- tclVar("N") ### set up for cancelling during run
                
             if(graph=="yes"){

               #### set live graph
                plotTk <- function() {
                                      par(pin = c(3.2, 3.2))
                                      pch.list <- rep(0, length(df$seen))
                                      pch.list[df$seen==F] <- 15
                                      pch.list[df$seen==T] <- 17
                                      col.list <- rep(0, length(df$seen))
                                      col.list[df$seen==F] <-"red"
                                      col.list[df$seen==T]<-"darkgreen"
                                  
                                      
                                      plot(df$x,df$y, pch=c(pch.list),col=c(col.list),xlim=c(-80, 80), ylim=c(-60, 60), yaxt="n",xaxt="n",	xlab="", ylab="") 
                                      points(df$x[i+2],df$y[i+2], pch=9,cex=2,col="black")
                                      points(df$x[i+2],df$y[i+2], pch="x",cex=1.3,col="black")
                                      axis(2, at = seq(-60, 60, by = 10))
                                      axis(1, at = seq(-80, 80, by = 10))
                                        
                                    }
                
                output.win <- tktoplevel() # open new window
                tktitle(output.win) <- "Run Log"
                
                # id and visit
                output.win$env$text1<-tk2label(output.win, text = paste0("Patient ID: ",as.character(tclvalue(idname))),font = fontTextLabel)
                output.win$env$text2<-tk2label(output.win, text = paste0("Visit: ",as.character(tclvalue(visitname))),font = fontTextLabel)
                tkgrid(output.win$env$text1, padx = 4, pady = c(0, 15),columnspan=4)
                tkgrid(output.win$env$text2, padx = 4, pady = c(0, 15),columnspan = 4)
               
                #### Progress
                output.win$env$text1log<-tk2label(output.win, text = paste0(i,"/",nrow(trialseq)," (",percent,"%)"))
                output.win$env$text2log<-tk2label(output.win, text = paste0(fp.fail,"/",fp," (",fp.fail/fp*100,"%)"))
                output.win$env$text3log<-tk2label(output.win, text = 0)
                tkgrid(tk2label(output.win, text = "  "),tk2label(output.win, text = "Presentation:"),output.win$env$text1log,sticky="e")
                tkgrid(tk2label(output.win, text = "  "),tk2label(output.win, text = "False Positives:"),output.win$env$text2log,sticky="e")
                tkgrid(tk2label(output.win, text = "  "),tk2label(output.win, text = "Response Time:"),output.win$env$text3log,sticky="e")
                
                ###Plot
                output.win$env$plot <- tkrplot(output.win, fun = plotTk)
                tkgrid(output.win$env$plot,columnspan = 5,rowspan=4,sticky="e")
                
                ### Space
                tkgrid(tk2label(output.win, text = "       ")) # Blank line                
                
                ### Pause and resume
                pause<- function() {tclvalue(wait)<-1 }
                output.win$env$butPause <- ttkbutton(output.win, text = "Pause",command = pause) ### set up pause button
                continue2<-tclVar(0)
                cont3<- function(){ tkdestroy(win2);tclvalue(wait)<-0}
                output.win$env$butResume <- ttkbutton(output.win, text = "Resume",command = cont3)
                tkgrid(tk2label(output.win, text = "  "),output.win$env$butPause, output.win$env$butResume,tk2label(output.win, text = ""), padx = 4, pady = c(0, 15))
                ### Cancel
                onOK2 <- function() {tkdestroy(output.win);tclvalue(stopped)<-"Y";stop();tkdestroy(win) }  ## set function to close window when ok is selected
                output.win$env$butCanc2 <- ttkbutton(output.win, text = "Cancel",command = onOK2) ### set up ok button
                tkgrid(tk2label(output.win, text = "  "),output.win$env$butCanc2 , padx = 4, pady = c(0, 15))
                ### Continue (active after the loop is run)
                cont2<- function(){ tclvalue(continue2) <- 1;tkdestroy(output.win)}
                output.win$env$butCont2<-ttkbutton(output.win, text = "Continue",command=cont2) 
                
             }

                if(!simulation){res <- tkmessageBox(title = "",message = "Please realign eye", icon = "info", type = "ok")}
                
########################################### beginning of for loop ########################

for( i in 1:nrow( trialseq ) ) {
                
                percent<-round(i/nrow(trialseq)*100)
                
                if(tclvalue(wait)==1){  
                win2 <- tktoplevel()
                win2$env$labelcont <- tk2label(win2, text = "System Paused")
                tkgrid(tk2label(win2, text = "    "),  win2$env$labelcont, padx = 4, pady = c(0, 15))
                tkwait.window(win2)
                }
                
                if(trialseq$fptrial[i]){fp<-fp+1}
                fp.fail<-sum(na.omit(trialseq$fptrial==T & trialseq$seen==T))
                
                if(tclvalue(stopped)=="Y"){stop()}
                if(graph=="yes"){       
                 
  
  # build stimulus for this presentation and inform next stimulus
  stim <- makeStim( trialseq[i,], maxInt = maxInt )
  if( i < nrow( trialseq ) ) stimNext <- makeStim( trialseq[i+1,], maxInt = maxInt )
  if( simulation ) {
    resp <- opiPresent( stim = stim )
  } else {
    resp <- opiPresent( stim = stim, stimNext = stimNext )
  }
  
                df.resp<-rbind.data.frame(df.resp,c(i,resp$time),stringsAsFactors=FALSE)
                tkconfigure(output.win$env$text3log, text = resp$time)}         
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
                    df<-rbind(df,trialseq[i,c("x","y","seen")])
                    if(graph=="yes"){ 
                  #    flush.console()
                  #    print(ggplot()+
                  #            geom_point(data=df, alpha=0.5, aes(x=x,y=y,size=seen,group=seen,shape=seen,colour=seen))+
                  #            scale_size_manual(values=c(4,2))+
                  #            scale_colour_manual(values=c("red","white"))+ 
                  #           theme_dark() +  
                  #            geom_point(data=df, color="black",size=6, aes(x=x[i+2],y=y[i+2],group=seen))+  
                  #            geom_point(data=df, color="gold",size=4, aes(x=x[i+2],y=y[i+2]))+
                  #            geom_point(data=df, color="black",size=5,shape=10, aes(x=x[i+2],y=y[i+2]))+  
                  #            xlim(-80, 80) + ylim(-60, 60))
                      
                    
             #     Sys.sleep(2)
                      tkconfigure(output.win$env$text2log, text = paste0(fp.fail,"/",fp," (",round(fp.fail/fp*100),"%)"))
                        tkrreplot(output.win$env$plot)  
                        tkconfigure(output.win$env$text1log, text = paste0(i,"/",nrow(trialseq)," (",percent,"%)"))
                        
                    }
                    }
}

####################################### End of for Loop ###########################################
                if(graph=="yes"){        
                  tkgrid.remove(output.win$env$butCanc2)
                  output.win$env$butCanc2 <- ttkbutton(output.win, text = "Cancel",command = onOK2) ### Need to repeat this to allow the proper format.
                  tkgrid(tk2label(output.win, text = "  "),output.win$env$butCanc2, output.win$env$butCont2, padx = 4, pady = c(0, 15))
                  tkgrid.remove( output.win$env$butResume, output.win$env$butPause)
                tkwait.window(output.win)  #### stop reading code until output window is closed
                  }             
             if(tclvalue(continue2)==1){     
             # if maximum luminance is 4000 asb, then change reference back to 10000
if( !zero_dB_is_10000_asb ) trialseq$stimLevel <- cdTodb( dbTocd( trialseq$stimLevel, 4000 / pi ) )

# remove all locations not seen
idx <- which( !is.na( trialseq$seen ) )
if( length( idx ) > 0 ) trialseq <- trialseq[idx,]
# result summary
trials      <- trials[,c(1:2)]
trials$seen <- "none"
trials$rt   <- ""
for( j in 1:nrow( trials ) ) {
  idx <- which( trialseq$x == trials$x[j] & trialseq$y == trials$y[j] & trialseq$fptrial == FALSE )
  if( length( idx ) == 1 ) {
    trials$seen[j] <- "first"
    trials$rt[j]   <- trialseq$rt[idx[1]]
  } else if( trialseq$seen[idx[2]] ) {
    trials$seen[j] <- "second"
    trials$rt[j]   <- trialseq$rt[idx[2]]
  }
}

            trials2<-NULL
            trials2$failed.falsepositives<-""
            trials2$failed.falsepositives[1]<-sum(na.omit(trialseq$fptrial==T & trialseq$seen==T))
            trials2$seen.1st.time<-""
            trials2$seen.1st.time[1]<-length(which(trials$seen=="first"))
            trials2$seen.2nd.time<-""
            trials2$seen.2nd.time[1]<-length(which(trials$seen=="second"))
            trials2$not.seen<-""
            trials2$not.seen[1]<-length(which(trials$seen=="none"))
            trials2<-as.data.frame(trials2)
  
                      
# save results
write.csv( trials, file = fname, row.names = FALSE )
write.csv( trialseq, file = fnamelog, row.names = FALSE )
write.csv( trials2, file = fname2, row.names = FALSE )

              r = c(20,60)
              slopes<-tan(seq(-75,75,by=15)*pi/180)
              
              # to save a tiff of the graph   
              if(!simulation){
                ggplot()+
                  ggtitle(paste(tclvalue(idname),tclvalue(visitname),sysdate))+
                  scale_x_continuous( limits=c(-80, 80),breaks=seq(-80,80,20),name ="") + 
                  scale_y_continuous( limits=c(-60,60),breaks=seq(-60,60,20),name ="")+
                  scale_size_manual(values=c(4,2))+
                  scale_colour_manual(values=c("red","green4"))+ 
                  theme_light()  + 
                  geom_abline(intercept = rep(0,length(slopes)), slope = slopes,color = "grey") +
                  geom_vline(xintercept = 0,color = "grey") + 
                  geom_circle(aes(x0=0, y0=0, r=60),color = "grey") +  
                  geom_circle(aes(x0=0, y0=0, r=20),fill="white",color = "grey")+
                  geom_point(data=df[3:nrow(df),], alpha=0.5, aes(x=x,y=y,size=seen,group=seen,shape=seen,colour=seen)) #+  xlim(-80, 80) + ylim(-60, 60)
                ggsave(fname.tiff,width = 9,height = 6) 
              }
            
              #### Change work directory prompt  
              if(work != getwd())      paste("Consider changing line 1 with: ",getwd()  )
              
}
