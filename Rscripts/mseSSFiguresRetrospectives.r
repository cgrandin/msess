# mseSSFiguresRetrospectives.r
#
# Retrospective assessment figures by simulation
#
# Authors: Chris Grandin

source("mseSSGlobals.r")

plotRetrospectives <- function(scenario,
                               retroSim,
                               figPath = .FIGS_DIR,
                               ylim    = c(0,1.7),
                               type    = "dep",
                               res     = .RESOLUTION,
                               width   = .WIDTH,
                               height  = .HEIGHT,
                               units   = .UNITS,
                               png     = .PNG,
                               verbose = .VERBOSE){
  # Plots retrospectives for assessments within a given simulation (retroSim)
  #  for a given scenario (scenario). Plots the single simulated value overlayed.
  # If the scenario has its runAssessments variable set to F, nothing will be done.
  # "type" must be one of (N is scenario number):
  # - "dep"        = simulated depletion (scenarios[[N]]$depl)

  # Time series limits (includes assessed years)
  firstTSYear     <- scenario$firstTSYear
  firstAssessYear <- scenario$firstAssessYear
  lastTSYear      <- scenario$lastAssessYear
  assessYears     <- firstAssessYear:lastTSYear
  tsYearLims      <- scenario$assessTSYears
  scenarioName    <- scenario$scenarioName
  scenarioPretty  <- scenario$prettyName
  simNum          <- scenario$simNums[retroSim]
  simLabel        <- paste(.SIM_FILE_PREFIX,simNum,sep="") # matches labels of matrix rows

  if(scenario$runAssessments){
    numSims <- length(scenario$simNums)
    if(retroSim < 1 || retroSim > numSims){
      cat("plotRetrospectives: Error - Scenario ",scenarioName,", retroSim must be between 1 and ",numSims,".\n\n",sep="")
    }else{
      # Setup the plot and output filenames for the given type
      if(type=="dep"){
        filename <- file.path(figPath,paste(Sys.Date(),"_Depletion_Retrospectives_",scenarioName,"_",scenario$simNums[retroSim],".png",sep=""))
        if(png){
          png(filename,res=res,width=width,height=height,units=units)
        }
        # dep is a ragged matrix with assessed years as rows and full time series years as columns
        # Look at scenario$simNums, it is in the same order as the lists of simulations, i.e. scenario$assessDepl
        dep <- scenario$assessDepl[[retroSim]]

        # depSim is the vector of simulated time series, to be overlaid on the assessment output
        rowMatch <- rownames(scenario$depl) == simLabel
        depSim <- scenario$depl[rowMatch,]

        # Plot the simulated time series first
        par(mar=c(5,4,3,1))
        #plot(0,type='n',xlim=c(min(tsYearLims),max(tsYearLims)),ylim=ylim,xaxs='i',yaxs='i',xlab="",ylab="",axes=TRUE)
        plot(tsYearLims,depSim,type='l',xlim=c(min(tsYearLims),max(tsYearLims)),ylim=ylim,xaxs='i',yaxs='i',xlab="",ylab="",col=3,lwd=3,axes=TRUE)
        legendList <- "Simulated"
        legendLine <- 1
        legendCols <- 3
        mtext(side=1,line=2,"Year")
        mtext(side=2,line=2,"Depletion")

        for(assessYear in 1:nrow(dep)){
          # Plot each line and build legend
          lty <- (assessYear %/% 8) + 1
          legendCols <- c(legendCols,assessYear)
          legendLine <- c(legendLine,lty)
          lines(tsYearLims,dep[assessYear,],col=assessYear,lty=lty,lwd=2)
        }
      }
      if(type=="recr"){
        filename <- file.path(figPath,paste(Sys.Date(),"_Recruitment_Retrospectives_",scenarioName,"_",scenario$simNums[retroSim],".png",sep=""))
        if(png){
          png(filename,res=res,width=width,height=height,units=units)
        }
        # dep is a ragged matrix with assessed years as rows and full time series years as columns
        # Look at scenario$simNums, it is in the same order as the lists of simulations, i.e. scenario$assessDepl
        recr <- scenario$assessRecr[[retroSim]]

        # depSim is the vector of simulated time series, to be overlaid on the assessment output
        rowMatch <- rownames(scenario$recruitment) == simLabel
        recrSim  <- scenario$recruitment[rowMatch,]

        # Plot the simulated time series first
        par(mar=c(5,4,3,1))
        #plot(0,type='n',xlim=c(min(tsYearLims),max(tsYearLims)),ylim=ylim,xaxs='i',yaxs='i',xlab="",ylab="",axes=TRUE)
        plot(tsYearLims,recrSim,type='l',xlim=c(min(tsYearLims),max(tsYearLims)),ylim=ylim,xaxs='i',yaxs='i',xlab="",ylab="",col=3,lwd=3,axes=TRUE)
        legendList <- "Simulated"
        legendLine <- 1
        legendCols <- 3
        mtext(side=1,line=2,"Year")
        mtext(side=2,line=2,"Recruitment")

        for(assessYear in 1:nrow(recr)){
          # Plot each line and build legend
          lty <- (assessYear %/% 8) + 1
          legendCols <- c(legendCols,assessYear)
          legendLine <- c(legendLine,lty)
          lines(tsYearLims,recr[assessYear,],col=assessYear,lty=lty,lwd=2)
        }
      }
      legendList <- c(legendList,assessYears)
      legend("topright",legend=legendList,col=legendCols,lty=legendLine,lwd=2,cex=0.7)
      mainTitle <- paste(scenarioPretty, " - sim",scenario$simNums[[retroSim]],sep="")
      title(mainTitle)
      box()
      if(png){
        dev.off()
      }
    }
  }
}
