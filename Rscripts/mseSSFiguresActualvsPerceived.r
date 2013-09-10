# mseSSFiguresActualVsPerceived.r
#
# Plot simulated vs estimated values
#
# Authors: Chris Grandin, Ian Taylor

source("mseSSGlobals.r")

plotActualvsPerceived <- function(scenarios,
                                  figPath = .FIGS_DIR,
                                  years,
                                  maxval  = 1.2,
                                  digits  = 0,
                                  main    = "",
                                  empty   = FALSE,
                                  res     = .RESOLUTION,
                                  width   = .WIDTH,
                                  height  = .HEIGHT,
                                  units   = .UNITS,
                                  png     = .PNG,
                                  verbose = .VERBOSE){
  # Simulated Vs. estimated plots of depletion for MSE
  # One plot per scenario (does not include noFishing and perfectInformation scenarios as they
  # have no assessment data)

  # Identify the scenario numbers which correspond to No Fishing and Perfect Information cases
  for(scenario in 1:length(scenarios)){
    if(scenarios[[scenario]]$zeroCatch){
      noFishingScenario <- scenario
    }
    if(scenarios[[scenario]]$perfectInfoCase){
      perfectInformationScenario <- scenario
    }
  }

  # define some bins within which to tally data
  categories <- data.frame(bin  = c(1  , 2  , 3  , 4  , 5  , 6  , 7  , 8  , 9  ),
                           xmin = c(0  , 0.1, 0.4, 0  , 0.1, 0.4, 0  , 0.1, 0.4),
                           xmax = c(0.1, 0.4, 10 , 0.1, 0.4, 10 , 0.1, 0.4, 10 ),
                           ymin = c(0  , 0  , 0  , 0.1, 0.1, 0.1, 0.4, 0.4, 0.4),
                           ymax = c(0.1, 0.1, 0.1, 0.4, 0.4, 0.4, 10 , 10 , 10 ))

  for(scenario in 1:length(scenarios)){
    if(scenario != noFishingScenario & scenario != perfectInformationScenario){
      # Get the minimum and maximum years from the simulated depletion
      minYear     <- scenarios[[scenario]]$firstAssessYear
      maxYear     <- scenarios[[scenario]]$lastAssessYear
      yearLims    <- scenarios[[scenario]]$assessTSYears
      depl        <- scenarios[[scenario]]$depl

      assessDepl  <- scenarios[[scenario]]$assessDeplMedianBySim

      x <- as.numeric(as.matrix(depl[,yearLims %in% years]))
      y <- as.numeric(as.matrix(assessDepl[,yearLims %in% years]))
      yearstring <- paste(min(years),"-",max(years),sep="")

      filename <- file.path(figPath,paste(Sys.Date(),"_",names(scenarios)[scenario],"_Actual_vs_Perceived_Depletion_In_",yearstring,".png"))
      if(png){
        png(filename,res=res,width=width,height=height,units=units)
      }

      plot(x,y,
           xlab=paste("Actual depletion in",yearstring),
           ylab=paste("Perceived depletion in",yearstring),
           xlim=c(0,maxval),ylim=c(0,maxval),main=main,xaxs="i",yaxs="i",
           axes=FALSE,pch=16,col=rgb(0.3,0.3,0.3,.2),type=ifelse(empty,"n","p"))
      axis(1,at=c(0,0.1,0.4,1.0))
      axis(2,at=c(0,0.1,0.4,1.0),las=1)
      box()
      abline(0,1,col=2,lty=1)
      abline(v=c(.1,.4),h=c(.1,.4),lty=3)
      for(ibin in 1:9){
        n <- sum(x >= categories$xmin[ibin] &
                 x <  categories$xmax[ibin] &
                 y >= categories$ymin[ibin] &
                 y <  categories$ymax[ibin])
        frac <- n/length(x)
        xmean <- (categories$xmin[ibin] + min(1,categories$xmax[ibin]))/2
        ymean <- (categories$ymin[ibin] + min(1,categories$ymax[ibin]))/2
        txt <- paste(round(100*frac,digits),'%',sep='')
        if(empty) txt <- "X%"
        text(xmean,ymean,txt,col=4,cex=1.6)
      }
      if(png){
        dev.off()
      }
    }
  }
}
