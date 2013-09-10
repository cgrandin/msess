# mseSSFiguresDensity.r
#
# All density figures for mseSS
#
# Authors: Ian Taylor, Chris Grandin

source("mseSSGlobals.r")

plotDensity <- function(scenarios,
                        figPath = .FIGS_DIR,
                        years,
                        type    = "dep",
                        addMean = FALSE,
                        showNoFishing = FALSE,
                        xMax    = 1,
                        yMax    = 4,
                        res     = .RESOLUTION,
                        width   = .WIDTH,
                        height  = .HEIGHT,
                        units   = .UNITS,
                        png     = .PNG,
                        verbose = .VERBOSE,
                        ...){

  # Density plots for mseSS
  # "type" must be one of (N is scenario number):
  # - "dep"     = simulated depletion (scenarios[[N]]$depl)
  # - "cat"     = simulated catch (scenarios[[N]]$catches)
  # - "aav"     = Average annual variability in catch (scenarios[[N]]$catches - scenarios[[N]]$catches lag 1 year)
  # - "aaveven" = Average annual variability in catch even years only (scenarios[[N]]$catches - scenarios[[N]]$catches lag 2 years)
  # - "aavodd"  = Average annual variability in catch odd years only (scenarios[[N]]$catches - scenarios[[N]]$catches lag 2 years)
  #
  # years is the year range for the plot, i.e. 2021:2030

  # Build the scenario lists of subsetted-by-years data
  deplList       <- NULL
  sprRatioList   <- NULL
  catchList      <- NULL
  aavList        <- NULL
  aavOddList     <- NULL
  aavEvenList    <- NULL
  assessSSBList  <- NULL
  assessDeplList <- NULL
  for(scenario in 1:length(scenarios)){
    deplList[[scenario]]       <- subsetData(scenarios,scenario,years,"depl",verbose=verbose)
    sprRatioList[[scenario]]   <- subsetData(scenarios,scenario,years,"SPRratio",verbose=verbose)

    realCatch                  <- subsetData(scenarios,scenario,years,"catchesReal",verbose=verbose)
    catchList[[scenario]]      <- realCatch
    realCatch1                 <- subsetData(scenarios,scenario,years-1,"catchesReal",verbose=verbose) # for the AAV calculation
    aavList[[scenario]]        <- apply(abs(realCatch-realCatch1),1,sum) / apply(realCatch,1,sum)

    is.even                    <- function(x) x %% 2 == 0
    is.odd                     <- function(x) x %% 2 != 0
    oddYears                   <- years[is.odd(years)]
    evenYears                  <- years[is.even(years)]
    oddCatch                   <- subsetData(scenarios,scenario,oddYears,"catchesReal",verbose=verbose)
    oddCatch1                  <- subsetData(scenarios,scenario,oddYears-1,"catchesReal",verbose=verbose)
    aavOddList[[scenario]]     <- apply(abs(oddCatch-oddCatch1),1,sum) / apply(oddCatch,1,sum)
    evenCatch                  <- subsetData(scenarios,scenario,evenYears,"catchesReal",verbose=verbose)
    evenCatch1                 <- subsetData(scenarios,scenario,evenYears-1,"catchesReal",verbose=verbose)
    aavEvenList[[scenario]]    <- apply(abs(evenCatch-evenCatch1),1,sum) / apply(evenCatch,1,sum)
  }
  # Setup the plot and output filenames for the given type
  if(type=="dep"){
    if(addMean){
      filename <- file.path(figPath,paste(Sys.Date(),"_Density_Depletion_With_Mean_",years[1],"_To_",years[length(years)],".png",sep=""))
    }else{
      filename <- file.path(figPath,paste(Sys.Date(),"_Density_Depletion_",years[1],"_To_",years[length(years)],".png",sep=""))
    }
    if(png){
      png(filename,res=res,width=width,height=height,units=units)
    }
    xlabel <- paste("Depletion from ",min(years)," - ",max(years),sep="")
    makePolys(scenarios,deplList,xMax=xMax,yMax=yMax,showNoFishing=showNoFishing,addMean=addMean,xlab="Depletion",verbose=verbose,...)
    abline(v=c(0.1,0.4),lty=3,col='grey40')
    axis(1)
  }
  if(type=="cat"){
    filename <- file.path(figPath,paste(Sys.Date(),"_Density_Catch_",years[1],"_To_",years[length(years)],".png",sep=""))
    if(png){
      png(filename,res=res,width=width,height=height,units=units)
    }
    xlabel <- paste("Average catch ('1000 mt) from ",min(years)," - ",max(years),sep="")
    makePolys(scenarios,catchList,xMax=xMax,yMax=yMax,showNoFishing=showNoFishing,addMean=addMean,xlab=xlabel,verbose=verbose,...)
    axis(1)
  }
  if(type=="aav"){
    filename <- file.path(figPath,paste(Sys.Date(),"_Density_AAV_",years[1],"_To_",years[length(years)],".png",sep=""))
    if(png){
      png(filename,res=res,width=width,height=height,units=units)
    }
    xlabel <- paste("Average Annual Variability in catch from ",min(years)," - ",max(years),sep="")
    makePolys(scenarios,aavList,xMax=xMax,yMax=yMax,showNoFishing=showNoFishing,addMean=addMean,xlab=xlabel,verbose=verbose,...)
    axis(1,at=seq(0,1.4,.2),lab=paste(seq(0,140,20),'%',sep=''))
  }
  if(type=="aaveven"){
    filename <- file.path(figPath,paste(Sys.Date(),"_Density_AAV_Even_Years_",years[1],"_To_",years[length(years)],".png",sep=""))
    if(png){
      png(filename,res=res,width=width,height=height,units=units)
    }
    xlabel <- paste("Average Annual Variability in catch for even years from ",min(years)," - ",max(years),sep="")
    makePolys(scenarios,aavEvenList,xMax=xMax,yMax=yMax,showNoFishing=showNoFishing,addMean=addMean,xlab=xlabel,verbose=verbose,...)
    axis(1,at=seq(0,1.4,.2),lab=paste(seq(0,140,20),'%',sep=''))
  }
  if(type=="aavodd"){
    filename <- file.path(figPath,paste(Sys.Date(),"_Density_AAV_Odd_Years_",years[1],"_To_",years[length(years)],".png",sep=""))
    if(png){
      png(filename,res=res,width=width,height=height,units=units)
    }
    xlabel <- paste("Average Annual Variability in catch for odd years from ",min(years)," - ",max(years),sep="")
    makePolys(scenarios,aavOddList,xMax=xMax,yMax=yMax,showNoFishing=showNoFishing,addMean=addMean,xlab=xlabel,verbose=verbose,...)
    axis(1,at=seq(0,1.4,.2),lab=paste(seq(0,140,20),'%',sep=''))
  }

  if(png){
    dev.off()
  }
}

makePolys <- function(scenarios,
                      quantlist,
                      xMax,
                      yMax,
                      borderAlpha=0.6,
                      shadeAlpha=0.1,
                      showNoFishing=FALSE,
                      addMean=FALSE,
                      verbose=FALSE,
                      ...){

  par(mar=c(5,4,1,1))
  plot(0,type='n',xlim=c(0,xMax),ylim=c(0,yMax),xaxs='i',yaxs='i',ylab='',axes=FALSE,...)
  mtext(side=2,line=2,"Density")

  legendList       <- NULL
  legendBorderCols <- NULL
  legendShadeCols  <- NULL

  for(scenario in 1:length(quantlist)){
    if(showNoFishing | !scenarios[[scenario]]$zeroCatch){
      col              <- getRGB(scenarios[[scenario]]$plotColor)
      borderCol        <- rgb(col[1],col[2],col[3],borderAlpha)
      shadeCol         <- rgb(col[1],col[2],col[3],shadeAlpha)
      legendBorderCols <- c(legendBorderCols,borderCol)
      legendShadeCols  <- c(legendShadeCols,shadeCol)
      legendList       <- c(legendList,scenarios[[scenario]]$prettyName)

      vals     <- as.numeric(as.matrix(quantlist[[scenario]]))
      z        <- density(vals,from=0,cut=0,n=10000)
      median.x <- median(vals)
      mean.x   <- mean(vals)
      # find y-value associated with closest matching x-value
      # "min" was added for a rare case where two values were equally close
      median.y <- min(z$y[abs(z$x-median.x)==min(abs(z$x-median.x))])
      mean.y   <- min(z$y[abs(z$x-mean.x)==min(abs(z$x-mean.x))])
      z$x      <- z$x[c(1,1:length(z$x),length(z$x))]
      z$y      <- c(0,z$y,0)           #just to make sure that a good looking polygon is created
      polygon(z,col=shadeCol,border=borderCol,lwd=2)
      lines(rep(median.x,2),c(0,median.y),col=borderCol,lwd=2)
      if(addMean){
        lines(rep(mean.x,2),c(0,mean.y),col=borderCol,lwd=2,lty=2)
      }
    }
  }
  box()
  legend("topright",legendList,col=legendBorderCols,fill=legendShadeCols,bty="n")
}

subsetData <- function(scenarios, scenario, years, dataSetName, removeCols=NULL, verbose=FALSE){
  # return the data subsetted by the years and with columns defined by removeCols removed.
  # return NULL if the subset fails, i.e. there are no data for this scenario/dataSetName combo.
  tryCatch({
    data <- scenarios[[scenario]][[dataSetName]]
    if(!is.null(removeCols)){
      data <- data[,-removeCols]
    }
    yearHeader     <- colnames(data)
    splitHeader    <- strsplit(yearHeader,"_")
    splitChoice    <- length(splitHeader[[1]]) # assumes at least one header exists
    minYear        <- as.numeric(splitHeader[[1]][splitChoice])
    maxYear        <- as.numeric(splitHeader[[length(splitHeader)]][splitChoice])

    if(dataSetName=="catchesReal" | dataSetName=="SSB"){
      data         <- data[,minYear:maxYear %in% years]/1000.0
    }else{
      data         <- data[,minYear:maxYear %in% years]
    }
    data           <- as.data.frame(data)  # in case there is only one year, make it a column
    colnames(data) <- years # ensure all datasets have column names years only, no funky names!
    return(data)
  },error=function(err){
    if(verbose){
      cat("subsetData: Problem trying to subset ",dataSetName," for ",scenarios[[scenario]]$prettyName,
          " scenario, for years ",min(years)," - ",max(years),"\n",sep="")
    }
    return(NULL)
  })
}



