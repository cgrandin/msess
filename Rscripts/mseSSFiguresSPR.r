# mseSSFiguresSPR.r
#
# All SPR figures for mseSS
#
# Authors: Ian Taylor, Chris Grandin

source("mseSSGlobals.r")

plotSPR <- function(scenarios,
                    figPath = .FIGS_DIR,
                    years,
                    type    = "dep",
                    yMax    = 1.5,
                    xlim    = c(0.5,0.7),
                    res     = .RESOLUTION,
                    width   = .WIDTH,
                    height  = .HEIGHT,
                    units   = .UNITS,
                    png     = .PNG,
                    verbose = .VERBOSE,
                    main    = NA,
                    scale   = 1,
                    ylab    = "Depletion",
                    legend  = TRUE,
                    legendloc = "topleft",
                    ...){

  # SPR plots for MSE
  # "type" must be one of (N is scenario number):
  # - "dep"     = simulated depletion (scenarios[[N]]$depl
  # - "spr"     = simulated SPR ratio (scenarios[[N]]$SPRratio)
  # - "cat"     = simulated catch (scenarios[[N]]$catchesReal)
  # - "catcv"   =
  # - "aav"     =
  # - "depcv"   =
  # - "fsprtgt" =
  #
  # years is the year range for the plot, i.e. 2021:2030

  perfectInfoScenario <- getPerfectInfoScenario(scenarios)
  sprList <- scenarios[[perfectInfoScenario]]$spr
  sprs    <- as.numeric(gsub(.SPR_DIR_PREFIX,"",names(sprList)))

  # Setup the output filenames for the given type
  if(type=="dep"){
    if(length(years)==1){
      result <- lapply(sprList, getVec, yr=years, name="depl")
      filename <- file.path(figPath,paste(Sys.Date(),"_SPRplot_Depletion_In_",years,".png",sep=""))
    }else{
      result <- lapply(sprList, getNyrMean, yrs=years, name="depl")
      filename <- file.path(figPath,paste(Sys.Date(),"_SPRplot_Average_Depletion_From_",years[1]," - ",years[length(years)],".png",sep=""))
    }
  }
  if(type=="cat"){
    if(length(years)==1){
      result <- lapply(sprList, getVec, yr=years, name="catchesReal")
      filename <- file.path(figPath,paste(Sys.Date(),"_SPRplot_Catch_In_",years,".png",sep=""))
    }else{
      result <- lapply(sprList, getNyrMean, yrs=years, name="catchesReal")
      filename <- file.path(figPath,paste(Sys.Date(),"_SPRplot_Average_Catch_From_",years[1]," - ",years[length(years)],".png",sep=""))
    }
  }
  if(type=="catcv"){
    result <- lapply(sprList, getNyrCV, yrs=years, name="catchesReal")
    filename <- file.path(figPath,paste(Sys.Date(),"_SPRplot_Average_Catch_CV_From_",years[1]," - ",years[length(years)],".png",sep=""))
  }
  if(type=="aav"){
    result <- lapply(sprList, getAAV, yrs=years, name="catchesReal")
    filename <- file.path(figPath,paste(Sys.Date(),"_SPRplot_Average_AAV_From_",years[1]," - ",years[length(years)],".png",sep=""))
  }
  if(type=="depcv"){
    result <- lapply(sprList, getNyrCV, yrs=years, name="depl")
    filename <- file.path(figPath,paste(Sys.Date(),"_SPRplot_Average_Depletion_CV_From_",years[1]," - ",years[length(years)],".png",sep=""))
  }
  if(type=="fsprtgt"){
    result <- lapply(sprList,function(x){return(x$F_SPRtgt)})
    filename <- file.path(figPath,paste(Sys.Date(),"_SPRplot_Exploitation_Rate_Associated_With_Target_Harvest_Rate_From_",years[1]," - ",years[length(years)],".png",sep=""))
  }
  if(png){
    png(filename, width=width, height=height, units=units, res=res)
  }

  sprsMinusOne <- 1 - sprs
  if(is.na(main)){
    par(mar=c(5,4,1,1)+.1) # chop off top of figure if no main is used
  }
  plot(0,main=main,xlab="Target harvest rate",ylab=ylab,
       xlim=xlim,ylim=c(0,yMax),axes=FALSE,
       las=1,type="n",yaxs="i")
  for(i in 1:length(sprsMinusOne)){
    yvals <- result[[i]]/scale
    in90  <- yvals < quantile(yvals,.95) & yvals > quantile(yvals,.05)
    col   <- ifelse(in90,rgb(0,0,0,.1),rgb(1,0,0,.1))
    points(rnorm(length(yvals), sprsMinusOne[i], 0.001), yvals,col=col,pch=16)
  }

  if(yMax<.5) abline(h=seq(0,yMax,0.1),lty=3,col='grey')
  if(yMax>.5 & yMax<2) abline(h=seq(0,yMax,0.2),lty=3,col='grey')
  if(yMax>200) abline(h=seq(0,yMax,200),lty=3,col='grey')
  lines(sprsMinusOne ,unlist(lapply(result,function(x){quantile(x,0.05)}))/scale,col='red')
  lines(sprsMinusOne ,unlist(lapply(result,function(x){quantile(x,0.95)}))/scale,col='red')
  lines(sprsMinusOne ,unlist(lapply(result,mean))/scale,col='green3')
  lines(sprsMinusOne ,unlist(lapply(result,median))/scale,col='blue')
  points(sprsMinusOne,unlist(lapply(result,mean))/scale,pch=23,bg='green3',col='white',cex=1.5)
  points(sprsMinusOne,unlist(lapply(result,median))/scale,pch=21,bg='blue',col='white',cex=1.5)
  fvec <- rev(sprs)
  # Make percentage labels
  fvec <- 100*fvec
  fvec <- paste(fvec,"%",sep="")

  # Tricky code here, makes the labels for the Fs on the x-axis
  # The .() operator must be used inside the subscript operator []
  # and the bquote function must be used to evaluate the expression
  # before adding it to the list.
  # The as.expression must be used to make it an expression class object
  labvec <- NULL
  for(spr in 1:length(sprs)){
    labvec <- c(labvec,bquote(paste(italic(F)[.(fvec[spr])])))
  }
  labvec <- as.expression(labvec)
  # End of labels code

  atvec <- rev(sprsMinusOne)
  axis(1,at=atvec,lab=labvec)
  axis(2,las=1)
  if(legend){
    legend(legendloc,
           legend=c("Mean","Median","Middle 90% of values","Outer 10% of values"),
                    pch=c(23,21,16,16),
                    pt.cex=c(1.5,1.5,1,1),
                    col=c("white","white",rgb(0,0,0,.2),rgb(1,0,0,.2)),
                    pt.bg=c("green3","blue",1,1),
                    bg=rgb(1,1,1,1))
  }
  box()

  if(png){
    dev.off()
  }
}

plotAAVDepvsCatch <- function(scenarios,
                              figPath=.FIGS_DIR,
                              years,
                              png=TRUE,
                              xlim=c(210,240),
                              res=300,
                              width=7,
                              height=7,
                              units="in",
                              depYlim=c(0,1),
                              aavYlim=c(0,0.4),
                              verbose=FALSE,
                              ...){
  # SPR plots for mseSS
  # Creates two plots with Median depletion vs Median average catch and
  # AAV vs Median average catch.
  #
  # years is the year range for the plot, i.e. 2021:2030

  sprList <- scenarios[[2]]$spr
  sprs    <- as.numeric(gsub(.SPR_DIR_PREFIX,"",names(sprList)))

  catch   <- lapply(sprList, getNyrMean, yr=years, name="catchesReal")
  dep     <- lapply(sprList, getNyrMean, yrs=years, name="depl")
  aav     <- lapply(sprList, getAAV, yrs=years, name="catchesReal")

  medianCatch     <- NULL
  medianDepletion <- NULL
  medianAAV       <- NULL
  for(spr in 1:length(catch)){
    medianCatch[spr]     <- median(catch[[spr]])/1000.0
    medianDepletion[spr] <- median(dep[[spr]])
    medianAAV[spr]       <- median(aav[[spr]])
  }

  filename <- file.path(figPath,paste(Sys.Date(),"_MedianDepletion_vs_MedianAverageCatch_From_",years[1],"_To_",years[length(years)],".png",sep=""))
  if(png){
    png(filename, width=width, height=height, units=units, res=res)
  }
    plot(medianCatch,
         medianDepletion,
         xlab="Median average catch (x1000 mt)",
         ylab="Median depletion",
         pch=18,
         cex=2,
         col="blue",
         xlim=xlim,
         ylim=depYlim)

  fvec <- rev(sprs)
  # Make percentage labels
  fvec <- 100*fvec
  fvec <- paste(fvec,"%",sep="")

  labvec <- NULL
  for(spr in 1:length(sprs)){
    labvec <- c(labvec,bquote(paste(italic(F)[.(fvec[spr])])))
  }
  labvec <- as.expression(rev(labvec))

  labvecLen <- length(labvec)

  ### REALLY BAD CODE HERE - NEED TO FIX THIS
  xOff <- rep(-1,labvecLen)
  yOff <- rep(-0.02,labvecLen)

  xOff[1] <- -0.75
  xOff[5] <- -0.75
  yOff[5] <- -0.04
  yOff[6] <- -0.03
  yOff[3] <- -yOff[3]
  yOff[4] <- -yOff[4]
  xOff[3] <- -xOff[3]
  ### END REALLY BAD CODE

  text(medianCatch+xOff,medianDepletion+yOff,labels=labvec,cex=0.7)

  box()
  if(png){
    dev.off()
  }

  filename <- file.path(figPath,paste(Sys.Date(),"_MedianAAV_vs_MedianAverageCatch_From_",years[1],"_To_",years[length(years)],".png",sep=""))
  if(png){
    png(filename, width=width, height=height, units=units, res=res)
  }
    plot(medianCatch,
         medianAAV,
         xlab="Median average catch (x1000 mt)",
         ylab="Median Annual Average Variability in Catch",
         pch=18,
         cex=2,
         col="blue",
         xlim=xlim,
         ylim=aavYlim)

  ### REALLY BAD CODE HERE - NEED TO FIX THIS
  xOff <- rep(-1,labvecLen)
  yOff <- rep(-0.01,labvecLen)

  yOff[1] <- -yOff[1]

  yOff[2] <- -yOff[2]

  xOff[3] <- -xOff[3]
  yOff[3] <- -yOff[3]

  yOff[4] <- -yOff[4]

  yOff[5] <- -yOff[5]

  yOff[6] <- -yOff[6]
  yOff[7] <- -yOff[7]

  yOff[8] <- -yOff[8]
  yOff[9] <- -yOff[9]
  ### END REALLY BAD CODE

  text(medianCatch+xOff,medianAAV+yOff,labels=labvec,cex=0.7)

  box()
  if(png){
    dev.off()
  }
}

getVec <- function(x,yr,name="depl") {
  # function to get a vector of values from a given year for a given quantity
  xx <- as.data.frame(x[[name]])
  return(as.numeric(xx[grep(yr,names(xx))][,1]))
}


getNyrMean <- function(x,yrs,name="depl") {
  # function to get mean values across a range of years for a given quantity
  columns <- NULL
  for(y in yrs) columns <- c(columns,grep(y,names(as.data.frame(x[[name]]))))
  xx <- as.data.frame(x[[name]][,columns]) # get data frame of all years of quantities
  xx <- apply(xx,1,mean) # get average over last N years
  return(xx)
}

getNyrCV <- function(x,yrs,name="depl") {
  # function to get mean values across a range of years for a given quantity
  columns <- NULL
  for(y in yrs) columns <- c(columns,grep(y,names(as.data.frame(x[[name]]))))
  xx <- as.data.frame(x[[name]][,columns]) # get data frame of all years of quantities
  xx <- apply(xx,1,function(x){sd(x)/mean(x)}) # get CV over last N years
  return(xx)
}

getAAV <- function(x,yrs,name="catches") {
  yrs <- c(min(yrs)-1,yrs) # adding a year required for calculations
  # function to get mean values across a range of years for a given quantity
  columns <- NULL
  for(y in yrs) columns <- c(columns,grep(y,names(as.data.frame(x[[name]]))))
  xx <- as.data.frame(x[[name]][,columns]) # get data frame of all years of quantities
  xx <- apply(xx,1,function(x){ mean(abs(x[-1] - x[-length(x)]))/mean(x[-1])}) # get AAV over last N years

  if(any(xx>10)){
    cat("Trimming values with AAV is greater than 10.\nTable of values > 10:\n")
    print(table(xx>10))
    xx <- xx[xx<10]
  }
  return(xx)
}

getPerfectInfoScenario <- function(scenarios){
  for(scenario in 1:length(scenarios)){
    if(scenarios[[scenario]]$perfectInfoCase){
      return(scenario)
    }
  }
  return(NULL)
}
