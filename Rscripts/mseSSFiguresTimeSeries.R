# mseSSFiguresTimeSeries.r
#
# All time series plots for mseSS
#
# Authors: Chris Grandin, Ian Taylor

source("mseSSGlobals.r")

plotTS <- function(scenarios,  # scenarios is created using mergeMSE() in mseSS.r
                   figPath = .FIGS_DIR,
                   ylim    = c(0,1),
                   type    = "dep",
                   intervals = TRUE,
                   CI      = 0.90,
                   lwd     = 3,
                   showNoFishing = FALSE,
                   addMean = FALSE,
                   res     = .RESOLUTION,
                   width   = .WIDTH,
                   height  = .HEIGHT,
                   units   = .UNITS,
                   png     = .PNG,
                   tgt     = 0.4,
                   verbose = .VERBOSE){
  # Time series plots for mseSS
  # "type" must be one of (N is scenario number):
  # - "dep"        = simulated depletion (scenarios[[N]]$depl)
  # - "spr"        = simulated SPR ratio (scenarios[[N]]$SPRratio)
  # - "cat"        = simulated catch (scenarios[[N]]$catchesReal)
  # - "tarcat"     = simulated forecast catch (scenarios[[N]]$catches)
  # - "ssbcompare" = simlulated ssb - estimated ssb
  # - "depcompare" = simulated depletion - estimated depletion
  # - "relerrssb"  = reletive error in estimated ssb
  # - "relerrdep"  = reletive error in estimated depletion

  # Get the minimum and maximum years from scenario with plot order #1
  if(!is.list(scenarios[[1]])){
    stop("plotTS: Error - Check the plot order values in the 'scenarios' object. e.g. scenarios$annual$plotOrder. Stopping..\n\n")
  }
  # Time series limits (includes assessed years)
  firstTSYear     <- scenarios[[1]]$firstTSYear
  lastTSYear      <- scenarios[[1]]$lastAssessYear
  tsYearLims      <- scenarios[[1]]$assessTSYears

  # Assessed years only limits
  firstAssessYear <- scenarios[[1]]$firstAssessYear
  lastAssessYear  <- lastTSYear
  assessYearLims  <- firstAssessYear:lastTSYear

  # Setup the plot and output filenames for the given type
  if(type=="dep"){
    if(addMean){
      filename <- file.path(figPath,paste(Sys.Date(),"_TS_Depletion_Mean_Line.png",sep=""))
    }else{
      filename <- file.path(figPath,paste(Sys.Date(),"_TS_Depletion.png",sep=""))
    }
    if(png){
      png(filename,res=res,width=width,height=height,units=units)
    }
    plot(assessYearLims,assessYearLims,ylim=ylim,type="n",xlim=c(firstAssessYear,lastAssessYear),las=1,xaxs='i',yaxs='i',
         main="Depletion",xlab="Year",ylab="Depletion",axes=FALSE)
    abline(h=c(0.1,0.4,1.0),lty=2)
  }
  if(type=="spr"){
    filename <- file.path(figPath,paste(Sys.Date(),"_TS_SPR.png",sep=""))
    if(png){
      png(filename,res=res,width=width,height=height,units=units)
    }
    plot(assessYearLims,assessYearLims,ylim=ylim,type="n",xlim=c(firstAssessYear,lastAssessYear),las=1,yaxs='i',xaxs='i',
         main="SPR",xlab="Year",ylab="SPR",axes=FALSE)
  }
  if(type=="cat"){
    filename <- file.path(figPath,paste(Sys.Date(),"_TS_Catch.png",sep=""))
    if(png){
      png(filename,res=res,width=width,height=height,units=units)
    }
    plot(firstAssessYear:(lastAssessYear+1),firstAssessYear:(lastAssessYear+1),ylim=ylim,type="n",xlim=c(firstAssessYear,lastAssessYear),las=1,yaxs='i',xaxs='i',
         main="Catch",xlab="Year",ylab="Catch ('1000 mt)",axes=FALSE)
  }
  if(type=="tarcat"){
    filename <- file.path(figPath,paste(Sys.Date(),"_TS_Target_Catch.png",sep=""))
    if(png){
      png(filename,res=res,width=width,height=height,units=units)
    }
    plot(firstAssessYear:(lastAssessYear+1),firstAssessYear:(lastAssessYear+1),ylim=ylim,type="n",xlim=c(firstAssessYear,lastAssessYear),las=1,yaxs='i',xaxs='i',
         main="Target catch",xlab="Year",ylab="Target catch ('000 mt)",axes=FALSE)
  }
  if(type=="ssbcompare"){
    filename <- file.path(figPath,paste(Sys.Date(),"_TS_SSB_Comparison.png",sep=""))
    if(png){
      png(filename,res=res,width=width,height=height,units=units)
    }
    plot((firstTSYear+1):lastTSYear,(firstTSYear+1):lastTSYear,ylim=ylim,type="n",xlim=c((firstTSYear+1),lastTSYear),las=1,xaxs='i',
         main="True SSB minus Predicted SSB",
         xlab="Year",ylab="True - Predicted SSB ('000 mt)",axes=FALSE)
  }
  if(type=="depcompare"){
    filename <- file.path(figPath,paste(Sys.Date(),"_TS_Depletion_Comparison.png",sep=""))
    if(png){
      png(filename,res=res,width=width,height=height,units=units)
    }
    plot((firstTSYear+1):lastTSYear,(firstTSYear+1):lastTSYear,ylim=ylim,type="n",xlim=c(firstTSYear,lastTSYear),las=1,xaxs='i',
         main="True Depletion minus Predicted Depletion",
         xlab="Year",ylab="True - Predicted Depletion",axes=FALSE)
  }
  if(type=="relerrssb"){
    filename <- file.path(figPath,paste(Sys.Date(),"_TS_Relative_Error_SSB.png",sep=""))
    if(png){
      png(filename,res=res,width=width,height=height,units=units)
    }
    plot((firstTSYear+1):lastTSYear,(firstTSYear+1):lastTSYear,ylim=ylim,type="n",xlim=c(firstTSYear,lastTSYear),las=1,xaxs='i',
         main="Relative error in estimated SSB",
         xlab="Year",ylab="(Estimated - True) / True SSB ('000 mt)",axes=FALSE)
  }
  if(type=="relerrdep"){
    filename <- file.path(figPath,paste(Sys.Date(),"_TS_Relative_Error_Depletion.png",sep=""))
    if(png){
      png(filename,res=res,width=width,height=height,units=units)
    }
    plot((firstTSYear+1):lastTSYear,(firstTSYear+1):lastTSYear,ylim=ylim,type="n",xlim=c(firstTSYear,lastTSYear),las=1,xaxs='i',
         main="Relative error in estimated Depletion",
         xlab="Year",ylab="(Estimated - True) / True Depletion",axes=FALSE)
  }

  if(intervals){
    for(scenario in 1:length(scenarios)){
        col <- getRGB(scenarios[[scenario]]$plotColor)
        if(type=="dep"){
          startInd <- ncol(scenarios[[scenario]]$depl) - length(assessYearLims) + 1
          endInd   <- ncol(scenarios[[scenario]]$depl)
          addpoly(yrvec=assessYearLims,
                  lower=apply(scenarios[[scenario]]$depl[,startInd:endInd],2,quantile,probs=(1-CI)/2),
                  upper=apply(scenarios[[scenario]]$depl[,startInd:endInd],2,quantile,probs=1-(1-CI)/2), r=col[1], g=col[2], b=col[3])
        }
        if(type=="spr"){
          startInd <- ncol(scenarios[[scenario]]$SPRratio) - length(assessYearLims) + 1
          endInd   <- ncol(scenarios[[scenario]]$SPRratio)
          addpoly(yrvec=assessYearLims,
                  lower=apply(1-scenarios[[scenario]]$SPRratio[,startInd:endInd]*(1-tgt),2,quantile,probs=(1-CI)/2),
                  upper=apply(1-scenarios[[scenario]]$SPRratio[,startInd:endInd]*(1-tgt),2,quantile,probs=1-(1-CI)/2), r=col[1], g=col[2], b=col[3])
        }
        if(type=="cat"){
          startInd <- ncol(scenarios[[scenario]]$catchesReal) - length(assessYearLims) + 1
          endInd   <- ncol(scenarios[[scenario]]$catchesReal)
          addpoly(yrvec=assessYearLims,
                  lower=apply(scenarios[[scenario]]$catchesReal[,startInd:endInd]/1000,2,quantile,probs=(1-CI)/2),
                  upper=apply(scenarios[[scenario]]$catchesReal[,startInd:endInd]/1000,2,quantile,probs=1-(1-CI)/2), r=col[1], g=col[2], b=col[3])
        }
        if(type=="tarcat" & !scenarios[[scenario]]$zeroCatch){
          if(scenarios[[scenario]]$perfectInfoCase){
            startInd <- 1
            endInd   <- ncol(scenarios[[scenario]]$catches)
          }else{
            startInd <- 1
            endInd   <- ncol(scenarios[[scenario]]$catches) - 1
          }
          addpoly(yrvec=assessYearLims,
                  lower=apply(scenarios[[scenario]]$catches[,startInd:endInd]/1000,2,quantile,probs=(1-CI)/2),
                  upper=apply(scenarios[[scenario]]$catches[,startInd:endInd]/1000,2,quantile,probs=1-(1-CI)/2), r=col[1], g=col[2], b=col[3])

        }
        if(type=="ssbcompare" | type=="relerrssb"){
          if(!scenarios[[scenario]]$runAssessments){
            if(verbose){
              cat("plotTS: ",type,": There are no assessment data for scenario '",scenarios[[scenario]]$prettyName,"'\n",sep="")
            }
          }else{
            indTrue   <- names(scenarios[[scenario]]$SSBMedian) %in% paste("SPB",(firstTSYear+1):lastTSYear,sep="_")
            indAssess <- names(scenarios[[scenario]]$assessSSBMedian) %in% paste((firstTSYear+1):lastTSYear,sep="")
            if(type=="ssbcompare"){
              diff <- t(apply(scenarios[[scenario]]$SSB[indTrue],1,'-',scenarios[[scenario]]$assessSSBMedian[indAssess]))
            }
            if(type=="relerrssb"){
              diff <- (t(apply(scenarios[[scenario]]$SSB[indTrue],1,'-',scenarios[[scenario]]$assessSSBMedian[indAssess])))/scenarios[[scenario]]$SSB[,indTrue]
            }
            addpoly(yrvec=(firstTSYear+1):lastTSYear,
                    lower=apply(diff/1000,2,quantile,probs=(1-CI)/2),
                    upper=apply(diff/1000,2,quantile,probs=1-(1-CI)/2), r=col[1] ,g=col[2], b=col[3])

          }
        }
        if(type=="depcompare" | type=="relerrdep"){
          if(!scenarios[[scenario]]$runAssessments){
            if(verbose){
              cat("plotTS: ",type,": There are no assessment data for scenario '",scenarios[[scenario]]$prettyName,"'\n",sep="")
            }
          }else{
            indTrue   <- names(scenarios[[scenario]]$deplMedian) %in% paste("SPB",(firstTSYear+1):lastTSYear,sep="_")
            indAssess <- names(scenarios[[scenario]]$assessDeplMedian) %in% paste((firstTSYear+1):lastTSYear,sep="")
            if(type=="depcompare"){
              diff <- t(apply(scenarios[[scenario]]$depl[indTrue],1,'-',scenarios[[scenario]]$assessDeplMedian[indAssess]))
            }
            if(type=="relerrdep"){
              diff <- (t(apply(scenarios[[scenario]]$depl[indTrue],1,'-',scenarios[[scenario]]$assessDeplMedian[indAssess])))/scenarios[[scenario]]$depl[,indTrue]
            }
            addpoly(yrvec=(firstTSYear+1):lastTSYear,
                    lower=apply(diff,2,quantile,probs=(1-CI)/2),
                    upper=apply(diff,2,quantile,probs=1-(1-CI)/2), r=col[1] ,g=col[2], b=col[3])
          }
        }
      }
  }
  # Plot the lines for the time series and make the legend color lists
  legendList <- NULL
  legendCols <- NULL
  for(scenario in 1:length(scenarios)){
    if((showNoFishing & scenarios[[scenario]]$zeroCatch) | !scenarios[[scenario]]$zeroCatch){
      col   <- getRGB(scenarios[[scenario]]$plotColor)
      alpha <- 0.6
      if((type=="ssbcompare" | type=="depcompare" | type=="relerrssb" | type=="relerrdep")
         & (!scenarios[[scenario]]$runAssessments)){
      }else{
        legendCols   <- c(legendCols,scenarios[[scenario]]$plotColor)
        legendList   <- c(legendList,scenarios[[scenario]]$prettyName)
      }
      if(type=="dep"){
        lines(assessYearLims,apply(scenarios[[scenario]]$depl[,startInd:endInd],2,median),
              col=rgb(col[1],col[2],col[3],alpha),lwd=lwd)
      }
      if(type=="spr"){
        lines(assessYearLims,apply(1-scenarios[[scenario]]$SPRratio[,startInd:endInd]*(1-tgt),2,median),
              col=rgb(col[1],col[2],col[3]),lwd=lwd)
      }
      if(type=="cat"){
        lines(scenarios[[scenario]]$firstAssessYear:(lastTSYear+1),c(apply(scenarios[[scenario]]$catchesReal[,startInd:endInd],2,median),10)/1000,
              col=rgb(col[1],col[2],col[3]),lwd=lwd)
      }
      if(type=="tarcat"){

        lines(firstAssessYear:(lastTSYear+1),c(apply(scenarios[[scenario]]$catches[,startInd:endInd],2,median),10)/1000,
              col=rgb(col[1],col[2],col[3]),lwd=lwd)
      }
      if(type=="ssbcompare" | type=="relerrssb"){
        if(scenarios[[scenario]]$runAssessments){
            indTrue   <- names(scenarios[[scenario]]$SSBMedian) %in% paste("SPB",(firstTSYear+1):lastTSYear,sep="_")
            indAssess <- names(scenarios[[scenario]]$assessSSBMedian) %in% paste((firstTSYear+1):lastTSYear,sep="")

          if(type=="ssbcompare"){
            diff <- t(apply(scenarios[[scenario]]$SSB[indTrue],1,'-',scenarios[[scenario]]$assessSSBMedian[indAssess]))

          }
          if(type=="relerrssb"){
            diff <- (t(apply(scenarios[[scenario]]$SSB[indTrue],1,'-',scenarios[[scenario]]$assessSSBMedian[indAssess])))/scenarios[[scenario]]$SSB[,indTrue]
          }
          lines((firstTSYear+1):lastTSYear,apply(diff,2,median)/1000,col=rgb(col[1],col[2],col[3]),lwd=lwd)
        }
      }
      if(type=="depcompare" | type=="relerrdep"){
        if(scenarios[[scenario]]$runAssessments){
          indTrue   <- names(scenarios[[scenario]]$deplMedian) %in% paste("SPB",(firstTSYear+1):lastTSYear,sep="_")
          indAssess <- names(scenarios[[scenario]]$assessDeplMedian) %in% paste((firstTSYear+1):lastTSYear,sep="")
          if(type=="depcompare"){
            diff <- t(apply(scenarios[[scenario]]$depl[indTrue],1,'-',scenarios[[scenario]]$assessDeplMedian[indAssess]))
          }
          if(type=="relerrdep"){
            diff <- (t(apply(scenarios[[scenario]]$depl[indTrue],1,'-',scenarios[[scenario]]$assessDeplMedian[indAssess])))/scenarios[[scenario]]$depl[,indTrue]
          }
          lines((firstTSYear+1):lastTSYear,apply(diff,2,median),col=rgb(col[1],col[2],col[3]),lwd=lwd)
        }
      }
    }
  }
  if(addMean){
    for(scenario in 1:length(scenarios)){
      if((showNoFishing & scenarios[[scenario]]$zeroCatch) | !scenarios[[scenario]]$zeroCatch){
        if(type=="dep"){
          col <- c(getRGB(scenarios[[scenario]]$plotColor),0.6)
          lines(assessYearLims,apply(scenarios[[scenario]]$depl[,startInd:endInd],2,mean),col=rgb(col[1],col[2],col[3],col[4]),lwd=lwd,lty=2)
        }
      }
    }
  }
  if(type=="ssbcompare" | type=="depcompare" | type=="relerrssb" | type=="relerrdep"){
    abline(h=0,lty=2)
  }

  legend("top",legendList,col=legendCols,lty=1,lwd=lwd,cex=0.7)
  if(type=="cat"){
    axis(1,assessYearLims)
  }else{
    axis(1,tsYearLims)
  }
  axis(2,las=1)
  box()
  if(png){
    dev.off()
  }
}

