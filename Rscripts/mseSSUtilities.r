# mseSSUtilities.r
#
# Utility functions for the MSE for SS, Pacific Hake 2013 version.
#   Anything in capitals starting with a period is defined in
#   mseSSGlobals.r
#
# Authors: Allan Hicks, Chris Grandin
#

source("mseSSGlobals.r")

setPlotColor <- function(scenarioNum,color){
  # Reads in the .RESULTS_FILE, sets the plot color to the new 'color'
  # and saves the file again.
  simResultsPath <- file.path(.SCENARIOS_DIR_NAME,.RESULTS_FILE)
  load(simResultsPath)
  scenarios[[scenarioNum]]$plotColor <<- color
  save(scenarios,file=simResultsPath)
}

getPlotColor <- function(scenarios){
  # Returns a data.table of scenario names, their plot order, and their plot colors.
  colorTable <- NULL
  for(scenario in 1:length(scenarios)){
    s <- scenarios[[scenario]]
    colorTable <- rbind(colorTable,c(s$scenarioName,s$plotOrder,s$plotColor))
  }
  colnames(colorTable) = c("scenarioName","plotOrder","plotColor")
  return(colorTable)
}

makeFigsDir <- function(figsDir=.FIGS_DIR){
  # Create the figsDir directory, but don't bother telling us if it's already there.
  suppressWarnings(
    dir.create(figsDir)
  )
}

addpoly <- function(yrvec, lower, upper, r, g, b, alpha1=0.05, alpha2=0.6, lty=3){
  # add shaded uncertainty intervals behind line
  good <- !is.na(lower) & !is.na(upper)
  polygon(x=c(yrvec[good],rev(yrvec[good])),
          y=c(lower[good],rev(upper[good])),
          border=NA,col=rgb(r,g,b,alpha1))
  lines(yrvec[good],lower[good],lty=lty,col=rgb(r,g,b,alpha2))
  lines(yrvec[good],upper[good],lty=lty,col=rgb(r,g,b,alpha2))
}

getRGB <- function(value){
  return(col2rgb(value)/255)
}


sendEmail <- function(emailAddress  = NULL,
                      from          = emailAddress,
                      subject       = "sendEmail default subject",
                      emailMessage  = "Hello World",
                      serverAndPort = .EMAIL_SMTP_SERVER,
                      attachFile    = NULL,
                      emailPassword = NULL){
  paramsList                    <- list()
  paramsList$toAddress          <- c("-t",emailAddress)
  paramsList$fromAddress        <- c("-f",from)
  paramsList$emailSubject       <- c("-u",subject)
  if(is.null(emailMessage)){
    paramsList$listemailMessage   <- c("-m",paste("Sent at ",format(Sys.time(),"%Y-%d-%m:%H-%M-%S"),sep=" "))
  }else{
    paramsList$listemailMessage   <- c("-m",emailMessage)
  }
  paramsList$serverAndPort      <- c("-s",serverAndPort)
  if(!is.null(attachFile)){
    paramsList$fileAttachPath   <- c("-a",attachFile)
  }
  paramsList$accUsername        <- c("-xu",emailAddress)
  paramsList$accPassword        <- c("-xp",emailPassword)
  # I think the following might be for UNIX, I changed dQuote to shQuote and it works on Windows 7.
  #paramsListQuoted              <- lapply(paramsList,function(x){x[2] <- dQuote(x[2]);paste(x,collapse = " ")})
  paramsListQuoted              <- lapply(paramsList,function(x){x[2] <- shQuote(x[2]);paste(x,collapse = " ")})
  suffixCall                    <- paste(do.call("c",paramsListQuoted),collapse = " ")
  commandCall                   <- paste(.SEND_EMAIL_EXE,suffixCall,sep = " ")
  returnVal                     <- system(commandCall,intern=T,wait=T)
  print(returnVal)
}

medianOfAssessList <- function(assessList){
  # assessList is a list of length equal to the number of simulations run
  # It is assumed that each list element is a matrix of the same dimensionality as the rest.
  #  with years as rows.
  # calculates the median of each year's value across the list.

  arr <- array(unlist(assessList), dim=c(nrow(assessList[[1]]), ncol(assessList[[1]]),length(assessList)))
  nc  <- ncol(assessList[[1]])
  nr  <- nrow(assessList[[1]])

  mat <- matrix(nrow=nr,ncol=nc)
  for(i in 1:nr){
    for(j in 1:nc){
      mat[i,j] <- median(arr[i,j,])
    }
  }
  colnames(mat) <- colnames(assessList[[1]])
  rownames(mat) <- rownames(assessList[[1]])
  return(mat)
}

sortDataFrameByRowName <- function(df){
  # sorts the data frame by its string row name and returns the new data frame
  sortedDF <- df[order(rownames(df)),]
  return(sortedDF)
}

sortVectorByName <- function(vec){
  # sorts the vector by its string names and returns the new vector
  sortedVec <- vec[order(names(vec))]
  return(sortedVec)
}

simpleCap <- function(x, stringSep=" ") {
  # Capitalize the first letter in every word in a string with stringSeps (spaces) in it.
  s <- strsplit(x, stringSep)[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=stringSep)
}

makePrettyNames <- function(info, stringSep=" "){
  # Takes the list of names and breaks each up into words by breaking on the capital letters
  # For example, if you have a scenario name "PerfectInformation" it will make this
  # "Perfect Information" for use on plots.  Capitalizes all words in the name as well.
  # For example, 'noFishing' will become 'No Fishing'

  names <- info$scenarioName
  for(i in 1:length(names)){
    names[i] <- gsub("(?!^)(?=[[:upper:]])", stringSep, names[i], perl=T)
    names[i] <- simpleCap(names[i], stringSep)
  }
  return(names)
}

createAssessVector <- function(vectorLength){
  return(vector(mode="numeric",length=vectorLength))
}

createSimAssessMatrix <- function(simulationNums,assessYears){
  newMat <- matrix(ncol=length(simulationNums),nrow=length(assessYears))
  colnames(newMat) <- assessYears
  rownames(newMat) <- simulationNums
  return(newMat)
}
