# mseSSPlotAll.r
#
# Functions used to make the MSE performance plots.
# Source this file, see the call to plotAll()
#  at the end of this file.
#
# Authors: Chris Grandin


source("mseSSGlobals.r")
source("mseSSUtilities.r")

source("mseSSFiguresTimeSeries.r")
source("mseSSFiguresDensity.r")
source("mseSSFiguresSPR.r")
source("mseSSFiguresActualvsPerceived.r")
source("mseSSFiguresRetrospectives.r")

plotAll <- function(plotTS     = T, # Make timeseries plots
                    plotDens   = T, # Make density plots
                    plotSPR    = T, # Make SPR plots for perfect information case
                    plotAAV    = T, # Make Average Annual Variability in catch plots
                    plotAvsP   = T, # Make Actual vs. Perceived plots
                    plotRetros = T, # Make single-simulation retrospective plots
                    retroSim   = 1, # Simulation index for plotting retrospectives
                    png        = .PNG,
                    verbose    = .VERBOSE
                    ){

  simResultsPath <- file.path(.SCENARIOS_DIR_NAME,.RESULTS_FILE)
  load(simResultsPath)
  makeFigsDir()
  shortTerm  <- 2013:2015
  mediumTerm <- 2016:2020
  longTerm   <- 2021:2029

  if(exists("scenarios")){
    if(plotTS){
      plotTS(scenarios, type="dep", showNoFishing=TRUE, addMean=FALSE, ylim=c(0,1.4), png=png, verbose=verbose)
      plotTS(scenarios, type="dep", showNoFishing=TRUE, addMean=TRUE, ylim=c(0,1.4), png=png, verbose=verbose)
      plotTS(scenarios, type="spr", showNoFishing=TRUE, addMean=FALSE, ylim=c(0,1.2), png=png, verbose=verbose)
      plotTS(scenarios, type="cat", showNoFishing=FALSE, addMean=FALSE, ylim=c(0,1000), png=png, verbose=verbose)
      plotTS(scenarios, type="tarcat", showNoFishing=TRUE, addMean=FALSE, ylim=c(0,1000), png=png, verbose=verbose)
      plotTS(scenarios, type="ssbcompare", showNoFishing=TRUE, addMean=FALSE, ylim=c(-1000,1000), png=png, verbose=verbose)
      plotTS(scenarios, type="relerrssb", showNoFishing=TRUE, addMean=FALSE, ylim=c(-0.01,0.01), png=png, verbose=verbose)
      plotTS(scenarios, type="depcompare", showNoFishing=TRUE, addMean=FALSE, ylim=c(-0.5,0.5), png=png, verbose=verbose)
      plotTS(scenarios, type="relerrdep", showNoFishing=TRUE, addMean=FALSE, ylim=c(-1.5,1.5), png=png, verbose=verbose)
    }
    if(plotDens){
      plotDensity(scenarios, type="dep", years=shortTerm, png=png, showNoFishing=TRUE, addMean=TRUE, xMax=1, yMax=4, verbose=verbose)
      plotDensity(scenarios, type="dep", years=mediumTerm, png=png, showNoFishing=TRUE, addMean=TRUE ,xMax=1, yMax=4, verbose=verbose)
      plotDensity(scenarios, type="dep", years=longTerm, png=png, showNoFishing=TRUE, addMean=TRUE, xMax=1, yMax=4, verbose=verbose)
      plotDensity(scenarios, type="cat", years=shortTerm, png=png, showNoFishing=FALSE, addMean=TRUE, xMax=2100, yMax=0.0035, verbose=verbose)
      plotDensity(scenarios, type="cat", years=mediumTerm, png=png, showNoFishing=FALSE, addMean=TRUE, xMax=2100, yMax=0.0035, verbose=verbose)
      plotDensity(scenarios, type="cat", years=longTerm, png=png, showNoFishing=FALSE, addMean=TRUE, xMax=2100, yMax=0.0035, verbose=verbose)
      plotDensity(scenarios, type="aav", years=longTerm, png=png, showNoFishing=FALSE, addMean=TRUE, xMax=1, yMax=6, verbose=verbose)
      plotDensity(scenarios, type="aaveven", years=longTerm, png=png, showNoFishing=FALSE, addMean=TRUE, xMax=1, yMax=6, verbose=verbose)
      plotDensity(scenarios, type="aavodd", years=longTerm, png=png, showNoFishing=FALSE, addMean=TRUE, xMax=1, yMax=6, verbose=verbose)
    }
    if(plotSPR){
      plotSPR(scenarios, type="dep", years=longTerm[length(longTerm)], ylab="Depletion", yMax=1.5, scale=1, png=png, verbose=verbose)
      plotSPR(scenarios, type="dep", years=longTerm, ylab="Depletion", yMax=1.5, scale=1, png=png, verbose=verbose)
      plotSPR(scenarios, type="cat", years=longTerm, ylab="Average Catch ('000 mt)",yMax=1200, scale=1000, png=png, verbose=verbose)
      plotSPR(scenarios, type="aav", years=longTerm, ylab="Average Annual Variability In Catch", yMax=0.7, scale=1, png=png, verbose=verbose)
      plotSPR(scenarios, type="fsprtgt", years=longTerm, ylab="Exploitation rate associated with target harvest rate",
              yMax=0.45, scale=1, png=png, verbose=verbose)
    }
    if(plotAAV){
      plotAAVDepvsCatch(scenarios, years=longTerm, depYlim=c(0,1), aavYlim=c(0,0.4), png=png, verbose=verbose)
    }
    if(plotAvsP){
      plotActualvsPerceived(scenarios, years=longTerm,  png=png, verbose=verbose)
    }
    if(plotRetros){
      for(scenario in scenarios){
        plotRetrospectives(scenario, type="dep",  ylim=c(0,3.5), retroSim=retroSim, png=png, verbose=verbose)
        plotRetrospectives(scenario, type="recr", ylim=c(0,7e7), retroSim=retroSim, png=png, verbose=verbose)
      }
    }
  }else{
    stop("Error: No 'scenarios' object.  Run mergeMSE() to create this.\n\n")
  }
}

plotAll(plotTS     = F, # Make timeseries plots
        plotDens   = F, # Make density plots
        plotSPR    = F, # Make SPR plots for perfect information case
        plotAAV    = F, # Make Average Annual Variability in catch plots
        plotAvsP   = F, # Make Actual vs. Perceived plots
        plotRetros = T, # Make Retrospective plots by simulation
        retroSim   = 2  # Simulation index for plotting retrospectives
        )

