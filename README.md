mseSS - Management Strategy Evaluation framework for Stock Synthesis
====================================================================

mseSS is a software framework in the R language designed to encapsulate the details of Fisheries Management Strategy Evaluations using Stock Synthesis (SS).

Directions on use:
------------------

Set up your operating models as laid out in the Stock Synthesis documentation.  Each operating model must
be placed as a subdirectory in the 'OperatingModels' directory, and they must have been previously run
with the following output files in place:

- posterior_mse.sso
- posterior_natage.sso
- posterior_selex.sso
- posterior_vectors.sso
- posteriors.sso
- simulation_control.ss

Additionally, the following input files and the ss3 executable must be present:

- data.ss
- derived_posteriors.ss
- forecast.ss
- forecastAssess.ss
- starter.ss
- wtatage.ss
- SS3.exe
- ss3.psv

Setup your Scenarios.csv file located in root mseSS directory.

Fields in Scenarios.csv:

  - plotOrder - (numeric) The order in which the lines or polygons will be drawn on any plots
    and appear in legends where 1 is first.

  - plotColor - (numeric) The R color to use for this scenarios' plotting color.

  - operatingModelName - (character string) The directory name of the operating model to use for the scenario,
    it must exist in the 'OperatingModels' directory and is case-sensitive.

  - scenarioName - (character string) The directory name of the scenario.  It must exist in the 'Scenarios' directory and is
    case-sensitive.

  - firstSimNum - (numeric) If running in sequential mode, this is the first simulation to run. If running
    in random draw mode, this must be set to -1.

  - lastSimNum - (numeric) If running in sequential mode, this is the last simulation to run. If running
    in random draw mode, this is the number of draws; in this case firstSimNum must be set to -1.

  - firstAssessYear - (numeric) This is the first year to run assessments on for the scenario.
    When this is set it does not mean assessments will be run, see 'runAssessments'.
    Even if assessments are not to be run, this should be set the same as those cases for which
    assessments will be run, as the value is used as a lower limit for some plots.

  - lastAssessYear - (numeric) This is the last year to run assessments on for the scenario.
    When this is set it does not mean assessments will be run, see 'runAssessments'.
    Even if assessments are not to be run, this should be set the same as those cases for which
    assessments will be run, as the value is used as a upper limit for some plots.

  - firstTSYear - (numeric) The first year to start in the time series of data.

  - perfectInfoCase - (boolean) If a scenario is the perfect information case or not.  Only one is
    allowed to be TRUE in the Scenarios.csv file.

  - runAssessments - (boolean) Whether or not to run assessments for this scenario.  If FALSE,
    it is assumed that the simulation MCMC was already run through SS and the following files exist
    in the scenarios' directory:
    - derived_posteriors.sso
    - posterior_vectors.sso
    - posterior_mse.sso
    - posterior_natage.sso
    - posterior_selex.sso


  - useAgeingErrorMatrix - (boolean) Use the ageing error matrix in the scenario. The file is defined as
    .AGE_ERROR_FILE in mseSSGlobals.r.

  - minA - (numeric) The minimum age for this stock.

  - maxA - (numeric) The maximum age for this stock.

  - plusA - (numeric) The plus group age class for this stock.

  - zeroCatch - (boolean) Used for the special case where no fishing will ever take place.

  - initialCatch - (numeric) The catch to be set for the first assessed year.

  - survFreq - (numeric) The number of years between surveys.

  - survSELog - (numeric) The survey standard error in log space.

  - survSETot - (numeric) The total survey standard error.

  - survNAge - (numeric) The number of survey ages to place in a bin for random multinomial draws of age comps.
    See the R help for rmultinom. Usage: rmultinom(1,survNage*survNageAdj).

  - survNAgeAdj - (numeric) Multiplier for survNAge. See R help for rmultinom. Usage: rmultinom(1,survNage*survNageAdj).

  - commNAge - (numeric) The number of commercial ages to place in a bin for random multinomial draws of age comps.
    See the R help for rmultinom. Usage: rmultinom(1,survNage*survNageAdj)

  - commNAgeAdj - (numeric) Multiplier for commNAge. See R help for rmultinom. Usage: rmultinom(1,commNage*commNageAdj).

  - minSimNum - (numeric) The first simulation to run, progressing sequantially by one to maxSimNum. If this is a negative number
    then that indicates that random draws from the posterior are to be used instead, with maxSimNum being the number
    of random draws to make.

  - maxSimNum - (numeric) The last simulation to run. If minSimNum is negative, then this is the number of random draws
    from the posterior.

* Make sure that the firstAssessYear and the lastAssessYear are the same for
  all scenarios.

* The plotting limits are taken from the scenario with plot order = 1 so make
  sure the firstAssessYear and lastAssessYear span the assessed years,
  even if the scenario does not run assessments, for example the noFishing scenario
  must have these years set properly.

* zeroCatch tells the program to apply zero catch for all years.  No assessments
  will be run in this mode.  Call this the 'No fishing' scenario.

* perfectInfoCase tells the program that this is the only perfect information case
  in this MSE run.  There can only be one and if there are multiple instances,
  an error will be issued and the program stopped.  Also, no assessments will be
  run in this scenario.

Running the MSE scenarios:
--------------------------

* Type runMSE(continue=T,useSystem=F,verbose=T,sendEmailUpdates=F,emailAddress="yourname@gmail.com",emailPassword=""), where:
  - continue=T (default) the simulations will begin one number after the last simX.rdata file
    that is found in the simulations directory, where X is an integer. If continue=F then the simulation files will
    be overwritten if they exist.

  - useSystem=F (default) then the R command 'shell' will be used. If useSystem=T then the R command 'system' will be used.
    This was added to ensure stability on some machines where one or the other command fails.

  - verbose=T (default) then many outputs will be shown.  If verbose=F then ill be limited output to the screen.

  - sendEmailUpdates=F (default).  If this is set to T you must include a valid GMAIL address and password (see next items).

  - emailAddress. Your valid GMAIL address.  Only GMAIL works with this option.

  - emailPassword. Your valid GMAIL password.

  - You can split up the scenarios, by cutting and pasting the project directory and its contents,
    and then modifying each Scenarios.csv file so that the scenarios and simulation numbers
    are specified differently for each working directory.  Once the simulations are all completed,
    copy the simX.rdata files for each scenario back into a single master project directory
    and run the mergeMSE() function.  This will aggregate all the scenarios/simulations'
    output and store it in a file called mseSSResults.rdata in the Scenarios directory.  The
    plotting code located in plotAll.r, and FiguresX.r can read this file and create
    plots comparing the various scenarios.


* Naming conventions:
  - Scenario names should consist of words delimited by capital letters, also known as
    camelCase.  An example is: perfectInformation. A pretty name used for plotting is
    generated for each scenario by breaking up the camelCase and seperating the words with spaces.
    For example, 'perfectInformation' will be given the pretty name 'Perfect Information'. The
    first letter will always be made into a capital letter.  See makePrettyNames() function to
    see how this is done.


* Output data structures:
  - Each simX.rdata file in each 'simulations' directory of each scenario will contain an object
    called simDat.  This object is a list of all the outputs of the given simulation. Simulated
    data and outputs and assessment outputs are present in the list. For example, look at
    simDat$assessRecr to see the estimated assessment recruitments. It is a matrix with
    the columns labelled from the 'firstTSYear' to the 'lastAssessYear' as defined in the
    Scenarios.csv file. The rows are labelled with each assessment year ('firstAssessYear' through
    'lastAssessYear') in the Scenarios.csv file. The simulated data are a single vector of
    numbers from 'firstTSYear' through 'lastAssessYear'. An example of this is simDat$recruitment.

  - After the function mergeMSE() has been run, a single file (Scenarios/mseSSResults.rdata) holds
    all the outputs from the MSE run. The mergeMSE() function does not depend on Scenarios.csv,
    so you can just reorganize things as you wish and run it. The mseSSResults.rdata file consists
    of an object, 'scenarios', which is a list of N scenarios, where N is whatever was found in
    the directory 'Scenarios'. The directory names of the scenarios merged are the same as the names
    in this list. Each member is itself a list of all the merged simulation outputs for that scenario.
    For example, open mseSSResults.rdata and look at 'names(scenarios)', then 'names(scenarios$annual)'.
    Try scenarios$annual$assessRecr. The assessed recruitments are in a list of matrices, one matrix
    per simulation, in the numerical order of the simulations. This can be verified by looking at the
    individual simX.rdata files in the simulations directory. Looking at 'scenarios$annual$SSB' shows
    the simulated SSB by simulation number. The names of any variables estimated in an assessment will
    begin with 'assess'.


* Special cases:
  - In these cases it is assumed that SS was already run in MCMC mode, and various files created from
    the runs exist:
    - derived_posteriors.sso
    - posterior_vectors.sso
    - posterior_mse.sso
    - posterior_natage.sso
    - posterior_selex.sso

  - No Fishing: This case is declared in the Scenarios.csv file when you set 'zeroCatch' to 'T'. In
    this case, no assessments will be run and the catch for each year will be set to the
    .MINIMUM_CATCH value, which is typically set to 10 tonnes for SS (.MINIMUM_CATCH in mseSSGlobals.r).
    Note that for SS, the minimum catch cannot be zero due to uncaught divide-by-zero errors.

  - Perfect Information: This case is declared in the Scenarios.csv file when you set
    'perfectInfoCase' to 'T'. In this case, no assessments will be run.



Authors
-------

Allan Hicks   (NWMFSC, USA)

Nathan Taylor (DFO, Canada)

Chris Grandin (DFO, Canada)

Ian Taylor    (NWMFSC, USA)

