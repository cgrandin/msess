# mseSSGlobals.r
#
# Constants and globals used in the Hake MSE framework
#
# Authors: Allan Hicks, Chris Grandin
#

# MSE_ROOT_DIR is the full path to the 'mseSS' directory
#  which has the subdirectories 'Rscripts' and 'Scenarios'
.MSE_ROOT_DIR <- file.path("..")

sourceDir <- function(path){
  for(name in list.files(path,pattern="\\.[rR]$")){
    source(file.path(path,name))
  }
}
#sourceDir("r4ss")

require(r4ss)

options(stringsAsFactors=FALSE)

# For gmail: You must get the TLS version of the SMTP command line program:
#  http://caspian.dotconf.net/menu/Software/SendEmail/
# which will allow R to send messages to gmail TLS encrypted accounts.
# ** You must put the directory containing the executable sendEmail.exe on your PATH
# I used this to figure it out:
#  http://www.r-bloggers.com/sending-email-from-r-sing-sendemail/
.SEND_EMAIL_EXE        <- "sendEmail"
.EMAIL_SMTP_SERVER     <- "smtp.gmail.com:587"

# Core directories
.OM_DIR_NAME           <- file.path(.MSE_ROOT_DIR,"OperatingModels")
.SCENARIOS_DIR_NAME    <- file.path(.MSE_ROOT_DIR,"Scenarios")
.FIGS_DIR              <- file.path(.MSE_ROOT_DIR,"FigsTables")
.SCENARIOS_INFO        <- file.path(.MSE_ROOT_DIR,"Scenarios.csv")

# Relative subdirectories
.SIMULATIONS_DIR_NAME  <- "simulations" # This is a subdirectory of each scenario directory.
.ASSESS_DIR_NAME       <- "assessment"  # This is a subdirectory of a simulations directory.

.SIM_FILE_PREFIX       <- "sim"         # Prefix for the RDATA files written after each simulation run.
.SIM_RDATA_FILE_EXT    <- ".rdata"      # Extension for the sim results files (binary R files).
.SPR_DIR_PREFIX        <- "spr"
.PERFECT_INFO_NAME     <- "PerfectInformation"
.RESULTS_FILE          <- "mseSSResults.rdata"

# SS Constants and filenames
# data and control files
# ----------------------
.COMBINEDSEX           <- 0
.FEMALE                <- 1
.MALE                  <- 2
.DATA_FILE_BASENAME    <- "data"
.SIM_CTL_FILE_BASE     <- "simulation_control" # Operating model control file
.ASSESS_CTL_FILE_BASE  <- "assessment_control" # Assessment model control file
.SS_FILE_EXT           <- ".ss"
.DATA_FILE             <- paste(.DATA_FILE_BASENAME,.SS_FILE_EXT,sep="")
.SIM_CONTROL_FILE      <- paste(.SIM_CTL_FILE_BASE,.SS_FILE_EXT,sep="")
.ASSESS_CONTROL_FILE   <- paste(.ASSESS_CTL_FILE_BASE,.SS_FILE_EXT,sep="")
.ORIG_DATA_FILE        <- paste(.DATA_FILE_BASENAME,"_Original",.SS_FILE_EXT,sep="")
.ORIG_SIM_CONTROL_FILE <- paste(.SIM_CTL_FILE_BASE,"_Original",.SS_FILE_EXT,sep="")
.ORIG_ASS_CONTROL_FILE <- paste(.ASSESS_CTL_FILE_BASE,"_Original",.SS_FILE_EXT,sep="")
# ----------------------
.FORECAST_FILE         <- "forecast.ss"
.FORECAST_ASSESS_FILE  <- "forecastAssess.ss"
.FORECAST_REPORT_FILE  <- "Forecast-report.sso"
.STARTER_FILE          <- "starter.ss"
.SS_EXE_FILE           <- "ss3.exe"
.SS_PSV_FILE           <- "ss3.psv"
.PAR_FILE              <- "ss3.par"
.WT_AT_AGE_FILE        <- "wtatage.ss"
.FORECAST_CATCH_TEXT   <- "Number of forecast catch levels to input"
.FORECAST_BASIS_TEXT   <- "basis for input Fcast catch"
.RECDEV_MARKER         <- "recdev1:"
.FCAST_RECS_MARKER     <- "Fcast_recruitments:"
.FCAST_WITH_FMATCH     <- "FORECAST:_With_F_to_match_adjusted_catch"

# The following are found in the dervived_posteriors file
.FCAST_CATCH           <- "ForeCatch"
.FB_TARGET             <- "Fstd_Btgt"
.FSPR_TARGET           <- "Fstd_SPRtgt"
.F_PREFIX              <- "F_"
.RECR_PREFIX           <- "Recr_"
.VIRGIN_RECR           <- "Recr_Virgin"
.INITIAL_RECR          <- "Recr_Initial"
.VIRGIN_SSB            <- "SPB_Virgin"
.INITIAL_SSB           <- "SPB_Initial"

.END_YEAR_RECDEV       <- "End year standard recruitment devs"        # used in modifyControlFile()
.LAST_YEAR_BIAS_CORR   <- "Last year for full bias correction in_MPD" # used in modifyControlFile()
.FIRST_YEAR_NOBIAS     <- "First_recent_yr_nobias_adj_in_MPD"         # used in modifyControlFile()
.MCEVAL                <- "-mceval"
.NOHESS                <- "-nohess"
.SELEX_MIN_AGE_IND     <- 6     # Column numbers in the selex object of the SS output
.SELEX_MAX_AGE_IND     <- 26    #  see getSelexByFleet() function.
.NATAGE_SEX_IND        <- 6     # Column number for the sex column in the .POST_NATAGE_FILE
.NATAGE_MIN_AGE_IND    <- 7     # These signify column numbers from .POST_NATAGE_FILE
.NATAGE_MAX_AGE_IND    <- 27    #  see runAssessment() function.
.NUM_AGE_ERR_DEFS      <- 40
.MINIMUM_CATCH         <- 10    # Do not set to zero, this will cause errors in SS. This is used for 'zero catch' situations.
.END_OF_INPUT          <- 999
.SEASON                <- 1     # Used in writing the catch data to the datafile for each assessment run
.AGE_ERROR_COL_PREFIX  <- "a"   # Prefix for all age matrices' row or column names.
.AGE_COMP_LABELS       <- c("assessYear","season","fleet","gender","part","ageError","lengthBinLo","lengthBinHi")
.WT_AT_AGE             <- c(0.03,0.0885,0.2562,0.3799,0.4913,0.5434,0.5906,0.662,0.7215,0.791,0.8629,0.9315,0.9681,1.0751,1.0016,1.0202,1.0202,1.0202,1.0202,1.0202,1.0202)
.BRATIO_REP            <- "Bratio"      # Found in the Report file and the derived_posteriors file
.SSB_REP               <- "SPB"         # Found in the Report file and the derived_posteriors file
.SSB_PREFIX            <- paste(.SSB_REP,"_",sep="")
.SPR_RATIO_REP         <- "SPRratio"    # Found in the derived_posteriors file
.SSB_MSESS             <- "SSB"         # Object name in mseSS for assessed spawning stock biomass
.DEPL_MSESS            <- "depl"        # Object name in mseSS for assessed depletion
.RUNTIME_MSESS         <- "runTimeSecs" # Object name in mseSS for assessment model runtime in seconds
.CATCHES_MSESS         <- "catch"       # Object name in mseSS for assessment model catches

# File prefixes/postfixes for removal during cleanup of assessment directories
.SS                    <- "ss3."
.POSTERIOR             <- "posterior"
.SS_NEW                <- "ss_new"
.ADMODEL               <- "admodel"

# Individual files removed during cleanup
.TEMP_PAR_FILE         <- "tmp.par"
.COVAR_FILE            <- "covar.sso"
.ECHOINPUT_FILE        <- "echoinput.sso"
.SISTABLE_FILE         <- "SIS_Table.sso"
.PARMTRACE_FILE        <- "ParmTrace.sso"
.RUNNUMBER_FILE        <- "runnumber.ss"
.CHECKUP_FILE          <- "checkup.sso"
.CUMREPORT_FILE        <- "CumReport.sso"
.EIGENVALUE_FILE       <- "eigv.rpt"
.FMINLOG_FILE          <- "fmin.log"
.REBUILD_FILE          <- "rebuild.sso"
.VARIANCE_FILE         <- "variance"

# The following files will be found inside the individual scenario directories
.DER_POST_FILE         <- "derived_posteriors.sso"
.POST_FILE             <- "posteriors.sso"
.POST_NATAGE_FILE      <- "posterior_natage.sso"
.MSE_POST_FILE         <- "posterior_mse.sso"
.AGE_ERROR_FILE        <- "AgeingError.csv"
.REP_FILE              <- "Report.sso"

# Convergence and jitter constants
.CONV_CRITERIA         <- 0.1
.MAX_ITERATIONS        <- 3
.JITTER                <- 1e-5
.JITTER_MULTIPLIER     <- 10

# Plotting and verbosity
.PNG                   <- TRUE
.RESOLUTION            <- 300
.WIDTH                 <- 7
.HEIGHT                <- 7
.UNITS                 <- "in"
.VERBOSE               <- FALSE

###################################################################
###################################################################
## mseR constants, for integration into mseR  --- NOT USED YET   ##
## INFO file                                                     ##
##.INFO_FILE_COMMENT     <- "## Information file for simulation "##
##.SIMTIME_LABEL         <- "# simTime"                          ##
##.MP_LABEL              <- "# mpLabel"                          ##
##.T_MP                  <- "# tMP"                              ##
##.N_T                   <- "# nT"                               ##
##.N_REPS                <- "# nReps"                            ##
##.RANK                  <- "# rank"                             ##
##.GROUP                 <- "# group"                            ##
##                                                               ##
### SIM_CONTROL file                                             ##
##.SIM_FILE_COMMENT      <- "# mseRguiSim GUI parameters written"##
##.SIM_FILE_HEADER       <- "parameter value\n"                  ##
###################################################################
###################################################################
