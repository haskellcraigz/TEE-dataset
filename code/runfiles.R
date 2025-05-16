# Script that runs all R scripts

# --- set working directory 

# set working directory to TEE-dataset-main folder
setwd("")

# --- run files

# Raster processing
terra::gdalCache(30000)

source("code/01_rasterprocessing/RASTERPROCESSING-temp-1979-2020_vFINAL.R")
rm(list=ls())

terra::gdalCache(30000)

source("code/01_rasterprocessing/RASTERPROCESSING-pop-weighted-2000-2022_vFINAL.R")
rm(list=ls())

terra::gdalCache(30000)

source("code/01_rasterprocessing/RASTERPROCESSING-UTCI_vFINAL.R")
rm(list=ls())

# Metrics
source("code/02_metricconstruction/METRIC-cdd.R")
rm(list=ls())

source("code/02_metricconstruction/METRIC-expected_daysoverthreshold.R")
rm(list=ls())

source("code/02_metricconstruction/METRIC-extremetempwavedays_global.R")
rm(list=ls())

source("code/02_metricconstruction/METRIC-extremetempwavedays_local.R")
rm(list=ls())

source("code/02_metricconstruction/METRIC-percentilethresholdtemp_days_local.R")
rm(list=ls())

source("code/02_metricconstruction/METRIC-tropicalnights.R")
rm(list=ls())

# source("code/02_metricconstruction/METRIC-utci.R")
# rm(list=ls())

# dataset compiling
source("code/03_compiledatasets/PRODUCEDATA-teedaily.R")
rm(list=ls())

source("code/03_compiledatasets/PRODUCEDATA-teeyearly.R")
rm(list=ls())

source("code/03_compiledatasets/PRODUCEDATA-teewave.R")
rm(list=ls())