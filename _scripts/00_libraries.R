# load libraries

library(terra)          # geospatial functions
library(tidyterra)      # tidy functions for terra objects
library(tidyverse)
library(tidyrgee) 
library(tidymodels)     # 
library(rgee)           # r wrapper for google earth engine
library(dataRetrieval)  # USGS data
library(caret)          # for ML functions
library(parallel)       # parallel processing
library(doParallel)     # parallel processing
library(MLmetrics)      
library(ggplot2)        # for plotting
library(scales)         # ggplot datetime scale
library(paletteer)      # colors
library(ggspatial)
library(plotly)         # for exploring plots
library(cluster)        # for the silhouette method
library(ranger)
library(sp)
library(reshape)
library(grid)
library(gridExtra)
# library(RStoolbox)
library(rasterVis)
library(corrplot)
library(doParallel)
library(NeuralNetTools)

tmpFiles(old=TRUE, remove=TRUE) # gets rid of temp files