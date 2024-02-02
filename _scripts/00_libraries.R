# load libraries

library(terra)
library(sf)
library(tidyverse)
library(tidyrgee)
library(raster)
library(rgee)
library(dataRetrieval)
library(plotly)
library(caret)
library(parallel)
library(doParallel)
library(MLmetrics)
library(ggplot2)
library(scales) # ggplot datetime scale
library(paletteer) # colors
library(ggspatial)

library(sp)
library(rgdal)
library(reshape)
library(grid)
library(gridExtra)
# library(RStoolbox)
library(caret)
library(rasterVis)
library(corrplot)
library(doParallel)
library(NeuralNetTools)

tmpFiles(old=TRUE, remove=TRUE) # gets rid of temp files