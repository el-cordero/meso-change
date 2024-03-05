source("~/Documents/Projects/USACE/ML Mesohabitats/Data/R/Mesohabitat_Detection/ML-Mesohabitats/_scripts/00_libraries.R")
source("~/Documents/Projects/USACE/ML Mesohabitats/Data/R/Mesohabitat_Detection/ML-Mesohabitats/_scripts/00_funcNormalise.R")

## filepaths
raster <- "~/Documents/Projects/USACE/ML Mesohabitats/Data/GIS/Kansas/Raster/"

classRaster <- rast(paste0(raster,'Analysis/classes.tif'))

# read in raster stacks
kmRaster <- rast(paste0(raster,'Analysis/Kmeans/combined/kmeansStack05.tif'))
rfRaster <- rast(paste0(raster,'Analysis/RandomForest/combined/rfStack.tif'))

r <- c(classRaster, kmRaster[[1]],rfRaster[[1]])
names(r) <- c('class','training','kmeans','randomforest')
rm(classRaster,kmRaster, rfRaster)

r <- mask(r, r$class)
r <- mask(r, r$kmeans)

# create dataframe from raster stack
df <- as.data.frame(r, cell=FALSE, na.rm=TRUE)

df$kmean <- ''
df[df$kmeans==1,]$kmean <- 'cropland'
df[df$kmeans==2,]$kmean <- 'bareland'
df[df$kmeans==3,]$kmean <- 'mixed'
df[df$kmeans==4,]$kmean <- 'forest'
df[df$kmeans==5,]$kmean <- 'water'
df <- df[,c('training','class','kmean','randomforest')]

for (i in 1:length(names(df))){
  df[,i] <- as.factor(df[,i])
}

df <- df[df$kmean != 'mixed',]
# confMatrix <- 
# kmeansVclass <- confusionMatrix(df[df$training == 0,]$kmean,df[df$training == 0,]$class)
kmeansVclass <- confusionMatrix(df$kmean,df$class)
rfVclass <- confusionMatrix(df[df$training == 0,]$randomforest,df[df$training == 0,]$class)
# confusionMatrix(df$randomforest,df$class)




classRaster <- rast(paste0(raster,'Analysis/classes.tif'))

# read in raster stacks
kmRaster <- rast(paste0(raster,'Analysis/Kmeans/combined/kmeansStack05.tif'))
rfRaster <- rast(paste0(raster,'Analysis/RandomForest/combined/rfStack.tif'))

r <- c(classRaster, kmRaster[[1]],rfRaster[[1]])
names(r) <- c('class','training','kmeans','randomforest')
rm(classRaster,kmRaster, rfRaster)


r <- mask(r, r$kmeans)

# create dataframe from raster stack
df <- as.data.frame(r[[c('kmeans','randomforest')]], cell=FALSE, na.rm=TRUE)

df$kmean <- ''
df[df$kmeans==1,]$kmean <- 'cropland'
df[df$kmeans==2,]$kmean <- 'bareland'
df[df$kmeans==3,]$kmean <- 'mixed'
df[df$kmeans==4,]$kmean <- 'forest'
df[df$kmeans==5,]$kmean <- 'water'
df <- df[,c('kmean','randomforest')]

for (i in 1:length(names(df))){
  df[,i] <- as.factor(df[,i])
}

df <- df[df$kmean != 'mixed',]

kmeanVrf <- confusionMatrix(df$kmean,df$randomforest)

# confusion matrix table
conftb <- data.frame(cbind(c('kmeans - observed',rep(NA,5),
                             'random forest - observed',rep(NA,5),
                             'kmeans - random forest', rep(NA,5)) ,
  rbind(NA,kmeansVclass$table,NA,rfVclass$table,NA,kmeanVrf$table)))
row.names(conftb) <- NULL
conftb$`landcover type` <- rep(c(NA,'bareland','cropland','forest','urban','water'),3)

conftb <- conftb[,c("V1","landcover type", "bareland", "cropland", 
                    "forest", "urban", "water")]
names(conftb) <- c("Prediction v Reference","Landcover Type", "bareland", 
                   "cropland", "forest", "urban", "water")

# confusion matrix overall statistics
overalltb <- data.frame(
  cbind(c('kmeans - observed','random forest - observed','kmeans - random forest'),
        rbind(kmeansVclass$overall,rfVclass$overall,kmeanVrf$overall)))
overalltb <- overalltb[,c("V1", "Kappa","Accuracy","AccuracyLower","AccuracyUpper",
                          "AccuracyNull","AccuracyPValue","McnemarPValue")]
names(overalltb) <- c("Prediction v Reference", "Kappa","Accuracy","AccuracyLower","AccuracyUpper",
                      "AccuracyNull","AccuracyPValue","McnemarPValue")

# confusion matrix statistics by class
byclasstb <- data.frame(cbind(c('kmeans - observed',rep(NA,5),
                             'random forest - observed',rep(NA,5),
                             'kmeans - random forest', rep(NA,5)) ,
                           rbind(NA,kmeansVclass$byClass,NA,rfVclass$byClass,NA,kmeanVrf$byClass)))
row.names(byclasstb) <- NULL
byclasstb$`landcover type` <- rep(c(NA,'bareland','cropland','forest','urban','water'),3)


byclasstb <- byclasstb[,c("V1","landcover type","Sensitivity","Specificity",
                          "Pos.Pred.Value","Neg.Pred.Value","Precision", 
                          "Recall","F1","Prevalence","Detection.Rate", 
                          "Detection.Prevalence","Balanced.Accuracy")
]
names(byclasstb) <- c("Prediction v Reference","landcover type","Sensitivity","Specificity",
                      "Positive Predictions","Negative Predictions","Precision", 
                      "Recall","F1 Score","Prevalence","Detection Rate", 
                      "Detection Prevalence","Balanced Accuracy")

write.csv(conftb, '~/Documents/Projects/USACE/ML Mesohabitats/Data/Tables/comparisonConfusionTable.csv',
          row.names = FALSE)

write.csv(overalltb, '~/Documents/Projects/USACE/ML Mesohabitats/Data/Tables/comparisonConfusionTable_overall.csv',
          row.names = FALSE)

write.csv(byclasstb, '~/Documents/Projects/USACE/ML Mesohabitats/Data/Tables/comparisonConfusionTable_byclass.csv',
          row.names = FALSE)


