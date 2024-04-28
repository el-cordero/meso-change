
km_knn_raster <- function(r,n_bands,model_date,optimal_k=6,na_rm=TRUE){
    # convert the raster to a data.frame
    nr <- as.data.frame(r, cell=TRUE, na.rm=na_rm)

    # Create 15 clusters, allow 500 iterations, 
    # 15 clusters based off the elbow method
    # start with 50 random sets using "Hartigan-Wong" method. 
    # Do not use the first column (cell number).
    kmncluster <- kmeans(nr[,-1], centers=optimal_k, iter.max = 500, 
                        nstart = 50, algorithm="Hartigan-Wong")


    kmr <- rast(r, nlyr=1)
    kmr[nr$cell] <- kmncluster$cluster

    print(paste0('baseline km_raster processed'))

    # Use the training raster object to set the cluster values to a new raster
    # with the same amount of layers as there are rasters
    knr <- rast(r,nlyr=nlyr(r)/n_bands)

    # Apply the same kmeans model to the other rasters
    # starting at the second raster
    for (i in 1:nlyr(knr)){
        # convert the raster into a dataframe
        nr2 <- nr[,c(1,(1:n_bands)+(n_bands*(i-1))+1)]
        kmnc2 <- kmncluster$center[,(1:n_bands)+(n_bands*(i-1))]
        
        # apply the model to the test raster data
        pred.knn <- FNN::get.knnx(data=kmnc2, 
                                    query=nr2[,-1], 
                                    k=1)
        pred.knn <- pred.knn$nn.index[,1]
        
        # set cluster values to the raster layer
        layerName <- paste0("lyr",i)
        knr[[layerName]][nr2$cell] <- pred.knn

        # remove testing rasters
        rm(nr2)
        
        print(paste0('km_raster',i,' processed'))
    }

    # set the layer names for the raster stack
    names(knr) <- dates
    names(kmr) <- 'baseline'

    kmeansRaster <- c(kmr,knr)

    return(kmeansRaster)
}
