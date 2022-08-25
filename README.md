# SOM

This contains Scottish multiple deprivation index (SIMD) of Edinburgh in 2016 and a neural network technology to use Self-organizing map (SOM) to realize the conditionality reduction of Scottish multiple deprivation index.

## SOM.R

It includes the conversion of data format, weight setting of differnet indexes, training of SOM, and visualization of final product.

The data should be input as:

    ed_map  <- readOGR（“ SG_SIMD_2016_EDINBURGH.shp ”，stringAsFactors  =  FALSE）
    
The result should be saved as:

    writeOGR(obj=ed_map, dsn="edinburgh_cluster", layer="edinburgh_map_clustered", driver="ESRI Shapefile")
    
    
