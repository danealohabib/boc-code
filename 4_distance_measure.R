rm(list = ls())
cat("\014")  
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

#Set the list of packages required
packageList = c("spatstat", "parallel",
                "pbmcapply","data.table","pbapply", 
                "sp", "rgdal","raster", "rgeos", 
                "maptools", "spdep", "RColorBrewer", 
                "stargazer","splm", "plm", "pander", "parallel", "pbapply", "DescTools",
                "RANN", "geosphere")

#Function to test of a package is installed
newPackages = packageList[!(packageList %in% installed.packages()[,"Package"])]
if(length(newPackages)){
  install.packages(newPackages)
} 

#Install all packages that are not already installed
lapply(packageList, library, character.only = TRUE)

#Import Data
branch_data = read.csv("../data/branch_locations_2008_2018_RAW.csv", header = TRUE, sep = ",", fill = TRUE)
branch_data = branch_data[which(branch_data$duplicateFlag == 0),]

#Import Shapefile
ogrInfo("../data/shape_fsa_2011","gfsa000b11a_e")
shp_2011 = readOGR("../data/shape_fsa_2011","gfsa000b11a_e")

ogrInfo("../data/shape_fsa_2016","lfsa000b16a_e")
shp_2016 = readOGR("../data/shape_fsa_2016","lfsa000b16a_e")
shp_2016 = spTransform(shp_2016, proj4string(shp_2011))

closest_branch_distance <- function(fsa, year, data, shapefile){
  #### STEP 1 - SUBSET THE SHAPEFILE BY FSA
  shp_postal = shapefile[which(shapefile$CFSAUID == fsa),]
  if(length(shp_postal$CFSAUID) == 0){
    return(NA)
  } else {
  
  #### STEP 2 - DEVELOP THE GRID IN THE FSA (JUST USE THE DISTMAP INTERNAL GRID CALC)
  window = as.owin(shapefile[shapefile$CFSAUID==fsa,])
  ppp_frame = ppp(data[,"lng"],data[,"lat"],window=window)
  raw_grid = distmap(ppp_frame)
  
  #### STEP 3 - CONVERT THE RAW GRID TO A DATAFRAME OF GRID POINTS
  lng_points = raw_grid$xcol
  lat_points = raw_grid$yrow
  grid = expand.grid(lat_points, lng_points)
  
  #### STEP 4 - SUBSET THE BRANCH DATA BY FSA
  data_subset = branch_data[which(branch_data$Year == year),]
  data_subset_lat_lng = na.omit(data_subset[,c("lat", "lng")]) #This line retrieves only the lat/lng rows and drops all <NA> values
  row.names(data_subset_lat_lng) <- 1:nrow(data_subset_lat_lng) #This line renumbers the rows
  
  #### STEP 5 - COMPUTE THE NEAREST NEIGHBOURS
  nearest_neighbour = nn2(data = data_subset_lat_lng, query = grid , k = 1, treetype = "kd", searchtype = "standard")
  nn_df = cbind(grid, data_subset_lat_lng[nearest_neighbour$nn.idx,])
  colnames(nn_df) = c("grid_lat","grid_lng", "branch_lat", "branch_lng")
  row.names(nn_df) <- 1:nrow(nn_df) 
  nn_df_shp = nn_df
  
  #### STEP 6 - CHECK IF GRID IS INSIDE FSA
  coordinates(nn_df_shp) = ~ grid_lng + grid_lat
  proj4string(nn_df_shp) <- proj4string(shp_postal)
  locations_df = over(nn_df_shp, shp_postal)
  all_data = cbind(nn_df,locations_df)
  refined_grid = na.omit(all_data)
  
  #### STEP7 - CALCULATE THE AVERAGE HAVERSINE DISTANCE (IN KM)
  hav_distance = distHaversine(refined_grid[,c("grid_lng", "grid_lat")], refined_grid[,c("branch_lng", "branch_lat")], r=6378137)/1000
  avg_hav = mean(hav_distance)
  
  return(avg_hav)
  
  }
}

fsa_list_2011 = as.character(shp_2011$CFSAUID) 
fsa_list_2016 = as.character(shp_2016$CFSAUID) 
fsa_list = union(fsa_list_2011,fsa_list_2016)

year_list = seq(2008,2018,1)

#Parallel Implementation
cores = detectCores()
coresAvailiable = cores - 2
cl = makeCluster(coresAvailiable)
clusterEvalQ(cl,{
  
  packageList = c("spatstat", "parallel",
                  "pbmcapply","data.table","pbapply", 
                  "sp", "rgdal","raster", "rgeos", 
                  "maptools", "spdep", "RColorBrewer", 
                  "stargazer","splm", "plm", "pander", "parallel", "pbapply", "DescTools",
                  "RANN", "geosphere")
  lapply(packageList, require, character.only = TRUE)
  sessionInfo()
  
})


clusterExport(cl, c("branch_data", "shp_2011", "shp_2016", "closest_branch_distance"))

results_df = data.frame(year=numeric(),
                        fsa = character(),
                        avg_dis_2011 = numeric(),
                        avg_dis_2016 = numeric())


for(i in year_list){
  clusterExport(cl,"i")
  year_rep = rep(i,length(fsa_list))
  distance_list_2011 = pblapply(fsa_list, function(x) closest_branch_distance(x,i,branch_data,shp_2011), cl=cl)
  distance_list_2016 = pblapply(fsa_list, function(x) closest_branch_distance(x,i,branch_data,shp_2016), cl=cl)
  
  distance_list_2011 = unlist(distance_list_2011)
  distance_list_2016 = unlist(distance_list_2016)
  
  data = as.data.frame(cbind(year_rep,fsa_list,distance_list_2011,distance_list_2016))
  
  results_df = rbind(results_df, data)
}

stopCluster(cl)

results_df = as.data.frame(results_df)
write.csv(results_df, "../data/distance_measure.csv",row.names = FALSE)


  
