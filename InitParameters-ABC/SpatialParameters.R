#### POPULATIONS PARAMETERS

############# REACHES ############
library(readxl)
library(sf)
#ReachesArea<-read.table("spatial/Reaches.txt",header = TRUE)
#reaches_Scorff <- read_excel("spatial/reaches_Scorff_single.xlsx")
reaches_Scorff <- read_excel("spatial/reaches_Scorff_22reaches.xlsx")

# Function to convert input string into a list of coordinates
parse_coordinates <- function(input_string) {
  # Remove curly braces and split on semicolons to get individual coordinate sets
  coord_strings <- unlist(strsplit(gsub("[{}]", "", input_string), ";"))
  
  # Parse each coordinate set and convert it to numeric values
  coords_list <- lapply(coord_strings, function(coord_str) {
    as.numeric(unlist(strsplit(gsub("[()]", "", coord_str), ",")))
  })
  
  return(coords_list)
}
reaches_Scorff$coordinates_UTM <- NA
# Reach to convert
for (reach in 1:nrow(reaches_Scorff)) {
  input_coords <- na.omit(reaches_Scorff$coordinates[reach])
  
  # Parse the coordinates
  coords_list <- parse_coordinates(input_coords)
  
  # Extract lon and lat vectors
  lon <- sapply(coords_list, function(coord) coord[1])
  lat <- sapply(coords_list, function(coord) coord[2])
  
  # Create a data frame
  coords <- data.frame(lon = lon, lat = lat)
  
  #check if duplicates
  if (any(duplicated(coords))) {
    cat(paste0(c("duplicate in coordinates of reach", reach)))
  }
  
  # Convert to an sf object with WGS84 CRS (EPSG:4326)
  sf_coords <- st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
  
  # Transform the coordinates to UTM (Zone 30N)
  utm_coords <- st_transform(sf_coords, crs = 32630)
  
  # Extract UTM coordinates
  utm_matrix <- st_coordinates(utm_coords)
  
  # Reformat the output as desired, in the form {(X,X,X);(X,X,X)}
  formatted_output <- paste0("{", paste0("(", 
                                         round(utm_matrix[,1], 2), ",", 
                                         round(utm_matrix[,2], 2), ",0)", collapse = ";"), "}")
  
  reaches_Scorff$coordinates_UTM[reach] <- formatted_output
}

ReachesArea<- reaches_Scorff#read.table("spatial/Reaches.txt",header = TRUE)
ID <- ReachesArea$ID
fatherID <- ReachesArea$fatherID
riverID <- ReachesArea$riverID
order <- ReachesArea$order
module <- ReachesArea$module
quality <- ReachesArea$quality
altitude <- ReachesArea$altitude
length <- ReachesArea$length
width <- ReachesArea$meanWidth
#fR_1SW <- ReachesArea$fishingRate_1SW
#fR_MSW <- ReachesArea$fishingRate_MSW
coord <- ReachesArea$coordinates_UTM
n_reaches = table(ReachesArea$riverID) # NB: 1st reach is estuary
# Simulate a proportion of the total habitat area observed
PropReachesArea <- prop * ReachesArea$habitatArea

############# RIVERS ############
Rivers<-read.table("spatial/Rivers.txt",header = TRUE)
ID_river <- Rivers$ID
name <- Rivers$name
coord_river <- sapply(reaches_Scorff$coordinates_UTM, function(x) strsplit(gsub("[{}]", "", x), ";")[[1]][1]) #Rivers$coordinates
distance <- Rivers$distanceToTheOtherRivers
n_rivers = max(ID_river)

# Simulate a proportion of the total habitat area observed
PropRiversArea <- prop * Rivers$habitatArea.m2.

############# WEIRS ############

#Weirs<-read.table("spatial/Weirs.txt",header = TRUE)
#weirs_Scorff <- read_excel("spatial/weirs_Scorff_single.xlsx")
weirs_Scorff <- read_excel("spatial/weirs_Scorff_22reaches.xlsx")

weirs_Scorff$coordinates_UTM <- NA
# Reach to convert
for (reach in 1:nrow(weirs_Scorff)) {
  input_coords <- na.omit(weirs_Scorff$coordinates[reach])
  
  #check if duplicates
  if (any(duplicated(input_coords))) {
    cat("duplicate in coordinates")
  }
  
  # Parse the coordinates
  coords_list <- parse_coordinates(input_coords)
  
  # Extract lon and lat vectors
  lon <- sapply(coords_list, function(coord) coord[1])
  lat <- sapply(coords_list, function(coord) coord[2])
  
  # Create a data frame
  coords <- data.frame(lon = lon, lat = lat)
  
  # Convert to an sf object with WGS84 CRS (EPSG:4326)
  sf_coords <- st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
  
  # Transform the coordinates to UTM (Zone 30N)
  utm_coords <- st_transform(sf_coords, crs = 32630)
  
  # Extract UTM coordinates
  utm_matrix <- st_coordinates(utm_coords)
  
  # Reformat the output as desired, in the form {(X,X,X);(X,X,X)}
  formatted_output <- paste0(paste0("(", 
                                    round(utm_matrix[,1], 2), ",", 
                                    round(utm_matrix[,2], 2), ",0)", collapse = ";"))
  
  weirs_Scorff$coordinates_UTM[reach] <- formatted_output
}

Weirs<- weirs_Scorff #read.table("spatial/Weirs.txt",header = TRUE)
ID_weir <- Weirs$ID
fatherID_weir <- Weirs$fatherID
riverID_weir <- Weirs$riverID
upstreamRate <- Weirs$upstreamRate
downstreamRate <- Weirs$downstreamRate
coord_weir <- Weirs$coordinates_UTM
n_weirs = nrow(Weirs)



############# SEA REGIONS ############

SeaRegions<-read.table("spatial/SeaRegions.txt",header = TRUE)
ID_searegion <- SeaRegions$ID
name_searegion <- SeaRegions$name
coord_searegion <- SeaRegions$coordinates
n_searegions = nrow(SeaRegions)
