#### POPULATIONS PARAMETERS

############# RIVERS ############
Rivers<-read.table("spatial/Rivers.txt",header = TRUE)
ID_river <- Rivers$ID
name <- Rivers$name
coord_river <- Rivers$coordinates
distance <- Rivers$distanceToTheOtherRivers
n_rivers = max(ID_river)

# Simulate a proportion of the total habitat area observed
PropRiversArea <- prop * Rivers$habitatArea.m2.

############# WEIRS ############

Weirs<-read.table("spatial/Weirs.txt",header = TRUE)
ID_weir <- Weirs$ID
fatherID_weir <- Weirs$fatherID
riverID_weir <- Weirs$riverID
upstreamRate <- Weirs$upstreamRate
downstreamRate <- Weirs$downstreamRate
coord_weir <- Weirs$coordinates
n_weirs = nrow(Weirs)

############# REACHES ############

ReachesArea<-read.table("spatial/Reaches.txt",header = TRUE)
ID <- ReachesArea$ID
fatherID <- ReachesArea$fatherID
riverID <- ReachesArea$riverID
order <- ReachesArea$Order
module <- ReachesArea$module.m3.s.
length <- ReachesArea$length.m.
width <- ReachesArea$meanWidth.m.
#fR_1SW <- ReachesArea$fishingRate_1SW
#fR_MSW <- ReachesArea$fishingRate_MSW
coord <- ReachesArea$coordinates
n_reaches = table(ReachesArea$riverID) # NB: 1st reach is estuary

# Simulate a proportion of the total habitat area observed
PropReachesArea <- prop * ReachesArea$habitatArea.m2.

 
############# SEA REGIONS ############

SeaRegions<-read.table("spatial/SeaRegions.txt",header = TRUE)
ID_searegion <- SeaRegions$ID
name_searegion <- SeaRegions$name
coord_searegion <- SeaRegions$coordinates
n_searegions = nrow(SeaRegions)
