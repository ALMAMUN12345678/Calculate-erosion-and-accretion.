############# Area Shape file ###########################################
getwd() # Set working directory
setwd('E:/........................................')

shapefile_dir <- "E:/..................................." ## My folder location 

area <- list.files(shapefile_dir, pattern = "\\.shp$", full.names = TRUE) ## List of my shapefiles
area <- sort(area) ## Sort it on the basis of year

# Read all shapefiles regarding given locations: lapply(work like as a loop)
hatiya_area <- lapply(area, function(file) {
  tryCatch({
    st_read(file)
  }, error = function(e) {
    warning(paste("Error reading file:", file, ":", e$message)) ## This is for error handling
    NULL
  })
})


######################### Intersection file ##########################################
getwd()
setwd('E:/...........')

shapefile_dir <- "E:/..................." 

area_int <- list.files(shapefile_dir, pattern = "\\.shp$", full.names = TRUE)
area_int <- sort(area_int)

hatiya_area_int <- lapply(area_int, function(file) {
  tryCatch({
    st_read(file)
  }, error = function(e) {
    warning(paste("Error reading file:", file, ":", e$message))
    NULL
  })
})


######################## final #########################################

erosion <- list() # create a blank list for erosion value
accretion <- list() # create a blank list for accretion value
p <- list() # this list for all graph
erosion_areas <- numeric(length(area) - 1) # create a column for erosion data where all value return zero.
accretion_areas <- numeric(length(area) - 1) # create a column for accretion data where all value return zero.
years <- character(length(area) - 1) # This is for year column. it return a character value

# In my dataset there were no geometrical difference. If our dataset have geometrical differecen then
# use st_transformation(,crs=)
for (i in 1:(length(area) - 1)) {
  erosion[[i]] <- st_difference(hatiya_area[[i]], hatiya_area_int[[i]]) # Geometrical difference between intersection file and 1991 shapefile.
  accretion[[i]] <- st_difference(hatiya_area[[i + 1]], hatiya_area_int[[i]]) # Geometrical difference between intersection file and 1992 shapefile.
  
  erosion_areas[i] <- sum(st_area(erosion[[i]])) # This line of code for calculate ersion area each year
  accretion_areas[i] <- sum(st_area(accretion[[i]])) # This line of code for calculate accretion area each year
  
  base_name_i <- tools::file_path_sans_ext(basename(area[[i]])) # Here I write code for the basename of shapefile like as 1991.
  base_name_next <- tools::file_path_sans_ext(basename(area[[i + 1]])) # Here I write code for the basename of shapefile like as 1992.
  years[i] <- paste0(base_name_i, " - ", base_name_next) # this code for year column, 199-1992
  
  # Plot erosion and accretion areas. we plot every erosion and accretion file and put it into p list.
  p[[i]] <- ggplot() +
    annotation_map_tile(type = "osm", progress = "none")+ # use for mapview
    geom_sf(data = erosion[[i]], aes(fill = 'Area'),color=NA) +
    geom_sf(data = accretion[[i]], aes(fill = 'Area1'),color=NA) +
    scale_fill_manual(values = c('Area' = 'red', 'Area1' = '#001d3d'))+
    labs(x = "Longitude", y = "Latitude",title = years[i]) +
    theme_bw() +
    theme(legend.position = "none") +
    theme(axis.text.x = element_text(angle =90,hjust =1,vjust=1,color = "black",size = 12,face ="plain"))+
    theme(axis.text.y = element_text(colour = "black",face ="plain",size = 12,vjust =0.5,hjust =1))+
    theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold",color = "black"),
          legend.title = element_text(face = "plain", size = 14),
          legend.text = element_text(color = "black", size = 14))+
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 12, face = "bold", margin = margin(t = 10)))
  
  #print(p[[i]])
}

combined_plot <- ggarrange(plotlist = p, ncol =3, nrow = 3) ## this code use for multiple graph in one page
print(combined_plot)

## Create a dataframe using erosion and accretion value
erosion_data <- data.frame(
  Year = as.factor(years),
  Erosion_Area = erosion_areas,
  Accretion_Area = accretion_areas
)

# convert it into km^2
erosion_data$Erosion_Area= round((erosion_data$Erosion_Area/1000000),2)
erosion_data$Accretion_Area= round((erosion_data$Accretion_Area/1000000),2)

# Line plot for erosion
erosion_plot <- ggplot(erosion_data, aes(x = Year, y = factor(Erosion_Area), group = 1)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(x = "Year", y = expression(paste("Erosion","(",km^-2,")")), title = "Erosion Area Over Year") +
  theme_bw() +
  theme(axis.text.x = element_text(angle =90,hjust =1,vjust=0.5,color = "black",size = 12,face ="plain"))+
  theme(axis.text.y = element_text(colour = "black",face ="plain",size = 12,vjust =0.5,hjust =1))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold",color = "black"),
        legend.title = element_text(face = "plain", size = 14),
        legend.text = element_text(color = "black", size = 14))+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 12, face = "bold", margin = margin(t = 10)))


# Line plot for accretion
accretion_plot <- ggplot(erosion_data, aes(x = Year, y = factor(Accretion_Area), group = 1)) +
  geom_line(color = "green", size = 1) +
  geom_point(color = "green", size = 2) +
  labs(x = "Year", y = expression(paste("Accretion","(",km^-2,")")), title = "Accretion Area Over Year") +
  theme_bw() +
  theme(axis.text.x = element_text(angle =90,hjust =1,vjust=0.5,color = "black",size = 12,face ="plain"))+
  theme(axis.text.y = element_text(colour = "black",face ="plain",size = 12,vjust =0.5,hjust =1))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold",color = "black"),
        legend.title = element_text(face = "plain", size = 14),
        legend.text = element_text(color = "black", size = 14))+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 12, face = "bold", margin = margin(t = 10)))


print(erosion_plot)
print(accretion_plot)






###################### Combine plot #############################################

all_erosion <- list()
all_accretion <- list()


for (i in 1:(length(area) - 1)) {
  # Perform for geometric differences
  erosion<- st_difference(hatiya_area[[i]], hatiya_area_int[[i]])
  accretion <- st_difference(hatiya_area[[i + 1]], hatiya_area_int[[i]])
  
  # This code for the append of list
  all_erosion <- c(all_erosion, list(erosion))
  all_accretion <- c(all_accretion, list(accretion))
}

# Megre all erosion and accretion value.
combined_erosion <- do.call(st_union, all_erosion) ## do.call() use for the call a function
combined_accretion <- do.call(st_union, all_accretion)


combined_plot <- ggplot() +annotation_map_tile(type = "osm", progress = "none")+
  geom_sf(data = combined_accretion, aes(fill = 'Accretion'), color = NA) +
  geom_sf(data = combined_erosion, aes(fill = 'Erosion'), color = NA) +
  scale_fill_manual(values = c('Erosion' = 'red', 'Accretion' = '#001d3d')) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() + theme(axis.text.x = element_text(angle =90,hjust =1,vjust=1,color = "black",size = 12,face ="plain"))+
  theme(axis.text.y = element_text(colour = "black",face ="plain",size = 12,vjust =0.5,hjust =1))+
  theme(axis.text=element_text(size=12),axis.title=element_text(size=12,face="bold",color = "black"),
        legend.title = element_text(face = "plain", size = 14),
        legend.text = element_text(color = "black", size = 14))+
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = margin(t = 10)))  # Centered title with margin
t
print(combined_plot)

st_area(combined_accretion)
st_area(combined_erosion)









