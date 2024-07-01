####A function to extract EXIF Metadata to obtain x,y, and z coordinates, date and time stamps, and RTK accuracy
##This is for images captured using DJI drones (e.g Phantom 4 RTK and/ Phantom 4 Multispectral)
## Script developed by Alan Nare and Andrew Cunliffe 2024

# Load necessary libraries----
library(exifr)

# Define the absolute path where images are located
directory_path <- ("C:/Users/202200875/OneDrive - buan.ac.bw/Documents/Drone research/Data/GCPs")

###Create function

extract.GCP.metadata <- function(directory_path, height_offset = 0) {
  # Extract directory name to use as the name for the output csv
  directory_name <- basename(directory_path)
  
  # Import image files
  files <- list.files(directory_path, full.names = TRUE)
  
  # Read EXIF metadata for each image
  metadata <- lapply(files, read_exif)
  
  # Extract x, y, z coordinates, GNSS-derived datetime stamp, and RTK data
  xy_coords <- lapply(metadata, function(x) data.frame(
    latitude = x$GPSLatitude,
    longitude = x$GPSLongitude,
    altitude = x$GPSAltitude - height_offset,
    datetime_stamp = x$DateTimeOriginal,
    RtkFlag = x$RtkFlag,
    RtkSrcType = x$RtkSrcType,
    RtkStdLon = x$RtkStdLon,
    RtkStdLat = x$RtkStdLat,
    RtkStdHgt = x$RtkStdHgt
  ))
  
  # Combine metadata into a single data frame
  combined_metadata <- do.call(rbind, xy_coords)
  
  # Define CRS of coordinates
  crs <- "EPSG:4326"        ####EPSG:4326 is a coordinate reference system (CRS)
                             #used to define geographic coordinates based on the 
                            #WGS84 (World Geodetic System 1984) datum. It specifies 
                            #locations using latitude and longitude in degrees. It is usually
                            #the default CRS for most software and systems
  # Insert a new row specifying CRS
  combined_metadata <- rbind(
    data.frame(
      latitude = "CRS",
      longitude = crs,
      altitude = NA,
      datetime_stamp = NA,
      RtkFlag = NA,
      RtkSrcType = NA,
      RtkStdLon = NA,
      RtkStdLat = NA,
      RtkStdHgt = NA
    ),
    combined_metadata
  )
  
  # Save metadata as CSV file
  write.csv(combined_metadata, file.path(directory_path, paste0(directory_name, "_GCP_data.csv")), row.names = FALSE)
}

# Call the function for each Area of Interest (AOI) folder
extract.GCP.metadata("C:/Users/202200875/OneDrive - buan.ac.bw/Documents/Drone research/Data/GCPs/AOI1", height_offset = 1)
extract.GCP.metadata("C:/Users/202200875/OneDrive - buan.ac.bw/Documents/Drone research/Data/GCPs/AOI2", height_offset = 1)
extract.GCP.metadata("C:/Users/202200875/OneDrive - buan.ac.bw/Documents/Drone research/Data/GCPs/AOI3", height_offset = 1)
