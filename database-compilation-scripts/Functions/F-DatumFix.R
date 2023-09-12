# Function to fix and GPS coordinates not in CRS 4269
library(sf)
library(tidyverse)

DatumFix <- function(dat) {
  df <- dat
  final <- tibble()
  vec <- unique(df$HorizontalCoordinateReferenceSystemDatumName)
  
  for(i in vec) {
    dat <- df %>%
      filter(HorizontalCoordinateReferenceSystemDatumName == i)
    assign(paste0("df","_",i), dat)
    remove(dat)
  }
  
  if(exists("df_NAD83")){
    df_NAD83 <- df_NAD83 %>%
      st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4269)
    df_NAD83 <- df_NAD83 %>%
      mutate(long = unlist(map(geometry,1)),
             lat = unlist(map(geometry,2)))
    df_NAD83 <- st_drop_geometry(df_NAD83)
    final <- bind_rows(final, df_NAD83)
    remove(df_NAD83)
  }
  
  if(exists("df_WGS84")){
    df_WGS84 <- df_WGS84 %>%
      st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4326)
    df_WGS84 <- st_transform(df_WGS84, 4269)
    df_WGS84 <- df_WGS84 %>%
      mutate(long = unlist(map(geometry,1)),
             lat = unlist(map(geometry,2)))
    df_WGS84 <- st_drop_geometry(df_WGS84)
    final <- rbind(final, df_WGS84)
    remove(df_WGS84)
  }
  
  if(exists("df_NAD27")){
    df_NAD27 <- df_NAD27 %>%
      st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4267)
    df_NAD27 <- st_transform(df_NAD27, 4269)
    df_NAD27 <- df_NAD27 %>%
      mutate(long = unlist(map(geometry,1)),
             lat = unlist(map(geometry,2)))
    df_NAD27 <- st_drop_geometry(df_NAD27)
    final <- rbind(final, df_NAD27)
    remove(df_NAD27)
  }
  
  if(exists("df_UNKWN")){
    df_UNKWN <- df_UNKWN %>%
      st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4269)
    df_UNKWN <- df_UNKWN %>%
      mutate(long = unlist(map(geometry,1)),
             lat = unlist(map(geometry,2)))
    df_UNKWN <- st_drop_geometry(df_UNKWN)
    final <- bind_rows(final, df_UNKWN)
    remove(df_UNKWN)
  }
  
  if(exists("df_UNKWN")){
    df_OTHER <- df_OTHER %>%
      st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4269)
    df_OTHER <- df_OTHER %>%
      mutate(long = unlist(map(geometry,1)),
             lat = unlist(map(geometry,2)))
    df_OTHER <- st_drop_geometry(df_OTHER)
    final <- bind_rows(final, df_OTHER)
    remove(df_OTHER)
  }
  
  if(exists("df_HARN")){
    df_HARN <- df_HARN %>%
      st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4152)
    df_HARN <- st_transform(df_HARN, 4269)
    df_HARN <- df_HARN %>%
      mutate(long = unlist(map(geometry,1)),
             lat = unlist(map(geometry,2)))
    df_HARN <- st_drop_geometry(df_HARN)
    final <- bind_rows(final, df_HARN)
    remove(df_HARN)
  }
  
  if(exists("df_Unknown")){
    df_Unknown <- df_Unknown %>%
      st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4269)
    df_Unknown <- df_Unknown %>%
      mutate(long = unlist(map(geometry,1)),
             lat = unlist(map(geometry,2)))
    df_Unknown <- st_drop_geometry(df_Unknown)
    final <- bind_rows(final, df_Unknown)
    remove(df_Unknown)
  }
  
  if(exists("df_WGS72")){
    df_WGS72 <- df_WGS72 %>%
      st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4322)
    df_WGS72 <- st_transform(df_WGS72, 4269)
    df_WGS72 <- df_WGS72 %>%
      mutate(long = unlist(map(geometry,1)),
             lat = unlist(map(geometry,2)))
    df_WGS72 <- st_drop_geometry(df_WGS72)
    final <- bind_rows(final, df_WGS72)
    remove(df_WGS72)
  }
  
  if(exists("df_GUAM")){
    df_GUAM <- df_GUAM %>%
      st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4675)
    df_GUAM <- st_transform(df_GUAM, 4269)
    df_GUAM <- df_GUAM %>%
      mutate(long = unlist(map(geometry,1)),
             lat = unlist(map(geometry,2)))
    df_GUAM <- st_drop_geometry(df_GUAM)
    final <- bind_rows(final, df_GUAM)
    remove(df_GUAM)
  }
  
  if(exists("df_OLDHI")){
    df_OLDHI <- df_OLDHI %>%
      st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 3562)
    df_OLDHI <- st_transform(df_OLDHI, 4269)
    df_OLDHI <- df_OLDHI %>%
      mutate(long = unlist(map(geometry,1)),
             lat = unlist(map(geometry,2)))
    df_OLDHI <- st_drop_geometry(df_OLDHI)
    final <- bind_rows(final, df_OLDHI)
    remove(df_OLDHI)
  }
  
  if(exists("df_PR")){
    df_PR <- df_PR %>%
      st_as_sf(coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4139)
    df_PR <- st_transform(df_PR, 4269)
    df_PR <- df_PR %>%
      mutate(long = unlist(map(geometry,1)),
             lat = unlist(map(geometry,2)))
    df_PR <- st_drop_geometry(df_PR)
    final <- bind_rows(final, df_PR)
    remove(df_PR)
  }
  
  return(final)
}
