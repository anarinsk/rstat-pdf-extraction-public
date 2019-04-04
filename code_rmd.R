librarian::shelf(here, ggridges,
                 tidyverse, magrittr, 
                 stringr, knitr, kableExtra, 
                 scales, 
                 tidyverse/lubridate,  
                 thomasp85/patchwork,
                 janitor, 
                 rmdformats, rstudio/fontawesome, 
                 #update_all = TRUE, 
                 quiet = TRUE)


librarian::shelf(ggmap, raster, rgdal, rmapshaper, 
                 broom, gpclib, viridis, sf, ggspatial, rgeos)


readRDS(here::here('data', 'LQEI.rds')) %>% 
  mutate(
    kor_name = if_else(si_do_1==si_do_2, 
                       paste0(si_do_1, gu_gun), 
                       paste0(si_do_1, si_do_2, gu_gun))
  ) %>% 
  mutate(
    index_jobqual = as.numeric(index_jobqual)
  )-> df2 

readRDS(here::here('data', 'df_sf.rds')) -> df_sf

my_key <- 'Put-your-own-googlemap-api'
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}
