librarian::shelf(tidyverse, ggmap, raster, rgdal, rmapshaper, 
                 broom, gpclib, viridis, sf, ggspatial, rgeos)
#install.packages("gpclib", type = "source")
# https://kuduz.tistory.com/1042 
# http://www.gisdeveloper.co.kr/?p=2332

### Make proper geocode ----
### prj 파일이 있어야 한다! 

korea_1 <- st_read(here::here('data', 'CTPRVN_201602', 'TL_SCCO_CTPRVN.shp'))
korea_2 <- st_read(here::here('data', 'SIG_201602', 'TL_SCCO_SIG.shp'))
#korea_3 <- st_read(here::here('data', 'EMD_201602', 'TL_SCCO_EMD.shp'))
#korea_4 <- st_read(here::here('data', 'LI_201602', 'TL_SCCO_LI.shp'))

cp_2_utf8 <- function(col){
  iconv(col, from = "CP949", to = "UTF-8", sub = NA, mark = TRUE, toRaw = FALSE)
}
change_enc <- function(var_name, df){
#  
  enq_var_name <- enquo(var_name)
  df %>% 
    mutate(!!enq_var_name := cp_2_utf8(!!enq_var_name))
#  
}
gen_uppercode <- function(var1, var2, df1, df2, cut_from_right){
  enq_1 <- enquo(var1)
  enq_2 <- enquo(var2)
  chr_1 <- deparse(substitute(var1))
  df1$geometry <- NULL 
  df2 %>% 
    mutate(
      !!enq_1 := str_extract(!!enq_2, paste0("^.{",cut_from_right,"}"))
    ) %>% 
    left_join(
      df1, by = chr_1
    )
}

label_1 <- change_enc(CTP_KOR_NM, korea_1)
label_2 <- change_enc(SIG_KOR_NM, korea_2)
#label_3 <- change_enc(EMD_KOR_NM, korea_3)
#label_4 <- change_enc(LI_KOR_NM, korea_4)

df_view <- label_1
df_view$geometry <- NULL 
df_view %>% View(.)

gen_uppercode(CTPRVN_CD, SIG_CD, label_1, label_2, 2) %>% 
  mutate(
    kor_name = str_remove(paste0(CTP_KOR_NM, SIG_KOR_NM), " ")
  ) -> df_sf0


### Prepare sample_shp files 
df_sf <- ms_simplify(df_sf0, keep = 0.0025, keep_shapes = T)
st_crs(df_sf) 
df_sf <- st_transform(df_sf, 3857)

saveRDS(df_sf0, here::here('data', 'df_sf0.rds')) 
saveRDS(df_sf, here::here('data', 'df_sf.rds')) 

### Read rds ----
readRDS(here::here('data', 'LQEI.rds')) %>% 
  mutate(
    kor_name = if_else(si_do_1==si_do_2, 
                       paste0(si_do_1, gu_gun), 
                       paste0(si_do_1, si_do_2, gu_gun))
  ) %>% 
  mutate(
    index_jobqual = as.numeric(index_jobqual)
  )-> df2 

df_sf %>% 
  left_join(df2, by = c("kor_name")) %>% 
  filter(year == 2015) -> df_sf2

### Gen map from Google 

my_key <- 'AIzaSyA-8v4SGqGyLAGxBOK8-hhWvUe_ove00-w'
register_google(key = my_key)
map <- get_map(location='south korea', zoom=7, maptype='roadmap', color='bw')
ggmap(map)  + labs(x = NULL, y = NULL) # Check size 
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
map <- ggmap_bbox(map)

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) +
  geom_sf(data = df_sf2, 
          aes(fill = index_jobqual), alpha=0.3, inherit.aes = FALSE) + 
  scale_fill_viridis(direction=-1) + 
  theme_void() + labs(fill = "JQ index")

my_key <- 'AIzaSyA-8v4SGqGyLAGxBOK8-hhWvUe_ove00-w'
register_google(key = my_key)
map <- get_map(location='seoul', zoom=11, maptype='roadmap', color='bw')
ggmap(map) + labs(x = NULL, y = NULL) # Check size 
map <- ggmap_bbox(map)

ggmap(map) + 
  coord_sf(crs = st_crs(3857)) +
  geom_sf(data = df_sf2 %>% filter(si_do_1 %in% c('서울특별시')), 
          aes(fill = index_jobqual), alpha=0.3, inherit.aes = FALSE) + 
  scale_fill_viridis(direction=-1) + 
  theme_void() + labs(fill = "JQ index")


saveRDS(sf_df2, here::here('data', 'sf_df2.rds'))
