librarian::shelf(ggmap, raster, rgdal, rmapshaper, 
                 broom, gpclib, viridis)
#install.packages("gpclib", type = "source")
# https://kuduz.tistory.com/1042 

### Make proper geocode ----
korea_1 <- shapefile(here::here('data', 'CTPRVN_201602', 'TL_SCCO_CTPRVN.shp'))
korea_2 <- shapefile(here::here('data', 'SIG_201602', 'TL_SCCO_SIG.shp'))
korea_3 <- shapefile(here::here('data', 'EMD_201602', 'TL_SCCO_EMD.shp'))
korea_4 <- shapefile(here::here('data', 'LI_201602', 'TL_SCCO_LI.shp'))

cp_2_utf8 <- function(col){
  iconv(col, from = "CP949", to = "UTF-8", sub = NA, mark = TRUE, toRaw = FALSE)
}
change_enc <- function(var_name, df){
#  
  enq_var_name <- enquo(var_name)
  df %>% pluck('data') -> df
  df %>% 
    mutate(!!enq_var_name := cp_2_utf8(!!enq_var_name))
#  
}
gen_uppercode <- function(var1, var2, df1, df2, cut_from_right){
  enq_1 <- enquo(var1)
  enq_2 <- enquo(var2)
  chr_1 <- deparse(substitute(var1))
  
  df2 %>% 
    mutate(
      !!enq_1 := str_extract(!!enq_2, paste0("^.{",cut_from_right,"}"))
    ) %>% 
    left_join(df1, by = chr_1)
}

label_1 <- change_enc(CTP_KOR_NM, korea_1)
label_2 <- change_enc(SIG_KOR_NM, korea_2)
label_3 <- change_enc(EMD_KOR_NM, korea_3)
label_4 <- change_enc(LI_KOR_NM, korea_4)

gen_uppercode(CTPRVN_CD, SIG_CD, label_1, label_2, 2) %>% 
  mutate(
    kor_name = str_remove(paste0(CTP_KOR_NM, SIG_KOR_NM), " ")
  ) %>% 
  select(SIG_CD, kor_name) -> df0

### Prepare sample_shp files 
kor_sample_shp <- ms_simplify(korea_2, keep = 0.005, keep_shapes = F)
tidy(kor_sample_shp, region = 'SIG_CD') -> df1

### Read rds ----
readRDS(here::here('data', 'LQEI.rds')) %>% 
  mutate(
    kor_name = if_else(si_do_1==si_do_2, 
                       paste0(si_do_1, gu_gun), 
                       paste0(si_do_1, si_do_2, gu_gun))
  ) -> df2 

df2 %>% 
  left_join(
    df0 %>% select(SIG_CD, kor_name), by = c('kor_name')
  )  -> df2

df2 %>% 
  mutate(
    index_jobqual = as.numeric(index_jobqual)
  )-> df2

df2 %>% left_join(df1, by = c('SIG_CD'='id')) -> df3 

df3 %>%
  #filter(year == 2015 & si_do_1 %in% c("서울특별시", "경기도")) %>% 
  ggplot() + 
  aes(x=long, y=lat, group=group, fill=index_jobqual, alpha=0.5) + 
  geom_polygon() -> p

p + 
  scale_fill_viridis(direction=-1) + 
  theme_void() + 
  theme(legend.position="left")

register_google(key = 'AIzaSyA-8v4SGqGyLAGxBOK8-hhWvUe_ove00-w')

map <- get_map(location='south korea', zoom=7, maptype='roadmap', color='bw')

ggmap(map) +  
  geom_polygon(data=df3, aes(x=long, y=lat, group=group, fill=index_jobqual), alpha=.75) + scale_fill_viridis(direction=-1) + theme_void() + guides(fill=F)

