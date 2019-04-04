### Initial 
librarian::shelf(tidyverse, tidyr)

###
### Load df for macos, linux 
read.delim(file=here::here('data', 'LQEI.txt'),
           header=F,
           #skip=1, 
           #fill = T, 
           encoding='UTF-8', 
           stringsAsFactors = F
           #Other settings
) -> df0

### Load df for windows 

readLines(here::here('data', 'LQEI.txt'), 
          encoding='UTF-8'
          ) %>% 
  tibble(.) %>% 
  slice(-c(1:6)) -> df0

mynames <-c('si_do_1', 'si_do_2', 'gu_gun',
            'shr_highincome', 'shr_higheducated', 'shr_highskilled', 
            'index_jobqual', 'cat', 'year')

merge_split <- function(vdf, colnames=mynames){
#  
  vdf %>% 
    separate(col=1, into=c(str_glue("v{1:13}")), sep = " ") %>% 
    filter(!is.na(v3)) %>% 
    filter(!v1 %in% c('지역', '고소득비중')) %>% 
    as_tibble() -> vdf1
  
  vdf1 %>% filter(is.na(v13) & v1 != "세종특별자치시") %>% dplyr::select(-v13) -> vdf2a
  vdf1 %>% filter(is.na(v13) & v1 == "세종특별자치시") %>% dplyr::select(-v12,-v13) -> vdf2b
  vdf1 %>% filter(!is.na(v13)) -> vdf3
  names(vdf2a) <- str_glue("v{1:13}")[-2]
  names(vdf2b) <- str_glue("v{1:13}")[-c(2,3)]
  
  bind_rows(vdf2a, vdf2b, vdf3) %>% 
     mutate(
       v2 = if_else(is.na(v2), v1, v2), 
       v3 = if_else(is.na(v3), v1, v3)
     ) %>% 
    dplyr::select(v1, v2, v3, everything()) -> vdf4
  # 
   vdf4 %>% dplyr::select(1:3, 4:8) %>% mutate(year = 2010) -> vdf5 
   vdf4 %>% dplyr::select(1:3, 9:13) %>% mutate(year = 2015) -> vdf6
   names(vdf5) <- colnames
   names(vdf6) <- colnames
   bind_rows(vdf5, vdf6) %>% 
     mutate_at(vars(contains("shr")), 
               ~as.numeric(.)) %>% 
     mutate_at(vars(contains("index")), 
               ~as.numeric(.))
#
}

merge_split(df0) -> df1

### Save rds & csv----
saveRDS(df1, here::here('data', 'LQEI.rds'))
write_excel_csv(df1, here::here('data', 'LQEI.csv'))

