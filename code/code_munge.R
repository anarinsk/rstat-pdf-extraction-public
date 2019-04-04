### Initial 
librarian::shelf(tidyverse, ropensci/tabulizer, tidyr, xlsx)

### Load df 
df <- extract_tables(here::here('data','LQEI.pdf'), 
                     guess=F, skip=2, 
                     output="data.frame") 

mynames <-c('si_do_1', 'si_do_2', 'gu_gun',
            'shr_highincome', 'shr_higheducated', 'shr_highskilled', 
            'index_jobqual', 'cat', 'year')

merge_split <- function(pdf_page, vdf, colnames=mynames){
#  
  vdf %>% 
    pluck(pdf_page) %>% 
    unite(data, sep = " ") %>% 
    separate(data, str_glue("v{1:13}"), sep = " ") %>% 
    filter(!v10 == "그룹") -> vdf1
  
  vdf1 %>% filter(is.na(v13) & v1 != "세종특별자치시") %>% select(-v13) -> vdf2a
  vdf1 %>% filter(is.na(v13) & v1 == "세종특별자치시") %>% select(-v12,-v13) -> vdf2b
  vdf1 %>% filter(!is.na(v13)) -> vdf3
  names(vdf2a) <- str_glue("v{1:13}")[-2]
  names(vdf2b) <- str_glue("v{1:13}")[-c(2,3)]
  
  bind_rows(vdf2a, vdf2b, vdf3) %>% 
    mutate(
      v2 = if_else(is.na(v2), v1, v2), 
      v3 = if_else(is.na(v3), v1, v3)
    ) %>% 
    select(v1, v2, v3, everything()) -> vdf4
  
  vdf4 %>% select(1:3, 4:8) %>% mutate(year = 2010) -> vdf5 
  vdf4 %>% select(1:3, 9:13) %>% mutate(year = 2015) -> vdf6
  names(vdf5) <- colnames
  names(vdf6) <- colnames
  bind_rows(vdf5, vdf6)
#
}
c(1,2,3,4) %>% map_df(function(x){merge_split(x, df)}) -> df1

### Save rds & csv----
saveRDS(df1, here::here('data', 'LQEI.rds'))
#write_excel_csv(df1, here::here('data', 'LQEI.csv'))

