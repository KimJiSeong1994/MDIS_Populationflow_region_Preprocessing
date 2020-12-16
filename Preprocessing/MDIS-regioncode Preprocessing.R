# ======================================================== [ MDIS region Code ] ======================================================
download_path = ""
writefile_path = ""
year = NULL # 기준년도 

## + [ Preprocessing function ] ==========================
region_parse = function(file_path) {
  if(!require(tidyverse)) install.packages("tidyverse")
  
  readLines(file_path, encoding = "UTF-8") %>%
    .[str_detect(., "mdis") == T]  %>%
    str_remove_all(., "[^0-9가-힣 ]") %>% 
    str_remove_all(., "\\s") %>% 
    str_sub(., 3, str_length(.)) %>%
    .[str_count(., "[0-9]") %in% c(1:15)] %>% 
    as.data.frame() %>% 
    separate(., into = c("emd_code", "region_emd_name"), 1, 10) %>% 
    filter(!str_detect(emd_code, "[가-힣]")) -> region_emd_code
  
  readLines(file_path, encoding = "UTF-8") %>%
    .[str_detect(., "mdis") == T]  %>%
    str_remove_all(., "[^0-9가-힣 ]") %>% 
    str_remove_all(., "\\s") %>% 
    str_sub(., 3, str_length(.)) %>%
    .[str_count(., "[0-9]") %in% c(1:15)] %>% 
    .[str_count(., "[0-9]") == 2] %>% 
    as.data.frame() %>% 
    separate(., into = c("cd_code", "region_cd_name"), 1, 2) -> region_cd_code
  
  readLines(file_path, encoding = "UTF-8") %>%
    .[str_detect(., "mdis") == T]  %>%
    str_remove_all(., "[^0-9가-힣 ]") %>% 
    str_remove_all(., "\\s") %>% 
    str_sub(., 3, str_length(.)) %>%
    .[str_count(., "[0-9]") %in% c(1:15)] %>% 
    .[str_count(., "[0-9]") == 5] %>% 
    as.data.frame() %>% 
    separate(., into = c("sgg_code", "region_sgg_name"), 1, 5) -> region_sgg_code
  
  region_emd_code %>% 
    mutate(cd_code = str_sub(emd_code, 1, 2),
           sgg_code = str_sub(emd_code, 1, 5),
           emd_code = emd_code) %>% 
    left_join(region_cd_code) %>% 
    left_join(region_sgg_code) %>% 
    select(cd_code, sgg_code, emd_code, region_cd_name, region_sgg_name, region_emd_name) -> region_code
  return(region_code)
}


i = 0
region_code = data.frame(NULL)
for(file in list.files(download_path)) {
  region_parse(paste0(download_path, "/", file)) %>% 
    mutate(year = year + i) %>%
    rbind(region_code, .) -> region_code
  i = i + 1
}

write.csv(region_code, paste0(writefile_path, "/MDIS_region_code.csv")) 


