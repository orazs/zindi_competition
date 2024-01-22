library(tidymodels)
library(tidyverse)
library(DT)
library(glue)

makeWide <- function(fields=composite_names,df){
  
  names(df) <- tolower(names(df))
  
  names(df)[names(df)=="seedingsowingtransplanting"] <- "seedingsowingtransplanting_date"
  df$rcnursestdate <- as.Date(df$rcnursestdate) #подготовка
  df$seedingsowingtransplanting_date <- as.Date(df$seedingsowingtransplanting_date) #сев
  df$croptillagedate <- as.Date(df$croptillagedate) #обработка
  df$harv_date <- as.Date(df$harv_date) #сбор
  df$threshing_date <- as.Date(df$threshing_date) #обмолот
  
  
  df <- df %>%
    mutate(tat_nurse_ref = as.integer(rcnursestdate-ref_date),
           tat_seeding_nurse = as.integer(seedingsowingtransplanting_date-rcnursestdate),
           tat_tillage_nurse= as.integer(croptillagedate-rcnursestdate),
           tat_tillage_seeding= as.integer(seedingsowingtransplanting_date-croptillagedate),
           tat_harv_tillage=as.integer(harv_date-croptillagedate))
  
  df[df==""] <- "NotDefined"
  names(df)
  
  df <- df %>%
    mutate(
      ganaura = ifelse(grepl("Ganaura",orgfertilizers,ignore.case = T),ganaura,0),
      croporgfym = ifelse(grepl("FYM",orgfertilizers,ignore.case = T),croporgfym,0),
      basaldap = ifelse(grepl("DAP",cropbasalferts,ignore.case = T),basaldap,0),
      basalurea = ifelse(grepl("Urea",cropbasalferts,ignore.case = T),basalurea,0)
      
      
      
    )
  
  train_wide <-  fields %>% map_dfc(function(x){
    df %>%
      dplyr::select(id,x) %>%
      mutate(split=str_split(.data[[x]]," ")) %>%
      unnest(split) %>%
      mutate(split=paste0(x,"_",split,"_new")) %>%
      mutate(value=1) %>% 
      pivot_wider(names_from = split) %>%
      dplyr::select(starts_with(glue("{x}_"))) %>%
      replace(is.na(.), 0)
    
    
  })
  
  
  
  df <- bind_cols(df, train_wide) %>%
    dplyr::select(-all_of(fields))
  
  
  
}



makeWide_v2 <- function(fields=composite_names,df){
  
  
  df <- df %>%
    mutate(across(ends_with("_date"),as.Date))
  
  
  df <- df %>%
    mutate(nursery_tat_ref = as.integer(nursery_date-ref_date),
           transplanting_tat_nursery = as.integer(transplanting_date-nursery_date),
           transplanting_tat_ref = as.integer(transplanting_date-ref_date),
           
           tillage_tat_nurse= as.integer(tillage_date-nursery_date),
           tillage_tat_ref = as.integer(tillage_date-ref_date),
           
           transplanting_tat_tillage= as.integer(transplanting_date-tillage_date),
           harvest_tat_ref= as.integer(harvest_date-ref_date),
           
           harvest_tat_tillage=as.integer(harvest_date-tillage_date)
           
           
           )
  
  df[df==""] <- "NotDefined"
  

  df <- df %>%
    rowwise() %>%
    mutate(
      transplanting_ganaura = ifelse(grepl("Ganaura",transplanting_org_fertilizers,ignore.case = T),transplanting_ganaura,0),
      transplanting_crop_org_FYM = ifelse(grepl("FYM",transplanting_org_fertilizers,ignore.case = T),transplanting_crop_org_fym,0),
      tillage_basal_dap = ifelse(grepl("DAP",tillage_basal_fertilizers,ignore.case = T),tillage_basal_dap,0),
      tillage_basal_urea = ifelse(grepl("Urea",tillage_basal_fertilizers,ignore.case = T),tillage_basal_urea,0),
      
      transplanting_org_fert_cnt= if_else(trimws(transplanting_org_fertilizers)=="NotDefined",0, transplanting_org_fertilizers%>% trimws() %>% str_split(" ") %>% unlist() %>% length()),
      tillage_basalferts_cnt= if_else(trimws(tillage_basal_fertilizers)=="NotDefined",0, tillage_basal_fertilizers%>% trimws() %>% str_split(" ") %>% unlist() %>% length()),
      tillage_method_cnt= if_else(trimws(tillage_method)=="NotDefined",0, tillage_method%>% trimws() %>% str_split(" ") %>% unlist() %>% length()),
      transplanting_factor_cnt= if_else(trimws(transplanting_factors)=="NotDefined",0, transplanting_factors%>% trimws() %>% str_split(" ") %>% unlist() %>% length()),
      nursery_factor_cnt= if_else(trimws(nursery_factors)=="NotDefined",0, nursery_factors%>% trimws() %>% str_split(" ") %>% unlist() %>% length()),
      
      tillage_basal_dap_per_acre = tillage_basal_dap/acre,
      tillage_urea_per_acre_second = tillage_urea_second_dose/acre,
      tillage_urea_per_acre_third = tillage_urea_third_dose/acre,
      tillage_basal_urea_per_acre = tillage_basal_urea/acre,
      transplanting_ganaura_per_acre = transplanting_ganaura*100/acre,
      transplanting_croporgfym_per_acre = transplanting_crop_org_FYM*100/acre
    )
  
  
  train_wide <-  fields %>% map_dfc(function(x){
    df %>%
      dplyr::select(id,x) %>%
      mutate(split=str_split(.data[[x]]," ")) %>%
      unnest(split) %>%
      mutate(split=paste0(x,"_",split,"_new")) %>%
      mutate(value=1) %>% 
      pivot_wider(names_from = split) %>%
      dplyr::select(starts_with(glue("{x}_"))) %>%
      replace(is.na(.), 0)
    
    
  })
  
  
  
  df <- bind_cols(df, train_wide) 
  
  
  
}