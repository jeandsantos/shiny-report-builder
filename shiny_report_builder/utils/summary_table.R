summary_table <- function(df, digits=2){
  
  df %>% 
    dplyr::select(ID, 
                  `Peak` = peak,
                  `Standard Deviation` = std_dev,
                  `% Total Area` = perc_total_area
    ) 
}