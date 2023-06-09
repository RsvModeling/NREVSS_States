scrape_func <- function(stateid){
  # Specify the URL of the website
  url1 <- paste0("https://www.cdc.gov/surveillance/nrevss/images/rsvstate/RSV1PPCent3AVG_State",stateid,".htm")
  url2 <- paste0("https://www.cdc.gov/surveillance/nrevss/images/rsvstate/RSV4PPCent3AVG_State",stateid,".htm")
  url3 <- paste0("https://www.cdc.gov/surveillance/nrevss/images/rsvstate/RSV14NumCent5AVG_State",stateid,".htm")
  
  
  # Read the HTML content of the website
  page1 <- read_html(url1)
  page2 <- read_html(url2)
  page3 <- read_html(url3)
  
  # Scrape the table data from the website
  table_data1 <- page1 %>%
    html_table(fill = TRUE)
  
  table_data2 <- page2 %>%
    html_table(fill = TRUE)
  
  table_data3 <- page3 %>%
    html_table(fill = TRUE)
  
  # Extract the desired table (in this case, it's the first table)
  df1 <- table_data1[[1]] %>% 
    dplyr::select(RepWeekDate,StateID,`Percent Positive`,`Total Antigen Detection Tests`) %>%
    rename(PctPosAntigen= `Percent Positive`)
  df2 <- table_data2[[1]]%>% 
    dplyr::select(RepWeekDate,StateID,`Percent Positive`,`Total PCR Tests`) %>%
    rename(PctPosPCR= `Percent Positive`)
  df3 <- table_data3[[1]]
  
  # Remove empty rows
  df1 <- df1[!apply(df1, 1, function(x) all(is.na(x))), ]
  df2 <- df2[!apply(df2, 1, function(x) all(is.na(x))), ]
  df3 <- df3[!apply(df3, 1, function(x) all(is.na(x))), ]
  
  df.combined <- df1 %>%
    full_join(., df2, by=c('StateID','RepWeekDate')) %>%
    full_join(., df3, by=c('StateID','RepWeekDate'))
  
  return(df.combined)
  
}