box::use(
  dplyr[...],
  vroom[vroom],
  magrittr[`%>%`],
  glue[glue],
  shiny[icon],
  htmltools[...],
  stringi[...]
)

#' It takes the raw inter state war data and creates the needed output table
#' @export
process_raw_war_data <- function(data_dir) {
  table_wars <- vroom(data_dir, show_col_types = FALSE)
  
  columns_to_keep <- c("WarName", "Side", "StateName",  "StartYear1", "EndYear1", "EndYear2",
                       "WhereFought", "Initiator", "Outcome", "BatDeath")
  table_wars$StateName <- lapply(X = table_wars$StateName, add_country_flags)
  
  table_wars <-  
    table_wars %>%
    select(columns_to_keep) %>%
    group_by(WarName) %>%
    mutate(
      WarName = ifelse(grepl("War", WarName), WarName, glue("{WarName} War")),
      start_year = min(StartYear1),
      end_year = max(c(EndYear1, EndYear2)),
      BatDeath = ifelse(BatDeath == -9, NA, BatDeath),
      total_deaths = sum(BatDeath),
      StateName = ifelse(
        Initiator == 1,
        glue("{StateName}  {htmltools::img(src = 'images/sword.png', style = 'height: 24px;')}"),
        StateName
        ),
      war_details = glue("{WarName}<br>
                         <div class = 'periodTime'> {start_year}-{end_year}</div><br>
                         <div class = 'casualties'>{total_deaths} {htmltools::img(src = 'images/skull.jpg', style = 'height: 24px;')}</div>"),
      country_details = glue("{StateName}  <div class = 'casualties'> {BatDeath} {htmltools::img(src = 'images/skull.jpg', style = 'height: 24px;')}</div><br>"),
      Initiator = ifelse(Initiator == 1, TRUE, FALSE)) %>%
    ungroup() %>%
    select(-c("EndYear1", "EndYear2"))
}



#' @export
aggregate_war_data <- function(table_wars) {

  table_sides <-
    table_wars %>%
    group_by(WarName, Side) %>%
    mutate(
      defenders = ifelse(
        all(Initiator == FALSE),
        glue("{paste0(country_details, collapse = '\n')} <br>
             Outcome: {Outcome}"),
        ""),
      initiators = ifelse(
        any(Initiator == TRUE),
        glue("{paste0(country_details, collapse = '\n')}<br> 
             Outcome: {Outcome}"),
        "")
    ) %>%
    ungroup()

    table_collapsed_data <-
      table_sides %>%
      select(c("war_details", "initiators", "defenders")) %>%
      group_by(war_details) %>%
      distinct() %>%
      mutate(
        initiators = paste0(initiators, collapse = ""),
        defenders = paste0(defenders, collapse = "")) %>%
      distinct() %>%
      mutate(
        initiators =  gsub(
          "Outcome: 1",
          htmltools::img(src = 'images/war_winner.png', style = 'height: 35px;'),
          initiators
        ),
        initiators =  gsub(
          "Outcome: 2",
          htmltools::img(src = 'images/war_defeated.png', style = 'height: 35px;'),
          initiators
        ),
        initiators =  gsub(
          "Outcome: 6",
          htmltools::img(src = 'images/stalemate.png', style = 'height: 35px;'),
          initiators
        ),
        defenders =  gsub(
          "Outcome: 6",
          htmltools::img(src = 'images/stalemate.png', style = 'height: 35px;'),
          defenders
        ),
        defenders =  gsub(
          "Outcome: 1",
          htmltools::img(src = 'images/war_winner.png', style = 'height: 35px;'),
          defenders
        ),
        defenders =  gsub(
          "Outcome: 2",
          htmltools::img(src = 'images/war_defeated.png', style = 'height: 35px;'),
          defenders
        )
      ) %>%
      ungroup()
    
}


#' #' @export
#' outcome_image_creator <- function(text){
#'   
#'   patterns <- c("Outcome: 1", "Outcome: 2", "Outcome: 6")
#'   
#'   for (pat in patterns) {
#'     
#'     if(grepl(pat, text)){
#'       gsub(pat, )
#'     }
#'     
#'   }
#' 
#'   
#'   text_with_image <- stri_replace(
#'     str = as.character(text),
#'     replacement = htmltools::img(src = 'images/war_winner.png', style = 'height: 35px;'),
#'     regex = "Outcome: 1"
#'     )
#'   
#' 
#' 
#'   
#'   return(text_with_image)
#' }


#' @export
add_country_flags <- function(country) {
  #print(country)
  countries_df <- 
    vroom("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv",
          show_col_types = FALSE) %>% 
    select(c("name","alpha-2")) %>% 
    mutate(
      `alpha-2` = tolower(`alpha-2`)
    )
  
  country_code <- countries_df$`alpha-2`[countries_df$name == country]
  
  if (length(country_code)== 0) country_code <- "xx"
  
  # Some countries need to add manually the code, since their names might not be in the accurate form 
  if (country == "United Kingdom") country_code <- "gb"
  if (country == "Russia") country_code <- "ru"
  if (country == "Papal States") country_code <- "va"
  if (country == "Iran") country_code <- "ir"
  if (country == "Bolivia") country_code <- "bo"
  if (country == "Yemen Arab Republic") country_code <- "ye"
  if (country == "South Korea") country_code <- "kr"
  if (country == "Nort Korea") country_code <- "kp"
  if (country == "Syria") country_code <- "sy"
  if (country == "Laos") country_code <- "la"
  if (country == "Vietnam") country_code <- "vn"
  if (country == "China (PRC)") country_code <- "cn"
  if (country == "Taiwan (PRC)" | country == "Taiwan") country_code <- "tw"
  if (country == "Tanzania") country_code <- "tz"
  if (country == "Democratic Republic of the Congo") country_code <- "cd"
  
  country_flag <- glue("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/{country_code}.svg")
  
  state_flag <- glue("<img src='{country_flag}' height='20'> {country}")
  return(state_flag)
}


