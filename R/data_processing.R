box::use(
  dplyr[...],
  vroom[vroom],
  magrittr[`%>%`],
  glue[glue],
  shiny[icon],
  htmltools[...],
  plyr[round_any],
  stringi[...]
)

#' It takes the raw inter state war data and creates the needed output table
#' @export
process_raw_war_data <- function(data_dir) {
  table_wars <- vroom(data_dir, show_col_types = FALSE)
  countries_df <- 
    vroom("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv",
          show_col_types = FALSE) %>% 
    select(c("name","alpha-2")) %>% 
    mutate(
      `alpha-2` = tolower(`alpha-2`)
    )
  table_wars$country_flag <- lapply(table_wars$StateName,add_country_flags, countries_df = countries_df)
  
  table_wars$StateNameShow <- glue("<img src='{table_wars$country_flag}' height='20'> {table_wars$StateName}")
  table_wars$BatDeath <- as.numeric(lapply(X = table_wars$BatDeath, round_casualties))
  
  columns_to_keep <- c("WarName", "Side", "StateName","country_flag","StateNameShow",  "StartYear1", "EndYear1", "EndYear2",
                       "WhereFought", "Initiator", "Outcome", "BatDeath")
  table_wars <-  
    table_wars %>%
    select(columns_to_keep) %>%
    group_by(WarName) %>%
    mutate(
      WarName = ifelse(grepl("War", WarName), WarName, glue("{WarName} War")),
      start_year = min(StartYear1),
      end_year = max(c(EndYear1, EndYear2)),
      BatDeath = ifelse(BatDeath == -9, NA, BatDeath),
      total_deaths = sum(BatDeath, na.rm = TRUE),
      StateNameShow = ifelse(
        Initiator == 1,
        glue("{StateNameShow}  {htmltools::img(src = 'images/sword.png', style = 'height: 24px;')}"),
        StateNameShow
        ),
      states_participants = paste0(StateName, collapse = ',') 
      ) %>%
    ungroup() %>% 
    arrange(desc(BatDeath)) %>% 
    mutate(
      readableBatDeath = readable_casualties(BatDeath),
      readable_total_deaths = readable_casualties(total_deaths)
    ) %>% 
    group_by(WarName) %>% 
    mutate(
      war_details = glue(
      "<div class = 'warName' style='font-size: 20px;'>{WarName}<br>
      <div class = 'periodTime'>{htmltools::img(src = 'images/calendar.jpg', style = 'height: 24px;')} {start_year}-{end_year}</div><br>
      <div class = 'casualties'>{readable_total_deaths} {htmltools::img(src = 'images/skull.jpg', style = 'height: 24px;')}</div>"
      ),
      country_details = glue("{StateNameShow}  <div class = 'casualties'> {readableBatDeath} {htmltools::img(src = 'images/skull.jpg', style = 'height: 24px;')}</div><br>"),
      Initiator = ifelse(Initiator == 1, TRUE, FALSE)) %>%
    ungroup() %>%
    arrange(desc(total_deaths)) %>% 
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
        glue(
        "Outcome: {Outcome}<br>
        <br>
        {paste0(country_details, collapse = '\n')}"),
        ""),
      initiators = ifelse(
        any(Initiator == TRUE),
        glue(
        "Outcome: {Outcome}<br>
        <br>
        {paste0(country_details, collapse = '\n')}"),
        "")
    ) %>%
    ungroup()

    table_collapsed_data <-
      table_sides %>%
      select(c("war_details", "initiators", "defenders","total_deaths","start_year","end_year","WarName","states_participants")) %>%
      group_by(war_details) %>%
      distinct() %>%
      mutate(
        initiators = paste0(initiators, collapse = ""),
        defenders = paste0(defenders, collapse = "")) %>%
      distinct() %>%
      mutate(
        initiators =  gsub(
          "Outcome: 1",
          htmltools::img(src = 'images/war_winner.png', style = 'height: 45px;'),
          initiators
        ),
        initiators =  gsub(
          "Outcome: 2",
          htmltools::img(src = 'images/war_defeated.png', style = 'height: 45px;'),
          initiators
        ),
        initiators =  gsub(
          "Outcome: 6",
          htmltools::img(src = 'images/stalemate.png', style = 'height: 45px;'),
          initiators
        ),
        defenders =  gsub(
          "Outcome: 6",
          htmltools::img(src = 'images/stalemate.png', style = 'height: 45px;'),
          defenders
        ),
        defenders =  gsub(
          "Outcome: 1",
          htmltools::img(src = 'images/war_winner.png', style = 'height: 45px;'),
          defenders
        ),
        defenders =  gsub(
          "Outcome: 2",
          htmltools::img(src = 'images/war_defeated.png', style = 'height: 45px;'),
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
#'     replacement = htmltools::img(src = 'images/war_winner.png', style = 'height: 45px;'),
#'     regex = "Outcome: 1"
#'     )
#'   
#' 
#' 
#'   
#'   return(text_with_image)
#' }


#' @export
add_country_flags <- function(country,countries_df) {

  
  country_code <- countries_df$`alpha-2`[countries_df$name == country]
  
  if (length(country_code)== 0) country_code <- "xx"
  
  # Some country names are not written in the same format, so the correct code is not assigned
  
  if (country_code == "xx") {
    country_code <- switch (
      country,
      "United Kingdom" = "gb",
      "Russia" = "ru",
      "Papal States" = "va",
      "Iran" = "ir",
      "Bolivia" = "bo",
      "Yemen Arab Republic" = "ye",
      "South Korea" = "kr",
      "North Korea" = "kp",
      "Syria" = "sy",
      "Laos" = "la",
      "Vietnam" = "vn",
      "China (PRC)" = "cn",
      "Taiwan (ROC)" = "tw",
      "Taiwan" = "tw",
      "Tanzania" = "tz",
      "Democratic Republic of the Congo" = "cd",
      country
    )
  }
  
  country_flag <- glue("https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/{country_code}.svg")
  
  # Old flags need to be added manually, it doesn't seem trivial to do this programatically
  
  country_flag <- switch (country,
    "USSR" = "https://upload.wikimedia.org/wikipedia/commons/a/a9/Flag_of_the_Soviet_Union.svg",
    "Sardinia/Piedmont" = "https://upload.wikimedia.org/wikipedia/commons/2/21/Civil_Flag_and_Civil_Ensign_of_the_Kingdom_of_Sardinia_%281816-1848%29.svg",
    "Two Sicilies" = "https://upload.wikimedia.org/wikipedia/commons/7/78/Flag_of_the_Kingdom_of_the_Two_Sicilies_%281816%29.svg",
    "Prussia" = "https://upload.wikimedia.org/wikipedia/commons/c/c1/Flag_of_the_Kingdom_of_Prussia_%281803-1892%29.svg" ,
    "Modena" = "https://upload.wikimedia.org/wikipedia/commons/f/f1/State_Flag_of_the_Duchy_of_Modena_and_Reggio_%281830-1859%29.svg",
    "Tuscany" = "https://upload.wikimedia.org/wikipedia/commons/e/e0/Flag_of_the_Grand_Duchy_of_Tuscany_with_Great_Coat_of_arms.svg",
    "Ottoman Empire" = "https://upload.wikimedia.org/wikipedia/commons/8/8e/Flag_of_the_Ottoman_Empire_%281844%E2%80%931922%29.svg",
    "Hesse Grand Ducal" = "https://upload.wikimedia.org/wikipedia/commons/9/97/Flagge_Gro%C3%9Fherzogtum_Hessen_ohne_Wappen.svg",
    "Wuerttemburg" = "https://upload.wikimedia.org/wikipedia/commons/1/14/Flagge_K%C3%B6nigreich_W%C3%BCrttemberg.svg",
    "Mecklenburg Schwerin" = "https://upload.wikimedia.org/wikipedia/commons/9/95/Flagge_Gro%C3%9Fherzogt%C3%BCmer_Mecklenburg.svg",
    "Bavaria" = "https://upload.wikimedia.org/wikipedia/commons/1/16/Flag_of_Bavaria_%28striped%29.svg",
    "Hesse Electoral" = "https://upload.wikimedia.org/wikipedia/commons/f/f7/Flag_of_Hesse.svg",
    "Saxony" = "https://upload.wikimedia.org/wikipedia/commons/e/e9/Flagge_K%C3%B6nigreich_Sachsen_%281815-1918%29.svg",
    "Baden" = "https://upload.wikimedia.org/wikipedia/commons/0/0b/Flagge_Gro%C3%9Fherzogtum_Baden_%281891%E2%80%931918%29.svg",
    "Hanover" = "https://upload.wikimedia.org/wikipedia/commons/0/08/Flag_of_Hanover_1837-1866.svg",
    "Yugoslavia" = "https://upload.wikimedia.org/wikipedia/commons/e/e5/Flag_of_Yugoslavia_%281918%E2%80%931941%29.svg",
    "Austria-Hungary" = "https://upload.wikimedia.org/wikipedia/commons/2/29/Flag_of_Austria-Hungary_%281869-1918%29.svg",
    "Czechoslovakia" = "https://upload.wikimedia.org/wikipedia/commons/c/cb/Flag_of_the_Czech_Republic.svg",
    "South Vietnam" = "https://upload.wikimedia.org/wikipedia/commons/e/e9/Flag_of_South_Vietnam.svg",
    "Bosnia" = "https://upload.wikimedia.org/wikipedia/commons/0/0f/Flag_of_Bosnia_and_Herzegovina_%281946%E2%80%931992%29.svg",
    country_flag
  )
  
  #state_flag <- glue("<img src='{country_flag}' height='20'> {country}")
  return(country_flag)
}

#' @export
round_casualties <- function(casualties) {
  if (is.na(casualties)) return(NA)
  if (casualties < 0) return(casualties)
  accuracy <- 10000
  if (casualties < 100000) accuracy <- 1000
  if (casualties < 10000) accuracy <- 100
  if (casualties < 1000) accuracy <- 10
  if (casualties < 100) accuracy <- 5
  as.numeric(round_any(casualties,accuracy , f = round))
}

#' @export
readable_casualties <- function(casualties) {
  case_when(
    casualties < 1e3 ~ as.character(casualties),
    casualties < 1e6 ~ paste0(as.character(round(casualties/1e3, digits = 1)), "K"),
    casualties < 1e9 ~ paste0(as.character(round(casualties/1e6, digits = 1)), "M")
    )
}


