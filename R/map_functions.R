box::use(
  maps[...],
  dplyr[...]
)
#' It maps the countries to the map, manually adds location of states that couldn't be mapped 
#' programatically.
#' @export
create_df_countries_locations <- function(participant_countries){
  countries_world <- maps::world.cities %>% 
    filter(capital == 1) %>%
    select(country = country.etc, latitude = lat, longitude = long) %>%
    add_row(country = "Yugoslavia", latitude = 44.49, longitude = 20.27) %>% 
    add_row(country = "United States of America", latitude = 47.75, longitude = -120.74) %>% 
    add_row(country = "United Kingdom", latitude = 55.37, longitude = -3.43) %>% 
    add_row(country = "USSR", latitude = 61.52, longitude = 105.31) %>% 
    add_row(country = "Austria-Hungary", latitude = 47.51, longitude = 14.55) %>% 
    add_row(country = "South Korea", latitude = 35.9, longitude = 127.76) %>% 
    add_row(country = "South Vietnam", latitude = 10.76, longitude = 106.66) %>% 
    add_row(country = "Baden", latitude = 48.75, longitude = 8.24) %>% 
    add_row(country = "Bavaria", latitude = 48.77, longitude = 11.43) %>% 
    add_row(country = "Ottoman Empire", latitude = 41.01, longitude = 28.97) %>% 
    add_row(country = "Hesse Grand Ducal", latitude = 50.81, longitude = 8.77) %>% 
    add_row(country = "Mecklenburg Schwerin", latitude = 54.08, longitude = 12.11) %>% 
    add_row(country = "Hesse Electoral", latitude = 50.05, longitude = 8.69) %>% 
    add_row(country = "Saxony", latitude = 50.1, longitude = 13.2) %>% 
    add_row(country = "Hanover", latitude = 52.37, longitude = 9.73) %>% 
    add_row(country = "Sardinia/Piedmont", latitude = 40.07, longitude = 9.28) %>% 
    add_row(country = "Czechoslovakia", latitude = 49.81, longitude = 15.47) %>% 
    add_row(country = "Tuscany", latitude = 43.76, longitude = 11.15) %>% 
    add_row(country = "Modena", latitude = 44.65, longitude = 10.92) %>% 
    add_row(country = "Prussia", latitude = 52.52, longitude = 13.40) %>% 
    add_row(country = "Bosnia", latitude = 43.41, longitude = 18.29) %>% 
    add_row(country = "Two Sicilies", latitude = 38.11, longitude = 13.37) %>% 
    add_row(country = "Democratic Republic of the Congo", latitude = -4.04, longitude = 21.76) %>% 
    add_row(country = "Papal States", latitude = 41.9, longitude = 12.45) %>% 
    add_row(country = "Yemen Arab Republic", latitude = 15.55, longitude = 48.51) %>%
    add_row(country = "China (PRC)", latitude = 35.86, longitude = 104.19) %>% 
    add_row(country = "Taiwan (ROC)", latitude = 23.69, longitude = 120.96) %>% 
    add_row(country = "North Korea", latitude = 40.34, longitude = 127.51) %>% 
    add_row(country = "Wuerttemburg", latitude = 48.78, longitude = 9.18) %>% 
    mutate(
      latitude = replace(latitude, country == "Italy", 42.34),
      longitude = replace(longitude, country == "Italy", 13.4)) %>%  # Separate from vatican city
    filter(country %in% participant_countries) %>% 
    filter(!(country == "Cyprus"  & latitude == 35.16)) # Removed one duplicate from Cyprus

   
}
