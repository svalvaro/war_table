box::use(
  reactable[...],
  glue[...]
  )

#' Creates the table inside the modals
#' @export
generate_war_table <- function(war_df, map_list) {
  war_df$map <- NA
  reactable(
    war_df,
    defaultColDef = colDef(show = FALSE),
    height = "800px",
    
    columns = list(
      war_details = colDef(
        html = TRUE,
        align = "center",
        name = "<div class = 'warFont tableHeader'>War</div>",
        show = TRUE,
        vAlign = "center"
      ),
      initiators = colDef(
        html = TRUE,
        name = glue("<div class = 'warFont tableHeader'>Side A</div>"),
        show = TRUE
      ),
      defenders = colDef(
        html = TRUE,
        name = glue("<div class = 'warFont tableHeader'>Side B</div>"),
        show = TRUE
      ),
      map = colDef(
        html = TRUE,
        show = TRUE,
        align = "center",
        vAlign = "center",
        name =  glue("<div class = 'warFont tableHeader'>Map</div>"),
        cell = function(value, index) {
          map_list[[war_df$WarName[[index]]]]
          })
    )
  )
}