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
    #pagination = FALSE,
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
        align = "center",
        name = glue("<div class = 'warFont tableHeader'>Initiators {htmltools::img(src = 'images/initiators.jpg', style = 'height: 24px;')}</div>"),
        show = TRUE
      ),
      defenders = colDef(
        html = TRUE,
        align = "center",
        name = glue("<div class = 'warFont tableHeader'>Defenders {htmltools::img(src = 'images/shield.jpg', style = 'height: 24px;')}</div>"),
        show = TRUE
      ),
      map = colDef(
        html = TRUE,
        show = TRUE,
        align = "center",
        name =  glue("<div class = 'warFont tableHeader'>Map</div>"),
        cell = function(value, index) {
          map_list[[war_df$WarName[[index]]]]
          })
    )
  )
}