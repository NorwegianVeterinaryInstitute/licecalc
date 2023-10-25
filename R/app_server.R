#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  observeEvent(input$docs, {
    if (session$userData$shiny.i18n$lang() == "en") {
      path = app_sys("app/www/documentation.Rmd") }
        else {
          path = app_sys("app/www/dokumentasjon.Rmd")
        }
    shiny::showModal(
        modalDialog(includeMarkdown(path)
      ))
  })

  #mod_plot_lice_server("plot_lice_1")
  mod_lice_model_v2_server("lice_model_v2_1")
}
