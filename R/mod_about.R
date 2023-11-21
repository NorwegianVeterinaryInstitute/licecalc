#' about UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_about_ui <- function(id){
  ns <- NS(id)

  shiny::uiOutput(ns('about'))

}

#' about Server Functions
#'
#' @noRd
mod_about_server <- function(id, selected_language){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    translator <- golem::get_golem_options(which = "translator")
    i18n <- reactive({
      get_reactive_translator(translator, selected_language())
    })

    output$about <- shiny::renderUI({

      if(i18n()$get_translation_language() == 'en') {
        shiny::tagList(
          tagList(
            shiny::includeMarkdown(app_sys("app/www/about.md"))
          )
        )} else {
          shiny::tagList(
            tagList(
              shiny::includeMarkdown(app_sys("app/www/about-nb.md"))
            )
          )

        }
    })

  })
}

## To be copied in the UI
# mod_about_ui("about_1")

## To be copied in the server
# mod_about_server("about_1")
