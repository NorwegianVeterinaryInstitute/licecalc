#' calculate_farm_level_prediction UI Function
#' this module is used for generating a farm level prediction
#' of lice in fish.
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_calculate_farm_level_prediction_ui <- function(id){
  ns <- NS(id)

  tagList(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
    shiny::uiOutput(ns('sidebar_farm'))
  ),
    shiny::plotOutput(ns("plot_farm"))
  ))
}

#' calculate_farm_level_prediction Server Functions
#'
#' @noRd
mod_calculate_farm_level_prediction_server <- function(id, selected_language){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    translator <- golem::get_golem_options(which = "translator")
    i18n <- reactive({
      get_reactive_translator(translator, selected_language())})


    output$sidebar_farm <- shiny::renderUI({

      shiny::tagList(
      shiny::textInput(
        ns("locality_number"),
        label = i18n()$t("Locality Number"),
        value = NULL
      ),
      shiny::textInput(
        ns("fish_weight_farm"),
        label = i18n()$t("Fish weight"),
        value = "2,2"
      ),
      shiny::textInput(
        ns("fish_abundance_farm"),
        label = i18n()$t("Fish abundance"),
        value = "130"
      ),
      shiny::checkboxInput(
        ns("cleaner_fish_farm"),
        label = i18n()$t("Presence of cleaner fish"),
        value = FALSE
      ),
      shiny::actionButton(
        inputId = ns("predict"),
        label = i18n()$t("Predict")
      ),
      shiny::helpText("
                    Bla bla bla"))

    })

    output$plot_farm <- shiny::renderPlot(
      {
        tryCatch(
          expr = {
        make_plot_from_location(location = input$locality_number,
                                weight = input$fish_weight_farm,
                                abundance = input$fish_abundance_farm,
                                cleaner = input$cleaner_fish_farm,
                                lang = i18n()$get_translation_language())
          },
        error = function(e) {
          showModal(
            modalDialog(
              title = "Error",
              "The locality was found, but we could not generate predictions."
            )
          )
        }
        )
      }
    ) |> shiny::bindEvent(input$predict)

  })
}

## To be copied in the UI
# mod_calculate_farm_level_prediction_ui("calculate_farm_level_prediction_1")

## To be copied in the server
# mod_calculate_farm_level_prediction_server("calculate_farm_level_prediction_1")
