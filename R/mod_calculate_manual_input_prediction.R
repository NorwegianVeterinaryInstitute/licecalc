#' calculate_manual_input_prediction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_calculate_manual_input_prediction_ui <- function(id) {
  ns <- NS(id)

  tagList(bslib::layout_sidebar(
    sidebar = bslib::sidebar(shiny::uiOutput(ns('manual'))),

    bslib::card(
      bslib::card_header(
        "Enter data in cells bellow. Double click to edit cells. Ctr+Enter to save."
      ),
      bslib::card_body(DT::dataTableOutput(ns(
        "manual_data_table"
      ),))
    ),
    bslib::card(
      bslib::card_header("Prediction per cage"),
      bslib::card_body(shiny::plotOutput(ns("plot_cage")))
    )
  ))
}

#' calculate_manual_input_prediction Server Functions
#'
#' @noRd
mod_calculate_manual_input_prediction_server <- function(id, selected_language){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    translator <- golem::get_golem_options(which = "translator")
    i18n <- reactive({
      get_reactive_translator(translator, selected_language())
    })

    output$manual <- shiny::renderUI({
      shiny::tagList(
        shiny::sliderInput(
          ns("infectious_pressure_manually_1week"),
          label = i18n()$t("Infectious presure first week"),
          min = 0,
          max = 22,
          step = 1,
          ticks = FALSE,
          value = 15
        ),
        shiny::sliderInput(
          ns("infectious_pressure_manually_2week"),
          label = i18n()$t("Infectious presure second week"),
          min = 0,
          max = 22,
          step = 1,
          ticks = FALSE,
          value = 15
        ),
        shiny::sliderInput(
          ns("sea_temperature_manually"),
          label = i18n()$t("Sea temperature"),
          min = 0,
          max = 22,
          step = 1,
          ticks = FALSE,
          value = 15
        ),
        shiny::actionButton(
          inputId = ns("predict"),
          label = i18n()$t("Predict")
        ),
        shiny::helpText("Bla bla bla")
      )
    })
})
}

## To be copied in the UI
# mod_calculate_manual_input_prediction_ui("calculate_manual_input_prediction_1")

## To be copied in the server
# mod_calculate_manual_input_prediction_server("calculate_manual_input_prediction_1")
