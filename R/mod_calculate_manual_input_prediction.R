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
        "manual_data_table_game"
      ),))
    ),
    bslib::card(
      bslib::card_header("Prediction per cage"),
      bslib::card_body(shiny::plotOutput(ns("plot_cage_game")))
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

    output$manual_data_table_game <- DT::renderDataTable({
      if (i18n()$get_translation_language() == 'en') {
        dat <- empty
        caption = "Double click to edit cells. Ctr+Enter to save."
      } else {
        dat = empty_nb
        caption = "Dobbeltklikk for å redigere celler. Ctr+Enter for å lagre."
      }

      DT::datatable(
        dat,
        # empty data frame, see data-raw for more.
        options = list(
          pageLength = 20,
          dom = "t",
          scrollY = "200px"
        ),
        selection = "none",
        editable = list(target = "row", disable = list(columns = 0)),
        class = "cell-border stripe",
        caption = caption
      )
    }, server =  FALSE)





    output$plot_cage <- shiny::renderPlot(
      {


          make_plot_for_game(
            ip1 = input$infectious_pressure_manually_1week,
            ip2 = input$infectious_pressure_manually_2week,
            st = input$sea_temperature_manually,
            user_data = manual_data_rct()
          )
      }
    ) |> shiny::bindEvent(input$predict)


})
}

## To be copied in the UI
# mod_calculate_manual_input_prediction_ui("calculate_manual_input_prediction_1")

## To be copied in the server
# mod_calculate_manual_input_prediction_server("calculate_manual_input_prediction_1")
