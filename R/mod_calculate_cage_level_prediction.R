#' calculate_cage_level_prediction UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_calculate_cage_level_prediction_ui <- function(id) {
  ns <- NS(id)
  tagList(bslib::layout_sidebar(
    sidebar = bslib::sidebar(shiny::uiOutput(ns('sidebar_cage'))),

    bslib::card(
      bslib::card_header("Enter data in cells bellow. Double click to edit cells. Ctr+Enter to save."),
      bslib::card_body(
    DT::dataTableOutput(ns("manual_data_table"),
      ))),
    bslib::card(
      bslib::card_header("Prediction per cage"),
      bslib::card_body(
    shiny::plotOutput(ns("plot_cage")))
  )))
}

#' calculate_cage_level_prediction Server Functions
#'
#' @noRd
mod_calculate_cage_level_prediction_server <-
  function(id, selected_language) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      translator <- golem::get_golem_options(which = "translator")
      i18n <- reactive({
        get_reactive_translator(translator, selected_language())
      })

      output$sidebar_cage <- shiny::renderUI({
        shiny::tagList(
          shiny::textInput(
            ns("locality_number"),
            label = i18n()$t("Locality Number"),
            value = NULL
          ),
          shiny::radioButtons(
            inputId = ns("additional_data"),
            label = i18n()$t("Additional data for your farm"),
            choices = setNames(1:2, c(
              "Upload table in csv format",
              "Enter data manually"
            )),
            selected = 1
          ),
          shiny::conditionalPanel(
            condition = sprintf("input[['%s']] === '1'", ns("additional_data")),
            div(
              class = "d-flex flex-column",
              shiny::fileInput(ns("csvdata"),
                               label = i18n()$t("Choose File")),
              shiny::p(
                shiny::a(
                  shiny::span(shiny::icon("download"), "Download example csv"),
                  href = "/www/lusedata.csv",
                  target = "blank",
                  class = "btn-primary main-button"
                )
              )
            )
          ),
          shiny::conditionalPanel(condition = sprintf(
            "input[['%s']] === '2'", ns("additional_data"))
          ),
          shiny::actionButton(
            inputId = ns("predict"),
            label = i18n()$t("Predict")
          ),
          shiny::helpText("Bla bla bla")
        )
      })

      observe({
        choices_radio_1 <- setNames(1:2, i18n()$t(
          c(
            "Enter the 5-digit locality number and get infection pressure automatically",
            "Select infection pressure manually"
          )
        ))
        updateRadioButtons(
          session,
          "infectious_pressure",
          label = i18n()$t("Infectious Pressure"),
          choices = choices_radio_1
        )
        choices_radio_3 <- setNames(1:2, i18n()$t(c(
          "Upload table in csv format",
          "Enter data manually"
        )))
        updateRadioButtons(
          session,
          "additional_data",
          label = i18n()$t("Additional data for your farm"),
          choices = choices_radio_3
        )
      })

      ##### --- CSV DATA --- #####

      user_data <- shiny::reactive({
        if (!is.null(input$csvdata)) {
          sample_data <- read.csv(
            input$csvdata$datapath,
            header = T, sep = ";", dec = ","
          )
        }
        return(sample_data)
      })

      ##### --- MANUAL DATA --- #####

      observeEvent(input$additional_data, {
        if (input$additional_data == 2) {
          output$manual_data_table <- DT::renderDataTable(
            empty,
            # empty data frame, see data-raw for more.
            options = list(
              pageLength = 20,
              dom = "t",
              scrollY = "200px"
            ),
            selection = "none",
            editable = "row",
            server = FALSE,
            class = "cell-border stripe",
            caption = "Double click to edit cells. Ctr+Enter to save."
          )
        }
      })

      manual_data_rct <- shiny::reactiveVal(empty)

      observeEvent(input$manual_data_table_cell_edit, {
        modified_row <-as.numeric(input$manual_data_table_cell_edit$value[1])
        entered_values <-
          as.numeric(input$manual_data_table_cell_edit$value[2:7])

        current_manual_data <- manual_data_rct()
        current_manual_data[modified_row, ] <- entered_values
        names(current_manual_data) <- names(luse_demo_data)
        manual_data_rct(current_manual_data)
      })

      # uncomment for debugging
      observeEvent(manual_data_rct(), {
        print(manual_data_rct())
      })

    })
  }

## To be copied in the UI
# mod_calculate_cage_level_prediction_ui("calculate_cage_level_prediction_1")

## To be copied in the server
# mod_calculate_cage_level_prediction_server("calculate_cage_level_prediction_1")
