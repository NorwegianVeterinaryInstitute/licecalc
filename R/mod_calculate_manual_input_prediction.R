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
  div(
    class = "d-flex flex-row p-2",
    style = "gap: 1rem;",
    div(
      class = "col-4 bg-light border rounded shadow-sm p-3",
      h5(class = "mb-2", "Salmon farm data"),
      tags$hr(),
      div(style = "margin-bottom: 1rem;"),
      shiny::helpText(
        "Enter farm-level data on infectious pressure and water temperature"
      ),
      div(
        class = "d-flex flex-row align-items-end",
        style = "gap: 1rem;",
        div(
          class = "col-4",
          shiny::numericInput(
            inputId = ns("infectious_pressure_manually_1week"),
            label = "Infectious pressure first week",
            min = 0,
            max = 22,
            step = 1,
            value = 15,
            width = "75%"
          )
        ),
        div(
          class = "col-4",
          shiny::numericInput(
            inputId = ns("infectious_pressure_manually_2week"),
            label = "Infectious pressure second week",
            min = 0,
            max = 22,
            step = 1,
            value = 15,
            width = "75%"
          )
        ),
        div(
          class = "col-4",
          shiny::numericInput(
            inputId = ns("sea_temperature_manually"),
            label = "Sea temperature",
            min = 0,
            max = 22,
            step = 1,
            value = 15,
            width = "75%"
          )
        )
      ),
      tags$hr(),
      shiny::helpText("Enter data per cage in the table below. To start, double
      click on a cell to enable editing of the whole row. Press `Ctrl+Enter`
      to save the data and `Esc` to cancel."),
      DT::dataTableOutput(ns(
        "manual_data_table_game"
      )),
      shiny::actionButton(
        inputId = ns("predict"),
        label = "Predict"#,
      #  class = "btn btn-lg btn-success mt-2"
      )
    ),
    div(
      class = "col-8 border rounded shadow-sm p-2",
      h5("Model results"),
      shiny::plotOutput(ns("plot_cage_game"))
    )
  )
}


#' calculate_manual_input_prediction Server Functions
#'
#' @noRd
mod_calculate_manual_input_prediction_server <- function(id, selected_language){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$manual_data_table_game <- DT::renderDataTable(
      {
        # if (i18n()$get_translation_language() == "en") {
        #   dat <- empty
        #   caption <- "Double click to edit cells. Ctr+Enter to save."
        # } else {
        #   dat <- empty_nb
        #   caption <- "Dobbeltklikk for å redigere celler. Ctr+Enter for å lagre."
        # }

        dat <- empty
        caption <- ""

        DT::datatable(
          dat,
          # empty data frame, see data-raw for more.
          options = list(
            pageLength = 20,
            dom = "t",
            # scrollY = "400px",
            autowidth = FALSE,
            columnDefs = list(
              list(width = "25px", targets = 0),
              list(width = "25px", targets = c(1:6))
            ),
            ordering = FALSE
          ),
          selection = "none",
          editable = list(
            target = "row",
            disable = list(columns = 0),
            area = c(1, 2, 3, 4, 5)
          ),
          class = "cell-border compact stripe",
          caption = caption,
          style = "bootstrap5"
        )
      },
      server = FALSE
    )

    manual_data_rct <- shiny::reactiveVal(empty)

    observeEvent(input$manual_data_table_game_cell_edit, {
      modified_row <- as.numeric(input$manual_data_table_game_cell_edit$value[1])
      entered_values <-
        input$manual_data_table_game_cell_edit$value[2:7]

      if (!entered_values[[6]] %in% c(0, 1)) {
        showModal(
          if (i18n()$get_translation_language() == 'en') {
            modalDialog(
              title = "Error",
              sprintf(
                "The %s column accepts only 0 (no), or 1 (yes)",
                names(empty)[[6]]
              )
            )} else {
              modalDialog(
                title = "Feil",
                sprintf(
                  "%s kolonnen godtar bare 0 (nei) eller 1 (ja)",
                  names(empty_nb)[[6]]
                )
              )
            }
        )
        return(NULL)
      }

      current_manual_data <- manual_data_rct()
      current_manual_data[modified_row, ] <- entered_values
      names(current_manual_data) <- names(luse_demo_data)
      manual_data_rct(current_manual_data)
    })

    # uncomment for debugging
    # observeEvent(manual_data_rct(), {
    #  print(manual_data_rct())
    # })


    output$plot_cage_game <- shiny::renderPlot({
      tryCatch(
        expr = {
          make_plot_for_game(
            ip1 = input$infectious_pressure_manually_1week,
            ip2 = input$infectious_pressure_manually_2week,
            st = input$sea_temperature_manually,
            user_data = manual_data_rct(),
            lang = "en" # i18n()$get_translation_language()
          )
        },
        error = function(e) {
          showModal(

            if (i18n()$get_translation_language() == 'en') {
              modalDialog(title = "Error",
                          "The locality was found, but we could not generate predictions.",
                          footer = modalButton("Dismiss"))
            }  else {
              modalDialog(title = "Feil",
                          "Lokaliteten ble funnet, men vi kunne ikke generere spådommer.",
                          footer = modalButton("Avskjedige"))

            }

          )
        }
      )
    }) |> shiny::bindEvent(input$predict)
  })
}

## To be copied in the UI
# mod_calculate_manual_input_prediction_ui("calculate_manual_input_prediction_1")

## To be copied in the server
# mod_calculate_manual_input_prediction_server("calculate_manual_input_prediction_1")
