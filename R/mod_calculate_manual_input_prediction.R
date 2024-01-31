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
    div(class = "col-4 bg-light border rounded shadow-sm p-3",
        shiny::uiOutput(ns('sidebar_cage'))),
    div(class = "col-8 border rounded shadow-sm p-2",
        h5(shiny::uiOutput(ns("card_title_plot"))),
        shiny::plotOutput(ns("plot_cage_game")))
  )
}

#' calculate_manual_input_prediction Server Functions
#'
#' @noRd
mod_calculate_manual_input_prediction_server <-
  function(id, selected_language) {
    moduleServer(id, function(input, output, session) {
      ns <- session$ns
      translator <- golem::get_golem_options(which = "translator")
      i18n <- reactive({
        get_reactive_translator(translator, selected_language())
      })

      output$sidebar_cage <- shiny::renderUI({
        shiny::tagList(
          div(
            class = "d-flex flex-row align-items-end",
            style = "gap: 1rem;",
            div(
              class = "col-4",
              shiny::numericInput(
                inputId = ns("infectious_pressure_manually_1week"),
                label = i18n()$t("Infectious pressure first week"),
                min = 0,
                max = 300000,
                step = 2000,
                value = 100000,
                width = "75%"
              )
            ),
            div(
              class = "col-4",
              shiny::numericInput(
                inputId = ns("infectious_pressure_manually_2week"),
                label = i18n()$t("Infectious pressure second week"),
                min = 0,
                max = 300000,
                step = 2000,
                value = 100000,
                width = "75%"
              )
            ),
            div(
              class = "col-4",
              shiny::numericInput(
                inputId = ns("sea_temperature_manually"),
                label = i18n()$t("Sea temperature"),
                min = 3,
                max = 18,
                step = 1,
                value = 9,
                width = "75%"
              )
            )
          ),
          shiny::uiOutput(ns("manual_data_card")),
          shiny::actionButton(
            inputId = ns("predict"),
            label = i18n()$t("Predict")
          ),
          shiny::br(),
          shiny::helpText( i18n()$t(
            "Click here to predict 1 and 2 weeks ahead. Circles will show the expected number of salmon lice of different stages per fish. The broad bands will show  90% prediction intervals for the true lice abundance. Thin bands will show 90% prediction intervals for lice counts, assuming lice are counted on 20 fish in each cage."))
        )
      })

      card_title_ui <- reactive({
        if (i18n()$get_translation_language() == 'en') {
          ui <-
            shiny::renderText(
              "Model results"
            )
        } else {
          ui <-
            shiny::renderText(
              "Modellresultater"
            )
        }})


      output$card_title_plot <- shiny::renderUI(
        {
          card_title_ui()
        }
      )

      ##### --- MANUAL DATA --- #####

      manual_data_ui <- reactive(
                                      {

                                          if (i18n()$get_translation_language() == 'en') {
                                            ui <- shiny::tagList(
                                              shiny::helpText(
                                                "Enter data in cells bellow. Check the demodata file for guidance."
                                              ),
                                              DT::dataTableOutput(ns("manual_data_table_game"),)
                                            )
                                          } else {
                                            ui <- shiny::tagList(
                                              shiny::helpText(
                                                "Skriv inn data i cellene nedenfor. Sjekk demodatafilen for veiledning."
                                              ),
                                              DT::dataTableOutput(ns("manual_data_table_game"),)
                                            )
                                          }



                                        return(ui)
                                      })

      output$manual_data_card <- shiny::renderUI({
        manual_data_ui()
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
              dat,  # empty data frame, see data-raw for more.
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

        }, server = FALSE)

      manual_data_rct <- shiny::reactiveVal(empty)

      observeEvent(input$manual_data_table_game_cell_edit, {
        modified_row <-
          as.numeric(input$manual_data_table_game_cell_edit$value[1])
        entered_values <-
          input$manual_data_table_game_cell_edit$value[2:7]

        if (!entered_values[[6]] %in% c(0, 1)) {
          showModal(if (i18n()$get_translation_language() == 'en') {
            modalDialog(title = "Error",
                        sprintf(
                          "The %s column accepts only 0 (no), or 1 (yes)",
                          names(empty)[[6]]
                        ))
          } else {
            modalDialog(title = "Feil",
                        sprintf(
                          "%s kolonnen godtar bare 0 (nei) eller 1 (ja)",
                          names(empty_nb)[[6]]
                        ))
          })
          return(NULL)
        }

        current_manual_data <- manual_data_rct()
        current_manual_data[modified_row,] <- entered_values
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
            showModal(if (i18n()$get_translation_language() == 'en') {
              modalDialog(
                title = "Error",
                "Something is wrong.  We could not generate predictions.",
                footer = modalButton("Dismiss")
              )
            }  else {
              modalDialog(
                title = "Feil",
                "Noe er galt. Vi kunne ikke generere spådommer.",
                footer = modalButton("Avskjedige")
              )

            })
          }
        )
      }) |> shiny::bindEvent(input$predict)
    })
  }

## To be copied in the UI
# mod_calculate_manual_input_prediction_ui("calculate_manual_input_prediction_1")

## To be copied in the server
# mod_calculate_manual_input_prediction_server("calculate_manual_input_prediction_1")
