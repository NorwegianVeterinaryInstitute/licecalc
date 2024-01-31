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
  tagList(
    div(
    class = "d-flex flex-row p-2",
    style = "gap: 1rem;",
    div(class = "col-4 bg-light border rounded shadow-sm p-3",
        shiny::uiOutput(ns('sidebar_cage'))
        ),
    div(class = "col-8 border rounded shadow-sm p-2",
        h5(
          shiny::uiOutput(ns("card_title_plot"))),
          shiny::plotOutput(ns("plot_cage"))
        ))
  )
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
          shiny::helpText("Enter 5-digit code to obtain this week’s 
                          lice counts and sea temperature automatically."),
          shiny::br(),
          shiny::br(),
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
                               label = i18n()$t("Choose File"))
            )
          ),
          shiny::conditionalPanel(condition = sprintf(
            "input[['%s']] === '2'", ns("additional_data"))
          ),
          shiny::p(
            shiny::a(
              shiny::span(shiny::icon("download"), i18n()$t("Download example csv")),
              href = "/www/lusedata.csv",
              target = "blank",
              class = "btn-primary main-button"
            )
          ),
          shiny::uiOutput(ns("manual_data_card")),
          shiny::actionButton(
            inputId = ns("predict"),
            label = i18n()$t("Predict")
          ),
          shiny::br(),
          shiny::helpText(i18n()$t("Click here to predict 1 and 2 weeks ahead. Circles will show the expected number of salmon lice of different stages per fish. The broad bands will show  90% prediction intervals for the true lice abundance. Thin bands will show 90% prediction intervals for lice counts, assuming lice are counted on 20 fish in each cage."))
        )
      })
      
      observe({
      #### THIS IS OBSOLETE CODE 
      #     choices_radio_1 <- setNames(1:2, i18n()$t(
      #     c(
      #       "Enter the 5-digit locality number and get infection pressure automatically",
      #       "Select infection pressure manually"
      #     )
      #   ))
      #   updateRadioButtons(
      #     session,
      #     "infectious_pressure",
      #     label = i18n()$t("Infectious Pressure"),
      #     choices = choices_radio_1
      #   )
      ####
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

      manual_data_ui <- eventReactive(input$additional_data,
                    {
                      if (input$additional_data == 2) {
                        if (i18n()$get_translation_language() == 'en') {
                          ui <- shiny::tagList(shiny::helpText(
                              "Enter data in cells bellow. Check the demodata file for guidance."
                            ),
                            DT::dataTableOutput(ns(
                              "manual_data_table"
                            ), )
                          )
                        } else {
                          ui <- shiny::tagList(shiny::helpText(

                              "Skriv inn data i cellene nedenfor. Sjekk demodatafilen for veiledning."
                            ),
                            DT::dataTableOutput(ns(
                              "manual_data_table"
                            ), )
                          )
                        }

                      } else {
                        ui <- shiny::div()
                      }

                      return(ui)
                    })

      output$manual_data_card <- shiny::renderUI(
        {
          manual_data_ui()
        }
      )

      card_title_ui <- reactive({
        if (i18n()$get_translation_language() == 'en') {
          ui <-
            shiny::renderText(
              "Prediction per cage"
            )
        } else {
          ui <-
            shiny::renderText(
              "Prediksjon per bur"
            )
        }})


      output$card_title_plot <- shiny::renderUI(
        {
          card_title_ui()
        }
      )

      observeEvent(input$additional_data, {
        if (input$additional_data == 2) {
          if (i18n()$get_translation_language() == 'en') {
            dat <- empty
            caption = "Double click to edit cells. Ctr+Enter to save."
          } else {
            dat = empty_nb
            caption = "Dobbeltklikk for å redigere celler. Ctr+Enter for å lagre."
          }
          output$manual_data_table <- DT::renderDataTable(

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
            ), server = FALSE)

    } })


      manual_data_rct <- shiny::reactiveVal(empty)

      observeEvent(input$manual_data_table_cell_edit, {
        modified_row <-as.numeric(input$manual_data_table_cell_edit$value[1])
        entered_values <-
          input$manual_data_table_cell_edit$value[2:7]

        if (!entered_values[[6]] %in% c(0, 1)) {
          showModal(

            if (i18n()$get_translation_language() == 'en') {
            modalDialog(
            title = "Error",
            sprintf(
              "The %s column accepts only 0 (no), or 1 (yes)",
              names(luse_demo_data)[[6]]
            )
            )} else {
              modalDialog(
                title = "Feil",
                sprintf(
                  "%s kolonnen godtar bare 0 (nei) eller 1 (ja)",
                  names(luse_demo_data)[[6]]
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

      output$plot_cage <- shiny::renderPlot({
        if (input$additional_data == 2) {
          print(input$additional_data)
          tryCatch(
            expr = {
              make_plot_for_cages_and_location(location = input$locality_number,
                                               user_data = manual_data_rct(),
                                               lang = i18n()$get_translation_language())
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
        }
        else {
          tryCatch(
            expr = {
              make_plot_for_cages_and_location(location = input$locality_number,
                                               user_data = user_data(),
                                               lang = i18n()$get_translation_language())
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
        }

      }) |> shiny::bindEvent(input$predict)

    })
  }

## To be copied in the UI
# mod_calculate_cage_level_prediction_ui("calculate_cage_level_prediction_1")

## To be copied in the server
# mod_calculate_cage_level_prediction_server("calculate_cage_level_prediction_1")
