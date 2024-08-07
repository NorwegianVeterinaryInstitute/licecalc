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
    div(
      class = "d-flex flex-row p-2",
      style = "gap: 1rem;",
      div(
        class = "col-3 bg-light border rounded shadow-sm p-3",
    shiny::uiOutput(ns('sidebar_farm'))
  ),
  div(
    class = "col-9 border rounded shadow-sm p-2",
    h5(shiny::uiOutput(ns("card_title_plot"))),
    shiny::plotOutput(ns("plot_farm")))
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

    card_title_ui <- reactive({
      if (i18n()$get_translation_language() == 'en') {
        ui <-
          shiny::renderText(
            "Prediction per farm"
          )
      } else {
        ui <-
          shiny::renderText(
            "Prediksjon per gård"
          )
      }})


    output$card_title_plot <- shiny::renderUI(
      {
        card_title_ui()
      }
    )

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
    ) |> shiny::bindEvent(input$predict)

  })
}

## To be copied in the UI
# mod_calculate_farm_level_prediction_ui("calculate_farm_level_prediction_1")

## To be copied in the server
# mod_calculate_farm_level_prediction_server("calculate_farm_level_prediction_1")
