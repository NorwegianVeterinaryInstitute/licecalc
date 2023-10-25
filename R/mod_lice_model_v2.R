#' lice_model_v2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_lice_model_v2_ui <- function(id) {
  ns <- NS(id)
  i18n <- golem::get_golem_options(which = "translator")

  tagList(shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::selectInput(
        ns("language"),
        label = i18n$t("Language"),
        choices = i18n$get_languages(),
        selected = i18n$get_key_translation()
      ),
      shiny::h4(
        i18n$t("Calculate the expected development of salmon lice per cage")
      ),
      shiny::radioButtons(
        inputId = ns("infectious_pressure"),
        label = i18n$t("Infectious pressure"),
        choices = setNames(
          1:2,
          c(
            "Enter the 5-digit locality number and get infection pressure automatically",
            "Select infection pressure manually"
          )
        ),
        selected = 1
      ),
      shiny::conditionalPanel(
        condition = sprintf("input[['%s']] === '1'", ns("infectious_pressure")),
        div(class = "d-flex flex-column",
            shiny::textInput(
              ns("locality_number"),
              label = i18n$t("Locality Number"),
              value = NULL
            ))
      ),
      shiny::actionButton(
        inputId = ns("calculate_development"),
        label = i18n$t("Calculate Development")
      )

    ),
    shiny::mainPanel(shiny::plotOutput(ns(
      "prediction_plot"
    )))
  ))
}

#' lice_model_v2 Server Functions
#'
#' @noRd
mod_lice_model_v2_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$prediction_plot <- shiny::renderPlot({
      shiny::validate(
        shiny::need(
          input$locality_number %in% rownames(infestation_pressure_all),
          message = "Please enter farm locality number."
        )
      )
      tryCatch(
        expr = {
          make_plot_from_location(input$locality_number)
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
    }) |> shiny::bindEvent(input$calculate_development)

  })
}

## To be copied in the UI
# mod_lice_model_v2_ui("lice_model_v2_1")

## To be copied in the server
# mod_lice_model_v2_server("lice_model_v2_1")
