#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  translator <- golem::get_golem_options(which = "translator")
  i18n <- reactive({
    get_reactive_translator(translator, input$selected_language)
  })

  output$app_title <- renderUI({
    shiny::h3(i18n()$t("Lice Calculator"))
  })

  output$home_tab <- renderUI({
    shiny::h6(i18n()$t("Home"))
  })

  output$farm_tab <- renderUI({
    shiny::h6(i18n()$t("Farm Prediction"))
  })

  output$cage_tab <- renderUI({
    shiny::h6(i18n()$t("Cage Prediction"))
  })

  output$game_tab <- renderUI({
    shiny::h6(i18n()$t("Manual Input"))
  })

  output$about_tab <- renderUI({
    shiny::h6(i18n()$t("About"))
  })

  mod_home_server("home_1", reactive({
    input$selected_language
  }))

  mod_calculate_farm_level_prediction_server("calculate_farm_level_prediction_1",
                                             reactive({
                                               input$selected_language
                                             }))

  mod_calculate_cage_level_prediction_server("calculate_cage_level_prediction_1",
                                             reactive({
                                               input$selected_language
                                             }))

  mod_calculate_manual_input_prediction_server("calculate_manual_input_prediction_1",
                                               reactive({
                                                 input$selected_language
                                               }))

  mod_about_server("about_1", reactive({
    input$selected_language
  }))
}
