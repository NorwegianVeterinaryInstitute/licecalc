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
      shiny::radioButtons(
        inputId = ns("additional_data"),
        label = i18n$t("Additional data for your farm"),
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
          shiny::fileInput(
            ns("csvdata"),
            label = i18n$t("Choose File")
          ),
          shiny::p(
            shiny::a(
              shiny::span(shiny::icon("download"), "Download example csv"),
              href = "/www/lusedata.csv",
              target = "blank",
              class = "btn-primary main-button"
            ))
        )
      ),
      shiny::conditionalPanel(
        condition = sprintf("input[['%s']] === '2'", ns("additional_data"))#,
        #div(
        #  class = "d-flex flex-column",
        #  shiny::h5(
        #    ns("manual_data")#,
        #label = i18n$t("Select Value")
        #)
        #)
      ),
      shiny::actionButton(
        inputId = ns("calculate_development"),
        label = i18n$t("Calculate Development")
      )

    ),
    shiny::mainPanel(
      DT::dataTableOutput(ns("manual_data_table")),
      shiny::plotOutput(ns("prediction_plot"))
    )
  ))
}

#' lice_model_v2 Server Functions
#'
#' @noRd
mod_lice_model_v2_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ##### --- TRANSLATION --- #####

    i18n_r <- reactive({
      golem::get_golem_options(which = "translator")
    })

    observeEvent(input$language, {
      shiny::req(input$language)
      shiny.i18n::update_lang(session = session, language = input$language)
      i18n_r()$set_translation_language(input$language)
    })

    observe({
      choices_radio_1 <- setNames(1:2, i18n_r()$t(c(
        "Enter the 5-digit locality number and get infection pressure automatically",
        "Select infection pressure manually"
      )))
      updateRadioButtons(session, "infectious_pressure",
                         label = i18n_r()$t("Infectious Pressure"),
                         choices = choices_radio_1
      )
      choices_radio_3 <- setNames(1:2, i18n_r()$t(c(
        "Upload table in csv format",
        "Enter data manually"
      )))
      updateRadioButtons(session, "additional_data",
                         label = i18n_r()$t("Additional data for your farm"),
                         choices = choices_radio_3
      )
    })
    ##### --- MANUAL DATA --- #####

    observeEvent(input$additional_data, {
      if (input$additional_data == 2) {
        output$manual_data_table <- DT::renderDataTable(
          empty, # empty data frame, see data-raw for more.
          options = list(pageLength = 20, dom = "t", scrollY = "200px"),
          selection = "none",
          editable = "row", server = FALSE, class = "cell-border stripe",
          caption = "Double click to edit cells. Ctr+Enter to save."
        )
      }
    })

    manual_data_rct <- shiny::reactiveVal(na.omit(empty))

    observeEvent(input$manual_data_table_cell_edit, {
      entered_values <- as.numeric(input$manual_data_table_cell_edit$value[2:5])

      if (!entered_values[[3]] %in% c(0, 1)) {
        showModal(modalDialog(
          title = "Error",
          sprintf(
            "The %s column accepts only 0 (no), or 1 (yes)",
            names(luse_demo_data)[[3]]
          )
        ))
        return(NULL)
      }

      if (!entered_values[[4]] %in% c(0, 1)) {
        showModal(modalDialog(
          title = "Error",
          sprintf(
            "The %s column accepts only 0 (no), or 1 (yes)",
            names(luse_demo_data)[[4]]
          )
        ))
        return(NULL)
      }

      current_manual_data <- manual_data_rct()
      new_manual_data <- rbind(current_manual_data, entered_values)
      names(new_manual_data) <- names(luse_demo_data)
      manual_data_rct(new_manual_data)
    })

    # uncomment for debugging
    observeEvent(manual_data_rct(), {
      print(manual_data_rct())
    })

    ##### --- PLOTS --- #####

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
