#' plot_lice UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_lice_ui <- function(id) {
  ns <- NS(id)
  i18n <- golem::get_golem_options(which = "translator")

  tagList(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(ns("language"),
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
          choices = setNames(1:2, c(
            "Enter the 5-digit locality number and get infection pressure automatically",
            "Select infection pressure manually"
          )),
          selected = 2
        ),
        shiny::conditionalPanel(
          condition = sprintf("input[['%s']] === '1'", ns("infectious_pressure")),
          div(
            class = "d-flex flex-column",
            shiny::numericInput(
              ns("locality_number"),
              label = i18n$t("Locality Number"),
              value = 0
            ),
            div(
              class = "h5",
              value = i18n$t("Start the calculation from")
            ),
            div(
              class = "d-flex flex-row",
              shiny::numericInput(
                ns("start_year"),
                label = i18n$t("Start Year"),
                value = 2017
              ),
              shiny::numericInput(
                ns("start_week"),
                label = i18n$t("Start Week"),
                value = 1,
                min = 1,
                max = 52,
                step = 1
              )
            )
          )
        ),
        shiny::conditionalPanel(
          condition = sprintf(
            "input[['%s']] === '2'", ns("infectious_pressure")
          ),
          div(
            class = "d-flex flex-column",
            shiny::sliderInput(
              ns("infectious_pressure_manually"),
              label = i18n$t("Select Value"),
              min = 0,
              max = 22,
              step = 1,
              ticks = FALSE,
              value = 15
            )
          )
        ),
        shiny::radioButtons(
          inputId = ns("treatment_strategy"),
          label = i18n$t("Treatment strategy against lice on your farm"),
          choices = c(
            "The entire locality",
            "Many Times"
          )
        ),
        shiny::radioButtons(
          inputId = ns("additional_data"),
          label = i18n$t("Additional data for your farm"),
          choices = setNames(1:2, c(
            "Upload table in csv format (download example.csv)",
            "Enter data manually"
          )),
          selected = 1
        ),
        DT::dataTableOutput(ns("manual_data_table")),
        shiny::conditionalPanel(
          condition = sprintf("input[['%s']] === '1'", ns("additional_data")),
          div(
            class = "d-flex flex-column",
            shiny::fileInput(
              ns("csvdata"),
              label = i18n$t("Choose File")
            )
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
        shiny::plotOutput(ns("plot_lice_1")),
        shiny::plotOutput(ns("plot_lice_2")),
        shiny::plotOutput(ns("plot_lice_3")),
        shiny::plotOutput(ns("plot_lice_4"))
      )
    )
  )
}

#' plot_lice Server Functions
#'
#' @noRd
mod_plot_lice_server <- function(id) {
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

      choices_radio_2 <- i18n_r()$t(c(
        "The entire locality",
        "Many Times"
      ))
      updateRadioButtons(session, "treatment_strategy",
        label = i18n_r()$t("Treatment strategy against lice on your farm"),
        choices = choices_radio_2
      )

      choices_radio_3 <- setNames(1:2, i18n_r()$t(c(
        "Upload table in csv format (download example.csv)",
        "Enter data manually"
      )))
      updateRadioButtons(session, "additional_data",
        label = i18n_r()$t("Additional data for your farm"),
        choices = choices_radio_3
      )
    })

    ##### --- DATA --- #####

    user_data <- shiny::reactive({
      sample_data <- luse_demo_data
      if (!is.null(input$csvdata)) {
        sample_data <- read.csv(
          input$csvdata$datapath,
          header = T, sep = ";", dec = ","
        )
      }
      return(sample_data)
    })

    mytid <- reactive({
      week <- input$start_week
      year <- input$start_year
      if (length(week) > 1) {
        stid <- paste0(year, week)
      } else {
        stid <- paste0(year, "0", week)
      }
      stid
    })

    ##### --- MANUAL DATA --- #####

    observeEvent(input$additional_data, {
      if (input$additional_data == 2) {
        output$manual_data_table <- DT::renderDataTable(
          empty,
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

    output$plot_lice_1 <- shiny::renderPlot({
      if (nrow(manual_data_rct()) > 0) {
        data_to_plot <- manual_data_rct()
      } else {
        data_to_plot <- user_data()
      }

      plotLok(
        loknr = input$locality_number,
        valgtSm = input$infectious_pressure_manually,
        tid = mytid(),
        innlestData = data_to_plot,
        plotnr = 1,
        hele = myhele,
        merd = input$treatment_strategy
      )
    }) |>
      bindEvent(input$calculate_development)

    output$plot_lice_2 <- shiny::renderPlot({
      if (nrow(manual_data_rct()) > 0) {
        data_to_plot <- manual_data_rct()
      } else {
        data_to_plot <- user_data()
      }

      plotLok(
        loknr = input$locality_number,
        valgtSm = input$infectious_pressure_manually,
        tid = mytid(),
        innlestData = data_to_plot,
        plotnr = 2,
        hele = myhele,
        merd = input$treatment_strategy
      )
    }) |>
      bindEvent(input$calculate_development)

    output$plot_lice_3 <- shiny::renderPlot({
      if (nrow(manual_data_rct()) > 0) {
        data_to_plot <- manual_data_rct()
      } else {
        data_to_plot <- user_data()
      }

      plotLok(
        loknr = input$locality_number,
        valgtSm = input$infectious_pressure_manually,
        tid = mytid(),
        innlestData = data_to_plot,
        plotnr = 2,
        hele = myhele,
        merd = input$treatment_strategy
      )
    }) |>
      bindEvent(input$calculate_development)

    output$plot_lice_4 <- shiny::renderPlot({
      if (nrow(manual_data_rct()) > 0) {
        data_to_plot <- manual_data_rct()
      } else {
        data_to_plot <- user_data()
      }

      plotLok(
        loknr = input$locality_number,
        valgtSm = input$infectious_pressure_manually,
        tid = mytid(),
        innlestData = data_to_plot,
        plotnr = 4,
        hele = myhele,
        merd = input$treatment_strategy
      )
    }) |>
      bindEvent(input$calculate_development)
  })
}

## To be copied in the UI
# mod_plot_lice_ui("plot_lice_1")

## To be copied in the server
# mod_plot_lice_server("plot_lice_1")
