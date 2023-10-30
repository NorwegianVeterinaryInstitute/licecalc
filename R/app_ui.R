#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("en")

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shiny.i18n::usei18n(i18n),

    # Font setup

    # Your application UI logic
    bslib::page_navbar(
      theme = bslib::bs_theme(
        bootswatch = "zephyr",
        version = 5,
        font_scale = 0.8,
         base_font = bslib::font_face(
           family = "Futura PT",
           style = "Medium",
           src = "www/Futura PT Medium.ttf"
        )
      ),
      inverse = FALSE,

      bslib::nav_item(
        shiny::tags$a(
          style = "padding: 0;",
          shiny::img(src = "www/vet-rgb-2.svg", height = "40px"),
          href = "https://www.vetinst.no/",
          target = "_blank"
        )
      ),
      bslib::nav_spacer(),
      bslib::nav_item(shiny::uiOutput("app_title")),

      bslib::nav_spacer(),
      bslib::nav_panel(title = "Home", mod_home_ui("home_1")),
      bslib::nav_panel(
        title = "Farm Prediction",
        mod_calculate_farm_level_prediction_ui("calculate_farm_level_prediction_1")
      ),
      bslib::nav_panel(
        title = "Cage Prediction",
        mod_calculate_cage_level_prediction_ui("calculate_cage_level_prediction_1")
        ),
      bslib::nav_panel(
        title = "Manual Input",
        mod_calculate_manual_input_prediction_ui("calculate_manual_input_prediction_1")),
      bslib::nav_panel(title = "About", mod_about_ui("about_1")),
      bslib::nav_item(
        shiny::selectInput(
          inputId = "selected_language",
          label = NULL,
          choices = c("en", "nb"),
          width = '75px'
        )
      ),

    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path("www",
                    app_sys("app/www"))

  tags$head(favicon(),
            bundle_resources(path = app_sys("app/www"),
                             app_title = "licecalc")
            # Add here other external resources
            # for example, you can add shinyalert::useShinyalert()
      )
}
