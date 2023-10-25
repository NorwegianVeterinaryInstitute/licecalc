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

    # Your application UI logic
    bslib::page_fluid(theme = bslib::bs_theme(bootswatch = "flatly", version = 5, font_scale = 0.8),
    shiny.i18n::usei18n(i18n),
    tags$header(div(class = "d-flex flex-row justify-content-between header-container align-items-center",
    img(class = "header-left", src = "www/vet-rgb-2.svg"),
    div(class = "header-middle", i18n$t("Lice Calculator")),
    div(class = "header-right", shiny::actionLink(inputId = "docs", label = i18n$t("About the calculator")))
)),

    #mod_plot_lice_ui("plot_lice_1")
    mod_lice_model_v2_ui("lice_model_v2_1")
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
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "licecalc"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
