tabJABBA <- function(id) {

    ns <- NS(id)

    tabItem("jabbaWidget",

            htmlOutput(ns("jabbaTitle"))

            )
}
