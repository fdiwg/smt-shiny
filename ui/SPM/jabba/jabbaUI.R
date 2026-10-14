

tabJABBA <- function(id) {
  
  ns <- NS(id)
  
  tabItem("jabbaWidget",
          
          htmlOutput(ns("jabbaTitle")),
          
          
          div(
            class = "alert alert-info",
            role = "status",
            style = "margin: 10px 0;",
            tags$b("JABBA coming soon. "),
            "We are working to make this method available soon in the SMT. ",
            "In the meantime, you can use the other available assessment methods."
          )
          
  )
}
