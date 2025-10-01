jabbaModule <- function(input, output, session) {

    ns <- session$ns

    output$jabbaTitle <- renderText({
        session$userData$page("jabba")
        text <- "<span><h3><b>Data-limited stock assessment with JABBA</b></h3></span>"
        text
    })


}
