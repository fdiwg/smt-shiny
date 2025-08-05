

run_spict <- function(inp) {
    set.seed(1)

    returnResults <- list()
    out <- tryCatch( {

        print(paste0("spict version: ", packageVersion("spict")))

        ##--------------------
        ##  Checks
        ##--------------------
        ## TODO

        ## withProgress(message = "Running SPiCT", value = 0, {
        res <- spict::fit.spict(inp)
        res <- spict::calc.osa.resid(res)
        res <- spict::calc.process.resid(res)
        ## })

        ## TODO informative error message if non converged?

        returnResults[['res']] <- res
    },
    error=function(cond) {
        print("There was an error here")
        message(paste0("Error!!", cond))
        errorResult = list()
        errorResult[['error']] <- gettext(cond)
        return (errorResult)
        ## Choose a return value in case of error
    },
    finally={
        print("Done")
    })
    if ('error' %in% names(out)) {
        returnResults[['error']] <- out$error
    }
    return (returnResults)
}
