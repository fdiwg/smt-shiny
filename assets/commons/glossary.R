

## Initial glossary data frame
glossary_df <- data.frame(
    Term = character(),
    Acronym = character(),
    Description = character(),
    stringsAsFactors = FALSE
)

## Function to add a row to the glossary
add2glossary <- function(term, acronym, description) {
    new_row <- data.frame(Term = term,
                          Acronym = acronym,
                          Description = description,
                          stringsAsFactors = FALSE)
    glossary_df <<- rbind(glossary_df, new_row)
}

add2glossary("Biomass", "B", "The total weight of all individuals in a fish stock.")
add2glossary("Spawning stock biomass", "SSB", "The total weight of all mature individuals in a fish stock.")
add2glossary("Exploitable stock biomass", "ESB", "The total weight of all exploitable individuals in a fish stock.")
add2glossary("Vulnerable biomass", "VB",
             "The total weight of all individuals in a fish stock that are vulnerable to fishing. This is similar to the exploitable stock biomass but might account for selectivity of the fishing gear but also availability of species.")
add2glossary("Fishing mortality", "F", "The proportion of a fish stock removed due to fishing over a period of time.")
add2glossary("Natural mortality", "M", "The proportion of a fish stock removed due to natural deaths over a period of time.")
add2glossary("Recruitment", "R", "The number of young fish entering the fishable or spawning population.")
add2glossary("Catch", "C", "The quantity of fish removed from the population by fishing.")
add2glossary("Catch per unit effort", "CPUE", "An index of relative abundance, calculated as the amount of catch per unit of fishing effort.")
add2glossary("Maximum sustainable yield", "MSY", "The largest long-term average catch that can be taken from a stock under prevailing environmental conditions.")
add2glossary("Biomass at MSY", "BMSY", "The biomass level that enables a stock to deliver the maximum sustainable yield.")
add2glossary("Fishing mortality at MSY", "FMSY", "The level of fishing mortality that allows a stock to deliver the maximum sustainable yield.")
add2glossary("Limit reference point", "LRP", "A biological threshold below which the stock is considered to be in a critical state.")
add2glossary("Target reference point", "TRP", "A desired level of biomass or fishing mortality that aims to maintain a stock at sustainable levels.")
add2glossary("Overfishing", NA, "A situation where fishing mortality exceeds the sustainable level (e.g., above FMSY).")
add2glossary("Overfished", NA, "A condition where the biomass of a stock is below a critical threshold (e.g., below BMSY or a limit reference point).")
add2glossary("Data-limited assessment", NA, "A stock assessment approach that uses minimal or indirect data, often catch or length data.")
add2glossary("Catch-only method", NA, "An assessment method that relies only on catch data, typically assuming priors for productivity (e.g., CMSY).")
add2glossary("Length-based method", NA, "A method that estimates stock status based on the length distribution of fish in the catch or survey (e.g., ELEFAN, LBSPR).")
add2glossary("CMSY", "CMSY", "A catch-only stock assessment method that estimates biomass trends using catch data and priors for productivity and resilience.")
add2glossary("ELEFAN", "ELEFAN", "A method for estimating growth parameters from length-frequency data using electronic length frequency analysis.")
add2glossary("Yield per recruit", "YPR", "A measure of how much catch can be expected per recruit over its lifetime at a given fishing mortality.")
add2glossary("Spawning biomass per recruit", "SBPR", "The amount of spawning biomass that can be expected per recruit over its lifetime at a given fishing mortality.")
add2glossary("Growth rate", "k", "The von Bertalanffy growth coefficient indicating how quickly a fish approaches its asymptotic size.")
add2glossary("Asymptotic length", "L∞", "The maximum average length a fish species can reach, according to the von Bertalanffy growth model.")
add2glossary("Length at maturity", "L50", "The length at which 50% of individuals in the population are sexually mature.")
add2glossary("Harvest control rule", "HCR", "A set of predefined rules or formulas that determine management actions (e.g., changes in fishing effort) based on stock status indicators.")
add2glossary("Productivity-Susceptibility Analysis", "PSA", "A qualitative method to assess the vulnerability of fish stocks based on their productivity and susceptibility to fishing.")
add2glossary("Management strategy evaluation", "MSE", "A simulation framework used to test and compare the performance of alternative management strategies under uncertainty.")
add2glossary("Life history parameters", NA, "Biological characteristics of a species, such as growth, maturity, and mortality, which are key to understanding stock dynamics.")
add2glossary("Mean length", NA, "The average length of individuals in a sample or population.")
add2glossary("Length-frequency data", NA, "Data that describe the number of individuals observed in different length classes.")
add2glossary("Monte Carlo simulation", NA, "A computational method using random sampling to explore the uncertainty of model outputs.")
add2glossary("Sensitivity analysis", NA, "A method to examine how changes in input assumptions or parameters affect model outcomes.")
add2glossary("Retrospective pattern", NA, "A pattern in which assessment results systematically change when more recent years of data are added.")
add2glossary("Empirical indicator", NA, "A simple measurable quantity (e.g., mean length or CPUE trend) used to infer the status of a stock.")
add2glossary("Bayesian method", NA, "A statistical approach that incorporates prior knowledge along with observed data to estimate model parameters.")
add2glossary("Maximum likelihood estimation", "MLE", "A statistical method for estimating parameters by maximizing the likelihood that the observed data would occur under the model.")

print(glossary_df)


render_glossary_html <- function(glossary_df) {
    glossary_df <- glossary_df[order(glossary_df$Term), ]  ## sort alphabetically

    html <- "<h4>Glossary of Terms</h4>"
    html <- paste0(html, "<ul>")

    for (i in seq_len(nrow(glossary_df))) {
        term <- glossary_df$Term[i]
        acronym <- glossary_df$Acronym[i]
        description <- glossary_df$Description[i]

        ## Format with or without acronym
        if (!is.na(acronym) && acronym != term) {
            html <- paste0(html, "<li><b>", term, " (", acronym, "):</b> ", description, "</li>")
        } else {
            html <- paste0(html, "<li><b>", term, ":</b> ", description, "</li>")
        }
    }

    html <- paste0(html, "</ul>")
    return(html)
}
