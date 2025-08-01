


tropfishrAssumptionsHTML <- function() {
    paste0(
        "<h4>Assumptions of TropFishR:</h4>",
        "<p>The methods implemented in TropFishR make specific assumptions about the data, the stock, and the fisheries targeting the stock. Understanding these assumptions is important to interpret the results appropriately.</p>",
        "<ul>",
        "<li><b>Representative length measurements:</b> The method assumes that the dataset is representative of the length distributions of the whole catch. This means that either the length of all individuals in the catch were measured or a randomized subsample of the catch was measured.</li>",
        "<li><b>Gear/fleet selectivity:</b> The method assumes sigmoidal selectivity, as commonly assumed for 'trawl-like' fishing gear. While gillnets and hook-based methods often have bell-shaped selectivity, the use of mixed mesh/hook sizes may result in an overall trawl-like selectivity.</li>",
        "<li><b>Equilibrium conditions:</b> The model assumes constant recruitment, fishing mortality, natural mortality, growth, and maturation within and across all years covered by the dataset.</li>",
        "<li><b>Density dependence:</b> The model assumes that maturity and somatic growth are independent of population density. All reference points are based on a per recruit model, without assuming a stock-recruitment relationship.</li>",
        "<li><b>Correlation of life-history parameters:</b> When natural mortality is estimated from empirical formulae, the model assumes that growth parameters (e.g., <i>L∞</i> and <i>K</i>) are reliable predictors of natural mortality.</li>",
        "<li><b>Length-independent natural mortality:</b> The current implementation of SMT assumes that the natural mortality is equal for all length classes.</li>",
        "<li><b>Somatic growth follows VBG:</b> Growth estimation with ELEFAN assumes logistic von Bertalanffy growth (VBG). While common for fish, this assumption may not hold for species with exoskeletons (e.g., crustaceans) or for early life stages.</li>",
        "<li><b>Closed population:</b> The method assumes no immigration or emigration. Violations of this assumption may bias estimates of mortality and stock status, especially if migration patterns vary by life stage.</li>",
        "</ul><br>"
    )
}


lbiAssumptionsHTML <- function() {
    paste0("<h4>Assumptions of LBIs:</h4>",
           "<p>LBIs make specific assumptions about the data, the stock, and the ",
           "fisheries targeting the stock. It is important to be aware of these assumptions and, thus, the limitations of the results.</p>",
           "<ul>",
           "<li><b>Representative length measurements:</b> The routine assumes that the dataset is representative of the length distributions of the whole catch. This means that either the length of all individuals in the catch were measured or a randomised subsample of the catch was measured.", "</li>",
           "<li><b>Equilibrium conditions:</b> The routine assumes constant recruitment, fishing and natural mortality as well as somatic growth and maturation over time, i.e. within the year and over all years covered by the dataset.</li>",
           "<li><b>Density dependence:</b> The routine assumes density independent maturity and somatic growth. All reference points are based on the 'per recruit' model, thus no assumptions are made about the stock recruitment relationship.</li>",
           "<li><b>Length-independent natural mortality:</b> The current implementation of SMT assumes that the natural mortality is equal for all length classes.</li>",
           "<li><b>Somatic growth follows VBG:</b> The estimation of growth parameters with ELEFAN assumes that the growth of individuals in length follows the logistic von Bertlanffy growth (VBG, function. This is an often made assumption for the growth of fish, but might not reflect well the growth of species with an exoskeleton, such as crustaceans, nor the growth of early life stages of fish.</li>",
           "<li><b>Closed population:</b> The routine assumes that the stock (population, under study is closed, meaning that there is no immigration or emigration taking place. Immigration and emigration can both bias estimated mortality rates and stock status. Furthermore, fish migrations often vary for various life stages and might thus affect the representativeness of the length measurements if the population is not closed.</li>",
           "</ul><br>"
           )
}


lbsprAssumptionsHTML <- function() {
    paste0("<h4>Assumptions of LBSPR:</h4>",
           "<p>LBSPR make specific assumptions about the data, the stock, and the ",
           "fisheries targeting the stock. It is important to be aware of these assumptions and, thus, the ",
           "limitations of the results.</p>", "<ul>",
           "<li><b>Representative length measurements:</b> The routine assumes that the dataset is representative of the length distributions of the whole catch. This means that either the length of all individuals in the catch were measured or a randomised subsample of the catch was measured.</li>",
           "<li><b>Equilibrium conditions:</b> The routine assumes constant recruitment, fishing and natural mortality as well as somatic growth and maturation over time, i.e. within the year and over all years covered by the dataset.</li>",
           "<li><b>Density dependence:</b> The routine assumes density independent maturity and somatic growth. All reference points are based on the 'per recruit' model, thus no assumptions are made about the stock recruitment relationship.</li>",
           "<li><b>Length-independent natural mortality:</b> The current implementation of SMT assumes that the natural mortality is equal for all length classes.</li>",
           "<li><b>Somatic growth follows VBG:</b> ",
           "The estimation of growth parameters with ELEFAN assumes that the growth of individuals in length follows the logistic von Bertlanffy growth (VBG) function. This is an often made assumption for the growth of fish, but might not reflect well the growth of species with an exoskeleton, such as crustaceans, nor the growth of early life stages of fish.</li>",
           "<li><b>Closed population:</b> The routine assumes that the stock (population) under study is closed, meaning that there is no immigration or emigration taking place. Immigration and emigration can both bias estimated mortality rates and stock status. Furthermore, fish migrations often vary for various life stages and might thus affect the representativeness of the length measurements if the population is not closed.</li>",
           "</ul><br>")
}

spictAssumptionsHTML <- function() {
    paste0("<h4>Assumptions of SPiCT:</h4>",
           "<p>SPiCT make specific assumptions about the data, the stock, and the ",
           "fisheries targeting the stock. It is important to be aware of these assumptions and, thus, the ",
           "limitations of the results.</p>", "<ul>",
           "<li><b>Exploitable stock biomass:</b> Biomass corresponds to the exploitable stock biomass, which is the part of the population that is vulnerable to the fishing gear.</li>",
           "<li><b>No lagged effects:</b> Lagged effects as caused by variability in the size/age distribution cannot be modeled by spict.</li>",
           "<li><b>Constant catchability:</b> No change in technology or fishing technique that changes q.</li>",
           "<li><b>Gear selectivity:</b> Gear selectivity is unmodelled.</li>",
           "<li><b>Representative abundance index:</b> TODO</li>",
           "<li><b>Density dependence:</b> TODO</li>",
           "<li><b>Closed population:</b> The routine assumes that the stock (population) under study is closed, meaning that there is no immigration or emigration taking place. Immigration and emigration can both bias estimated mortality rates and stock status. Furthermore, fish migrations often vary for various life stages and might thus affect the representativeness of the length measurements if the population is not closed.</li>",
           "</ul><br>")
}
