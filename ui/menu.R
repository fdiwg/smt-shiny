menuHome <- menuItem("Home", tabName="homeTab", selected = TRUE)
menuCmsy <- menuItem("CMSY",
         menuSubItem("Introduction", tabName = "cmsyIntro"),
         menuSubItem("CMSY Method", tabName = "cmsyWidget"),
         menuSubItem("CMSY Sample Dataset", tabName = "cmsySampleDataset"), id="cmsy-main"
)
menuLengthMethods <- menuItem("Length-based methods",
                              menuSubItem("Information", tabName = "lbmIntro"),
                              menuItem("TropFishR",
                                       menuSubItem("Information",
                                                   tabName = "ElefanIntro"),
                                       menuSubItem("Tool",
                                                   tabName = "ElefanGaWidget")
                                       ## menuSubItem("Sample Datasets",
                                       ##             tabName = "ElefanSampleDataset")
                                       ),
                              # menuSubItem("Elefan SA", tabName = "ElefanSaWidget"),
                              # menuSubItem("Elefan", tabName = "ElefanWidget"),
                              menuItem("LBI",
                                       menuSubItem("Information",
                                                   tabName = "lbiIntro"),
                                       menuSubItem("Tool",
                                                   tabName = "lbiWidget")
                                       ## menuSubItem("Sample Datasets",
                                       ##             tabName = "lbiSampleDataset")
                                       ),
                              menuItem("LBSPR",
                                       menuSubItem("Information",
                                                   tabName = "lbsprIntro"),
                                       menuSubItem("Tool",
                                                   tabName = "lbsprWidget")
                                       ## menuSubItem("Sample Datasets",
                                       ##             tabName = "lbsprSampleDataset")
                                       )
                              )
# menuFishMethods <- menuItem("Fish Methods",
#          menuSubItem("Introduction", tabName = "FishMethodsIntro"),
#          menuSubItem("SBPR", tabName = "SBPRWidget"),
#          menuSubItem("YPR", tabName = "YPRWidget"),
#          menuSubItem("Fishmethods Sample Dataset", tabName = "FishMethodsSampleDataset")
# )
menuSupportingTools <- menuItem("Supporting Tools",
         menuSubItem("Schaefer logistic growth", tabName = "BasicSchaefer"),
         menuSubItem("Von Bertalanffy growth function", tabName = "BasicVonBertalannfy"),
         menuSubItem("Seasonal Von Bertalanffy", tabName = "SeasonalVonBertalannfy"),
         menuSubItem("Natural Mortality Estimators", tabName = "NaturalMortality")
)
