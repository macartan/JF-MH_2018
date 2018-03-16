# Function to grab data from dataverse 
  Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")
  f <- function(filename){
    f   <- get_file(paste0(filename, ".csv"), "doi:10.7910/DVN/28006")
    tmp <- tempfile(fileext = ".csv")
    writeBin(as.vector(f), tmp)
    a <- read.csv(tmp)
    rm(f)
    rm(tmp)
    return(a)}
 
# Datasets     
  List <- f("baseline_list")
  survey1 <- f("liberia_bl_survey1")
  survey2 <- f("liberia_bl_survey1")
  indiv   <- f("liberia_bl_survey_indiv")
  pvill   <- f("game_village_list")
  hh      <- f("household_survey_final_games")
  
  # End