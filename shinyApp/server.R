server <- function(input, output){
  # pre-load the data ----
  file_path <- "./JMP_2021_WLD.xlsx"
  
  water_data <- get_water_data(file_path)
  sanitation_data <- get_sanitation_data(file_path)
  hyginene_data <- get_hyginene_data(file_path)
  
  # TODO
  # observers
}