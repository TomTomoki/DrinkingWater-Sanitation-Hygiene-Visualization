#' @title: load_data


# Water data ----
get_water_data <- function(file_path) {
  water_data <- read_excel(file_path, sheet = "Water")
  
  water_data <- rename(water_data, 'NATIONAL_At least basic' = 'NATIONAL')
  water_data <- rename(water_data, 'RURAL_At least basic' = 'RURAL')
  water_data <- rename(water_data, 'URBAN_At least basic' = 'URBAN')
  water_data <- rename(water_data, 'NATIONAL_PROP_Safely managed' = 'NATIONAL_PROP')
  water_data <- rename(water_data, 'RURAL_PROP_Safely managed' = 'RURAL_PROP')
  water_data <- rename(water_data, 'URBAN_PROP_Safely managed' = 'URBAN_PROP')
  
  column_names <- c(
    "NATIONAL_Limited (more than 30 mins)",
    "NATIONAL_Unimproved",
    "NATIONAL_Surface water",
    "NATIONAL_Annual rate of change in basic",
    "RURAL_Limited (more than 30 mins)",
    "RURAL_Unimproved",
    "RURAL_Surface water",
    "RURAL_Annual rate of change in basic",
    "URBAN_Limited (more than 30 mins)",
    "URBAN_Unimproved",
    "URBAN_Surface water",
    "URBAN_Annual rate of change in basic",
    "NATIONAL_PROP_Accessible on premises",
    "NATIONAL_PROP_Available when needed",
    "NATIONAL_PROP_Free from contamination",
    "NATIONAL_PROP_Annual rate of change in safely managed",
    "NATIONAL_PROP_Piped",
    "NATIONAL_PROP_Non-piped",
    "RURAL_PROP_Accessible on premises",
    "RURAL_PROP_Available when needed",
    "RURAL_PROP_Free from contamination",
    "RURAL_PROP_Annual rate of change in safely managed",
    "RURAL_PROP_Piped",
    "RURAL_PROP_Non-piped",
    "URBAN_PROP_Accessible on premises",
    "URBAN_PROP_Available when needed",
    "URBAN_PROP_Free from contamination",
    "URBAN_PROP_Annual rate of change in safely managed",
    "URBAN_PROP_Piped",
    "URBAN_PROP_Non-piped",
    "SI"
  )
  
  water_data <- water_data %>% rename_with(~column_names, starts_with("..."))
  
  water_data <- water_data[-c(1,2),]
  water_data <- water_data %>% select(-SI)
  water_data <- water_data %>% mutate_all(~ifelse(. == "-", NA, .))
  
  
  
  # Clean the non-numeric values such as "<1", ">99"
  # For now, "<1" is replaced with 0.5, and ">99" is replaced with 99.5
  water_data %>%
    filter_all(any_vars(. %in% c("<1", ">99")))
  
  water_data <- water_data %>% mutate_all(~ifelse(. == "<1", 0.5, .))
  water_data <- water_data %>% mutate_all(~ifelse(. == ">99", 99.5, .))
  
  # str(water_data)
  
  # Gives no warning such as "NAs introduced by coercion", and the mutation was successful
  water_data <- water_data %>%
    mutate_at(6:41, as.numeric)
  
  
  ### Convert wide to long format
  water_data_national <- water_data %>%
    select(c(1:5, 6:10, 21:27, 42:45)) %>%
    mutate(Region = "National")
  
  water_data_rural <- water_data %>%
    select(c(1:5, 11:15, 28:34, 42:45)) %>%
    mutate(Region = "Rural")
  
  water_data_urban <- water_data %>%
    select(c(1:5, 16:20, 35:41, 42:45)) %>%
    mutate(Region = "Urban")
  
  # str(water_data_national)
  # str(water_data_rural)
  # str(water_data_urban)
  
  # Rename column names so that we can merge 3 data frames
  dput(names(water_data_national))
  
  final_column_names_water <- c("COUNTRY, AREA OR TERRITORY", "ISO3", "Year", "Population (thousands)",
                                "% urban", "At least basic", "Limited (more than 30 mins)", 
                                "Unimproved", "Surface water", "Annual rate of change in basic", 
                                "PROP_Safely managed", "PROP_Accessible on premises", 
                                "PROP_Available when needed", "PROP_Free from contamination", 
                                "PROP_Annual rate of change in safely managed", "PROP_Piped", 
                                "PROP_Non-piped", "SDG region", "WHO region", "UNICEF Programming region", 
                                "UNICEF Reporting region", "Region")
  
  names(water_data_national) <- final_column_names_water
  names(water_data_rural) <- final_column_names_water
  names(water_data_urban) <- final_column_names_water
  
  water_data_long <- rbind(water_data_national, water_data_rural, water_data_urban)
  unique(water_data_long$Region)
  
  return(water_data_long)
}



# Sanitation data ----
get_sanitation_data <- function(file_path) {
  sanitation_data <- read_excel(file_path, sheet = "Sanitation")
  
  sanitation_data <- rename(sanitation_data, 'NATIONAL_At least basic' = 'NATIONAL...6')
  sanitation_data <- rename(sanitation_data, 'RURAL_At least basic' = 'RURAL...12')
  sanitation_data <- rename(sanitation_data, 'URBAN_At least basic' = 'URBAN...18')
  sanitation_data <- rename(sanitation_data, 'NATIONAL_Exc_Safely managed' = 'NATIONAL...24')
  sanitation_data <- rename(sanitation_data, 'NATIONAL_Inc_Latrines and others' = '...29')
  sanitation_data <- rename(sanitation_data, 'RURAL_Exc_Safely managed' = 'RURAL...32')
  sanitation_data <- rename(sanitation_data, 'RURAL_Inc_Latrines and others' = '...37')
  sanitation_data <- rename(sanitation_data, 'URBAN_Exc_Safely managed' = 'URBAN...40')
  sanitation_data <- rename(sanitation_data, 'URBAN_Inc_Latrines and others' = '...45')
  
  column_names <- c(
    "NATIONAL_Limited (shared)",
    "NATIONAL_Unimproved",
    "NATIONAL_Open defecation",
    "NATIONAL_Annual rate of change in basic",
    "NATIONAL_Annual rate of change in open defecation",
    "RURAL_Limited (shared)",
    "RURAL_Unimproved",
    "RURAL_Open defecation",
    "RURAL_Annual rate of change in basic",
    "RURAL_Annual rate of change in open defecation",
    "URBAN_Limited (shared)",
    "URBAN_Unimproved",
    "URBAN_Open defecation",
    "URBAN_Annual rate of change in basic",
    "URBAN_Annual rate of change in open defecation",
    "NATIONAL_Exc_Disposed in situ",
    "NATIONAL_Exc_Emptied and treated",
    "NATIONAL_Exc_Wastewater treated",
    "NATIONAL_Exc_Annual rate of change in safely managed",
    "NNATIONAL_Inc_Septic tanks",
    "NATIONAL_Inc_Sewer connections",
    "RURAL_Exc_Disposed in situ",
    "RURAL_Exc_Emptied and treated",
    "RURAL_Exc_Wastewater treated",
    "RURAL_Exc_Annual rate of change in safely managed",
    "RURAL_Inc_Septic tanks",
    "RURAL_Inc_Sewer connections",
    "URBAN_Exc_Disposed in situ",
    "URBAN_Exc_Emptied and treated",
    "URBAN_Exc_Wastewater treated",
    "URBAN_Exc_Annual rate of change in safely managed",
    "URBAN_Inc_Septic tanks",
    "URBAN_Inc_Sewer connections",
    "SI"
  )
  
  sanitation_data <- sanitation_data %>% rename_with(~column_names, starts_with("..."))
  
  sanitation_data <- sanitation_data[-c(1,2),]
  sanitation_data <- sanitation_data %>% select(-SI)
  sanitation_data <- sanitation_data %>% mutate_all(~ifelse(. == "-", NA, .))
  
  
  
  # Clean the non-numeric values such as "<1", ">99"
  # For now, "<1" is replaced with 0.5, and ">99" is replaced with 99.5
  sanitation_data %>%
    filter_all(any_vars(. %in% c("<1", ">99")))
  
  sanitation_data <- sanitation_data %>% mutate_all(~ifelse(. == "<1", 0.5, .))
  sanitation_data <- sanitation_data %>% mutate_all(~ifelse(. == ">99", 99.5, .))
  
  str(sanitation_data)
  
  # Gives no warning such as "NAs introduced by coercion", and the mutation was successful
  sanitation_data <- sanitation_data %>%
    mutate_at(6:47, as.numeric)
  
  
  ### Convert wide to long format
  sanitation_data_national <- sanitation_data %>%
    select(c(1:5, 6:11, 24:31, 48:51)) %>%
    mutate(Region = "National")
  
  sanitation_data_rural <- sanitation_data %>%
    select(c(1:5, 12:17, 32:39, 48:51)) %>%
    mutate(Region = "Rural")
  
  sanitation_data_urban <- sanitation_data %>%
    select(c(1:5, 18:23, 40:47, 48:51)) %>%
    mutate(Region = "Urban")
  
  # str(sanitation_data_national)
  # str(sanitation_data_rural)
  # str(sanitation_data_urban)
  
  # Rename column names so that we can merge 3 data frames
  dput(names(sanitation_data_national))
  
  final_column_names_sanitation <- c("COUNTRY, AREA OR TERRITORY", "ISO3", "Year", "Population (thousands)", 
                                     "% urban", "At least basic", "Limited (shared)", "Unimproved", "Open defecation", "Annual rate of change in basic", 
                                     "Annual rate of change in open defecation", "Exc_Safely managed", "Exc_Disposed in situ", "Exc_Emptied and treated", 
                                     "Exc_Wastewater treated", "Exc_Annual rate of change in safely managed", "Inc_Latrines and others", "Inc_Septic tanks", "Inc_Sewer connections", "SDG region", "WHO region", 
                                     "UNICEF Programming region", "UNICEF Reporting region", "Region")
  
  names(sanitation_data_national) <- final_column_names_sanitation
  names(sanitation_data_rural) <- final_column_names_sanitation
  names(sanitation_data_urban) <- final_column_names_sanitation
  
  sanitation_data_long <- rbind(sanitation_data_national, sanitation_data_rural, sanitation_data_urban)
  unique(sanitation_data_long$Region)
  
  return(sanitation_data_long)
}


# Hyginene data ----
get_hyginene_data <- function(file_path) {
  hygiene_data <- read_excel(file_path, sheet = "Hygiene")
  
  hygiene_data <- rename(hygiene_data, 'NATIONAL_Basic' = 'NATIONAL')
  hygiene_data <- rename(hygiene_data, 'NATIONAL_Limited (without water or soap)' = '...7')
  hygiene_data <- rename(hygiene_data, 'NATIONAL_No facility' = '...8')
  hygiene_data <- rename(hygiene_data, 'NATIONAL_Annual rate of change in basic' = '...9')
  hygiene_data <- rename(hygiene_data, 'RURAL_Basic' = 'RURAL')
  hygiene_data <- rename(hygiene_data, 'RURAL_Limited (without water or soap)' = '...11')
  hygiene_data <- rename(hygiene_data, 'RURAL_No facility' = '...12')
  hygiene_data <- rename(hygiene_data, 'RURAL_Annual rate of change in basic' = '...13')
  hygiene_data <- rename(hygiene_data, 'URBAN_Basic' = 'URBAN')
  hygiene_data <- rename(hygiene_data, 'URBAN_Limited (without water or soap)' = '...15')
  hygiene_data <- rename(hygiene_data, 'URBAN_No facility' = '...16')
  hygiene_data <- rename(hygiene_data, 'URBAN_Annual rate of change in basic' = '...17')
  hygiene_data <- rename(hygiene_data, 'SI' = '...18')
  hygiene_data <- rename(hygiene_data, 'SDG region' = '...19')
  hygiene_data <- rename(hygiene_data, 'WHO region' = '...20')
  hygiene_data <- rename(hygiene_data, 'UNICEF Programming region' = '...21')
  hygiene_data <- rename(hygiene_data, 'UNICEF Reporting region' = '...22')
  
  hygiene_data <- hygiene_data[-c(1),]
  hygiene_data <- hygiene_data %>% select(-SI)
  hygiene_data <- hygiene_data %>% mutate_all(~ifelse(. == "-", NA, .))
  
  
  
  # Clean the non-numeric values such as "<1", ">99"
  # For now, "<1" is replaced with 0.5, and ">99" is replaced with 99.5
  hygiene_data %>%
    filter_all(any_vars(. %in% c("<1", ">99")))
  
  hygiene_data <- hygiene_data %>% mutate_all(~ifelse(. == "<1", 0.5, .))
  hygiene_data <- hygiene_data %>% mutate_all(~ifelse(. == ">99", 99.5, .))
  
  str(hygiene_data)
  
  # Gives no warning such as "NAs introduced by coercion", and the mutation was successful
  hygiene_data <- hygiene_data %>%
    mutate_at(6:17, as.numeric)
  
  
  ### Convert wide to long format
  hygiene_data_national <- hygiene_data %>%
    select(c(1:5, 6:9, 18:21)) %>%
    mutate(Region = "National")
  
  hygiene_data_rural <- hygiene_data %>%
    select(c(1:5, 10:13, 18:21)) %>%
    mutate(Region = "Rural")
  
  hygiene_data_urban <- hygiene_data %>%
    select(c(1:5, 14:17, 18:21)) %>%
    mutate(Region = "Urban")
  
  # str(hygiene_data_national)
  # str(hygiene_data_rural)
  # str(hygiene_data_urban)
  
  # Rename column names so that we can merge 3 data frames
  dput(names(hygiene_data_national))
  
  final_column_names_hygiene <- c("COUNTRY, AREA OR TERRITORY", "ISO3", "Year", "Population (thousands)", 
                                  "% urban", "Basic", "Limited (without water or soap)", "No facility", "Annual rate of change in basic", 
                                  "SDG region", "WHO region", "UNICEF Programming region", "UNICEF Reporting region", 
                                  "Region")
  
  names(hygiene_data_national) <- final_column_names_hygiene
  names(hygiene_data_rural) <- final_column_names_hygiene
  names(hygiene_data_urban) <- final_column_names_hygiene
  
  hygiene_data_long <- rbind(hygiene_data_national, hygiene_data_rural, hygiene_data_urban)
  unique(hygiene_data_long$Region)
  
  return(hygiene_data_long)
}

# Bar Plot
population <- function(df, year, top_num) {
  df_year <- df %>%
    filter(Year == year) %>%
    group_by(`COUNTRY, AREA OR TERRITORY`) %>%
    summarise(`Population (thousands)` = max(`Population (thousands)`)) %>%
    ungroup() %>%
    arrange(desc(`Population (thousands)`)) %>%
    head(top_num)
  
  hist <- ggplot(df_year, aes(x = `COUNTRY, AREA OR TERRITORY`, y = `Population (thousands)`)) +
    geom_bar(stat = "identity") +
    labs(x = "Country/Area/Territory",
         y = "Population (thousands)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle=30, hjust=1))
  
  return(hist)
}

#population(water_data_long, 2015,10)
#population(sanitation_data_long, 2015,15)
#population(hygiene_data_long, 2017, 20)


