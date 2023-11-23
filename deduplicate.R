##################################################
### A script workflow for Record linkage ----------
##################################################

library(tidyverse)
library(unhcrthemes)
library(fontawesome)
# install.packages("fastLink")
library(fastLink)  

## Load the data - 
# which is here already the results of merging multiple list from different excel files
data <- readxl::read_excel(here::here("data-raw", "Registros2.xlsx"),
                           sheet = "Sheet1") |> janitor::clean_names()

## Cleaning functions ###############


#' clean_age
#' 
#' This functions cleans the age identifiers in the context of record linkage.
#' 
#' If the date of birth is present, it will parse it, extract year and month, 
#' and recalculate both age and age range
#' 
#' If the date of birth is not mentioned, then it will use age and date_record
#' to reconstruct an estimated date of birth - and recalculate the rest
#' 
#' @param frame frame with the data
#' @param date_birth variable name for the date of birth in the frame
#' @param date_record variable name for the date of birth in the frame
#' @param year_birth variable name for the year of birth in the frame
#' @param month_birth variable name for the month of birth in the frame
#' @param age variable name for the date of birth in the frame
#' @param age_range variable name for the date of birth in the frame
#' 
#' @return the same column but cleaned... 
#' 
#' @export
clean_age <- function(frame, 
                      date_birth, 
                      date_record, 
                      year_birth, 
                      month_birth, 
                      age, 
                      age_range){
  
  frame2 <- frame |>
  dplyr::mutate(
     DateOfBirth = lubridate::as_date(DoB),
    
    dob_day = as.numeric(
      lubridate::day(DateOfBirth)),
    
    dob_month = as.numeric(
      lubridate::month(DateOfBirth)),
    
    dob_year = as.numeric( 
      lubridate::year(DateOfBirth)),
      
    age = today(),     
        
    age_cohort = dplyr::case_when(
          ~ "0-4",
          ~ "4-11",
          ~ "12-17",
          ~ "18-59",
          ~ "60+",
          TRUE ~  NA  )   )
    
    return(frame2)
}



#' separate_fullname  
#' 
#' This function clean name decomposition - in case it is not included in the original data
#' 
#' Performing this name decomposition is important in order to enhance record linkage
#' as the name pattern can be different (either "firstname_fathername_mother_name" or
#'  "fathername_mothername_firstname") which will minimise linkage probabibility
#' 
#' In case, there's only 2 elements, it will fill only firstname and fathername
#' The function also identify family prefix to be bind such as for spanish 
#' "DEL", 'DE", "DE LOS", "DE LAS"
#' 
#' @param fullname full name including everything together
#' @param firstname full name including everything together
#' @param fathername full name including everything together
#' @param mothername full name including everything together
#' @param namepattern either "firstname_fathername_mother_name" or
#'                         "fathername_mothername_firstname"
#' @param  nameprefix  default vector with stuff to remove from name
#'                    c("DE ", "DE LA ", "DEL ", "DE LOS ", "DE LAS")
#' @return a clean list with c("firstname","fathername","mothername")
#' 
#' @export
separate_fullname <- function(frame, 
                              fullname, 
                              firstname,
                              fathername,
                              mothername,
                              namepattern,
                              nameprefix = data.frame(
                                pat1 = c( "DE LA ", "DEL ", "DE LOS ", "DE LAS ","DE "),
                                pat2 = c( "DE_LA_", "DEL_", "DE_LOS_", "DE_LAS_", "DE_")  ) 
                              ){
  
  
  # Utility sub-function to remove spaces based on patterns
  remove_spaces_based_on_patterns <- function(vector, nameprefix) {
    for (i in  1:nrow(nameprefix) ) {
      # i <- 4
      #cat(paste0(nameprefix[i, c("pat1")],"\n",  vector, "\n"))
      vector <- stringr::str_replace_all(string = vector, 
                                         pattern =  nameprefix[i, c("pat1")], 
                                         replacement = nameprefix[i, c("pat2")])
      # cat(paste0(vector, "\n"))
    }
    return(vector)
  }
  ## Test function
  #remove_spaces_based_on_patterns(vector = "ADRIENNE DE LOS ANGELES MILANESE PARISIANNA", nameprefix)
  
  # Utility sub-function to remove spaces based on patterns
  reset_spaces_based_on_patterns <- function(vector, nameprefix) {
    for (i in  1:nrow(nameprefix) ) {
      # i <- 1
     # cat(paste0( nameprefix[i, c("pat1")],"\n",vector,  "\n"))
      vector <- stringr::str_replace_all(string = vector, 
                                         pattern =  nameprefix[i, c("pat2")], 
                                         replacement = nameprefix[i, c("pat1")])
      #cat(paste0(vector, "\n"))
    }
    return(vector)
  }
  
  ## Let's go!  
  framesp <- frame |>
    ###  Lets get everything to uper
    dplyr::mutate( fullname_or = fullname, 
                   fullname = trimws(stringr::str_squish(fullname), 
                                        which = "both", 
                                        whitespace = "[ \t\r\n]"),
                   fullname = toupper(fullname)) |>
    ###  Lets apply the prefix space replacement...
    dplyr::mutate( fullname_pref = remove_spaces_based_on_patterns(vector = fullname,
                                                              nameprefix),
                   fullname = fullname_pref ) |>
    
  #  "firstname_fathername_mother_name" or
  #'                         "fathername_mothername_firstname"
    tidyr::separate_wider_delim(fullname,
                                delim = " ", 
                                 names_sep = "", 
                                 too_few = "align_start") |>
    
    ### Now reconstruct the first name, father and mother name based on cases
    
    
    
    tidyr::separate( fullname, 
                     #into=c("FirstName", "FatherName", "MotherName"), 
                     sep=" ")  |>
    ###  Lets reset the prefix space replacement...
    dplyr::mutate( FatherName = reset_spaces_based_on_patterns(vector = FatherName,
                                                              nameprefix),
                   MotherName = reset_spaces_based_on_patterns(vector = MotherName,
                                                               nameprefix))
    
  # 
  return(framesp)
}

## Testing...
frame <- data |> 
  dplyr::filter(  is.na(nombres) ) |> 
  dplyr::select(nombre_completo, name_pattern)  |>
  dplyr::rename( fullname= "nombre_completo",
                  namepattern = "name_pattern")

 
sep <- separate_fullname(frame = frame,
                         fullname = "fullname", 
                         namepattern= "namepattern")

#' cleanvar
#' 
#' function for data cleaning with additional name removal logic
#' 
#' @param names_column name of the column to treat
#' @param  toRemove  default vector with stuff to remove from name
#'                    c(" JR", " SR", " IV", " III", " II")
#' @return  names_column_new name of the column treat
#' 
#' @export
cleanvar <- function(names_column,
                     toRemove = c(" JR", " JUNIOR", " SR", " IV", " III", " II")) {
  #  Convert to uppercase
  names_column_new <- toupper(names_column)
  # Remove specified name suffixes
  for (tR in toRemove) {
    names_column_new <- gsub(tR, "", names_column_new)
  }
  # Convert special characters to ASCII equivalents
  names_column_new <- iconv(names_column_new, "latin1", "ASCII//TRANSLIT", sub = "")
  # Remove punctuation, digits, and all spaces
  names_column_new <- gsub("[[:punct:][:digit:]][[:space:]]", "", names_column_new)
  # Create a new variable with only alphabetic characters
  names_column_new <- gsub("[^[:alpha:]]", "", names_column_new)
  
  return(names_column_new)
}


## Pipeline for data post processing #####################
data.prep <- data |>
  
  ## Filter where the phone number is not available -- "NO REFIERE"
  dplyr::filter( telefono != "NO REFIERE") |>
  
  ## Clean age_range
  data |> dplyr::select(age_range) |> dplyr::distinct() |> dplyr::pull()
  # dplyr::mutate( age_range = dplyr::case_when(
  #   !(is.null(edad)) & (edad <5 ) ~ "0-4",
  #   genero %in% c("M" , "MASCULINO" , "Masculino") ~ "M",
  #   genero %in% c("X",  "Otro") ~ "Ot",
  #   TRUE ~  age_range  )) |>
  
  ## Clean the gender variable
  # data |> dplyr::select(genero) |> dplyr::distinct() |> dplyr::pull()
  dplyr::mutate(gender = dplyr::case_when(
    genero %in% c("F" ,"FEMENINO" ,"f",  "Femenino") ~ "F",
    genero %in% c("M" , "MASCULINO" , "Masculino") ~ "M",
    genero %in% c("X",  "Otro") ~ "Ot",
    TRUE ~  NA  )) |>
  
  ## Only retain the nationality of interest
  # data |> dplyr::select(nacionalidad) |> dplyr::distinct() |> dplyr::pull()
  dplyr::mutate(nationality = dplyr::case_when(
    nacionalidad %in% c("Venzuela", "venezuela",  "Venezolana", 
                        "VENEZUELA",
                        "Venezuela", "VENEZOLANO", "VENEZOLANA") ~ "VEN",
    nacionalidad %in% c("COLOMBIANO", "COLOMBIANA", "COLOMBIA",
                        "colombia", "Colombia", "Nac. Colombia",
                        "Colombiana" ) ~ "COL",
    TRUE ~ "other"  )) |>
  dplyr::filter( nationality %in% c("VEN", "COL" )) |>
  ## Apply cleanvar()
  # Perform data cleaning on dfA using the clean_names function
  dplyr::mutate_at( dplyr::vars(nombres, apellido_paterno, apellido_materno,
                                asistencia, departamento, telefono,
                                planilla, socio), 
                    list(new = cleanvar))  |>
  
  
  ### identify single data source
  dplyr::mutate(datasource = paste0(socio_new, "_", planilla_new)) |>
  
  # ## Retain only fields for record linkage
  dplyr::select(datasource, nationality, nombres_new, apellido_paterno_new,
                apellido_materno_new, asistencia_new, departamento_new,
                telefono_new, gender)

table(data.prep$gender, useNA = "ifany")
table(data.prep$nationality, useNA = "ifany")

## Check the datasource that we will compare
table(data.prep$datasource, useNA = "ifany")

## See if can use departamento for blocking
table(data.prep$datasource, data.prep$departamento_new, useNA = "ifany")

alldatasource <- data.prep |> 
                 dplyr::select(datasource) |> 
                 dplyr::distinct() |> 
                 dplyr::pull()




## Let's get 2 comparison dataset... ##########
dfA  <- data.prep |> 
  dplyr::filter(datasource == alldatasource[9]) |> 
  dplyr::select( - datasource)

dfB <- data.prep |> 
  dplyr::filter(datasource == alldatasource[6])|> 
  dplyr::select( - datasource)

matches.out <- fastLink::fastLink(
  dfA = dfA, 
  dfB = dfB, 
  # Specify the vector of variable names to be used for matching.
  # These variable names should exist in both dfA and dfB
  varnames = c("nationality" ,  "nombres_new" , "apellido_paterno_new" ,"apellido_materno_new",
               "asistencia_new" ,      "departamento_new",     "telefono_new"  ,       "gender" ),
  
  # Specify which variables among varnames should be compared using string distance
  stringdist.match = c(  "nombres_new" , "apellido_paterno_new" ,
                       "apellido_materno_new"),
  
  # Specify which variables present in  stringdist.match can be partially matched
  partial.match = c( "nombres_new" , "apellido_paterno_new" ),
  
  # Specify which variables should be matched numerically
  # Must be a subset of 'varnames' and must not be present in 'stringdist.match'.
  numeric.match = c("telefono_new" 
                    #'dob_day', 'dob_month', 'dob_year'
                    ),
  
  # Specify the number of CPU cores to utilize (parallel processing). 
  ## Get the number of detected cores minus 1,  Reserve one core for 
  #non-computational tasks to help prevent system slowdowns or unresponsiveness
  n.cores = parallel::detectCores() - 1,
  return.df = TRUE)

## Review the matching in each orginal frame
matchedA <- dfA[matches.out$matches$inds.a, ]
matchedB <- dfB[matches.out$matches$inds.b, ]

# Confusion Matrice
fastLink::confusion(matches.out, threshold = 0.98)

# Examine the EM object:
matches.out$EM

# Summarize the accuracy of the match:
# each column gives the match count, match rate,
# false discovery rate (FDR) and false negative rate (FNR)
# under different cutoffs for matches based on the posterior 
# probability of a match. 
summary(matches.out)

## Gt the output...
matched_dfs <- fastLink::getMatches(
  dfA = dfA, 
  dfB = dfB, 
  fl.out = matches.out, 
  threshold.match = 0.85
)

# Display the matches ###################
# convert dfA rownames to a column 
dfA_clean <- dfA %>% rownames_to_column()

# convert dfB rownames to a column
dfB_clean <- dfB %>% rownames_to_column()  

# convert all columns in matches dataset to character, 
#so they can be joined to the rownames
matches_clean <- matched_dfs %>%
  mutate(across(everything(), as.character))

# Join matches to dfA, then add dfB
# column "inds.b" is added to dfA
complete <- left_join(dfA_clean, matches_clean, by = c("rowname" = "inds.a"))

# column(s) from dfB are added 
complete <- left_join(complete, dfB_clean, by = c("inds.b" = "rowname"))



# Preprocessing Matches via Blocking #################

blockgender_out <- fastLink::blockData(dfA, dfB, varnames = "gender")

## Subset dfA into blocks
dfA_block1 <- dfA[blockgender_out$block.1$dfA.inds,]
dfA_block2 <- dfA[blockgender_out$block.2$dfA.inds,]

## Subset dfB into blocks
dfB_block1 <- dfB[blockgender_out$block.1$dfB.inds,]
dfB_block2 <- dfB[blockgender_out$block.2$dfB.inds,]

## Run fastLink on each
link.1 <- fastLink::fastLink(
  dfA = dfA_block1, 
  dfB = dfB_block1,
  # Specify the vector of variable names to be used for matching.
  # These variable names should exist in both dfA and dfB
  varnames = c("nationality" ,  "nombres_new" , "apellido_paterno_new" ,"apellido_materno_new",
               "asistencia_new" ,      "departamento_new",     "telefono_new"  ,       "gender" ),
  
  # Specify which variables among varnames should be compared using string distance
  stringdist.match = c(  "nombres_new" , "apellido_paterno_new" ,
                         "apellido_materno_new"),
  
  # Specify which variables present in  stringdist.match can be partially matched
  partial.match = c( "nombres_new" , "apellido_paterno_new" ),
  
  # Specify which variables should be matched numerically
  # Must be a subset of 'varnames' and must not be present in 'stringdist.match'.
  numeric.match = c("telefono_new" 
                    #'dob_day', 'dob_month', 'dob_year'
  ),
  
  # Specify the number of CPU cores to utilize (parallel processing). 
  ## Get the number of detected cores minus 1,  Reserve one core for 
  #non-computational tasks to help prevent system slowdowns or unresponsiveness
  n.cores = parallel::detectCores() - 1,
  return.df = TRUE) 

link.2 <- fastLink::fastLink(
  dfA = dfA_block2, 
  dfB = dfB_block2,
  # Specify the vector of variable names to be used for matching.
  # These variable names should exist in both dfA and dfB
  varnames = c("nationality" ,  "nombres_new" , "apellido_paterno_new" ,"apellido_materno_new",
               "asistencia_new" ,      "departamento_new",     "telefono_new"  ,       "gender" ),
  
  # Specify which variables among varnames should be compared using string distance
  stringdist.match = c(  "nombres_new" , "apellido_paterno_new" ,
                         "apellido_materno_new"),
  
  # Specify which variables present in  stringdist.match can be partially matched
  partial.match = c( "nombres_new" , "apellido_paterno_new" ),
  
  # Specify which variables should be matched numerically
  # Must be a subset of 'varnames' and must not be present in 'stringdist.match'.
  numeric.match = c("telefono_new" 
                    #'dob_day', 'dob_month', 'dob_year'
  ),
  
  # Specify the number of CPU cores to utilize (parallel processing). 
  ## Get the number of detected cores minus 1,  Reserve one core for 
  #non-computational tasks to help prevent system slowdowns or unresponsiveness
  n.cores = parallel::detectCores() - 1,
  return.df = TRUE)

## aggregate multiple matches into a single summary with aggregateEM() 
agg.out <- fastLink::aggregateEM(em.list = list(link.1, link.2))



# Preprocessing Matches via Blocking #################