##################################################
### A script workflow for Record linkage ----------
##################################################

library(tidyverse)
# install.packages("fastLink")
library(fastLink)  

## Load the data - 
# which is here already the results of merging multiple list from different excel files
data <- readxl::read_excel(here::here("data-raw", "Registros2.xlsx"),
                           sheet = "Sheet1", 
                           col_types = c("numeric", 
                             "text", "text", "text", "text", "text", 
                             "text", "text", "date", "numeric", 
                             "numeric", "numeric", "text", "text", 
                            "text", "text", "text", "text", "text", 
                            "text", "text", "text", "text", "text", 
                            "text", "text", "text", "text", "text", 
                            "text", "text")) |> 
     janitor::clean_names()


#dput(names(data))
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
#' @param age variable name for the date of birth in the frame
#' @param age_range variable name for the date of birth in the frame
#' 
#' @return the same column but cleaned... 
#' 
#' @export
clean_age <- function(frame, 
                      date_birth, 
                      date_record, 
                      age, 
                      age_range){
  
  #frame$date_birth
  frame2 <- frame |>
    ## Rename variable
     dplyr::rename( date_birth = paste0(date_birth),
                    date_record = paste0(date_record),
                    age = paste0(age),
                    age_range = paste0(age_range) ) |>
     dplyr::mutate(
       ## In case there is no date of birth - but we have an age, we recalculate it..
       
       date_birth =     dplyr::case_when(
         # Case we age but no DOB and date registration  
         is.na(date_birth) & !(is.na(age)) & !(is.na(date_record))  ~
           lubridate::as_date(date_record) - lubridate::dyears(age) ,
         
         # Case we age but no DOB and date registration  
         is.na(date_birth) & !(is.na(age)) & is.na(date_record) ~
           today() - lubridate::dyears(age) ,  
         
         # Case take what we have...  
         TRUE ~   lubridate::as_date(date_birth) ) ,
       ## make sure it is in the correct format
       date_birth =  lubridate::as_date(date_birth),
        
         day_birth = as.numeric(
          lubridate::day(date_birth)),
        
         month_birth = as.numeric(
          lubridate::month(date_birth)),
        
         year_birth= as.numeric( 
          lubridate::year(date_birth)),
          
        age = round( as.numeric(today() - date_birth) / 365),     
            
        age_range = dplyr::case_when(
          ## if age cohort was already present and we have no DOb, retain it
         # !(is.na(age_range )) & is.na( date_birth) &
            age  <5    ~ "0-4",
          
         # !(is.na(age_range )) & is.na( date_birth) & 
            age >=5 & age <= 11   ~ "4-11",
          
          #!(is.na(age_range )) & is.na( date_birth) & 
            age >= 12 & age <= 17     ~ "12-17",
         # !(is.na(age_range )) & is.na( date_birth) & 
            age >= 18 & age <= 59     ~ "18-59",
          
         # !(is.na(age_range )) & is.na( date_birth) &
            age >= 60     ~ "60+",
          
          TRUE ~  age_range   )   )
    
    return(frame2)
}


## Testing...
# frame <- data |>  
#   dplyr::select(fecha_de_nacimiento, 
#                 date_record, edad,  age_range)  |>
#   clean_age( date_birth = "fecha_de_nacimiento", 
#              date_record = "date_record", 
#              age = "edad", 
#              age_range = "age_range")



#' remove_spaces_based_on_patterns 
#' Utility sub-function to remove spaces based on patterns
#' This function helps cleaning name decomposition - in case it is not included in the original data
#' 
#' In case, there's only 2 elements, it will fill only firstname and fathername
#' The function also identify family prefix to be bind such as for spanish 
#' "DEL", 'DE", "DE LOS", "DE LAS"
#' 
#' @param vector a list of string with names
remove_spaces_based_on_patterns <- function(vector,
                                            nameprefix = data.frame(
                                              pat1 = c( "DE LA ", "DEL ", "DE LOS ", "DE LAS ","DE "),
                                              pat2 = c( "DE_LA_", "DEL_", "DE_LOS_", "DE_LAS_", "DE_")  ) ) {
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
remove_spaces_based_on_patterns(vector = "ADRIENNE DE LOS ANGELES MILANESE PARISIANNA")

 
#' reset_spaces_based_on_patterns 
#' Utility sub-function to remove spaces based on patterns
#' This function helps cleaning name decomposition - in case it is not included in the original data
#' 
#' In case, there's only 2 elements, it will fill only firstname and fathername
#' The function also identify family prefix to be bind such as for spanish 
#' "DEL", 'DE", "DE LOS", "DE LAS"
#' 
#' @param vector a list of string with names
reset_spaces_based_on_patterns <- function(vector, 
                                           nameprefix = data.frame(
                                             pat1 = c( "DE LA ", "DEL ", "DE LOS ", "DE LAS ","DE "),
                                             pat2 = c( "DE_LA_", "DEL_", "DE_LOS_", "DE_LAS_", "DE_")  ) ) {
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
#' @param firstname first name
#' @param fathername father name  
#' @param mothername mother name 
#' @param namepattern either "firstname_fathername_mother_name" or
#'                         "fathername_mothername_firstname"
#'                         
#' @return a clean list with c("firstname","fathername","mothername")
#' 
#' @export
separate_fullname <- function(frame, 
                              fullname, 
                              firstname,
                              fathername,
                              mothername,
                              namepattern
                              ){
  ## Let's go!  
  framesp <- frame |>
    ## Rename variable
    dplyr::rename( fullname = paste0(fullname),
                   firstname = paste0(firstname),
                   fathername = paste0(fathername),
                   mothername = paste0(mothername),
                   namepattern = paste0(namepattern) ) |>
    
    ###  Lets clean all spaces and get everything to upper
    dplyr::mutate( fullname_or = fullname, 
                   fullname = trimws(stringr::str_squish(fullname), 
                                        which = "both", 
                                        whitespace = "[ \t\r\n]"),
                   fullname = toupper(fullname)) |>
    ###  Lets apply the prefix space replacement...
    dplyr::mutate( fullname_pref = remove_spaces_based_on_patterns(vector = fullname),
                   fullname = fullname_pref ) |>
    ## Counting th enumber of space to understand the structure of the full name
     dplyr::mutate( numspace = stringr::str_count(fullname, ' ')) |>
    tidyr::separate_wider_delim(fullname,
                                delim = " ", 
                                 names_sep = "", 
                                 too_few = "align_start") |>
    
    ### Now reconstruct the first name, father and mother name based on cases
    
    ## Let summarize the logic...!
    ## Based on the number of componnent in the full name -  ranging from 0 to 4
    
    ## Case A: "firstname_fathername_mother_name" 
    # numspace == 0 ##  only firstname = fullname1  
    # numspace  = 1 ## firstname = fullname1 & fathername = fullname2
    # numspace  = 2 ## firstname = fullname1 & fathername  = fullname2 &   mothername  = fullname3
    # numspace  = 3 ## firstname = paste0(fullname1, " ", fullname2) & fathername  = fullname3 & mothername  = fullname4
    # numspace  = 4 ## firstname = paste0(fullname1, " ", fullname2, " ", fullname3) & fathername  = fullname4 &  mothername  = fullname5
       
    # ## Case B: "fathername_mothername_firstname"    
    # numspace == 0 ##  only fathername = fullname1  
    # numspace  = 1 ##  fathername = fullname1 & firstname = fullname2
    # numspace  = 2 ##  fathername  = fullname1 & mothername  = fullname2 &   firstname  = fullname3
    # numspace  = 3 ##  fathername  = fullname1 & mothername  = fullname2 & firstname = paste0(fullname3, " ", fullname3) 
    # numspace  = 4 ##  fathername  = fullname1 &  mothername  = fullname2 & firstname = paste0(fullname3, " ", fullname4, " ", fullname5) 

    ### Now reconstruct fathername
    dplyr::mutate( fathername = dplyr::case_when(
      is.na(fathername) &  namepattern == "firstname_fathername_mother_name" &
        numspace == 0  ~ "",
      
      is.na(fathername) &  namepattern == "firstname_fathername_mother_name" &
        numspace == 1  ~ fullname2,
      
      is.na(fathername) &  namepattern == "firstname_fathername_mother_name" &
        numspace == 2  ~ fullname2,
    
      is.na(fathername) &  namepattern == "firstname_fathername_mother_name" &
        numspace == 3  ~ fullname3,
      
      is.na(fathername) &  namepattern == "firstname_fathername_mother_name" &
        numspace == 4  ~ fullname4,
      
      
      is.na(fathername) &  namepattern == "fathername_mothername_firstname" &
        numspace == 0  ~ fullname1,
      
      is.na(fathername) &  namepattern == "fathername_mothername_firstname" &
        numspace == 1  ~ fullname1,
      
      is.na(fathername) &  namepattern == "fathername_mothername_firstname" &
        numspace == 2  ~ fullname1,
      
      is.na(fathername) &  namepattern == "fathername_mothername_firstname" &
        numspace == 3  ~ fullname1,
      
      is.na(fathername) &  namepattern == "fathername_mothername_firstname" &
        numspace == 4  ~ fullname1,
      
      TRUE ~ fathername    )) |>

    ### Now reconstruct mothername
    dplyr::mutate( mothername = dplyr::case_when(
      is.na(mothername) &  namepattern == "firstname_fathername_mother_name" &
        numspace == 0  ~ "",
      
      is.na(mothername) &  namepattern == "firstname_fathername_mother_name" &
        numspace == 1  ~ "",
      
      is.na(mothername) &  namepattern == "firstname_fathername_mother_name" &
        numspace == 2  ~ fullname3,
      
      is.na(mothername) &  namepattern == "firstname_fathername_mother_name" &
        numspace == 3  ~ fullname4,
      
      is.na(mothername) &  namepattern == "firstname_fathername_mother_name" &
        numspace == 4  ~ fullname5,
      
      
      is.na(mothername) &  namepattern == "fathername_mothername_firstname" &
        numspace == 0  ~ "",
      
      is.na(mothername) &  namepattern == "fathername_mothername_firstname" &
        numspace == 1  ~ "",
      
      is.na(mothername) &  namepattern == "fathername_mothername_firstname" &
        numspace == 2  ~ fullname2,
      
      is.na(mothername) &  namepattern == "fathername_mothername_firstname" &
        numspace == 3  ~ fullname2,
      
      is.na(mothername) &  namepattern == "fathername_mothername_firstname" &
        numspace == 4  ~ fullname2,
      
      TRUE ~ mothername    )) |>
    
    
    ### Now reconstruct firstname
    dplyr::mutate( firstname = dplyr::case_when(
      is.na(firstname) &  namepattern == "firstname_fathername_mother_name" &
        numspace == 0  ~ fullname1,
      
      is.na(firstname) &  namepattern == "firstname_fathername_mother_name" &
        numspace == 1  ~ fullname1,
      
      is.na(firstname) &  namepattern == "firstname_fathername_mother_name" &
        numspace == 2  ~ fullname1,
      
      is.na(firstname) &  namepattern == "firstname_fathername_mother_name" &
        numspace == 3  ~ paste0(fullname1, " ", fullname2 ),
      
      is.na(firstname) &  namepattern == "firstname_fathername_mother_name" &
        numspace == 4  ~ paste0(fullname1, " ", fullname2, " ", fullname3),
      
      is.na(firstname) &  namepattern == "fathername_mothername_firstname" &
        numspace == 0  ~ "",
      
      is.na(firstname) &  namepattern == "fathername_mothername_firstname" &
        numspace == 1  ~ fullname2,
      
      is.na(firstname) &  namepattern == "fathername_mothername_firstname" &
        numspace == 2  ~ fullname3,
      
      is.na(firstname) &  namepattern == "fathername_mothername_firstname" &
        numspace == 3  ~ paste0(fullname3, " ", fullname4 ) ,
      
      is.na(firstname) &  namepattern == "fathername_mothername_firstname" &
        numspace == 4  ~ paste0(fullname3, " ", fullname4, " ", fullname5) ,
      
      TRUE ~ firstname    )) |>

                   
    ###  Lets reset the prefix space replacement...
    dplyr::mutate(
      firstname = reset_spaces_based_on_patterns(vector = firstname),
      fathername = reset_spaces_based_on_patterns(vector = fathername),
      mothername = reset_spaces_based_on_patterns(vector = mothername)) |>
    
    ## then clean intermediate variables
    dplyr::select( - fullname1, - fullname2, - fullname3, - fullname4, 
                   - fullname5, fullname_or, - fullname_pref, - numspace )
    
    
  return(framesp)
}

## Testing...
# frame <- data |> 
#   dplyr::filter(  is.na(nombres) ) |> 
#   # dplyr::select(nombre_completo, name_pattern,
#   #               nombres, apellido_paterno, apellido_materno)  |>
#   separate_fullname(fullname= "nombre_completo", 
#                     namepattern= "name_pattern",
#                     firstname =  "nombres",
#                     fathername = "apellido_paterno",
#                     mothername = "apellido_materno") 





#' separate_firstname  
#' 
#' This function decomposition - in case there is a space in
#' 
#' Performing this name decomposition is important in order to enhance record linkage  
#'  
#' @param firstname first name 
#' @return a clean list with c("firstname1","firstname2","firstname3")
#' 
#' @export
separate_firstname <- function(frame, 
                              firstname ) {
  ## Let's go!  
  framesp <- frame |>
    ## Rename variable
    dplyr::rename(  firstname = paste0(firstname) ) |>
    
    ###  Lets clean all spaces and get everything to upper
    dplyr::mutate( firstname_or = firstname, 
                   firstname = trimws(stringr::str_squish(firstname), 
                                     which = "both", 
                                     whitespace = "[ \t\r\n]"),
                   firstname = toupper(firstname)) |>
    dplyr::mutate( firstname = remove_spaces_based_on_patterns(vector = firstname) ) |>
   
    ## Counting the number of space to understand the structure of the full name
    tidyr::separate_wider_delim(firstname,
                                delim = " ", 
                                names_sep = "", 
                                too_few = "align_start") |>
    dplyr::mutate( firstname1 = reset_spaces_based_on_patterns(vector = firstname1),
                   firstname2 = reset_spaces_based_on_patterns(vector = firstname2),
                   firstname3 = reset_spaces_based_on_patterns(vector = firstname3))
  
  return(framesp)
}

frame <- data |>
  separate_firstname(firstname = "nombres" ) 
       


#' separate_familyname  
#' 
#' This function helps in decomposing names in case there is a space 
#' in the father name and the mother name is empty
#' 
#' Performing this name decomposition is important in order to enhance record linkage  
#'  
#' @param fathername father name  
#' @param mothername mother name 
#' @return a clean list with c("firstname1","firstname2","firstname3")
#' 
#' @export
separate_familyname <- function(frame, 
                                fathername,
                                mothername) {
  ## Let's go!  
  framesp <- frame |>
    ## Rename variable
    dplyr::rename(  fathername = paste0(fathername),
                    mothername = paste0(mothername) ) |>
    
    ###  Lets clean all spaces and get everything to upper
    dplyr::mutate(  fathername_or =  fathername, 
                    fathername= trimws(stringr::str_squish( fathername), 
                                      which = "both", 
                                      whitespace = "[ \t\r\n]"),
                    fathername = toupper( fathername)) |>
    dplyr::mutate(  fathername = remove_spaces_based_on_patterns(vector =  fathername) ) |>
    
    ## Counting the number of space to understand the structure of the full name
    tidyr::separate_wider_delim(firstname,
                                delim = " ", 
                                names_sep = "", 
                                too_few = "align_start") |>
    dplyr::mutate( firstname1 = reset_spaces_based_on_patterns(vector = firstname1),
                   firstname2 = reset_spaces_based_on_patterns(vector = firstname2),
                   firstname3 = reset_spaces_based_on_patterns(vector = firstname3))
  
  return(framesp)
}

frame <- data |>
  separate_familyname(fathername = "nombres" ) 


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
  #data |> dplyr::select(age_range) |> dplyr::distinct() |> dplyr::pull()
   dplyr::mutate( age_range = dplyr::case_when(
     age_range == "18 A 59 AÑOS" ~ "0-4",
      TRUE ~  age_range  )) |>
  
  ## Clean DOb & Age
    clean_age( date_birth = "fecha_de_nacimiento",
               date_record = "date_record",
               age = "edad",
               age_range = "age_range") |>
  
  ## Clean the names
  separate_fullname(fullname= "nombre_completo", 
                    namepattern= "name_pattern",
                    firstname =  "nombres",
                    fathername = "apellido_paterno",
                    mothername = "apellido_materno")  |>
  
  
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
  dplyr::mutate_at( dplyr::vars(firstname, fathername, mothername,
                                asistencia, departamento, #telefono,
                                planilla, socio), 
                    list(new = cleanvar))  |>
  
  
  ### identify single data source
  dplyr::mutate(datasource = paste0(socio_new, "_", planilla_new)) #|>
  
  # # ## Retain only fields for record linkage
  # dplyr::select(datasource, nationality, 
  #               firstname_new, fathername_new, mothername_new, 
  #               asistencia_new, departamento_new,
  #               telefono, gender)



dput(names(data.prep))


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