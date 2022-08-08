library(tidyverse)
library(tidygeocoder)

udf <- read_tsv("G:\\Disease_X\\Master Case List\\UDF_data.tsv", na = c("","N/A","n/a"), quote = "")

cases <- udf %>%
  filter(RStatus %in% c("Confirmed","Probable")) %>%
  select(IncidentID, Address, City, Zip, State, CntyOfResid)

cases$Address <- gsub("\\Apt.*", "", cases$Address) #Remove all apartment numbers in address
cases$Address <- gsub("\\Unit.*", "", cases$Address) #Remove all unit numbers in address
cases$Address <- gsub("\\Space.*", "", cases$Address) #Remove all space numbers in address
cases$Address <- gsub("\\Spc.*", "", cases$Address)
cases$Address <- gsub("\\#.*", "", cases$Address) #Remove all apartment numbers in address

cases$Address <- str_replace_all(cases$Address, " St $| St$", " Street")
cases$Address <- str_replace_all(cases$Address, " Ave $| Ave$", " Avenue")
cases$Address <- str_replace_all(cases$Address, " Ln $| Ln$", " Lane")
cases$Address <- str_replace_all(cases$Address, " Dr $| Dr$", " Drive")
cases$Address <- str_replace_all(cases$Address, " Cir $| Cir$", " Circle")
cases$Address <- str_replace_all(cases$Address, " Rd $| Rd$", " Road")
cases$Address <- str_replace_all(cases$Address, " Ct $| Ct$", " Court")
cases$Address <- str_replace_all(cases$Address, " Pl $| Pl$", " Place")

cases <- cases %>% mutate_if(is.character, ~replace(., .=="UNK", NA_character_))

cases <- cases %>% mutate_if(is.character, ~replace(., grepl("Refused|Declined|Not Provided|Unknown", ., ignore.case = TRUE), NA_character_))

cases <- cases %>%
  mutate(City = case_when(
    grepl("Hanaheim|Anheim|Aneheim|Anaheim |ANAHEMI|ANAHIEM|ANAHEIM HILLS|92802|Anaheiim|Anaehim|Anahiam", City, ignore.case = TRUE) ~ "Anaheim",
    grepl("Alisa Viejo", City, ignore.case = TRUE) ~ "Aliso Viejo",
    grepl("92823", City, ignore.case = TRUE) ~ "Brea",
    grepl("Buena Park,", City, ignore.case = TRUE) | City == "Buena" ~ "Buena Park",
    grepl("Coasta Mesa", City, ignore.case = TRUE) ~ "Costa Mesa",
    grepl("Coto De Caza", City, ignore.case = TRUE) ~ "Coto de Caza",
    grepl("Capistrano Beach|Capo Beach|Monarch Beach", City, ignore.case = TRUE) ~ "Dana Point",
    grepl("Fountain Vly", City, ignore.case = TRUE) ~ "Fountain Valley",
    grepl("Fulllerton", City, ignore.case = TRUE) ~ "Fullerton",
    grepl("Graden Grove|Gadern Grove|GG|Garden Grove Ca", City, ignore.case = TRUE) ~ "Garden Grove",
    grepl("Hb|Hunington Beach|Huntingtn Bch|Huntington Beach |Sunset Beach|Huntington|Huntingtn Beach|Hintington Beach", City, ignore.case = TRUE) ~ "Huntington Beach",
    grepl("EAST IRVINE|Irivne", City, ignore.case = TRUE) ~ "Irvine",
    CntyOfResid == "Orange" & grepl("La Habra Heights", City, ignore.case = TRUE) ~ "La Habra",
    grepl("Ladera Ranch Ca", City, ignore.case = TRUE) ~ "Ladera Ranch",
    grepl("Laguna Miguel|Laguna Nigel", City, ignore.case = TRUE) ~ "Laguna Niguel",
    City == "Los Al" ~ "Los Alamitos",
    grepl("EL TORO|Foothill Ranch|Foothill Rnch|Portola Hills", City, ignore.case = TRUE) ~ "Lake Forest",
    grepl("Mission Viego|Mission Veijo|Mission. Viejo", City, ignore.case = TRUE) | City=="Viejo" ~ "Mission Viejo",
    grepl("Balboa Island|Corona Del Mar|NEW PORT BEACH|Newport Coast", City, ignore.case = TRUE) ~ "Newport Beach",
    grepl("Orange |ORGANE|92869", City, ignore.case = TRUE) ~ "Orange",
    grepl("Atwood", City, ignore.case = TRUE) ~ "Placentia",
    grepl("RANCHO MISSION VIE", City, ignore.case = TRUE) ~ "Rancho Mission Viejo",
    grepl("Rancho Santa Margari|Rancho Sta Marg|Rcho Sta Marg|Rsm|RANCHO SANTA MAR|Rancho Santa Ma|Rancho Santa M|Rancho Santa  M", City, ignore.case = TRUE)           ~ "Rancho Santa Margarita",
    grepl("San Juan Capo|San Juan Capistr|San Juan Cap", City, ignore.case = TRUE) ~ "San Juan Capistrano",
    grepl("SANT ANA|SANTA  ANA|Santa An A|Santa Aba|Santa Ana Ca.", City, ignore.case = TRUE) ~ "Santa Ana",
    grepl("Surfside", City, ignore.case = TRUE) ~ "Seal Beach",
    grepl("Santon", City, ignore.case = TRUE) ~ "Stanton",
    grepl("Trabuco Cyn|Trabuco", City, ignore.case = TRUE) ~ "Trabuco Canyon",
    grepl("North Tustin|N Tustin|Tustian|Cowan Heights", City, ignore.case = TRUE) ~ "Tustin",
    grepl("WESTMINISTER|Wesminister", City, ignore.case = TRUE) ~ "Westminster",
    grepl("Dove Canyon", City, ignore.case = TRUE) ~ "Other",
    grepl("HOMELESS|UNK|30Orange|3Orange|30Orange|9282|CONFIDENTIAL|None|NONE|Declined|Refused", City, ignore.case = TRUE) ~ NA_character_,
    City == "Na" ~ NA_character_,
    City == "NA" ~ NA_character_,
    City == "Null" ~ NA_character_,
    City == "Not Provided" ~ NA_character_,
    City == "." ~ NA_character_,
    City == "0" | City == "00" ~ NA_character_,
    TRUE ~ City ))

##Note: if an address does not geocode, try removing the apartment, unit, or space info and try again

Geocode_OSM <- cases %>%
  head(50) %>%
  mutate(Address = str_to_title(Address),
         City = str_to_title(City),
         State = str_to_title(State)) %>%
  drop_na(Address) %>%
  mutate(Clean_Address = paste0(Address,", ", City, ", ", State, ", ", Zip)) %>%
  geocode(Clean_Address, method = "osm", lat = latitude , long = longitude, full_results = TRUE, return_type = "geographies")

Geocode_ArcGIS <- cases %>%
  head(50) %>%
  mutate(Address = str_to_title(Address),
         City = str_to_title(City),
         State = str_to_title(State)) %>%
  drop_na(Address) %>%
  mutate(Clean_Address = paste0(Address,", ", City, ", ", State, ", ", Zip)) %>%
  geocode(Clean_Address, method = "arcgis", lat = latitude , long = longitude, full_results = TRUE, return_type = "geographies")