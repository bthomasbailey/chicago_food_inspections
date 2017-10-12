library(tidyverse)
library(lubridate)
library(stringr)
library(gtools)
library(XML)
library(RCurl)


food <- read_csv("Food_Inspections.csv")

food <- food %>%
          mutate(`Inspection Date` = parse_date(`Inspection Date`, "%m/%d/%Y")) %>%
          mutate_at(vars(Risk, Results), as.factor)


## Look for variation w/in variables ##
# Results var
food %>% 
  count(Results)

ggplot(data = food) +
  geom_bar(mapping = aes(x = Results))

# Inspection Date - look at nbr of inspections per year
food %>% 
  count(year(Inspection.Date))

ggplot(data = food) +
  geom_bar(mapping = aes(x = year(Inspection.Date)))

#significantly fewer records for 2017, but this makes sense b/c we are still in 2017

# nbr of inspections in each month (across years)
food %>% 
  count(month(Inspection.Date))

ggplot(data = food) +
  geom_bar(mapping = aes(x = month(Inspection.Date)))

# facility type - looks like this has 447 different categories
# think about cleaning this field up using text parsing or regexes
food %>% 
  count(Facility.Type)

# risk - 68 obs have zero length string, 22 have "All", which seems
# to be an unclear risk rating
food %>%
  count(Risk)

ggplot(data = food) +
  geom_bar(mapping = aes(x = Risk))

# What is distribution of results for each level of risk? (get rid of "All" and "" in
# Risk variable)
food %>%
  filter(!(Risk %in% c("All", ""))) %>%
  count(Risk, Results)

# try visualizing with tile plot
food %>%
  filter(!(Risk %in% c("All", ""))) %>%
  count(Risk, Results) %>%
  ggplot(mapping = aes(x = Risk, y = Results)) +
    geom_tile(mapping = aes(fill = n))

ggplot(data = food %>%
                filter(!(Risk %in% c("All", "")))
       ) +
          geom_bar(mapping = aes(x = Results)) +
          facet_grid(. ~ Risk)

# those are raw numbers, but rates of failure, success, etc within each
# Risk group would be better
rates_rr <- food %>%
              filter(!(Risk %in% c("All", ""))) %>%
              group_by(Risk, Results) %>%
              summarize(n = n()) %>%
              mutate(rate = n / sum(n))

ggplot(data = rates_rr) +
  geom_col(mapping = aes(x = Results, y = rate)) +
  facet_grid(Risk ~ .)

# Failure rate is higher and success rate lower in Low Risk group compared to 
# medium and high risk groups. How does graph look if we group 'Pass w/ Conditions' 
# in with 'Pass'? 
# Group 'Pass' and 'Pass w/Conditions', keep 'Fail', drop all other obs

rates_rr2 <- food %>%
                filter(!(Risk %in% c("All", ""))) %>%
                filter(Results %in% c("Pass", "Pass w/ Conditions", "Fail")) %>%
                mutate(Results = ifelse(Results %in% c("Pass", "Pass w/ Conditions"), 
                                        "Pass", 
                                        "Fail")
                       ) %>%
                group_by(Risk, Results) %>%
                summarize(n = n()) %>%
                mutate(rate = n / sum(n))

ggplot(data = rates_rr2) +
  geom_col(mapping = aes(x = Results, y = rate)) +
  facet_grid(. ~ Risk)

# Failure rate is almost 32% for low risk places and about 22% for
# medium and high risk. 


food_parsed <- food %>%
                mutate(vio_split = strsplit(Violations, "|", fixed = T)) %>%
                unnest(vio_split)

# We would continue this dplyr chain, but want to illustrate the error we will get
# when we try to use the dplyr separate() function
# run into trouble when we do this b/c too many values b/c comments: appears multiple times 
# for some records ; eg, look at 20th record of test2 where it has "inspector comments:" as well
food_parsed <- food_parsed %>%
                separate(vio_split, 
                         into = c("var", "val"), 
                         sep = "Comments:")
  
# instead look for hyphen in front of comments to split, put * after space in case it 
# appears as "-Comments:" instead of "- Comments:"; also use [Cc] in case Comments is
# not capitalized. This increases record count from 156,065 to 576,711
food_parsed <- food %>%
                mutate(vio_split = strsplit(Violations, "|", fixed = T)) %>%
                unnest(vio_split) %>%
                separate(vio_split, 
                         into = c("var", "val"), 
                         sep = "- *[Cc]omments:") %>%
                mutate_at(vars("var", "val"), funs(str_trim))

# Let's break the chain here, because we're going to get an error when we try to run dplyr's
# spread() function, which unstacks the data and gives each distinct value in var its own column
food_parsed <- food_parsed %>%
                spread(var, val)

# we get Error: Duplicate identifiers for rows (314334, 314336),... why? 
# Let's look at the first two rows they mention
# appears there are multiple keys for this restaurant JEWEL FOOD STORE #3030 for Inspection ID 1296681
food_parsed %>%
  filter(row_number() %in% c(314334, 314336)) %>%
  select(`Inspection ID`, `DBA Name`, var, val) %>%
  print(width = Inf)
 

# Let's look at the original data frame for this Inspection ID
food %>%
  filter(`Inspection ID` == 1296681) %>%
  select(`Inspection Date`, Violations) %>%
  print(width = Inf)

# the inspection on 2013-11-12 had multiple entries for 
# 1. SOURCE SOUND CONDITION, NO SPOILAGE, FOODS PROPERLY LABELED, SHELLFISH TAGS IN PLACE
# which is causing the error we got above
# How to get around this? One thought is do a group by on food_parsed for everything except val, 
# and merge val together for duplicate records. 
food_parsed <- food_parsed %>%
                group_by_at(names(.)[names(.) != "val"]) %>%
                summarize(val = paste(val, collapse = " ")) %>%
                ungroup()   # need to ungroup or else many of the columns will still be grouped  

# Now look at Inspection.ID of 1296681, which is JEWEL FOOD STORE #3030 inspection on 2013-11-12 and
# filter for records with key/var of 10.
# Strings from two different records have now been concatenated into one
food_parsed %>%
  filter(`Inspection ID` == 1296681, grepl("1. SOURCE SOUND CONDITION", var)) %>%
  select(val) %>%
  print(width = Inf)

# now we can continue with the dplyr::spread() function
food_parsed <- food_parsed %>%
                spread(var, val, fill = NA)

# drop NA column
food_parsed <- food_parsed %>%
                  select(-`<NA>`)

# names of new columns are incredibly long. just store numbers and save descriptions to 
# separate data frame
reqs <- names(food_parsed)[grepl("\\d{1,2}\\.", names(food_parsed))]
nbr_matches <- str_match(reqs, "(\\d{1,2})\\.")  # look for number of 1 or 2 digits immediately followed by period, we will get a matrix with has the full match in 1st column, and then the group in the 2nd column
req_nbrs <- as.numeric(nbr_matches[, 2]) # only take 2nd column
req_descs <- str_trim(gsub("\\d{1,2}\\.", "", reqs))

reqs <- data.frame(nbr = req_nbrs, desc = req_descs) %>%
          arrange(nbr)

# now clean up columns in food_parsed and reorder columns
names(food_parsed)[grepl("\\d{1,2}\\.", names(food_parsed))] <- paste0("req", req_nbrs)
req_cols <- mixedsort(names(food_parsed)[grepl("\\d{1,2}", names(food_parsed))])

food_parsed <- food_parsed[!(names(food_parsed) %in% req_cols)] %>%
                cbind(food_parsed %>%
                        select(one_of(req_cols))
                      )


# # Data is now much tidier than it originally was, but we can still further parse 
# # the individual req columns. Most values follow the format of the specific violation 
# # which is almost always lowercase, followed by inspector's comments, which are almost always
# # all CAPS. 
# 
# 
# # write function that takes in value, and returns same value with "|" to indicate split point
# # between the rule and what was observed
# divide_req <- function(val) {
#   words <- str_split(val, " |:|\\.")[[1]]
#   words <- words[words != ""]
#   
#   if (sum(str_to_upper(words) == words & grepl("[[:alpha:]]", words)) == 0) 
#     val
#   else {
#     rule <- str_trim(str_c(words[str_to_upper(words) != words], collapse = " "))
#     observed <- str_trim(str_c(words[str_to_upper(words) == words], collapse = " "))
#     str_c(observed, rule, sep = "|")
#   }
# }


# Determine which inspections were serious, which were critical
# sometimes we see "CITATION ISSUED SERIOUS", mostly just "SERIOUS VIOLATION/CITATION"

get_severity <- function(val) {
  severity <- NA_character_
  
  if(grepl("serious (violation|citation)", val, ignore.case = T))
    severity <- "serious"
  else if(grepl("critical (violation|citation)", val, ignore.case = T))
    severity <- "critical"
  
  if(!is.na(severity)) {
    sents <- str_split(val, "\\.")[[1]]
    if(sum(grepl("(violation|citation).+serious", 
                 sents, 
                 ignore.case = T)) > 0)
      
      severity <- "serious"
    
    else if(sum(grepl("(violation|citation).+serious", 
                      sents, 
                      ignore.case = T)) > 0)
      
      severity <- "critical"
  }
  
  severity
  
}


is_corrected <- function(val) {
  if(grepl("not (corrected|abated)", val, ignore.case = T)) 
    FALSE
  else if(grepl("corrected|abated", val, ignore.case = T))
    TRUE
}


# More details on the violations can be found online
url <- "https://www.cityofchicago.org/city/en/depts/"
url <- str_c(url, "cdph/provdrs/inspections_and_permitting/svcs/")
url <- str_c(url, "understand_healthcoderequirementsforfoodestablishments.html")

resp <- getURL(url)
tbl <- readHTMLTable(resp)
violation_info <- tbl[[1]] 

violation_info <- violation_info %>%
                    set_names(c("vio_nbr", "standard", "type")) %>%
                    








# Of inspections that failed, how many had serious violations?
food_parsed %>%
  filter(Results == "Fail") %>%
  select(one_of(req_cols)) %>%
  map(~sum(grepl("serious violation", ., ignore.case = T)))

food_parsed %>%
  filter(Results == "Fail") %>%
  select(one_of(req_cols)) %>%
  map(~sum(grepl("critical violation", ., ignore.case = T))) %>%
  unlist()
