library(tidyverse)
library(lubridate)
library(tm)
library(stringr)
library(gtools)

colClasses <- c("integer", rep("character", 2), "integer",
                rep("factor", 2), rep("character", 2), "factor",
                "integer", "character", rep("factor", 2),
                "character", rep("numeric",2), "character")

food <- read.csv("Food_Inspections.csv", colClasses = colClasses)

food <- food %>%
          mutate(Inspection.Date = as.Date(Inspection.Date, 
                                           format = "%m/%d/%Y")
                 )

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


### Violations field is free-form text; split and get words
test <- strsplit(food$Violations[4], "[ |-]")
test <- unlist(test)
test <- gsub("[^[:alpha:]|']", "", test)
test <- tolower(test)
test <- test[!(test %in% c(stopwords("english"),""))]
as.list(test)




test <- food %>%
  mutate(s = strsplit(Violations, "|", fixed = T))
  
test2 <- test %>%
  unnest(s)


# run into trouble when we do this b/c too many values b/c comments: appears multiple
# times for some records ; eg, look at 20th record of test2 where it has "inspector comments:" as
# well
test3 <- test2 %>%
  separate(s, into = c("var", "val"), sep = "Comments:")
  
# instead look for hyphen in front of comments to split, put * after space in case it 
# appears as "-Comments:" instead of "- Comments:"; also use [Cc] in case Comments is
# not capitalized. This increases record count from 156,065 to 576,711
test3 <- test2 %>%
  separate(s, into = c("var", "val"), sep = "- *[Cc]omments:") %>%
  mutate_at(vars("var", "val"), funs(str_trim))

test4 <- test3 %>%
  spread(var, val)

# we get Error: Duplicate identifiers for rows (181869, 181870),... why
test3[c(181869, 181870), c("Inspection.ID", "DBA.Name", "var", "val")]  #appears there are multiple keys for this restaurant Cafe on the Grove
food %>%
  filter(DBA.Name == "CAFE ON THE GROVE") 

food %>%
  filter(DBA.Name == "CAFE ON THE GROVE") %>%
  select(Inspection.Date, Violations) 
# the inspection on 5/26/15 had multiple entries for 
# 10. SEWAGE AND WASTE WATER DISPOSAL, NO BACK SIPHONAGE, CROSS  CONNECTION AND/OR BACK FLOW
# which is causing the error we got above
# How to get around this? One thought is do a group by on test3 DF for inspection ID and var, 
# and merge val together for duplicate records. Record count drops from 576,711 to 569,002
test4 <- test3 %>%
  group_by_at(names(.)[names(.) != "val"]) %>%
  summarize(val = paste(val, collapse = " ")) %>%
  ungroup()   # need to ungroup or else many of the columns will still be grouped  

# now look at the val field for the Inspection.ID that had the duplicate entries (1546425)
t<-test4 %>% 
  filter(Inspection.ID == 1546425) %>%
  filter(grepl("10.", var))

# The 2 entries for 10. SEWAGE AND WASTE WATER DISPOSAL... are now concatenated into 1 string 
t$val
t$Violations

# now we can continue with the dplyr::spread() function
test6 <- test4 %>%
          spread(var, val, fill = NA)

# names of new columns are incredibly long. just store numbers and save descriptions to 
# separate data frame
reqs <- names(test6)[grepl("\\d{1,2}\\.", names(test6))]
nbr_matches <- str_match(reqs, "(\\d{1,2})\\.")  # look for number of 1 or 2 digits immediately followed by period, we will get a matrix with has the full match in 1st column, and then the group in the 2nd column
req_nbrs <- as.numeric(nbr_matches[, 2]) # only take 2nd column
req_descs <- str_trim(gsub("\\d{1,2}\\.", "", reqs))

reqs <- data.frame(nbr = req_nbrs, desc = req_descs) %>%
          arrange(nbr)

# now clean up columns in test6 and reorder columns
names(test6)[grepl("\\d{1,2}\\.", names(test6))] <- paste0("req", req_nbrs)
req_cols <- mixedsort(names(test6)[grepl("\\d{1,2}", names(test6))])

test7 <- test6[ , names(test6)[!(names(test6) %in% req_cols)]] %>%
          cbind(test6 %>%
                  select(one_of(req_cols))
                )

# our resulting data frame only has 124,812, but the original food DF has 156,065
# what happened to these 31,253 records?
# they dropped out when we used dplyr::spread() b/c Violations fld is empty 

x <- food %>%
      filter(!(Inspection.ID %in% test7$Inspection.ID)) %>%
      select(Results, Violations)

# look at the Results; violations NA b/c they either passed or were not inspected for some reason,
# although some Failed or Passed w/Conditions but had no Violations listed, which is interesting
x %>%
  filter(Results %in% c("Fail", "Pass w/ Conditions"), Violations == "") %>%
  count()

# add them back to the DF we created with the req columns
test8 <- food %>%
            filter(!(Inspection.ID %in% test7$Inspection.ID)) %>%
            bind_rows(test7)

