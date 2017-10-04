library(tidyverse)
library(lubridate)

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
  group_by(Risk) %>%
  count(Results)  

ggplot(data = food %>%
                filter(!(Risk %in% c("All", "")))
       ) +
          geom_bar(mapping = aes(x = Results)) +
          facet_grid(Risk ~ .)

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




