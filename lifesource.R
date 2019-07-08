###################
## load packages ##
###################
library(tidyverse)
library(lubridate)




##################
## read-in data ##
##################
df_lifesource <- read_csv("data_input/lifesource.csv")




###############
## wrangling ##
###############
## changing column names to snake case
names(df_lifesource) <-
    names(df_lifesource) %>% 
    str_to_lower() %>% 
    gsub(pattern = " ", replacement = "_")


## converting referral_date_time datetime 
df_lifesource$referral_date_time <- 
    parse_date_time(
        df_lifesource$referral_date_time
        , '%m/%d/%Y %I:%M:%S %p'
        , tz = 'America/Chicago'
    )


## weight to lbs
# custom function that converts kgs to lbs and leaves lbs as lbs
to_lbs <- function(wt, units){
    
    # if NA then just return NA values
    if (is.na(wt)) return(wt)
    if (is.na(units)) return(wt)
    
    if (units == "kgs") {
        wt * 2.2
    } else {
        wt
    }
}

# use purrr::map2_dbl to vectorize the custom function above
# use mutate to calculate the new weight in lbs columns
df_lifesource <- 
    df_lifesource %>% 
        mutate(weight_lbs = map2_dbl(weight, weight_unit, to_lbs))

# checking our work
# (challenge: modify my query to check the ones with kg)
df_lifesource %>% 
    select(weight, weight_unit, weight_lbs) %>% 
    filter(weight_unit == "lbs")





#################################
## exploring organ disposition ##
#################################
# checking for distinct values
df_lifesource %>% distinct(organ_disposition)

# dataframe with counts
df_lifesource %>% 
    group_by(organ_disposition) %>% 
    summarize(num = n()) %>% 
    arrange(desc(num))





####################
## hospital state ##
####################
# dataframe with counts
df_lifesource %>% 
    group_by(hospital_state) %>% 
    summarize(num = n()) %>% 
    arrange(desc(num))


# simple barchart with the same data
df_lifesource %>% 
    group_by(hospital_state) %>% 
    summarize(num = n()) %>% 
    ggplot(aes(x = reorder(hospital_state, desc(num)), y = num)) +
        geom_bar(stat = "identity")





##################
## organ donors ##
##################
df_donor <- 
    df_lifesource %>% 
        filter(organ_disposition == "Donor")

# barchart with counts of hospital names of donors
df_donor %>% 
    group_by(hospital_name) %>% 
    summarize(num = n()) %>% 
        ggplot(aes(x = reorder(hospital_name, num), y = num)) +
        geom_bar(stat = "identity") +
        coord_flip()

# barchart with counts of monthly donors
df_donor %>% 
    mutate(year_mo = format(referral_date_time, "%Y-%m")) %>% 
    group_by(year_mo) %>% 
    summarize(num = n()) %>% 
    ggplot(aes(x = year_mo, y = num)) +
        geom_bar(stat = "identity") +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
    


