library(plotly)
library(rjson)
library(RColorBrewer)
library(wesanderson)
library(rcartocolor)

df <- read_csv('www/data/Vaccines.gov__Flu_vaccinating_provider_locations.csv')
df <- df[,c(3:5,7:16,19:20,22:27)]
df <-
  rename(
    df,
    Phone = loc_phone,
    Name = loc_name,
    Street = loc_admin_street1,
    City = loc_admin_city,
    State = loc_admin_state,
    zip = loc_admin_zip,
    Sunday=sunday_hours,Monday=monday_hours,Tuesday=tuesday_hours,Wednesday=wednesday_hours,Thursday=thursday_hours,Friday=friday_hours,Saturday=saturday_hours
  )
df <- df[!duplicated(df),]
df$City <- toupper(df$City)
df <- df %>% filter(quantity_last_updated == '2023-03-22')

df <- df %>% mutate(across(c(7:13), ~ ifelse(is.na(.), 'CLOSED', .)))

# extract the first 5 digits of the "zipcode" column
df$zip <- substr(df$zip, 1, 5)

url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'

# choose the display columns
default_col1 <- c('Name','Street','City','State',
                  'zip','searchable_name') 
default_col2 <- c('Name','Street','City','State','Phone',
                  'insurance_accepted','walkins_accepted','searchable_name')
default_col3 <- c('Name','State','City','Street','Phone','zip',
                  'insurance_accepted','walkins_accepted','searchable_name','supply_level')
