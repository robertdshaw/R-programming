#install.packages('pxweb')
library(pxweb)
library(lubridate)
library(dplyr)
library(ggplot2)

# PXWEB query 
pxweb_query_list <- 
  list("Region"=c("00"),
       "Drivmedel"=c("100","110","120","130","140","150","160","190"),
       "ContentsCode"=c("TK1001AA"),
       "Tid"=c("2006M01","2006M02","2006M03","2006M04","2006M05","2006M06","2006M07","2006M08","2006M09","2006M10","2006M11","2006M12","2007M01","2007M02","2007M03","2007M04","2007M05","2007M06","2007M07","2007M08","2007M09","2007M10","2007M11","2007M12","2008M01","2008M02","2008M03","2008M04","2008M05","2008M06","2008M07","2008M08","2008M09","2008M10","2008M11","2008M12","2009M01","2009M02","2009M03","2009M04","2009M05","2009M06","2009M07","2009M08","2009M09","2009M10","2009M11","2009M12","2010M01","2010M02","2010M03","2010M04","2010M05","2010M06","2010M07","2010M08","2010M09","2010M10","2010M11","2010M12","2011M01","2011M02","2011M03","2011M04","2011M05","2011M06","2011M07","2011M08","2011M09","2011M10","2011M11","2011M12","2012M01","2012M02","2012M03","2012M04","2012M05","2012M06","2012M07","2012M08","2012M09","2012M10","2012M11","2012M12","2013M01","2013M02","2013M03","2013M04","2013M05","2013M06","2013M07","2013M08","2013M09","2013M10","2013M11","2013M12","2014M01","2014M02","2014M03","2014M04","2014M05","2014M06","2014M07","2014M08","2014M09","2014M10","2014M11","2014M12","2015M01","2015M02","2015M03","2015M04","2015M05","2015M06","2015M07","2015M08","2015M09","2015M10","2015M11","2015M12","2016M01","2016M02","2016M03","2016M04","2016M05","2016M06","2016M07","2016M08","2016M09","2016M10","2016M11","2016M12","2017M01","2017M02","2017M03","2017M04","2017M05","2017M06","2017M07","2017M08","2017M09","2017M10","2017M11","2017M12","2018M01","2018M02","2018M03","2018M04","2018M05","2018M06","2018M07","2018M08","2018M09","2018M10","2018M11","2018M12","2019M01","2019M02","2019M03","2019M04","2019M05","2019M06","2019M07","2019M08","2019M09","2019M10","2019M11","2019M12","2020M01","2020M02","2020M03","2020M04","2020M05","2020M06","2020M07","2020M08","2020M09","2020M10","2020M11","2020M12","2021M01","2021M02","2021M03","2021M04","2021M05","2021M06","2021M07","2021M08","2021M09","2021M10","2021M11","2021M12","2022M01","2022M02","2022M03","2022M04","2022M05","2022M06","2022M07","2022M08","2022M09","2022M10","2022M11","2022M12","2023M01","2023M02","2023M03","2023M04","2023M05","2023M06","2023M07","2023M08","2023M09","2023M10","2023M11","2023M12","2024M01","2024M02","2024M03"))

# Download data 
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/en/ssd/TK/TK1001/TK1001A/PersBilarDrivMedel",
            query = pxweb_query_list)

# Convert to data.frame 
px_data_frame <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# Get pxweb data comments 
# px_data_comments <- pxweb_data_comments(px_data)
# px_data_comments_df <- as.data.frame(px_data_comments)

# Cite the data as 
# pxweb_cite(px_data)

px_data_frame
str(px_data_frame)
View(px_data_frame$fuel)
unique_fuel_types <- unique(px_data_frame$fuel)
print(unique_fuel_types)


# 1. Plot all new registered cars by fuel type 
px_data_frame$month <- as.Date(paste0(substr(px_data_frame$month, 1, 4), "-", substr(px_data_frame$month, 6, 7), "-01"))
ggplot(px_data_frame, aes(x = month, y = `New registered passenger cars`, group = fuel, color = fuel)) +
  geom_line() +
  labs(title = "Trend of New Registered Passenger Cars (2006-2024)",
       x = "Year",
       y = "Number of New Registered Cars",
       color = "Fuel Type") +
  theme_minimal() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")  # Adjust date breaks for better x-axis readability


#2. Plot all registered cars w/o fuel type specification to see general trend
total_cars_per_month <- px_data_frame %>%
  group_by(month) %>%
  summarize(`New registered passenger cars` = sum(`New registered passenger cars`))
ggplot(total_cars_per_month, aes(x = month, y = `New registered passenger cars`)) +
  geom_line() +
  labs(title = "Total New Registered Passenger Cars (2006-2024)",
       x = "Year",
       y = "Total Number of New Registered Cars") +
  theme_minimal() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")

#3. Plot registered cars by environmental classification
px_data_frame$month <- as.Date(paste0(substr(px_data_frame$month, 1, 4), "-", substr(px_data_frame$month, 6, 7), "-01"))
environmentally_friendly <- c("electricity", "electric hybrid", "plug-in hybrid", "ethanol/ethanol flexifuel")
px_data_frame$Environment_Class <- ifelse(px_data_frame$fuel %in% environmentally_friendly, 
                                          "Environmentally Friendly", 
                                          "Not Environmentally Friendly")
cars_by_environment <- px_data_frame %>%
  group_by(month, Environment_Class) %>%
  summarize(`New registered passenger cars` = sum(`New registered passenger cars`), .groups = 'drop')
ggplot(cars_by_environment, aes(x = month, y = `New registered passenger cars`, color = Environment_Class)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE, aes(group = Environment_Class)) +
  labs(title = "New Registered Passenger Cars by Environmental Classification (2006-2024)",
       x = "Year",
       y = "Number of New Registered Cars",
       color = "Category") +
  theme_minimal() +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y")
