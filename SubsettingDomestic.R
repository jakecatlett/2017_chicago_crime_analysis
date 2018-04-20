# Create a master data frame which all crimes committed per day in the dataset
  #This data frame is used for all further subsetting going forward

  #First all observations are included with only Date and X,Y coordinates included

crimes_total <- crimes_truncated %>%
                        select(Date, X.Coordinate, Y.Coordinate, Domestic, FBI.Code, District)

  # Then columns are added to identify the season, month, and year
  # in which each observation lies, enabling further aggregating and 
  # aiding in creation of visualizations

crimes_total$Date <- as.Date(crimes_total$Date, format="%m/%d/%Y")

crimes_total$Season <- as.factor(sapply(strftime(crimes_total$Date, "%m"), switch,
                                "12" = "winter",
                                "01" = "winter",
                                "02" = "winter",
                                "03" = "spring",
                                "04" = "spring",
                                "05" = "spring",
                                "06" = "summer",
                                "07" = "summer",
                                "08" = "summer",
                                "09" = "autumn",
                                "10" = "autumn",
                                "11" = "autumn"))

crimes_total$Month <- as.factor(sapply(strftime(crimes_total$Date, "%m"), switch,
                                "12" = "12",
                                "01" = "01",
                                "02" = "02",
                                "03" = "03",
                                "04" = "04",
                                "05" = "05",
                                "06" = "06",
                                "07" = "07",
                                "08" = "08",
                                "09" = "09",
                                "10" = "10",
                                "11" = "11"))

crimes_total$Year <- as.factor(sapply(strftime(crimes_total$Date, "%Y"), switch,
                                "2001" = "2001",
                                "2002" = "2002",
                                "2003" = "2003",
                                "2004" = "2004",
                                "2005" = "2005",
                                "2006" = "2006",
                                "2007" = "2007",
                                "2008" = "2008",
                                "2009" = "2009",
                                "2010" = "2010",
                                "2011" = "2011",
                                "2012" = "2012",
                                "2013" = "2013",
                                "2014" = "2014",
                                "2015" = "2015",
                                "2016" = "2016",
                                "2017" = "2017"))



####################################################################################


  #Aggregate all crimes by the date on which they were committed

crimes_per_day <- crimes_total %>%
  group_by(Date) %>%
  summarise(no_rows = length(Date))

  # Create a season column identify seasons and holidays

crimes_per_day$Season <- sapply(strftime(crimes_per_day$Date, "%m"), switch,
                                        "12" = "winter",
                                        "01" = "winter",
                                        "02" = "winter",
                                        "03" = "spring",
                                        "04" = "spring",
                                        "05" = "spring",
                                        "06" = "summer",
                                        "07" = "summer",
                                        "08" = "summer",
                                        "09" = "autumn",
                                        "10" = "autumn",
                                        "11" = "autumn")

  # Modify the season column to identify New Year's Day, 4th of July, and Christmas, respectively

crimes_per_day$Season <- if_else(strftime(crimes_per_day$Date, "%m")=="01" &
                                   strftime(crimes_per_day$Date, "%d")=="01",
                                 "XNYD", crimes_per_day$Season)

crimes_per_day$Season <- if_else(strftime(crimes_per_day$Date, "%m")=="07" &
                                   strftime(crimes_per_day$Date, "%d")=="04",
                                 "XIND", crimes_per_day$Season)

crimes_per_day$Season <- if_else(strftime(crimes_per_day$Date, "%m")=="12" &
                                   strftime(crimes_per_day$Date, "%d")=="25",
                                 "XXMS", crimes_per_day$Season)

crimes_per_day$Season <- as.factor(crimes_per_day$Season)

  #Create a data frame aggregating total crimes committed in each calendar month

crimes_total_YearMo <- crimes_total %>%
  mutate(YearMo = format(Date, "%Y%m")) %>%
  group_by(YearMo) %>%
  summarise(no_rows = length(YearMo))

crimes_total_YearMo$YearMo <- as.factor(crimes_total_YearMo$YearMo)
crimes_total_YearMo$Year <- as.numeric(substr(crimes_total_YearMo$YearMo, 1, 4))
crimes_total_YearMo$Month <- as.numeric(substr(crimes_total_YearMo$YearMo, 5, 6))

crimes_monthly_summary <- as.data.frame(matrix(ncol=4, nrow=12))

colnames(crimes_monthly_summary) <- c("Month", "Total_Crime",
                                      "Mean_Crime", "SD_Crime")

crimes_monthly_summary$Month <- as.numeric(crimes_monthly_summary$Month)
crimes_monthly_summary$Total_Crime <- as.numeric(crimes_monthly_summary$Total_Crime)
crimes_monthly_summary$Mean_Crime <- as.numeric(crimes_monthly_summary$Mean_Crime)
crimes_monthly_summary$SD_Crime <- as.numeric(crimes_monthly_summary$SD_Crime)


for(i in 1:12){
  for(j in 1:4){
    tempFrame <- filter(crimes_total_YearMo, Month==as.character(i))
    
    switch(j,
           crimes_monthly_summary[i, j] <- i,
           crimes_monthly_summary[i, j] <- sum(as.numeric(tempFrame$no_rows)),
           crimes_monthly_summary[i, j] <- round(mean(as.numeric(tempFrame$no_rows)), dig=0),
           crimes_monthly_summary[i, j] <- round(sd(as.numeric(tempFrame$no_rows)), dig=1))
  }
}
 
crimes_monthly_summary$Month <- as.factor(crimes_monthly_summary$Month)
 
  #Create a data frame aggregating total crimes commited in each calendar year

crimes_per_year <- crimes_total %>%
  filter(Year!=2017) %>%
  group_by(Year) %>%
  summarise(no_rows = length(Year))

  #Create a data frame aggregating total crimes committed by 
  #the district in which they were committed

crimes_by_district <- as.data.frame(matrix(ncol=6, nrow=25))

colnames(crimes_by_district) <- c("District", 
                                  "numCrime", 
                                  "X.Coor.Ag",
                                  "Y.Coor.Ag",
                                  "X.Avg",
                                  "Y.Avg")

crimes_by_district$District <- as.numeric(crimes_by_district$District)
crimes_by_district$numCrime <- as.numeric(crimes_by_district$numCrime)
crimes_by_district$X.Coor.Ag <- as.numeric(crimes_by_district$X.Coor.Ag)
crimes_by_district$Y.Coor.Ag <- as.numeric(crimes_by_district$Y.Coor.Ag)
crimes_by_district$X.Avg <- as.numeric(crimes_by_district$X.Avg)
crimes_by_district$Y.Avg <- as.numeric(crimes_by_district$Y.Avg)


for(i in 1:25){
  for(j in 1:6){
    tempFrame <- filter(crimes_total, District==i)
    
    switch(j,
           crimes_by_district[i, j] <- i,
           crimes_by_district[i, j] <- nrow(tempFrame),
           crimes_by_district[i, j] <- sum(as.numeric(tempFrame$X.Coordinate)),
           crimes_by_district[i, j] <- sum(as.numeric(tempFrame$Y.Coordinate)),
           crimes_by_district[i, j] <- sum(as.numeric(tempFrame$X.Coordinate))/nrow(tempFrame),
           crimes_by_district[i, j] <- sum(as.numeric(tempFrame$Y.Coordinate))/nrow(tempFrame))
  }
}

####################################################################################


#Create a subset of all non-violent crimes by including only non-domestic crimes and excluding
#those crimes which have an FBI Code that indicates a violent crime

nonV_crimes <- filter(crimes_total, Domestic=="false")
nonV_crimes <- filter(nonV_crimes, FBI.Code != "01A")
nonV_crimes <- filter(nonV_crimes, FBI.Code != "02")
nonV_crimes <- filter(nonV_crimes, FBI.Code != "03")
nonV_crimes <- filter(nonV_crimes, FBI.Code != "04A")
nonV_crimes <- filter(nonV_crimes, FBI.Code != "04B")

  #Aggregate all non-violent crimes by the date on which they were committed

nonV_crimes_per_day <- nonV_crimes %>%
  group_by(Date) %>%
  summarise(no_rows = length(Date))

  #Create a column to identify the season in which a non-violent crime was committed

nonV_crimes_per_day$Season <- sapply(strftime(nonV_crimes_per_day$Date, "%m"), switch,
                                "12" = "winter",
                                "01" = "winter",
                                "02" = "winter",
                                "03" = "spring",
                                "04" = "spring",
                                "05" = "spring",
                                "06" = "summer",
                                "07" = "summer",
                                "08" = "summer",
                                "09" = "autumn",
                                "10" = "autumn",
                                "11" = "autumn")

  # Modify the season column to identify holidays

nonV_crimes_per_day$Season <- if_else(strftime(nonV_crimes_per_day$Date, "%m")=="01" &
                                   strftime(nonV_crimes_per_day$Date, "%d")=="01",
                                 "XNYD", nonV_crimes_per_day$Season)

nonV_crimes_per_day$Season <- if_else(strftime(nonV_crimes_per_day$Date, "%m")=="07" &
                                   strftime(nonV_crimes_per_day$Date, "%d")=="04",
                                 "XIND", nonV_crimes_per_day$Season)

nonV_crimes_per_day$Season <- if_else(strftime(nonV_crimes_per_day$Date, "%m")=="12" &
                                   strftime(nonV_crimes_per_day$Date, "%d")=="25",
                                 "XXMS", nonV_crimes_per_day$Season)

nonV_crimes_per_day$Season <- as.factor(nonV_crimes_per_day$Season)

#Create a data frame aggregating total non-violent crimes committed in each calendar month

nonV_crimes_YearMo <- nonV_crimes %>%
  mutate(YearMo = format(Date, "%Y%m")) %>%
  group_by(YearMo) %>%
  summarise(no_rows = length(YearMo))

nonV_crimes_YearMo$YearMo <- as.factor(nonV_crimes_YearMo$YearMo)
nonV_crimes_YearMo$Year <- as.numeric(substr(nonV_crimes_YearMo$YearMo, 1, 4))
nonV_crimes_YearMo$Month <- as.numeric(substr(nonV_crimes_YearMo$YearMo, 5, 6))

nonV_crimes_monthly_summary <- as.data.frame(matrix(ncol=4, nrow=12))

colnames(nonV_crimes_monthly_summary) <- c("Month", "Total_nonV",
                                      "Mean_nonV", "SD_nonV")

nonV_crimes_monthly_summary$Month <- as.numeric(nonV_crimes_monthly_summary$Month)
nonV_crimes_monthly_summary$Total_nonV <- as.numeric(nonV_crimes_monthly_summary$Total_nonV)
nonV_crimes_monthly_summary$Mean_nonV <- as.numeric(nonV_crimes_monthly_summary$Mean_nonV)
nonV_crimes_monthly_summary$SD_nonV <- as.numeric(nonV_crimes_monthly_summary$SD_nonV)


for(i in 1:12){
  for(j in 1:4){
    tempFrame <- filter(nonV_crimes_YearMo, Month==as.character(i))
    
    switch(j,
           nonV_crimes_monthly_summary[i, j] <- i,
           nonV_crimes_monthly_summary[i, j] <- sum(as.numeric(tempFrame$no_rows)),
           nonV_crimes_monthly_summary[i, j] <- round(mean(as.numeric(tempFrame$no_rows)), dig=0),
           nonV_crimes_monthly_summary[i, j] <- round(sd(as.numeric(tempFrame$no_rows)), dig=1))
  }
}

nonV_crimes_monthly_summary$Month <- as.factor(nonV_crimes_monthly_summary$Month)

  #Create a data frame aggregating total non-violent crimes commited in each calendar year

nonV_crimes_per_year <- nonV_crimes %>%
  filter(Year!=2017) %>%
  group_by(Year) %>%
  summarise(no_rows = length(Year))

  #Create a data frame aggregating total non-violent crimes committed by 
  #the district in which they were committed

nonV_crimes_by_district <- as.data.frame(matrix(ncol=6, nrow=25))

colnames(nonV_crimes_by_district) <- c("District", 
                                 "numNonV", 
                                 "X.Coor.Ag",
                                 "Y.Coor.Ag",
                                 "X.Avg",
                                 "Y.Avg")

nonV_crimes_by_district$District <- as.numeric(nonV_crimes_by_district$District)
nonV_crimes_by_district$numNonV <- as.numeric(nonV_crimes_by_district$numNonV)
nonV_crimes_by_district$X.Coor.Ag <- as.numeric(nonV_crimes_by_district$X.Coor.Ag)
nonV_crimes_by_district$Y.Coor.Ag <- as.numeric(nonV_crimes_by_district$Y.Coor.Ag)
nonV_crimes_by_district$X.Avg <- as.numeric(nonV_crimes_by_district$X.Avg)
nonV_crimes_by_district$Y.Avg <- as.numeric(nonV_crimes_by_district$Y.Avg)


for(i in 1:25){
  for(j in 1:6){
    tempFrame <- filter(nonV_crimes, District==i)
    
    switch(j,
           nonV_crimes_by_district[i, j] <- i,
           nonV_crimes_by_district[i, j] <- nrow(tempFrame),
           nonV_crimes_by_district[i, j] <- sum(as.numeric(tempFrame$X.Coordinate)),
           nonV_crimes_by_district[i, j] <- sum(as.numeric(tempFrame$Y.Coordinate)),
           nonV_crimes_by_district[i, j] <- sum(as.numeric(tempFrame$X.Coordinate))/nrow(tempFrame),
           nonV_crimes_by_district[i, j] <- sum(as.numeric(tempFrame$Y.Coordinate))/nrow(tempFrame))
  }
}


####################################################################################


#Create a subset of all domestic crimes

dom_crimes <- crimes_total %>%
  filter(Domestic=="true")

  #Aggregate total domestic crimes by the date on which they were committed

dom_crimes_per_day <- dom_crimes %>%
  group_by(Date) %>%
  summarise(no_rows = length(Date))

  #Create a column to identify which season in which a crime was committed

dom_crimes_per_day$Season <- sapply(strftime(dom_crimes_per_day$Date, "%m"), switch,
                                     "12" = "winter",
                                     "01" = "winter",
                                     "02" = "winter",
                                     "03" = "spring",
                                     "04" = "spring",
                                     "05" = "spring",
                                     "06" = "summer",
                                     "07" = "summer",
                                     "08" = "summer",
                                     "09" = "autumn",
                                     "10" = "autumn",
                                     "11" = "autumn")

  #Modify the season column to identify New Year's Day, 4th of July, and Christmas, respectively

dom_crimes_per_day$Season <- if_else(strftime(dom_crimes_per_day$Date, "%m")=="01" &
                                        strftime(dom_crimes_per_day$Date, "%d")=="01",
                                      "XNYD", dom_crimes_per_day$Season)

dom_crimes_per_day$Season <- if_else(strftime(dom_crimes_per_day$Date, "%m")=="07" &
                                        strftime(dom_crimes_per_day$Date, "%d")=="04",
                                      "XIND", dom_crimes_per_day$Season)

dom_crimes_per_day$Season <- if_else(strftime(dom_crimes_per_day$Date, "%m")=="12" &
                                        strftime(dom_crimes_per_day$Date, "%d")=="25",
                                      "XXMS", dom_crimes_per_day$Season)

dom_crimes_per_day$Season <- as.factor(dom_crimes_per_day$Season)

  #Create a data frame aggregating total domestic crimes committed in each calendar month

dom_crimes_YearMo <- dom_crimes %>%
  mutate(YearMo = format(Date, "%Y%m")) %>%
  group_by(YearMo) %>%
  summarise(no_rows = length(YearMo))

dom_crimes_YearMo$YearMo <- as.factor(dom_crimes_YearMo$YearMo)
dom_crimes_YearMo$Year <- as.numeric(substr(dom_crimes_YearMo$YearMo, 1, 4))
dom_crimes_YearMo$Month <- as.numeric(substr(dom_crimes_YearMo$YearMo, 5, 6))

dom_crimes_monthly_summary <- as.data.frame(matrix(ncol=4, nrow=12))

colnames(dom_crimes_monthly_summary) <- c("Month", "Total_Domestic",
                                           "Mean_Domestic", "SD_Domestic")

dom_crimes_monthly_summary$Month <- as.numeric(dom_crimes_monthly_summary$Month)
dom_crimes_monthly_summary$Total_Domestic <- as.numeric(dom_crimes_monthly_summary$Total_Domestic)
dom_crimes_monthly_summary$Mean_Domestic <- as.numeric(dom_crimes_monthly_summary$Mean_Domestic)
dom_crimes_monthly_summary$SD_Domestic <- as.numeric(dom_crimes_monthly_summary$SD_Domestic)


for(i in 1:12){
  for(j in 1:4){
    tempFrame <- filter(dom_crimes_YearMo, Month==as.character(i))
    
    switch(j,
           dom_crimes_monthly_summary[i, j] <- i,
           dom_crimes_monthly_summary[i, j] <- sum(as.numeric(tempFrame$no_rows)),
           dom_crimes_monthly_summary[i, j] <- round(mean(as.numeric(tempFrame$no_rows)), dig=0),
           dom_crimes_monthly_summary[i, j] <- round(sd(as.numeric(tempFrame$no_rows)), dig=1))
  }
}

dom_crimes_monthly_summary$Month <- as.factor(dom_crimes_monthly_summary$Month)

  #Create a data frame aggregating total domestic crimes commited in each calendar year

dom_crimes_per_year <- dom_crimes %>%
  filter(Year!=2017) %>%
  group_by(Year) %>%
  summarise(no_rows = length(Year))
  
  #Create a data frame aggregating total domestic crimes committed by 
  #the district in which they were committed

dom_crimes_by_district <- as.data.frame(matrix(ncol=6, nrow=25))

colnames(dom_crimes_by_district) <- c("District", 
                                       "numDomestic", 
                                       "X.Coor.Ag",
                                       "Y.Coor.Ag",
                                       "X.Avg",
                                       "Y.Avg")

dom_crimes_by_district$District <- as.numeric(dom_crimes_by_district$District)
dom_crimes_by_district$numDomestic <- as.numeric(dom_crimes_by_district$numDomestic)
dom_crimes_by_district$X.Coor.Ag <- as.numeric(dom_crimes_by_district$X.Coor.Ag)
dom_crimes_by_district$Y.Coor.Ag <- as.numeric(dom_crimes_by_district$Y.Coor.Ag)
dom_crimes_by_district$X.Avg <- as.numeric(dom_crimes_by_district$X.Avg)
dom_crimes_by_district$Y.Avg <- as.numeric(dom_crimes_by_district$Y.Avg)


for(i in 1:25){
  for(j in 1:6){
    tempFrame <- filter(dom_crimes, District==i)
    
    switch(j,
           dom_crimes_by_district[i, j] <- i,
           dom_crimes_by_district[i, j] <- nrow(tempFrame),
           dom_crimes_by_district[i, j] <- sum(as.numeric(tempFrame$X.Coordinate)),
           dom_crimes_by_district[i, j] <- sum(as.numeric(tempFrame$Y.Coordinate)),
           dom_crimes_by_district[i, j] <- sum(as.numeric(tempFrame$X.Coordinate))/nrow(tempFrame),
           dom_crimes_by_district[i, j] <- sum(as.numeric(tempFrame$Y.Coordinate))/nrow(tempFrame))
  }
}



####################################################################################


#Create a subset of all violent crimes, not including murder, as identified by their FBI Code

v_crimes <- crimes_total %>%
  filter(FBI.Code == "02" |
           FBI.Code == "03" |
           FBI.Code == "04A"|
           FBI.Code == "04B")

  #Aggregate violent crimes by the date on which they were committed

v_crimes_per_day <- v_crimes %>%
  group_by(Date) %>%
  summarise(no_rows = length(Date))

  #Create a column which identifies which season in which a crime was committed

v_crimes_per_day$Season <- sapply(strftime(v_crimes_per_day$Date, "%m"), switch,
                                    "12" = "winter",
                                    "01" = "winter",
                                    "02" = "winter",
                                    "03" = "spring",
                                    "04" = "spring",
                                    "05" = "spring",
                                    "06" = "summer",
                                    "07" = "summer",
                                    "08" = "summer",
                                    "09" = "autumn",
                                    "10" = "autumn",
                                    "11" = "autumn")

  # Modify the season column to identify New Year's Day, 4th of July, and Christmas, respectively

v_crimes_per_day$Season <- if_else(strftime(v_crimes_per_day$Date, "%m")=="01" &
                                       strftime(v_crimes_per_day$Date, "%d")=="01",
                                     "XNYD", v_crimes_per_day$Season)

v_crimes_per_day$Season <- if_else(strftime(v_crimes_per_day$Date, "%m")=="07" &
                                       strftime(v_crimes_per_day$Date, "%d")=="04",
                                     "XIND", v_crimes_per_day$Season)

v_crimes_per_day$Season <- if_else(strftime(v_crimes_per_day$Date, "%m")=="12" &
                                       strftime(v_crimes_per_day$Date, "%d")=="25",
                                     "XXMS", v_crimes_per_day$Season)

v_crimes_per_day$Season <- as.factor(dom_crimes_per_day$Season)

  #Create a data frame aggregating total violent crimes committed in each calendar month

v_crimes_YearMo <- v_crimes %>%
  mutate(YearMo = format(Date, "%Y%m")) %>%
  group_by(YearMo) %>%
  summarise(no_rows = length(YearMo))

v_crimes_YearMo$YearMo <- as.factor(v_crimes_YearMo$YearMo)
v_crimes_YearMo$Year <- as.numeric(substr(v_crimes_YearMo$YearMo, 1, 4))
v_crimes_YearMo$Month <- as.numeric(substr(v_crimes_YearMo$YearMo, 5, 6))

v_crimes_monthly_summary <- as.data.frame(matrix(ncol=4, nrow=12))

colnames(v_crimes_monthly_summary) <- c("Month", "Total_Violent",
                                          "Mean_Violent", "SD_Violent")

v_crimes_monthly_summary$Month <- as.numeric(v_crimes_monthly_summary$Month)
v_crimes_monthly_summary$Total_Violent <- as.numeric(v_crimes_monthly_summary$Total_Violent)
v_crimes_monthly_summary$Mean_Violent <- as.numeric(v_crimes_monthly_summary$Mean_Violent)
v_crimes_monthly_summary$SD_Violent <- as.numeric(v_crimes_monthly_summary$SD_Violent)


for(i in 1:12){
  for(j in 1:4){
    tempFrame <- filter(v_crimes_YearMo, Month==as.character(i))
    
    switch(j,
           v_crimes_monthly_summary[i, j] <- i,
           v_crimes_monthly_summary[i, j] <- sum(as.numeric(tempFrame$no_rows)),
           v_crimes_monthly_summary[i, j] <- round(mean(as.numeric(tempFrame$no_rows)), dig=0),
           v_crimes_monthly_summary[i, j] <- round(sd(as.numeric(tempFrame$no_rows)), dig=1))
  }
}

v_crimes_monthly_summary$Month <- as.factor(v_crimes_monthly_summary$Month)

  #Create a data frame aggregating total violent crimes commited in each calendar year

v_crimes_per_year <- v_crimes %>%
  filter(Year!=2017) %>%
  group_by(Year) %>%
  summarise(no_rows = length(Year))

  #Create a data frame aggregating total non-violent crimes committed by 
  #the district in which they were committed

v_crimes_by_district <- as.data.frame(matrix(ncol=6, nrow=25))

colnames(v_crimes_by_district) <- c("District", 
                                       "numViolent", 
                                       "X.Coor.Ag",
                                       "Y.Coor.Ag",
                                       "X.Avg",
                                       "Y.Avg")

v_crimes_by_district$District <- as.numeric(v_crimes_by_district$District)
v_crimes_by_district$numViolent <- as.numeric(v_crimes_by_district$numViolent)
v_crimes_by_district$X.Coor.Ag <- as.numeric(v_crimes_by_district$X.Coor.Ag)
v_crimes_by_district$Y.Coor.Ag <- as.numeric(v_crimes_by_district$Y.Coor.Ag)
v_crimes_by_district$X.Avg <- as.numeric(v_crimes_by_district$X.Avg)
v_crimes_by_district$Y.Avg <- as.numeric(v_crimes_by_district$Y.Avg)


for(i in 1:25){
  for(j in 1:6){
    tempFrame <- filter(v_crimes, District==i)
    
    switch(j,
           v_crimes_by_district[i, j] <- i,
           v_crimes_by_district[i, j] <- nrow(tempFrame),
           v_crimes_by_district[i, j] <- sum(as.numeric(tempFrame$X.Coordinate)),
           v_crimes_by_district[i, j] <- sum(as.numeric(tempFrame$Y.Coordinate)),
           v_crimes_by_district[i, j] <- sum(as.numeric(tempFrame$X.Coordinate))/nrow(tempFrame),
           v_crimes_by_district[i, j] <- sum(as.numeric(tempFrame$Y.Coordinate))/nrow(tempFrame))
  }
}

####################################################################################


#Create a subset of all murders

murder <- crimes_total %>%
  filter(FBI.Code == "01A")

  #Create a data frame aggregating total murders committed in each calendar month

#Create a data frame aggregating total violent crimes committed in each calendar month

murder_YearMo <- murder %>%
  mutate(YearMo = format(Date, "%Y%m")) %>%
  group_by(YearMo) %>%
  summarise(no_rows = length(YearMo))

murder_YearMo$YearMo <- as.factor(murder_YearMo$YearMo)
murder_YearMo$Year <- as.numeric(substr(murder_YearMo$YearMo, 1, 4))
murder_YearMo$Month <- as.numeric(substr(murder_YearMo$YearMo, 5, 6))

murder_monthly_summary <- as.data.frame(matrix(ncol=4, nrow=12))

colnames(murder_monthly_summary) <- c("Month", "Total_Murder",
                                        "Mean_Murder", "SD_Murder")

murder_monthly_summary$Month <- as.numeric(murder_monthly_summary$Month)
murder_monthly_summary$Total_Murder <- as.numeric(murder_monthly_summary$Total_Murder)
murder_monthly_summary$Mean_Murder <- as.numeric(murder_monthly_summary$Mean_Murder)
murder_monthly_summary$SD_Murder <- as.numeric(murder_monthly_summary$SD_Murder)


for(i in 1:12){
  for(j in 1:4){
    tempFrame <- filter(murder_YearMo, Month==as.character(i))
    
    switch(j,
           murder_monthly_summary[i, j] <- i,
           murder_monthly_summary[i, j] <- sum(as.numeric(tempFrame$no_rows)),
           murder_monthly_summary[i, j] <- round(mean(as.numeric(tempFrame$no_rows)),dig=0),
           murder_monthly_summary[i, j] <- round(sd(as.numeric(tempFrame$no_rows)), dig=1))
  }
}

murder_monthly_summary$Month <- as.factor(murder_monthly_summary$Month)

  #Create a data frame aggregating total murders commited in each calendar year

murder_per_year <- murder %>%
  filter(Year!=2017) %>%
  group_by(Year) %>%
  summarise(no_rows = length(Year))

  #Create a data frame aggregating total murders committed by district in which they were committed

murder_by_district <- as.data.frame(matrix(ncol=6, nrow=25))

colnames(murder_by_district) <- c("District", 
                                  "numMurder", 
                                  "X.Coor.Ag",
                                  "Y.Coor.Ag",
                                  "X.Avg",
                                  "Y.Avg")

murder_by_district$District <- as.numeric(murder_by_district$District)
murder_by_district$numMurder <- as.numeric(murder_by_district$numMurder)
murder_by_district$X.Coor.Ag <- as.numeric(murder_by_district$X.Coor.Ag)
murder_by_district$Y.Coor.Ag <- as.numeric(murder_by_district$Y.Coor.Ag)
murder_by_district$X.Avg <- as.numeric(murder_by_district$X.Avg)
murder_by_district$Y.Avg <- as.numeric(murder_by_district$Y.Avg)


for(i in 1:25){
  for(j in 1:6){
    tempFrame <- filter(murder, District==i)
    
    switch(j,
           murder_by_district[i, j] <- i,
           murder_by_district[i, j] <- nrow(tempFrame),
           murder_by_district[i, j] <- sum(as.numeric(tempFrame$X.Coordinate)),
           murder_by_district[i, j] <- sum(as.numeric(tempFrame$Y.Coordinate)),
           murder_by_district[i, j] <- sum(as.numeric(tempFrame$X.Coordinate))/nrow(tempFrame),
           murder_by_district[i, j] <- sum(as.numeric(tempFrame$Y.Coordinate))/nrow(tempFrame))
  }
}