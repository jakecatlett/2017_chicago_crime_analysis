library(ggplot2)
library(ggmap)
library(mapproj)
library(maptools)
library(DT)

chicago <- readShapePoly("PoliceDistrict")
chicago <- fortify(chicago)

## Initial plot to show overall crime trend and seasonality

plot(y=crimes_per_day$no_rows,
     x=crimes_per_day$Date,
     main = "Total Crimes per Day, 2001-2017",
     xlab = "Year",
     ylab = "Total Number of Crimes Reported",
     pch = 20,
     cex = .75,
     col = "blue")

  #Plots to show means and sd for overall crimes

CrimeMean_Plot <- ggplot(data=crimes_monthly_summary, aes(x=Month, y=Mean_Crime)) +
  geom_bar(stat="identity", aes(fill=crimes_monthly_summary$SD_Crime)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title="Mean Number of All Crimes per Month",
       x="Month", y="Number of Crimes Reported")

CrimeSD_Plot <- ggplot(data=crimes_monthly_summary, aes(x=Month, y=SD_Crime)) +
  geom_bar(stat="identity", color="black", fill="lightgreen") +
  theme_minimal() +
  labs(title="Standard Deviation All Crimes per Month",
       x="Month", y="Number of Crimes Reported")

  #Create a map plot which shows relative frequency of crime by district in Chicago

CrimesDistrict_Plot<- ggplot(chicago) +
  geom_path(aes(x=long, y=lat, group=group)) +
  geom_point(data=crimes_by_district, aes(x=X.Avg,
                                          y=Y.Avg,
                                          colour=numCrime,
                                          size=numCrime)) +
  labs(title="Total Crimes by Police District") +
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank())

# order crimes_per_day alphabetically on season column
# this is done so that when points are plotted the holidays are plotted last, and therefore
# stand out on the chart instead of getting plotted over by other points

crimes_per_day <- arrange(crimes_per_day, Season)

# crimes_per_day plotted, highlighting seasonality and three key holidays

plot(y=crimes_per_day$no_rows,
     x=crimes_per_day$Date,
     main = "Total Crimes per Day, 2001-2017",
     xlab = "Date",
     ylab = "Total Number of Crimes Reported",
     pch = if_else(crimes_per_day$Season=="XNYD", 3, 
                   if_else(crimes_per_day$Season=="XIND", 8, 
                           if_else(crimes_per_day$Season=="XXMS", 17,
                                   20))),
     cex = if_else(crimes_per_day$Season=="XNYD" | 
                     crimes_per_day$Season=="XIND" |
                     crimes_per_day$Season=="XXMS", 2,
                   .75),
     col = if_else(crimes_per_day$Season=="XNYD", "red",
                   if_else(crimes_per_day$Season=="XIND", "yellow",
                           if_else(crimes_per_day$Season=="XXMS", "green",
                                   if_else(crimes_per_day$Season=="winter", "red",
                                           if_else(crimes_per_day$Season=="summer", "blue",
                                                   "black"))))))

legend("topright", 
       legend=c("New Year's Day", "4th of July", "Christmas", "Winter", "Summer", "Spring & Autumn"),
       pch=c(3, 8, 17, 20, 20, 20),
       cex=c(.585),
       col=c("red", "yellow", "green", "red", "blue", "black"),
       bg="gray90")


############################################################################################

nonV_Mean_Plot <- ggplot(data=nonV_crimes_monthly_summary, aes(x=Month, y=Mean_nonV)) +
  geom_bar(stat="identity", aes(fill=nonV_crimes_monthly_summary$SD_nonV)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title="Mean Number of All Non-Violent Crimes per Month",
       x="Month", y="Number of Non-Violent Crimes Reported")

nonV_SD_Plot <- ggplot(data=nonV_crimes_monthly_summary, aes(x=Month, y=SD_nonV)) +
  geom_bar(stat="identity", color="black", fill="lightgreen") +
  theme_minimal() +
  labs(title="Standard Deviation All Crimes per Month",
       x="Month", y="Number of Non-Violent Crimes Reported")

nonV_District_Plot<- ggplot(chicago) +
  geom_path(aes(x=long, y=lat, group=group)) +
  geom_point(data=nonV_crimes_by_district, aes(x=X.Avg,
                                          y=Y.Avg,
                                          colour=numNonV,
                                          size=numNonV)) +
  labs(title="Non-Violent Crimes by Police District") +
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank())

# order nonV_crimes_per_day alphabetically on Season column
# this is done so that when points are plotted the holidays are plotted last, and therefore
# stand out on the chart instead of getting plotted over by other points

nonV_crimes_per_day <- arrange(nonV_crimes_per_day, Season)

plot(y=nonV_crimes_per_day$no_rows,
     x=nonV_crimes_per_day$Date,
     main = "Total Non-Violent Crimes per Day, 2001-2017",
     xlab = "Date",
     ylab = "Number of Non-Violent Crimes Reported",
     pch = if_else(nonV_crimes_per_day$Season=="XNYD", 3, 
                   if_else(nonV_crimes_per_day$Season=="XIND", 8, 
                           if_else(nonV_crimes_per_day$Season=="XXMS", 17,
                                   20))),
     cex = if_else(nonV_crimes_per_day$Season=="XNYD" | 
                     nonV_crimes_per_day$Season=="XIND" |
                     nonV_crimes_per_day$Season=="XXMS", 1,
                   .75),
     col = if_else(nonV_crimes_per_day$Season=="XNYD", "red",
                   if_else(nonV_crimes_per_day$Season=="XIND", "yellow",
                           if_else(nonV_crimes_per_day$Season=="XXMS", "green",
                                   if_else(nonV_crimes_per_day$Season=="winter", "red",
                                           if_else(nonV_crimes_per_day$Season=="summer", "blue",
                                                   "black"))))))

legend("topright", 
       legend=c("New Year's Day", "4th of July", "Christmas", "Winter", "Summer", "Spring & Autumn"),
       pch=c(3, 8, 17, 20, 20, 20),
       cex=c(.585),
       col=c("red", "yellow", "green", "red", "blue", "black"),
       bg="gray90")


#####################################################################


DomesticMean_Plot <- ggplot(data=dom_crimes_monthly_summary, aes(x=Month, y=Mean_Domestic)) +
  geom_bar(stat="identity", aes(fill=dom_crimes_monthly_summary$SD_Domestic)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title="Mean Number of All Domestic Crimes per Month",
       x="Month", y="Number of Domestic Crimes Reported")

DomesticSD_Plot <- ggplot(data=dom_crimes_monthly_summary, aes(x=Month, y=SD_Domestic)) +
  geom_bar(stat="identity", color="black", fill="lightgreen") +
  theme_minimal() +
  labs(title="Standard Deviation All Domestic Crimes per Month",
       x="Month", y="Number of Domestic Crimes Reported")

DomesticDistrict_Plot<- ggplot(chicago) +
  geom_path(aes(x=long, y=lat, group=group)) +
  geom_point(data=dom_crimes_by_district, aes(x=X.Avg,
                                          y=Y.Avg,
                                          colour=numDomestic,
                                          size=numDomestic)) +
  labs(title="Domestic Crimes by Police District") +
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank())


# order dom_crimes_per_day alphabetically on Season column
# this is done so that when points are plotted the holidays are plotted last, and therefore
# stand out on the chart instead of getting plotted over by other points

dom_crimes_per_day <- arrange(dom_crimes_per_day, Season)

# dom_crimes_per_day plotted, highlighting seasonality and three key holidays

plot(y=dom_crimes_per_day$no_rows,
     x=dom_crimes_per_day$Date,
     main = "Total Domestic Crimes per Day, 2001-2017",
     xlab = "Date",
     ylab = "Number of Domestic Crimes Reported",
     pch = if_else(dom_crimes_per_day$Season=="XNYD", 3, 
                   if_else(dom_crimes_per_day$Season=="XIND", 8, 
                           if_else(dom_crimes_per_day$Season=="XXMS", 17,
                                   20))),
     cex = if_else(dom_crimes_per_day$Season=="XNYD" | 
                     dom_crimes_per_day$Season=="XIND" |
                     dom_crimes_per_day$Season=="XXMS", 1,
                   .75),
     col = if_else(dom_crimes_per_day$Season=="XNYD", "red",
                   if_else(dom_crimes_per_day$Season=="XIND", "yellow",
                           if_else(dom_crimes_per_day$Season=="XXMS", "green",
                                   if_else(dom_crimes_per_day$Season=="winter", "red",
                                           if_else(dom_crimes_per_day$Season=="summer", "blue",
                                                   "black"))))))

legend("topright", 
       legend=c("New Year's Day", "4th of July", "Christmas", "Winter", "Summer", "Spring & Autumn"),
       pch=c(3, 8, 17, 20, 20, 20),
       cex=c(.585),
       col=c("red", "yellow", "green", "red", "blue", "black"),
       bg="gray90")


##################################################################################

ViolentMean_Plot <- ggplot(data=v_crimes_monthly_summary, aes(x=Month, y=Mean_Violent)) +
  geom_bar(stat="identity", aes(fill=v_crimes_monthly_summary$SD_Violent)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title="Mean Number of All Violent Crimes per Month",
       x="Month", y="Number of Violent Crimes Reported")

ViolentSD_Plot <- ggplot(data=v_crimes_monthly_summary, aes(x=Month, y=SD_Violent)) +
  geom_bar(stat="identity", color="black", fill="lightgreen") +
  theme_minimal() +
  labs(title="Standard Deviation All Violent Crimes per Month",
       x="Month", y="Number of Violent Crimes Reported")

ViolentDistrict_Plot<- ggplot(chicago) +
  geom_path(aes(x=long, y=lat, group=group)) +
  geom_point(data=v_crimes_by_district, aes(x=X.Avg,
                                              y=Y.Avg,
                                              colour=numViolent,
                                              size=numViolent)) +
  labs(title="Violent Crimes by Police District") +
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank())


# order v_crimes_per_day alphabetically on season column
# this is done so that when points are plotted the holidays are plotted last, and therefore
# stand out on the chart instead of getting plotted over by other points

v_crimes_per_day <- arrange(v_crimes_per_day, Season)

# v_crimes_per_day plotted, highlighting seasonality and three key holidays

plot(y=v_crimes_per_day$no_rows,
     x=v_crimes_per_day$Date,
     main = "Total Violent Crimes per Day, 2001-2017",
     xlab = "Date",
     ylab = "Number of Violent Crimes Reported",
     pch = if_else(v_crimes_per_day$Season=="XNYD", 3, 
                   if_else(v_crimes_per_day$Season=="XIND", 8, 
                           if_else(v_crimes_per_day$Season=="XXMS", 17,
                                   20))),
     cex = if_else(v_crimes_per_day$Season=="XNYD" | 
                     v_crimes_per_day$Season=="XIND" |
                     v_crimes_per_day$Season=="XXMS", 1,
                   .75),
     col = if_else(v_crimes_per_day$Season=="XNYD", "red",
                   if_else(v_crimes_per_day$Season=="XIND", "yellow",
                           if_else(v_crimes_per_day$Season=="XXMS", "green",
                                   if_else(v_crimes_per_day$Season=="winter", "red",
                                           if_else(v_crimes_per_day$Season=="summer", "blue",
                                                   "black"))))))

legend("topright", 
       legend=c("New Year's Day", "4th of July", "Christmas", "Winter", "Summer", "Spring & Autumn"),
       pch=c(3, 8, 17, 20, 20, 20),
       cex=c(.585),
       col=c("red", "yellow", "green", "red", "blue", "black"),
       bg="gray90")


###########################################################################


MurderDistrict_Plot <- ggplot(chicago) +
  geom_path(aes(x=long, y=lat, group=group)) +
  geom_point(data=murder_by_district, aes(x=X.Avg,
                                          y=Y.Avg,
                                          colour=numMurder,
                                          size=numMurder)) +
  labs(title="Murders by Police District") +
  theme(
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank())

MurderPlot <- ggplot(murder_YearMo, aes(x=YearMo, y=no_rows, colour="red", show.legend=FALSE)) +
  geom_point() + 
  ggtitle("Murders by Month from 2001-2017") +
  labs(y="Number of Murders Reported", x="2001-20017") +
  theme(
    axis.text.x=element_blank(),
    axis.ticks=element_blank(),
    legend.position = "none")

MurderMean_Plot <- ggplot(data=murder_monthly_summary, aes(x=Month, y=Mean_Murder)) +
  geom_bar(stat="identity", aes(fill=murder_monthly_summary$SD_Murder)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title="Mean Number of All Murders per Month",
       x="Month", y="Number of Murders Reported")

MurderSD_Plot <- ggplot(data=murder_monthly_summary, aes(x=Month, y=SD_Murder)) +
  geom_bar(stat="identity", color="black", fill="lightgreen") +
  theme_minimal() +
  labs(title="Standard Deviation All Murders per Month",
       x="Month", y="Number of Murders Reported")
                         