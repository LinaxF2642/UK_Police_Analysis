
install.packages("tidyverse")
install.packages("rgdal")
install.packages("ggpubr")
install.packages("plotly")
install.packages("writexl")
install.packages("tmap")
install.packages("GGally")

library(GGally)
library(tmap)
library(rgdal)
library(writexl)
library(tidyverse)
library(ggpubr)
library(rgdal)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(viridis)
library(tidyr)
library(cluster)
library(ggmap)
library(maps)
library(plotly)
library(corrplot)
library(corrgram)

###  Load the crime datasets

dec_18 <- read.csv("2018-12-greater-manchester-street.csv")
nov_18 <- read.csv("2018-11-greater-manchester-street.csv")
oct_18 <- read.csv("2018-10-greater-manchester-street.csv")
sep_18 <- read.csv("2018-09-greater-manchester-street.csv")
aug_18 <- read.csv("2018-08-greater-manchester-street.csv")
jul_18 <- read.csv("2018-07-greater-manchester-street.csv")
jun_18 <- read.csv("2018-06-greater-manchester-street.csv")
may_18 <- read.csv("2018-05-greater-manchester-street.csv")
apr_18 <- read.csv("2018-04-greater-manchester-street.csv")
mar_18 <- read.csv("2018-03-greater-manchester-street.csv")
feb_18 <- read.csv("2018-02-greater-manchester-street.csv")
jan_18 <- read.csv("2018-01-greater-manchester-street.csv")

dec_17 <- read.csv("2017-12-greater-manchester-street.csv")
nov_17 <- read.csv("2017-11-greater-manchester-street.csv")
oct_17 <- read.csv("2017-10-greater-manchester-street.csv")
sep_17 <- read.csv("2017-09-greater-manchester-street.csv")
aug_17 <- read.csv("2017-08-greater-manchester-street.csv")
jul_17 <- read.csv("2017-07-greater-manchester-street.csv")
jun_17 <- read.csv("2017-06-greater-manchester-street.csv")
may_17 <- read.csv("2017-05-greater-manchester-street.csv")
apr_17 <- read.csv("2017-04-greater-manchester-street.csv")
mar_17 <- read.csv("2017-03-greater-manchester-street.csv")
feb_17 <- read.csv("2017-02-greater-manchester-street.csv")
jan_17 <- read.csv("2017-01-greater-manchester-street.csv")

dec_16 <- read.csv("2016-12-greater-manchester-street.csv")
nov_16 <- read.csv("2016-11-greater-manchester-street.csv")
oct_16 <- read.csv("2016-10-greater-manchester-street.csv")
sep_16 <- read.csv("2016-09-greater-manchester-street.csv")
aug_16 <- read.csv("2016-08-greater-manchester-street.csv")
jul_16 <- read.csv("2016-07-greater-manchester-street.csv")
jun_16 <- read.csv("2016-06-greater-manchester-street.csv")
may_16 <- read.csv("2016-05-greater-manchester-street.csv")
apr_16 <- read.csv("2016-04-greater-manchester-street.csv")
mar_16 <- read.csv("2016-03-greater-manchester-street.csv")
feb_16 <- read.csv("2016-02-greater-manchester-street.csv")
jan_16 <- read.csv("2016-01-greater-manchester-street.csv")

dec_15 <- read.csv("2015-12-greater-manchester-street.csv")
nov_15 <- read.csv("2015-11-greater-manchester-street.csv")
oct_15 <- read.csv("2015-10-greater-manchester-street.csv")
sep_15 <- read.csv("2015-09-greater-manchester-street.csv")
aug_15 <- read.csv("2015-08-greater-manchester-street.csv")
jul_15 <- read.csv("2015-07-greater-manchester-street.csv")
jun_15 <- read.csv("2015-06-greater-manchester-street.csv")
may_15 <- read.csv("2015-05-greater-manchester-street.csv")
apr_15 <- read.csv("2015-04-greater-manchester-street.csv")
mar_15 <- read.csv("2015-03-greater-manchester-street.csv")
feb_15 <- read.csv("2015-02-greater-manchester-street.csv")
jan_15 <- read.csv("2015-01-greater-manchester-street.csv")

m_crime <- bind_rows(may_17, jun_17, jul_17, aug_17, sep_17, oct_17, nov_17, dec_17, apr_17, mar_17, feb_17, jan_17,
dec_18, nov_18, oct_18, sep_18, aug_18, jul_18, jun_18, may_18, apr_18, mar_18, feb_18, jan_18,
dec_16, nov_16, oct_16, sep_16, aug_16, jul_16, jun_16, may_16, apr_16, mar_16, feb_16, jan_16,
dec_15, nov_15, oct_15, sep_15, aug_15, jul_15, jun_15, may_15, apr_15, mar_15, feb_15, jan_15)

m_crime <- select(m_crime, Month, Crime.type, Longitude, Latitude, Location, LSOA.code, LSOA.name)

### devide month into year and month
M_crime <- m_crime %>% separate(Month, into = c("year", "month"))
names(M_crime)[names(M_crime) == "Crime.type"] <- "crime_type"

### Crime Over Time (2015-2018) data 
M_crime_year <- M_crime %>% select(year, crime_type) %>% group_by(year) %>% summarise(num.crimes=n())
M_crime_year <- mutate(M_crime_year, new_col = M_crime_year$num.crimes/1000)

###  Data Visualisation
###  Crime Over Time (2015-2018)
ggplot(M_crime_year, aes(x = year, y = new_col, group = 1, label = new_col))+
geom_line(color = "blue", size = 2) + ylim(300, 450)+
geom_point(aes(y = new_col), shape = 21, color = "black", fill = "#FF8000", size = 4) +
labs(x = "Year", y = "Number of Crimes(thousands)", title = "Manchester Crime Rate from 2015 to 2018",
caption = "Data source:Police recorded crime, Home Office.") + theme(axis.text.x = element_text(hjust = 1)) + theme_bw() 

### Factor by Crime Categories data
y_crime_group <- M_crime %>% select(year, crime_type) %>% group_by(crime_type, year) %>% summarise(num.crimes=n())

###  Data Visualisation
### Factor by Crime Categories
ggplot(y_crime_group, aes( x = crime_type, y = num.crimes, fill = crime_type)) + 
geom_bar(position = "dodge", stat = "identity") + theme_bw() + scale_fill_viridis(discrete = T) +
ggtitle("") + coord_flip() + labs(fill = "", y = "Number of Crimes", x = "", title = "Comparison of Crime Types in Manchester",
caption = "Data source:Police recorded crime, Home Office.") + facet_wrap(~year)

### Crime Category Over Time data
y_crime_indicator <- M_crime %>% group_by(year, crime_type) %>% summarise(num.crimes=n())
y_crime_indicator <- y_crime_indicator %>% filter(crime_type %in% c("Anti-social behaviour",
"Violence and sexual offences", "Criminal damage and arson", "Burglary", "Other theft", "Shoplifting", "Vehicle crime",
"Public order" ))

### Data Visualisation
### Crime Category Over Time
ggplot(data = y_crime_indicator, aes(x = year, y = num.crimes, color = crime_type, group = crime_type)) + geom_line(size = 1) + 
theme_bw() + labs(x = 'Year', y = 'Number of Crimes',title = "Major Crime Indicators in Manchester Between 2015 and 2018",
caption = "Data source:Police recorded crime, Home Office.") + theme(plot.title = element_text(size = 16) ,
axis.title = element_text(size = 12, face = "bold"))

### Factor by Location data
location_group <- group_by(M_crime, Location)
crime_by_location <- summarise(location_group, num.crimes=n())
crime_by_location <- crime_by_location[order(crime_by_location$num.crimes, decreasing = TRUE), ]
crime_by_location_top10 <- head(crime_by_location, 10)

### Data Visualisation
### the most crimes in cetain locations 
ggplot(data = crime_by_location_top10, aes(x = Location,y = num.crimes), y = num.crimes) +
geom_bar(fill="#f68060",stat = 'identity', width = 0.6) + geom_text(aes(label = num.crimes), stat = 'identity', 
data = crime_by_location_top10, hjust = -0.1, size = 3) + coord_flip() + xlab('Neighbourhoods') + ylab('Number of Crimes') +
ggtitle('Neighbourhoods with Most Crimes - Top 10 (2015-2018)') + theme_bw() + theme(plot.title = element_text(size = 16),
axis.title = element_text(size = 12, face = "bold"))

### The safest location
tail(crime_by_location, 5)

### Location with Major Crime Indicator data
offence_location_group <- group_by(M_crime, Location, crime_type)
offence_type_by_location <- summarise(offence_location_group, num.crimes = n())
offence_type_by_location <- offence_type_by_location[order(offence_type_by_location$num.crimes, decreasing = TRUE), ]
offence_type_by_location_top30 <- head(offence_type_by_location, 30)

### Data Visualisation
### Location with Major Crime Indicator
ggplot(data= offence_type_by_location_top30, aes(x = Location, y = num.crimes, fill = crime_type)) +
geom_bar(stat = 'identity', position = position_dodge(), width = 0.8) + xlab('Neighbourhoods') + ylab('Number of Crimes') +
ggtitle('Major Crime Type vs. Neighbourhood in Manchester') + theme_bw() +
labs(fill = "") + theme(plot.title = element_text(size = 16), axis.title = element_text(size = 12, face = "bold"),
axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

### Crime Category in 2015 Heatmap data
crime_count_2015 <- M_crime %>% group_by(month, crime_type) %>% filter(crime_type == "Anti-social behaviour" | 
crime_type == "Violence and sexual offences"| crime_type == "Criminal damage and arson" |
crime_type == "Burglary" | crime_type == "Other theft" | crime_type == "Shoplifting"| crime_type == "Vehicle crime"|
crime_type == "Public order", year == 2015) %>% summarise(num.crimes = n())
crime_count_2015$month <- ordered(crime_count_2015$month)

### Data Visualisation
### Crime Category in 2015 Heatmap 
plot_2015<- ggplot(crime_count_2015, aes(month, crime_type, fill = num.crimes)) +
geom_tile(size = 1, color = "white") + scale_fill_viridis()  + geom_text(aes(label = num.crimes), color = 'white') +
labs(x = "", y = "", fill = "Number of Crimes", title="Major Crime Indicators by Month (2015)") +
theme(plot.title = element_text(size = 16), axis.title = element_text(size = 12, face = "bold"))+ 
theme(legend.position = "none")

### Crime Category in 2016 Heatmap data
crime_count_2016 <- M_crime %>% group_by(month, crime_type) %>% filter(crime_type == "Anti-social behaviour" | 
crime_type == "Violence and sexual offences" | crime_type == "Criminal damage and arson" |
crime_type == "Burglary" | crime_type == "Other theft" | crime_type == "Shoplifting" | crime_type == "Vehicle crime"|
crime_type == "Public order", year == 2016) %>% summarise(num.crimes = n())
crime_count_2016$month <- ordered(crime_count_2016$month)

### Data Visualisation
### Crime Category in 2016 Heatmap
plot_2016 <- ggplot(crime_count_2016, aes(month, crime_type, fill = num.crimes)) +
geom_tile(size = 1, color = "white") + scale_fill_viridis()  + geom_text(aes(label = num.crimes), color = 'white') +
labs(x = "Month",y = "",fill = "Number of Crimes", title = "Major Crime Indicators by Month (2016)",
caption = "Data source:Police recorded crime, Home Office" ) + theme(plot.title = element_text(size = 16), 
axis.title = element_text(size = 12, face = "bold")) + theme(legend.position = "none")

cowplot::plot_grid(plot_2015, plot_2016, ncol = 1)+ theme(legend.position = "right")

### Crime Category in 2017 Heatmap data
crime_count_2017 <- M_crime %>% group_by(month, crime_type) %>% 
filter(crime_type == "Anti-social behaviour" | crime_type == "Violence and sexual offences"|
crime_type == "Criminal damage and arson" | crime_type== "Burglary" | crime_type == "Other theft" |
crime_type == "Shoplifting" | crime_type == "Vehicle crime"| crime_type == "Public order", year == 2016) %>% 
summarise(num.crimes=n())
crime_count_2017$month <- ordered(crime_count_2017$month)

### Data Visualisation
### Crime Category in 2017 Heatmap 
plot_2017 <- ggplot(crime_count_2017, aes(month, crime_type, fill = num.crimes)) +
geom_tile(size = 1, color = "white") + scale_fill_viridis() + geom_text(aes(label = num.crimes), color = 'white') +
labs(x = "",y = "",fill = "Number of Crimes", title = "Major Crime Indicators by Month (2017)") + 
theme(plot.title = element_text(size = 16), axis.title = element_text(size = 12, face = "bold"))+
theme(legend.position = "none")

### Crime Category in 2018 Heatmap data
crime_count_2018 <- M_crime %>% group_by(month, crime_type) %>% filter(crime_type == "Anti-social behaviour" | 
crime_type == "Violence and sexual offences"| crime_type == "Criminal damage and arson" |
crime_type == "Burglary" | crime_type == "Other theft" | crime_type== "Shoplifting"| crime_type == "Vehicle crime"| 
crime_type == "Public order", year==2016) %>% summarise(num.crimes = n())
crime_count_2018$month <- ordered(crime_count_2018$month)

####Data Visualisation
### Crime Category in 2018 Heatmap
plot_2018<- ggplot(crime_count_2018, aes(month, crime_type, fill = num.crimes)) + geom_tile(size = 1, color = "white") +
scale_fill_viridis()  + geom_text(aes(label = num.crimes), color = 'white') + 
labs(x = "Month", y = "",fill = "Number of Crimes", title = "Major Crime Indicators by Month (2018)",
caption = "Data source:Police recorded crime, Home Office.") + theme(plot.title = element_text(size = 16), 
axis.title = element_text(size = 12, face = "bold")) + theme(legend.position = "none")

cowplot::plot_grid(plot_2017, plot_2018, ncol = 1)+ theme(legend.position = "right")

### Hotspot mapping
### Reading and plotting a shapefile
manchesterShape <- readOGR(dsn = "./BoundaryData_m", layer = "england_lsoa_2011")
head(manchesterShape@data, n=2)
head(manchesterShape@polygons, n=1)
qtm(manchesterShape)

### Making the plot interactive
tmap_mode("view")
qtm(manchesterShape)
qtm(manchesterShape, fill=NULL)
tmap_mode("plot")

### Colouring the thematic map
### Plotting crimes in Manchester using map
numCrimesByLSOA <- m_crime %>% select(LSOA.code, LSOA.name, Crime.type) %>% group_by(LSOA.code) %>% summarise(num.crimes = n())
names(numCrimesByLSOA)[names(numCrimesByLSOA) == "num.crimes"] <- "Number of Total Crimes"
numCrimesByLSOA
manchesterShape@data<-left_join(manchesterShape@data, numCrimesByLSOA, by=c('code'='LSOA.code'))
tmap_mode("view")
tm_shape(manchesterShape) + tm_fill("Number of Total Crimes", alpha = 0.5, style = "kmeans", border.col = "black") +
tm_borders(alpha=0.5)

### Reading and plotting a shapefile
manchesterShape<-readOGR(dsn = "./BoundaryData_m", layer = "england_lsoa_2011")
head(manchesterShape@data, n = 2)
head(manchesterShape@polygons, n = 1)
qtm(manchesterShape)

### Making the plot interactive
tmap_mode("view")
qtm(manchesterShape)
qtm(manchesterShape, fill=NULL)
tmap_mode("plot")

### Colouring the thematic map
### Plotting crimes in Manchester using map (Anti-social behaviour)

numCrimesByLSOA <- M_crime %>% select(LSOA.code, LSOA.name, crime_type) %>% filter(crime_type %in% c("Anti-social behaviour")) %>% 
group_by(LSOA.code, crime_type) %>% summarise(num.crimes = n())
names(numCrimesByLSOA)[names(numCrimesByLSOA) == "num.crimes"] <- "Number of Crimes (Anti-social behaviour)"
numCrimesByLSOA
manchesterShape@data<-left_join(manchesterShape@data, numCrimesByLSOA, by=c('code' = 'LSOA.code'))
tmap_mode("view")
tm_shape(manchesterShape) + tm_fill("Number of Crimes (Anti-social behaviour)", alpha = 0.5, style = "kmeans", border.col = "black" ) +
tm_borders(alpha=0.5)

### Plotting crimes in Manchester using map (Violence and sexual offences)
numCrimesByLSOA <- M_crime %>% select(LSOA.code, LSOA.name, crime_type) %>% 
filter(crime_type %in% c("Violence and sexual offences")) %>% group_by(LSOA.code,crime_type) %>% summarise(num.crimes = n())
names(numCrimesByLSOA)[names(numCrimesByLSOA) == "num.crimes"] <- "Number of Crimes (Violence and sexual offences)"
numCrimesByLSOA
manchesterShape@data <- left_join(manchesterShape@data, numCrimesByLSOA, by = c('code' = 'LSOA.code'))
tmap_mode("view")
tm_shape(manchesterShape) + tm_fill("Number of Crimes (Violence and sexual offences)", alpha = 0.5, style = "kmeans", 
border.col = "black" ) + tm_borders(alpha = 0.5)

### Plotting crimes in Manchester using map (Public order offences)
numCrimesByLSOA <- M_crime%>% select(LSOA.code, LSOA.name, crime_type) %>% 
filter(crime_type %in% c("Public order")) %>% group_by(LSOA.code, crime_type) %>% summarise(num.crimes = n())
names(numCrimesByLSOA)[names(numCrimesByLSOA) == "num.crimes"] <- "Number of Crimes (Public order offences)"
numCrimesByLSOA
manchesterShape@data<-left_join(manchesterShape@data, numCrimesByLSOA, by=c('code' = 'LSOA.code'))
tmap_mode("view")
tm_shape(manchesterShape) + tm_fill("Number of Crimes (Public order offences)", alpha = 0.5, style = "kmeans", border.col = "black" ) +
tm_borders(alpha=0.5)

### Plotting crimes in Manchester using map (Criminal damage and arson)
numCrimesByLSOA <- M_crime %>% select(LSOA.code, LSOA.name, crime_type) %>% 
filter(crime_type %in% c("Criminal damage and arson")) %>% group_by(LSOA.code,crime_type) %>% summarise(num.crimes = n())
names(numCrimesByLSOA)[names(numCrimesByLSOA) == "num.crimes"] <- "Number of Crimes (Criminal damage and arson)"
numCrimesByLSOA
manchesterShape@data<-left_join(manchesterShape@data, numCrimesByLSOA, by=c('code' = 'LSOA.code'))
tmap_mode("view")
tm_shape(manchesterShape) + tm_fill("Number of Crimes (Criminal damage and arson)", alpha = 0.5, style = "kmeans", 
border.col = "black" ) + tm_borders(alpha = 0.5)

### Plotting crimes in Manchester using map (Burglary)
numCrimesByLSOA <- M_crime %>% select(LSOA.code, LSOA.name, crime_type) %>% 
filter(crime_type %in% c("Burglary")) %>% group_by(LSOA.code, crime_type) %>% summarise(num.crimes = n())
names(numCrimesByLSOA)[names(numCrimesByLSOA) == "num.crimes"] <- "Number of Crimes (Burglary)"
numCrimesByLSOA
manchesterShape@data <- left_join(manchesterShape@data, numCrimesByLSOA, by = c('code'='LSOA.code'))
tmap_mode("view")
tm_shape(manchesterShape) + tm_fill("Number of Crimes (Burglary)", alpha = 0.5, style = "kmeans", border.col = "black" ) +
tm_borders(alpha=0.5)

### Plotting crimes in Manchester using map (Other theft)
numCrimesByLSOA <- M_crime %>% select(LSOA.code, LSOA.name, crime_type) %>% 
filter(crime_type %in% c("Other theft")) %>% group_by(LSOA.code, crime_type) %>% summarise(num.crimes = n())
names(numCrimesByLSOA)[names(numCrimesByLSOA) == "num.crimes"] <- "Number of Crimes (Other theft)"
numCrimesByLSOA
manchesterShape@data<-left_join(manchesterShape@data, numCrimesByLSOA, by = c('code'='LSOA.code'))
tmap_mode("view")
tm_shape(manchesterShape) + tm_fill("Number of Crimes (Other theft)", alpha=0.5, style = "kmeans", border.col = "black" ) +
tm_borders(alpha=0.5)

### Plotting crimes in Manchester using map (Shoplifting)
numCrimesByLSOA <- M_crime %>% select(LSOA.code, LSOA.name, crime_type) %>% 
filter(crime_type %in% c("Shoplifting")) %>% group_by(LSOA.code, crime_type) %>% summarise(num.crimes = n())
names(numCrimesByLSOA)[names(numCrimesByLSOA) == "num.crimes"] <- "Number of Crimes (Shoplifting)"
numCrimesByLSOA
manchesterShape@data<-left_join(manchesterShape@data, numCrimesByLSOA, by = c('code' = 'LSOA.code'))
tmap_mode("view")
tm_shape(manchesterShape) + tm_fill("Number of Crimes (Shoplifting)", alpha = 0.5, style = "kmeans", border.col = "black" ) +
tm_borders(alpha=0.5)

### Plotting crimes in Manchester using map (Vehicle crime)
numCrimesByLSOA <- M_crime %>% select(LSOA.code, LSOA.name, crime_type) %>% filter(crime_type %in% c("Vehicle crime")) %>% 
group_by(LSOA.code, crime_type) %>% summarise(num.crimes = n())
names(numCrimesByLSOA)[names(numCrimesByLSOA) == "num.crimes"] <- "Number of Crimes (Vehicle crime)"
numCrimesByLSOA
manchesterShape@data<-left_join(manchesterShape@data, numCrimesByLSOA, by = c('code'='LSOA.code'))
tmap_mode("view")
tm_shape(manchesterShape) + tm_fill("Number of Crimes (Vehicle crime)", alpha = 0.5, style = "kmeans", border.col = "black" ) +
tm_borders(alpha=0.5)

### K-means clustering analysis data
by_groups <- group_by(M_crime, crime_type, Location)
groups <- summarise(by_groups, n=n())
groups <- groups[c("Location", "crime_type", "n")]
groups_wide <- spread(groups, key = crime_type, value = n)

### Data Visualisation
### K-means clustering
df <- groups_wide[, -c(1,1)]
df <- df[complete.cases(df), ]
m <- apply(df, 2, mean)
s <- apply(df, 2, sd)
df <- scale(df, m, s)

wss <- (nrow(df)-1) * sum(apply(df, 2, var))
for (i in 2:20) wss[i] <- sum(kmeans(df, centers = i)$withiness)
plot(1:20, wss, type = 'b', xlab = 'Number of Clusters', ylab = 'Within groups sum of squares')
kc <- kmeans(df, 2)
kc

df2 <- data.frame(df, kc$cluster)
clusplot(df2, kc$cluster, color = TRUE, shade = F, labels = 0, lines = 0, main = 'k-Means Cluster Analysis')

### Crime Correlations
groups_wide <- groups_wide[c(2, 5, 9, 13, 15, 3, 7, 8, 10, 14, 4, 11, 12, 6)]
newdata <- na.omit(groups_wide)
str(df)
num.cols<- sapply(newdata, is.numeric)
cor.data<- cor(newdata[ ,num.cols])
ggcorr(cor.data, geom = "circle", nbreaks = 5, hjust = 0.2, angle = 75) 


