library(tidyverse)

#download the data from the Github page
# expeditions (exped_tidy.csv)
# peaks (peaks_tidy.csv)
# expeditions file contains details about each expedition from 2020 till 2024

#peaks contain details about the peaks, including names, heights and other details
exped_url <- 
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-21/exped_tidy.csv" 
exped <- read.csv(exped_url)

peak_url<-
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-21/peaks_tidy.csv" 
peaks<- peak_url

#investigate the datasets using the head(), glimpse() or the View() functions 
#Let's look at the first few rows 
head(exped)

#see all the column names and types 
glimpse(exped) ##each column is a variable and each row is a single expedition 

#view' the data 
view(exped)

#total number of expeditions and the peaks
cat(nrow(exped), "records of expeditions") # records of expeditions
cat(nrow(peaks), "records of peaks") #records of peaks


#The first thing we might want to do is to see what is the range of the years in our dataset – we know it’s from 2020-2024, but still would be good to confirm this 
range(exped$YEAR,na.rm = TRUE) # range of the years in our dataset

# the distribution of the expeditions over the years
#use the count() function together with the R’s pipe (%>%) feature. 
#helps to chain multiple operations together, and using the ‘%>%’, we instruct R to move the output of the left part of the expression to the right. 

exped%>%
  count(YEAR) # expedition counts over the years

#plot  a table called ‘expeditions_per_year’ and then create a line chart using the plot() function
expedition_per_year<- table(exped$YEAR)
view(expedition_per_year)
plot(names(expedition_per_year),
     expedition_per_year,
     type = "l", #l = line plot
     main = "Number of Expedition by Year",
     xlab = "Year",
     ylab = "Number of Expedition")

# shows that with the Covid pandemic, we observed low expeditions, but it looks like number of 
#expeditions have been increasing over the years since the pandemic. There’s a drop in 2024 though, 
#which could be explained by the fact that perhaps the data isn’t complete for 2024. 


#see how the expeditions vary over the seasons. 
# looking at the SEASON_FACTOR column and plotting this. We will create a new table called seasons_dist 

season_dist<- table(exped$SEASON_FACTOR)

#However, a line plot wouldn’t be a good option for this plot since seasons aren’t very well connected 
#in a sequential manner, particularly when we are looking at trends across multiple years. So, we will 
#create a bar plot instead

barplot(season_dist,
        xlab = "Season",
        ylab = "Number of Expeditions",
        main = "Number of Expeditions by Season")
#shows that Spring is the most common time that expeditions are organised
# while the least expeditions are during summer and winter

#However, it could just be a 
#trend in a particular year so we may want to look at the expeditions per season over multiple years. 
#create another table t to structure the data in an easy to use way

year_season_table<- table(exped$SEASON_FACTOR,exped$YEAR)
barplot(year_season_table,
        beside = TRUE, #beside=TRUE makes grouped bars 
        col=c("blue", "green", "yellow", "orange"),
        main = "EXpedition per Year and Season",
        xlab= "Year",
        ylab="Number of Expeditions",
        legend.text = TRUE,
        args.legend=list(title="Season", x="topleft"))

# ‘legend’ – this helps understand what the colours indicate in our plot.
# x-axis is set to the year, and each year’s season provides the count of the number of expeditions.


# move our legend to outside the graph by adding a separate legend plot  
par(mar = c(5,4,4,8))  #increase the right margin 
# Grouped barplot
barplot(year_season_table,
        beside = TRUE,
        col = c("blue", "green", "yellow", "orange"),
        main = "Expedition per Year and Season",
        xlab = "Year",
        ylab = "Number of Expedition",
        las = 2) # rotate x-axis labels 


#Add legend outside the plot (right side) 
legend(x= par("usr") [2] + 0.5, # just past right edge of plot 
       y = max(year_season_table),
       legend = row.names(year_season_table),  # explicitly use season names
       fill = c("blue", "green", "yellow", "orange"),
       title = "Season",
       xpd = TRUE,
       bty = "n")

#the 2024 autumn data is missing, which explains why we observed a drop in 2024 – just that the data hasn’t been collected. This also shows us that actually spring isn’t when the 
# expeditions are conducted, but autumn and our missing autumn data just makes it look like 
#we have a different pattern.  


#PEAKS
#which peaks are the most climbed
exped %>% 
  count(PEAKID)

#Without the peakIDs explained, this distribution doesn’t really help so we will try to join the two 
#datasets. So, first we will create a table that contains the peak ID counts, then convert that into a 
#data frame. Converting to data frame allows us to easily sort the data and join with the peaks data. 

peak_counts <- table(exped$PEAKID) 
peak_counts_df <- as.data.frame(peak_counts) 
names(peak_counts_df) <- c("PEAKID", "expedition_count") 
#sort the peak counts in descending order 
peak_counts_df <- peak_counts_df[order(
  peak_counts_df$expedition_count), ] 
#let's try to join this dataframe with the peaks data so that we can 
#know the names of the peaks 
# Merge will match peakID from both datasets 
peak_joined <- merge(peak_counts_df, peaks, by="PEAKID",all.x = 
                       TRUE) 
#sort the peak counts in descending order 
peak_counts_df <- peak_counts_df[order(
  peak_counts_df$expedition_count), ] 
#let's try to join this dataframe with the peaks data so that we can 
#know the names of the peaks 
# Merge will match peakID from both datasets 
peak_joined <- merge(peak_counts_df, peaks, by="PEAKID",all.x = TRUE) 
#Now we can create a barplot of top 20 peaks attempted -but first we 
#will sort the dataframe based on expedition count 
peak_joined <- peak_joined[order(-peak_joined$expedition_count),] 
top20peaks <- head(peak_joined, 20) 
barplot(top20peaks$expedition_count, 
        names.arg = top20peaks$PKNAME, 
        las = 2, 
        col = "lightblue", 
        main = "Top 20 Most Climbed Peaks", 
        ylab = "Number of Expeditions")
#This should give us a plot of the top 20 most climbed peaks

# success in expeditions
#The grepl() function is used for pattern matching and stands for grep logical which simply searches for a sequence of 
#characters from a piece of text
exped$SUCCESS<- grepl ("Success", exped$TERMREASON_FACTOR, ignore.case = TRUE)
# Calculate success rate per year
success_years <- sort(unique(exped$YEAR))
success_rate <- numeric(length(years))
for (i in 1:length(success_years)) {
  data_year <- exped[exped$YEAR == success_years[i], ]
  success_rate[i] <- mean(data_year$SUCCESS, na.rm = TRUE)
  }

#This provides us with the success_rate table, which we can now plot using the following
# Line plot of success rate
plot(success_years,
     success_rate,
     type = "l",
     col = "darkgreen",
     main = "Expedition Success Rate Over Years",
     xlab = "Year",
     ylab = "Success Rate")

#Let’s now study the level of success per peak, and for that, we will need to follow a similar approach by creating a merged data frame called peak_summary

#Let’s now plot this data as a barplot – but since this will be all of the peaks, it would be useful to set our margins appropriately to let our names fit in.
# Make sure long names fit in the plot area
par(mar = c(10, 5, 4, 2)) # increase bottom margin
barplot(peak_summary$success_rate,
        names.arg = peak_summary$PKNAME,
        las = 2,
        cex.names = 0.4,
        col = "lightgreen",
        main = "Top 10 Peaks by Expedition Success Rate",
        ylab = "Success Rate",
        ylim = c(0, 1))
