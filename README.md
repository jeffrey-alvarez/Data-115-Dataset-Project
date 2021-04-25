# Data-115-Dataset-Project

# Motivation

The overarching question I sought to answer through this project was how has the popularity of metal music genres changed in the last 50 years. To answer this question, I needed data on many bands year of formation and what genre they identify as. This led me to www.metal-archives.com, a very comprehensive source on many, many bands. This website also contained easy to access information on each bands country of origin and their current status (active vs. broken up) which could prove to be useful data in futher analysis.

# Data Process

Most of the data available from metal-archives is in javascript generated tables within many alphabetcally sorted lists. These tables are sourced by multiple JSON files that contain all of the data used to generate each table. Looking through the source code for one of the table web pages will lead you to the URL of the JSON which generates the table you are on. The general form for the JSON URL is "https://www.metal-archives.com/browse/ajax-letter/l/[chr]/json/1" where [chr] is the letter that the bands in the table start with. Additionally, since this URL form will only display the first 500 bands of a given letter, it is required to add a query string that modifies the variable "iDisplayStart" to display access bands past the first 500. For example, to access the second JSON, you need to set iDisplayStart to 500 which will display bands 500-999. This gives the final URL form: "https://www.metal-archives.com/browse/ajax-letter/l/[chr]/json/1?iDisplayStart=[start]" where [start] is the number of the first band displayed on the JSON within the given <chr>.
  
  To quickly access all of the JSON files available, I created a loop which systematically reads in every available JSON and converts each to a data frame and compiles them into a single usable data frame. The loop that reads in the JSONs is in the first half of "metal_archives_scraper.r". This loop took around 15 minutes to run and yielded the contents of "rawbanddata.csv" and is mostly unusable in this state as much of the data is contained within HTML tags. The next step then was to clean the data by removing the superfluous text that formed the HTML tags. I split the first column into two which made the "Name" and "ID" columns. The "Name" column so each row has an easy identifier and the "ID" column to form individual band URLs later. The "Country" and "Genre" columns were already in ideal condition, so I did not need to modify them, but the "Status" column also needed HTML tags to be removed to make the data smaller and more readable.
  
  Finally, I was able to proceed in the next step of the data collection process. Unfortunately, year of formation data was not available within the JSON files and can only be accessed on a given bands individual page. Thus, I created a second loop that loops through the ID column of the cleaned data, forms a URL of the form "https://www.metal-archives.com/bands/view/[ID]", where [ID] is the ID number of the band in question. I used functions in the rvest package to read in each web page, extract the formation year from the correct "dd" HTML tag, convert it to readable text, and insert it into the data frame. This loop is contained in the second half of "metal_archives_scraper.R" and took about 40 hours to run, most likely due to my slow internet connection. After all of the years were read in, I converted the Year column from a character to a numeric column for use in visualizations and exported the final dataframe to "bandlistcomplete.csv".
  
  # Visualization
  
  I wanted a visualization that showed the change in number of bands formed by year, so I broke up the main data frame into a subset that only contained bands with available years (removed bands with "N/A" listed as their year of formation). I then created new data frame based on keywords found within each badns genre. I used most the the genres listed on "https://www.metal-archives.com/browse/genre" but left out some of them because I felt they were too fringe and were more so descriptors of already existing genres rather than genres in and of themselves. I then plotted each genre onto a graph by number of bands formed per year.
  
  Looking at the graph, it is clear that death, black, and thrash metal are the most popular genres. Looking at the change in each line can tell an interesting story however. Towards the beginning, heavy metal had the most bands and was really the only genre until the early 80s when other genres began to emerge. Thrash metal was the next genre to shoot up in popularity possibly due to the advent of the "big 4" of thrash in the early 80s which included Metallica, Slayer, and Anthrax in 1981 and Megadeth in 1983. Death metal started to surge in the mid to late 80s in the wake of many massively influential bands formed in the mid 80s such as Death, Cannibal Corpse, and Bolt Thrower. Black metal was the last of the three to gain major traction, not gaining many bands until the 90s. Black metal started in smaller countries in Europe, so it may have taken longer for it to spread to larger countries like the US, where the number of black metal bands is highest today.
  
  ![Number of Formed Bands by Year](https://github.com/jeffrey-alvarez/Data-115-Dataset-Project/blob/main/BandsPerYear.png)
  
  # Analysis
  
  To analyze this data set, I chose to perform logistic regression on the "Status" column of the data frame. I first took the data frame of only legitimate years and created a new data frame containing only bands with either "Active" or "Split-up" as their status. This removed unclear or ambiguous statuses such as "Changed Name", "Unkown", or "Disputed". Then, I changed the status column to be completely binary, Active status became 1 and Split-up status became 0. I then used R's glm function to create a predictive model of a bands status based on their formation year. Finally, I created a plot of formation years vs. status and plotted the predictive model on top of it. Not surprisingly, I suppose, older bands were generally more likely to have broken up than newer bands.
  
  ![Status Logistic Regression](https://github.com/jeffrey-alvarez/Data-115-Dataset-Project/blob/main/LogisticStatus.png)
