#Metal Archives Web Scraper
#Author: Jeff Alvarez
#Date: 4/24/2021
#This script access information from www.metal-archives.com
#and retrieves the name, ID number, country of origin, status, and
#year of formation of every band available on the website

#Load the necessary libraries
library(rvest)
library(jsonlite)
library(dplyr)
library(rio)

#Create a vector containing the necessary characters to create the URLs to access the JSON files where the table data is stored
#NBR is for bands that start with a number
#~ is for bands that start with a special or foreign character
alphabet <- c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z","NBR","~")

#Initialize data frame we will add to
bandframe <- data.frame()

#Go through every letter
for (chr in alphabet)
{
  #Create a URL to access the first JSON of the current letter
  current_url <- paste0("https://www.metal-archives.com/browse/ajax-letter/l/",chr,"/json/1")
  #Repeat until JSON is successfully read in
  repeat
  {
    #Error handling to prevent timeouts
    current_page <- tryCatch(readLines(current_url),
    error=function(err){print("Timeout, retrying...")
                        return(NULL)})
    if(!is.null(current_page))
    {
      break
    }
  }
  #The second line in each JSON is of the form '"iTotalRecords": num,', where num is the total number of bands starting with the current letter
  #Use several strsplits to extract num to find the total number of bands starting with the current letter
  num_bands <- strtoi(strsplit(unlist(strsplit(current_page[2],' '))[2],","))
  
  #Initialize counter
  display_start <- 0
  
  #Each JSON contains 500 bands
  #To access JSONs past the first for each letter, we need to modify "iDisplayStart" in the query string of each URL
  while (display_start < num_bands)
  {
    #Create the current URL
    current_url <- paste0("https://www.metal-archives.com/browse/ajax-letter/l/",chr,"/json/1?iDisplayStart=", display_start)
    #Print statement for a loading screen
    print(paste("Getting bands starting with '",chr,"': page ",display_start/500 + 1,sep=""))
    #Read in the JSON with error handling to prevent timeouts
    current_page <- tryCatch(readLines(current_url),
                             error=function(err){print("Timeout, retrying...")
                                                  return(NULL)})
    if(is.null(current_page))
    {
      next
    }
    #The second through fourth line of each JSON do not contain band data and cause the fromJSON function to not perform correctly
    #Remove lines 2 through 4 from each file
    current_page <- current_page[-(2:4)]
    #Convert the read JSON to a data frame and add it to the main data frame
    bandframe <- rbind(bandframe,data.frame(fromJSON(current_page)))
    #Increment counter to access next JSON
    display_start <- display_start + 500
  }
}


print("Cleaning data...")

#Change column names from "aaData" to something readable
colnames(bandframe) = c("Name", "Country", "Genre", "Status")

#Extract the link from the HTML tag in each band
bandframe <- mutate(rowwise(bandframe), Link = unlist(strsplit(Name,"'"))[2])

#Extract band names from each link
bandframe <- mutate(rowwise(bandframe), Name = unlist(strsplit(unlist(strsplit(Name,">"))[2],"<"))[1])

#Extract single status word from HTML tag
bandframe <- mutate(rowwise(bandframe), Status = unlist(strsplit(unlist(strsplit(Status,">"))[2],"<"))[1])

#Extract band ID numbers from link for later use in URL creation
bandframe <- mutate(rowwise(bandframe), ID = unlist(strsplit(Link,"/"))[6])

#Convert ID column to numbers so they are sortable
bandframe <- mutate(rowwise(bandframe), ID = as.numeric(ID))

#Reorder columns and remove link column to make data frame more readable
bandframe <- bandframe[,c(1,6,2,3,4)]


print("Getting formation years...")

#Initialize Year column
bandframe <- mutate(bandframe,Year = '')

#Band formation years are not available in the JSONs
#Need to go to each individual page to access that information
#Loop through all IDs
counter <- 1
for (band in bandframe$ID)
{
  #Add an empty check so I can pick up where I left off if I need to stop the script for some reason
  if (bandframe$Year[counter] == "")
  {
    #Print statement as a progress indicator
    print(bandframe$Name[counter])
    #Create URL
    current_url <- paste0("https://www.metal-archives.com/band/view/id/",band)
    #Repeat until page is successfully read in
    repeat
    {
      current_page <- tryCatch(read_html(current_url),
                               error=function(err){print("Timeout, retrying...")
                                 return(NULL)})
      if (!is.null(current_page))
      {
        break
      }
    }
    #Pertinent information is contained in the "dd" tags in the HTML code for each page
    #Formation year is the fourth "dd" tag encountered in each page
    bandframe$Year[counter] <- html_text(html_nodes(current_page,"dd")[4])
  }
}

export(bandframe, "bandlistcomplete.csv")