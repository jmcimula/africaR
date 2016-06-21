

#loading some libraries

library(rvest)
library (dplyr)

source("prcontent.R")
source("sector.R")
source("http.R")
source("subfunc.R")

sector   <- c(
               #climate_change, water, infrastructure ,environmentt, human, energy, agriculture,information	   
			    #economic, education, health, gender, transport
				gender, health
			)
            #example
			
afriData <- data.frame()

for ( i in 1 : length(sector)) {

#paste the http with the sector
httpR <- paste (http, sector[i], "/", sep = "")

urlR <- read_html(httpR) #Scrapping the corpus

#number of sub pages to explore - 1
nbSubpage <- getPageNumber(
                            substr(     
				        urlR,
				        unlist(gregexpr(pattern = "out of", urlR))[1], 
					unlist(gregexpr(pattern = "out of", urlR))[1] + 30
				)
			)
						
afriData <- rbind(afriData, cbind(getData(nbSubpage), sector = sector[i] ) )

}#End-for


View(afriData)

table(afriData$country)
table(afriData$status)
