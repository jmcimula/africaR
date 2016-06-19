

#loading some libraries

library(rvest)
source("http.R")
source("subfunc.R")

#Function which checks the url of all the project and their status
getSearchReference <- function (urlR){

		urlR <- urlR
		urlR <- read_html(urlR)
			
		urlR <- urlR %>%
                html_nodes("tr td")%>%
			    html_text()
        
		UI <- urlR
		UI <- as.data.frame(UI)
		nbrow <- nrow(UI)
				
        #print(UI)
		valR <- data.frame()
		for (j in 1 : nbrow){

            if ( j %% 3 == 0 ){
	
		            status  <-  getStatus (UI[j,]) #Getting the status 
					country <-  getCountry (UI[j-1,]) #Getting country
					link    <-  paste(httProj, getLink(UI[j-2, ]), sep = "" )#Creating the link
					
					vloop <- data.frame (country, link, status)
					valR  <- rbind(valR, vloop)
		            #setStatusRedict(link,pStatus)

	        }#if-condition

        }#End for
	
    return (valR)
}#End function


############# 

#paste the http with the sector
httpR <- paste (http, "information-communication-technology/", sep = "")

urlR <- read_html(httpR) #Scrapping the corpus

#number of sub pages to explore - 1
nbSubpage <- getPageNumber(
                            substr(     
							        urlR,
							        unlist(gregexpr(pattern = "out of", urlR))[1], 
									unlist(gregexpr(pattern = "out of", urlR))[1] + 30
								)
						)
						
#Browsing all the subpages related to the choice of the user from the interface
getData <- function (x){

    valR <- data.frame ()
	
	for (j in 1 : x - 1){
		
		#List of project and creation of link + status
		    
		if (j > 0) {
                urlR  <- paste(httpR, j, sep = "")
		vloop <- getSearchReference(urlR) #Call the function to collect all the project i.e link and status
            }else{ 
		vloop <- getSearchReference(httpR)
		}#if-condition 
    
        valR <- rbind(valR, vloop)	
    }#End for
    #return
    return (valR)

}#End-function

a <- getData(nbSubpage)
View(a)
