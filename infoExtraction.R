

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
		
		
		for (j in 1 : nbrow){

            if ( j %% 3 == 0 ){
	
		            status  <-  getStatus (UI[j,]) #Getting the status 
					country <-  getCountry (UI[j-1,]) #Getting country
					link    <-  paste(httProj, getLink(UI[j-2, ]), sep = "" )#Creating the link
					
					valR <- paste (country, link, status, sep = ";")
					print (valR)
		            #setStatusRedict(link,pStatus)

	        }#if-condition

        }#End for
    
}#End function


############# 

#paste the http with the sector
httpR <- paste (http, "transport/", sep = "")

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
for (j in 1 : nbSubpage - 1){
   
   #List of project and creation of link + status
   if (j > 0) {
                urlR <- paste(httpR, j, sep = "")
	            print(j)
				o <- getSearchReference(urlR) #Call the function to collect all the project i.e link and status
				print(o)
   }else{
                print("0")
	            o <- getSearchReference(httpR)
				print (o)
   }#if-condition 
   
}#End for
