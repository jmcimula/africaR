
getAmount <- function (xx, yy){
    
    w <- read_html(xx)
	
	#status of the projects are approved, ongoing, lending and pipeline
	
	if ( str_trim(yy) == "approved" || str_trim(yy) == "ongoing"){
		
			z <-  w %>%
			      html_nodes("table")%>%
				  .[[1]] %>%
				  html_table()
				  
			colnames(z) <- c("A","B") 
			
			amount <- z %>%
			            filter(str_trim(tolower(A)) %in% "total")
			
			amount <- str_replace_all(amount[2], "[:alpha:]", "") #Replace letters by void    
			
	}else{#Case of lending and pipeline
	     
		    z <-  w %>%
			      html_nodes ("table tbody") %>%
				  html_text()
				  
			amount <- str_replace_all(z, "[:alpha:]", "")
			amount <- str_trim (amount)
	}

	#return amount
	return (str_trim(amount))
}



#     z <- y %>%
#          html_nodes("div li strong") %>%
#          html_text()

