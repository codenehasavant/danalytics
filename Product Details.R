

# -------------------------------------------------------------------------
#Author:    
#Dt:        02-12-2019
#Objective: To Webscrape the Amazon Website against ASINs to fetch product details

# -------------------------------------------------------------------------


# Clean Environment -------------------------------------------------------
rm(list= ls())


# Load Packages -----------------------------------------------------------
packages = c("xml2","rvest","httr","purrr","stringr")

ipak = function(pkg){
  new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)}

ipak(packages)

# Set Directory -----------------------------------------------------------
path = "F:/My Profile/DE/Amazon"
setwd(path)

# Read Inputs -------------------------------------------------------------
market = read.csv("input/Market.csv") #Taken from https://en.wikipedia.org/wiki/Amazon_(company)
#market = market[8,]
list_of_ASIN = read.csv("input/List_of_ASIN-2.csv")
names(list_of_ASIN) = c('ASIN')


# Declare Output Frame ----------------------------------------------------
result <- data.frame(matrix(ncol = 8, nrow = 0))
colnames(result) = c( "Region", "Market","ASIN", "Product.Name",
                      "Product.Link","Price", "Star.Rating", "No.Reviews")


# Compute Output ----------------------------------------------------------
for(m in 1:nrow(market)){  
  #print('market')
  #print(market[m,1])
  
  for (p in 1:nrow(list_of_ASIN)){ #nrow(list_of_ASIN)
    
    tryCatch({
      print(p)
      #print(list_of_ASIN[p,1])
      
      weblink = gsub(" ", '',paste('https://www.amazon.',market[m,4], '/dp/',list_of_ASIN[p,1] ))
      #print(url)
      
      con <- url(weblink, "rb") 
      webpage <- read_html(con)
      
      
      product.name = html_text(html_nodes(webpage, "span#productTitle"))
      product.name = gsub( "  ", '',str_replace_all(product.name, "[\r\n]" , ""))
      product.name = ifelse(length(product.name) == 0, 'Not Found', product.name)
      product.name = substring(product.name, 1, 50)
      
      price = html_text(html_nodes(webpage, "span#priceblock_ourprice"))
      price = ifelse(length(price) == 0, 'Not Found', price)
      
      star.rating = html_text(html_nodes(webpage, "span#acrPopover"))
      star.rating = str_trim(str_replace_all(star.rating, "[\r\n]" , ""))
      star.rating= ifelse(length(star.rating) == 0, 'Not Found', star.rating)
      
      
      no.reviews = html_text(html_nodes(webpage, "span#acrCustomerReviewText"))
      no.reviews = str_trim(str_replace_all(no.reviews, "[\r\n]" , ""))
      no.reviews = ifelse(length(no.reviews) == 0, 'Not Found', no.reviews)
      
      result.frame = data.frame(Region = market[m,1], 
                                Country = market[m,2],
                                Market = market[m,1], 
                                ASIN = list_of_ASIN[p,1],
                                Product.Name = product.name, 
                                Product.Link = gsub(" ", "", 
                                                    paste('https://www.',
                                                          market[m,3], 
                                                          '/dp/',
                                                          list_of_ASIN[p,1])),
                                Price = price,
                                Star.Rating = star.rating, 
                                No.Reviews = no.reviews)
      
      result = rbind(result, result.frame)
      
      
      
    }, 
    error=function(e){
      cat("ERROR :",conditionMessage(e), "\n")
      }
    )
    
  
    
    
  }
}


# Save Output Frame -------------------------------------------------------
result = unique(result)
dt = format(Sys.time(), "%d-%b-%Y %H.%M")
write.csv(result, paste('output/Product Details',dt,'.csv'))


#--------------------------------------------------------------------------




