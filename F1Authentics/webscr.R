library(tidyverse)#full library(tidyverse) maybe useful but not necessary if you want to just get the data use dplyr instead
library(rvest)
#library(lubridate)  not actually used, but pretty nice library if you want to do something more with dates
#library(ggthemes) for some pretty graph templates(themes) - not necessary

lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")

link <- "https://auction.f1authentics.com/iSynApp/allAuction.action"
nof <- read_html(link) %>% html_nodes(".result-count") %>% html_text() %>% str_remove_all("\u000A") 
nof <- nof %>% substr(nchar(nof)-2,nchar(nof)) %>% str_remove_all(" ") %>% paste("rc=",.,sep="")
page <- read_html(paste(link,nof,sep="?"))

title <- page %>% html_nodes(".list-item__title a") %>% html_text()
time <- page %>% html_nodes(".list-item__opened span") %>% html_text() %>% strptime(., "%b %d, %Y %I:%M:%S %p",tz="Europe/London")

price <- page %>% html_nodes(".list-item__curbid strong") %>% html_text() %>% str_remove_all("[£,]") %>% as.numeric()
n_of_bids <- page %>% html_nodes(".list-item__numbids strong span") %>% html_text() %>%  as.numeric()
product_img <- page %>% html_nodes(".list-item__img-container img") %>% html_attr("src")
link <- page %>% html_nodes(".list-item__title a") %>% html_attr("href") %>% paste("https://auction.f1authentics.com",.,sep="")

get_closer_info <- function(product_link){
  page <- read_html(product_link)
  
  bidinc <- page %>% html_nodes(".auction-stats__increment td") %>% html_text() %>% str_remove_all("[£,]") %>% as.numeric()
  qua <- page %>% html_nodes(".auction-stats__increment+ .auction-stats__qty td") %>% html_text() %>% as.numeric()
  listin <- page %>% html_nodes(".auction-stats__id td") %>% html_text()
  skuu <- page %>% html_nodes(".auction-stats__tag td") %>% html_text()
  open <- page %>% html_nodes(".auction-stats__tag+ .auction-stats__qty td") %>% html_text()
  listinT <- page %>% html_nodes(".listing-type-value") %>% html_text()
  if(listinT == "RESERVE AUCTION"){
    met <- page %>% html_nodes(".mt-xs-3 div") %>% html_elements("span") %>% html_text()
  }
  else{
    met <- NA
  }
  descc <- page %>% html_nodes(".sharing--auction-display , p") %>% html_elements("p") %>% html_text() %>% str_remove_all("\u000A") %>% paste(collapse = " ") 

  return(c(bidinc,qua,listin,skuu,open,listinT,descc,met))
}

closer_info = sapply(link, FUN = get_closer_info, USE.NAMES = FALSE)
offered_items <- tibble(Title = title, Desc = closer_info[7,], Start =  strptime(closer_info[5,], "%b %d, %Y %I:%M:%S %p",tz="Europe/London"), End=time, CurrentPrice=price, NBids=n_of_bids, MetNot = closer_info[8,], ProductImg=product_img, Link=link, BidIncrement = closer_info[1,], Quantity = closer_info[2,],  Listing = closer_info[3,], SKU = closer_info[4,], ListingType = closer_info[6,])
write.csv2(offered_items , file = "~/Documents/R/f1/tracks_ex/data.csv", row.names = FALSE)


#Searching(filtering) by the title
low <- tolower(offered_items$Title)
offered_items %>% filter(grepl("alonso", low))
#


#Simple graph example(actually not interesting)
offered_items %>% 
  mutate(isReserve=(ListingType=="RESERVE AUCTION")) %>% 
  ggplot(aes(linetype=isReserve,x = CurrentPrice,y=as.numeric(NBids), color=as.numeric(NBids))) +
  labs(title = "Histogram of Current Price",
       x="Current Price",
       y="Pieces",
       color="Is the reserve met?") +
  geom_line(alpha=0.5)+
  geom_smooth()+
  theme_classic() + #this theme is from ggplot2
  theme(axis.title = element_text(),plot.title = element_text(hjust=0.5)) #for custom colors just add vector of colors hex values to scale_color_manual()
#
