install.packages("rvest", "dplyr", "lubridate", "RSelenium", "seleniumPipes", "stringr")
edward <- function(){
        require(rvest)
        require(dplyr)
        require(lubridate)
        require(RSelenium) 
        require(seleniumPipes)
        require(stringr)
        
        rD <- rsDriver (browser = 'chrome',chromever = "latest",port = 4444L) # start a server with utility function
        #open browser
        remDr <- remoteDr(browserName = "chrome")
        
        webpage <- read_html("https://salesweb.civilview.com/")
        county <- webpage %>% html_nodes("a") %>% html_text(trim = TRUE)
        county_url <- webpage %>% html_nodes("a") %>% html_attr("href") %>%
                paste0("https://salesweb.civilview.com",.)
        county_list <- cbind(seq_along(county), county)
        print(county_list)
        num  <- readline("Please enter the number corresponding to the county you would like to scrape: ") %>%
                as.numeric()
        
        webpage <- remDr %>% go("https://www.cleargov.com/new-jersey")
        cities <- remDr %>% getPageSource() %>% html_nodes(".municipality-name") %>% html_text(trim = TRUE)
        cities <- unique(cities)
        
        remDr %>% go(county_url[num])
        webpage <- remDr %>% getPageSource()
        
        sales_date <- webpage %>% html_nodes("table") %>% html_table() %>% .[[1]] %>% .[,3]
        address <- webpage %>% html_nodes("table") %>% html_table() %>% .[[1]] %>% .[,6]
        details_url <- webpage %>% html_nodes(".hidden-print a") %>% html_attr("href") %>%
                paste0("https://salesweb.civilview.com",.)
        
        sales_date <- strptime(sales_date, format="%m/%d/%Y", tz="EST")
        sales_day <- day(sales_date)
        sales_month <- month(sales_date)
        
        county_table <- data.frame(sales_date=sales_date, sales_day=sales_day, sales_month=sales_month,
                                   address=address, details_url=details_url)
        
        d  <- readline("Enter the numeric value of the day: ") %>% as.numeric()
        m  <- readline("Enter the numeric value of the month: ") %>% as.numeric()
        
        county_filtered <- county_table %>% filter(sales_day == d, sales_month == m)
        
        city <- vector()
        adress <- county_filtered$address %>% as.character()
        for (i in seq_along(adress)){
                for(j in seq_along(cities)){
                        if(grepl(cities[j], adress[i], ignore.case = T)){
                                city[i] <- cities[j]
                        }
                }
        }
        
        
        if(!nrow(county_filtered) > 0){
                stop("There are no listings on this day")
        }
        message("Working...")
        details <- list()
        for(i in seq_along(county_filtered$details_url)){
                remDr %>% go(county_filtered$details_url[i])
                webpage <- remDr %>% getPageSource()
                
                #strin <- webpage %>% html_nodes("table") %>% html_text() %>% .[[1]]
                #s <- str_split(strin, "\n")
                #s <- sapply(s, str_trim)
                sheriff_num <- webpage %>% html_nodes("tbody:nth-child(1) tr:nth-child(1) td+ td") %>% html_text(trim = TRUE)
                court_case <- webpage %>% html_nodes("tbody:nth-child(1) tr:nth-child(2) td+ td") %>% html_text(trim = TRUE)
                plaintiff <- webpage %>% html_nodes("tbody:nth-child(1) tr:nth-child(4) td+ td") %>% html_text(trim = TRUE)
                defendant <- webpage %>% html_nodes("tbody:nth-child(1) tr:nth-child(5) td+ td") %>% html_text(trim = TRUE)
                #address <- webpage %>% html_nodes("tbody:nth-child(1) tr:nth-child(6) td+ td") %>% html_text(trim = TRUE)
                desc <- webpage %>% html_nodes("tbody:nth-child(1) tr:nth-child(7) td+ td") %>% html_text(trim = TRUE)
                approx <- webpage %>% html_nodes("tbody:nth-child(1) tr:nth-child(8) td+ td") %>% html_text(trim = TRUE)
                attorney <- webpage %>% html_nodes("tbody:nth-child(1) tr:nth-child(9) td+ td") %>% html_text(trim = TRUE)
                #deed <- webpage %>% html_nodes("tbody:nth-child(1) tr:nth-child(10) td+ td") %>% html_text(trim = TRUE)
                #deed_address <- webpage %>% html_nodes("tbody:nth-child(1) tr:nth-child(11) td+ td") %>% html_text(trim = TRUE)
                schedule <- webpage %>% html_nodes("thead+ tbody tr:nth-child(1) td+ td") %>% html_text(trim = TRUE)
                scheduled = as_date(schedule, "%m/%d/%Y", tz="EST")
                details[[i]] <- data.frame(sheriff_num=sheriff_num[1], court_case=court_case[1], 
                                           plaintiff=plaintiff[1], defendant=defendant[1],
                                           #priors=priors[1], 
                                           description=desc[1], attorney=attorney[1], approx_judgement=approx[1],
                                           #, deed=deed[1], deed_address=deed_address[1],
                                           scheduled=scheduled[1]
                )
        }
        deets <- do.call(rbind, details)
        
        message("Details extraction complete!")
        message("Opening RPR website...")
        
        ####################################
        email <- "ed@culture.estate"
        password <- "Fiverr!123"
        county_filtered$address <- gsub("\\(.*\\)", "", county_filtered$address)
        county_filtered$address = gsub("(Lane).*([A-Z]{2})","\\1 \\2", county_filtered$address, ignore.case = TRUE)
        county_filtered$address <- gsub(".* aka ","", county_filtered$address, ignore.case = TRUE)
        
        
        remDr %>% go("https://www.narrpr.com")
        
        remDr %>% findElement(using = "css selector", value = "#SignInEmail") %>% elementSendKeys(email)
        remDr %>% findElement(using = "css selector", value = "#SignInPassword") %>% elementSendKeys(password, key = "enter")
        Sys.sleep(5)
        
        records <- list()
        for(i in seq_along(county_filtered$address)){
                remDr %>% go("https://www.narrpr.com")
                remDr %>% findElement(using = "css selector", value = "#SiteSearchForm_SearchTxt") %>% 
                        elementSendKeys(county_filtered$address[i], key="enter")
                Sys.sleep(3)
                #remDr %>% findElement("css selector", "#location-details-panel > div.cpContent > div.rpr-propertyNote > textarea") %>% elementClick()
                driver_error <- tryCatch(remDr %>% findElement("css selector", "#location-details-panel > div.cpContent > div.rpr-propertyNote > textarea") %>% elementClick(), error = function(e) NA)
                driver_error
                if(is.na(driver_error)){
                        webpage <- read_html("https://www.freecodecamp.org/the-fastest-web-page-on-the-internet")
                        rpr_url=NA
                        rvm <- webpage %>% html_nodes(".priceGray") %>% html_text(trim = TRUE)
                        home_type <- webpage %>% html_nodes(".basicFactsHeading+ .item .nowrap") %>% html_text(trim = TRUE)
                        living_area <- webpage %>% html_nodes(".basicFactsHeading~ .item+ .item .nowrap") %>% html_text(trim = TRUE)
                        mls_name <- webpage %>% html_nodes(xpath = '//*[@id="sidebarHistoricalListings"]/div[1]/text()') %>% html_text(trim = TRUE)
                        listing_id <- webpage %>% html_nodes(xpath = '//*[@id="sidebarHistoricalListings"]/div[2]/text()') %>% html_text(trim = TRUE)
                        list_date <- webpage %>% html_nodes(xpath = '//*[@id="sidebarHistoricalListings"]/div[3]/text()') %>% html_text(trim = TRUE)
                        flood_zone <- webpage %>% html_nodes(".floodZoneInfo") %>% html_text(trim = TRUE)
                        tax_amount <- webpage %>% html_nodes(".col2 .item:nth-child(4) , .col2 .item:nth-child(5)") %>% html_text(trim = TRUE) %>% 
                                paste(., collapse = " ") %>% str_extract(., " (\\$[0-9,.]+.*?)") %>% str_trim()
                        
                }else{
                        Sys.sleep(7)
                        webpage <- remDr %>% getPageSource()
                        rpr_url <- remDr %>% getCurrentUrl()
                        rvm <- webpage %>% html_nodes(".priceGray") %>% html_text(trim = TRUE)
                        home_type <- webpage %>% html_nodes(".basicFactsHeading+ .item .nowrap") %>% html_text(trim = TRUE)
                        living_area <- webpage %>% html_nodes(".basicFactsHeading~ .item+ .item .nowrap") %>% html_text(trim = TRUE)
                        mls_name <- webpage %>% html_nodes(xpath = '//*[@id="sidebarHistoricalListings"]/div[1]/text()') %>% html_text(trim = TRUE)
                        listing_id <- webpage %>% html_nodes(xpath = '//*[@id="sidebarHistoricalListings"]/div[2]/text()') %>% html_text(trim = TRUE)
                        list_date <- webpage %>% html_nodes(xpath = '//*[@id="sidebarHistoricalListings"]/div[3]/text()') %>% html_text(trim = TRUE)
                        flood_zone <- webpage %>% html_nodes(".floodZoneInfo") %>% html_text(trim = TRUE)
                        tax_amount <- webpage %>% html_nodes(".col2 .item:nth-child(4) , .col2 .item:nth-child(5)") %>% html_text(trim = TRUE) %>% 
                                paste(., collapse = " ") %>% str_extract(., " (\\$[0-9,.]+.*?)") %>% str_trim()
                }  
                
                records[[i]] <- data.frame(rvm=rvm[1], home_type=home_type[1], living_area=living_area[1], 
                                           mls_name=mls_name[1], listing_id=listing_id[1], list_date=list_date[1],
                                           flood_zone=flood_zone[1], tax_amount=tax_amount[1], url=rpr_url[1])
                #remDr %>% findElement(using = "css selector", value = "#detailsPageHeader .rightArrow") %>% elementClick()
                
        }
        rec <- do.call(rbind, records)
        final_data <- cbind(county_filtered, deets, rec, addr=adress, town=city)
        final_data <- final_data %>% select(-sales_day, -sales_month, -details_url, -address)
        final_data <- final_data %>% select(url, sheriff_num, town, addr,rvm,approx_judgement, flood_zone, mls_name,
                                            listing_id, list_date, home_type, living_area, tax_amount, attorney,
                                            scheduled, sales_date, court_case, defendant, plaintiff, description)
        
        saveas <- readline("Enter the name to save the excel file: ")
        tryCatch(write.csv(final_data, paste0(saveas, ".csv"), row.names = FALSE), error=function(e){
                message("Please enter a unique file name")
                saveas <- readline("Enter the name to save the excel file: ")
                write.csv(final_data, paste0(saveas, ".csv"), row.names = FALSE)
        })
        
        if(grep(saveas, list.files())){
                message("File saved in the current directory")
        }
        remDr %>% deleteSession()
}




edward()