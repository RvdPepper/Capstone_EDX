#CREATE Traing set, validation set

# Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")

##DOWNLOAD
  dl <- tempfile()
  download.file("https://github.com/RvdPepper/Capstone_EDX/raw/master/nyc-rolling-sales.csv", dl)
  dat <- read_csv(dl)

##LOKAAL!!
  #filename <- "nyc-rolling-sales.csv"
  #fullpath <- file.path("C:/Users/Robert vd Heijden/Documents/Studio R documenten/Capstone", filename)
  #dat <- read_csv(fullpath)

##Buil DataSet
  dat <- dat %>% select("BOROUGH","BUILDING CLASS CATEGORY", "ZIP CODE", "RESIDENTIAL UNITS", "COMMERCIAL UNITS", "YEAR BUILT", "TAX CLASS AT TIME OF SALE", "BUILDING CLASS AT TIME OF SALE", "SALE DATE", "SALE PRICE")
  NYC_Sales <- as.data.frame(dat) %>% rename(BUILDING = "BUILDING CLASS CATEGORY",
                                                             ZIPCODE = "ZIP CODE",
                                                             RESIDENTIAL = "RESIDENTIAL UNITS",
                                                             COMMERCIAL = "COMMERCIAL UNITS",
                                                             YEARBUILT = "YEAR BUILT",
                                                             TAXCLASSSALE = "TAX CLASS AT TIME OF SALE",
                                                             BUILDINGCLASSSALE = "BUILDING CLASS AT TIME OF SALE",
                                                             SALEDATE = "SALE DATE",
                                                             SALEPRICE = "SALE PRICE") 

##Only records with a Salesprice and zipcode imported
  NYC_Sales <- filter(NYC_Sales,SALEPRICE > 1000)
  NYC_Sales <- filter(NYC_Sales,ZIPCODE != 0)

##Convert records to numeric or character
  NYC_Sales <- as.data.frame(NYC_Sales) %>% mutate(BOROUGH = as.numeric(BOROUGH),
                                              BUILDING = as.character(BUILDING),
                                              ZIPCODE = as.numeric(ZIPCODE),
                                              RESIDENTIAL = as.numeric(RESIDENTIAL),
                                              COMMERCIAL = as.numeric(COMMERCIAL),
                                              YEARBUILT = as.numeric(YEARBUILT),
                                              TAXCLASSSALE = as.numeric(TAXCLASSSALE),
                                              BUILDINGCLASSSALE = as.character(BUILDINGCLASSSALE),
                                              SALEDATE = SALEDATE,
                                              SALE = SALEDATE,
                                              SALEPRICE = as.numeric(SALEPRICE))

## Validation set will be 10% of New York City Sales dataset
  set.seed(1, sample.kind="Rounding")
  test_index <- createDataPartition(y = NYC_Sales$SALEPRICE, times = 1, p = 0.1, list = FALSE)
  NYC_Train <- NYC_Sales[-test_index,]
  NYC_Validation <- NYC_Sales[test_index,]

#DATA Exploration

  ## The first ten lines of the dataset:
    NYC_Train[1:10,]

  ## summary of the statistics of the dataset:
    summary(NYC_Train)

  ##The number of rows
    nrow(NYC_Train)

  ##The number of columns
    ncol(NYC_Train)

  ##How many different zipcodes are in the NYC dataset?
    Zipcode <- NYC_Train %>% select(ZIPCODE) %>% distinct()

#DATA Cleaning (Convert the saledate into Year-Month Format (needed for Data Visualisation))
  NYC_Train <- NYC_Train %>% separate(SALE, c("YEAR", "MONTH", "DAY"), "-") %>% mutate(YEAR =as.numeric(YEAR), MONTH = as.numeric(MONTH), DAY = as.numeric(DAY)) %>% unite("YEARMONTH", YEAR:MONTH, sep="-") %>% select(-DAY)
  NYC_Validation <- NYC_Validation %>% separate(SALE, c("YEAR", "MONTH", "DAY"), "-") %>% mutate(YEAR =as.numeric(YEAR), MONTH = as.numeric(MONTH), DAY = as.numeric(DAY)) %>% unite("YEARMONTH", YEAR:MONTH, sep="-") %>% select(-DAY)

#DATA VISUALISATION

  ##Histogram of the Sales Price (outcome) and Borough and Taxclass (predictors)
    hist_SALEPRICE<- NYC_Train %>% filter(SALEPRICE < 10000000) %>% ggplot(aes(x=SALEPRICE/1000000)) + geom_histogram(binwidth = 0.5, fill = "Blue", col = "Black") + theme(axis.text=element_text(size=8), axis.title=element_text(size=8), plot.title=element_text(size=12)) + xlab("Sales Price NYC (*10^6)") + ggtitle("Histogram Sales Price")
    hist_BOROUGH <- NYC_Train %>% filter(SALEPRICE < 10000000) %>% ggplot(aes(x=SALEPRICE/1000000)) +  geom_histogram(binwidth = 0.5, fill = "Blue", col = "Black") + theme(axis.text=element_text(size=6), axis.title=element_text(size=6), plot.title=element_text(size=10)) + xlab("Sales Price NYC (*10^6)") + ggtitle("Histogram Borough") + facet_wrap(~BOROUGH, nrow=1)
    hist_TAXCLASS <- NYC_Train %>% filter(SALEPRICE < 10000000) %>% ggplot(aes(x=SALEPRICE/1000000)) + theme(axis.text=element_text(size=6), axis.title=element_text(size=6), plot.title=element_text(size=10)) + geom_histogram(binwidth = 0.5, fill = "Blue", col = "Black") + xlab("Sales Price NYC (*10^6)") + ggtitle("Histogram Tax Sale") + facet_wrap(~TAXCLASSSALE, nrow=1)  
  ##Scatterplot of the Year Sale, Year Built, Residential Units, Commercial Units (predictors)
    scat_YEARSALE <- NYC_Train %>% mutate(date = round_date(SALEDATE, unit = "week")) %>% group_by(date) %>% summarize(price_in_billion = mean(SALEPRICE)/1000000) %>% ggplot(aes(date, price_in_billion)) + ggtitle("Scatterplot Sale Date") + theme(axis.text=element_text(size=8), axis.title=element_text(size=8), plot.title=element_text(size=12)) + xlab("Sale Date") + ylab("Sales Price NYC (*10^6)") + geom_point() + geom_smooth()
    scat_YEARBUILT <- NYC_Train %>% filter(YEARBUILT > 1800) %>% group_by(YEARBUILT) %>% summarize(price_in_billion = mean(SALEPRICE)/1000000) %>% ggplot(aes(YEARBUILT, price_in_billion)) + geom_point() + ggtitle("Built Date") + theme(axis.text=element_text(size=6), axis.title=element_text(size=6), plot.title=element_text(size=10)) + xlab("Built Date") + ylab("Sales Price NYC (*10^6)") + geom_smooth()
    scat_Residential <- NYC_Train %>% filter(SALEPRICE < 10000000) %>% filter(RESIDENTIAL < 1000) %>% group_by(RESIDENTIAL) %>% summarize(price_in_billion = mean(SALEPRICE)/1000000) %>% ggplot(aes(RESIDENTIAL, price_in_billion)) + ggtitle("Residential Units") + theme(axis.text=element_text(size=6), axis.title=element_text(size=6), plot.title=element_text(size=10)) + xlab("Nr of Residential Units") + ylab("Sales Price NYC (*10^6)") + geom_point() + geom_smooth()
    scat_Commercial <- NYC_Train %>% filter(SALEPRICE < 10000000) %>% filter(COMMERCIAL < 1000) %>% group_by(COMMERCIAL) %>% summarize(price_in_billion = mean(SALEPRICE)/1000000) %>% ggplot(aes(COMMERCIAL, price_in_billion)) + ggtitle("Commercial Units") + theme(axis.text=element_text(size=6), axis.title=element_text(size=6), plot.title=element_text(size=10)) + xlab("Nr of Commercial Units") + ylab("Sales Price NYC (*10^6)") + geom_point() + geom_smooth()
  ##Boxplot of the Monthly Sales (outcome), Borough, Zipcode and Building Class (predictors)
    Boxplot_MonthlySale <- NYC_Train %>% filter(SALEPRICE < 100000000) %>% group_by(SALEPRICE) %>% summarize(price_in_billion = mean(SALEPRICE)/1000000, year = as.character(first(YEARMONTH))) %>% ggplot(aes(year, price_in_billion)) + geom_boxplot() + ggtitle("Boxplot Sale date") + theme(axis.text=element_text(size=6), axis.title=element_text(size=6), plot.title=element_text(size=10)) + xlab("Sale Date") + ylab("Sales Price NYC (*10^6)")
    Boxplot_Borough <- NYC_Train %>% filter(SALEPRICE < 100000000) %>% group_by(SALEPRICE) %>% summarize(price_in_billion = mean(SALEPRICE)/1000000 , Borough = as.character(first(BOROUGH))) %>% ggplot(aes(Borough, price_in_billion)) + geom_boxplot() + ggtitle("Boxplot Borough") + theme(axis.text=element_text(size=6), axis.title=element_text(size=6), plot.title=element_text(size=10)) + xlab("Borough") + ylab("Sales Price NYC (*10^6)")
    Boxplot_Zipcode <- NYC_Train %>% filter(SALEPRICE < 100000000) %>% group_by(SALEPRICE) %>% summarize(price_in_billion = mean(SALEPRICE)/1000000 , Zipcode = as.character(first(ZIPCODE))) %>% ggplot(aes(Zipcode, price_in_billion)) + geom_boxplot() + ggtitle("Boxplot Zipcode") + theme(axis.text=element_text(size=6), axis.title=element_text(size=6), plot.title=element_text(size=10)) + xlab("Zipcode") + ylab("Sales Price NYC (*10^6)")
    Boxplot_BUILDINGCLASS <- NYC_Train %>% filter(SALEPRICE < 10000000) %>% group_by(SALEPRICE) %>% summarize(price_in_billion = mean(SALEPRICE)/1000000 , Building = as.character(first(BUILDINGCLASSSALE))) %>% ggplot(aes(Building, price_in_billion)) + geom_boxplot() + ggtitle("Boxplot Building Class") + theme(axis.text=element_text(size=6), axis.title=element_text(size=6), plot.title=element_text(size=10)) + xlab("Building Class at Sale") + ylab("Sales Price NYC (*10^6)")
  ##Grid of the different graphs
    library(gridExtra)
    grid.arrange(hist_BOROUGH, hist_TAXCLASS,scat_YEARBUILT, scat_Residential,scat_Commercial,Boxplot_MonthlySale,Boxplot_Borough, Boxplot_Zipcode, Boxplot_BUILDINGCLASS, layout_matrix = matrix(c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 9), byrow=TRUE, ncol=6))

#MODELLING Approach

  ##RMSE function
    RMSE <- function(true_salesprice, predicted_salesprice){sqrt(mean((true_salesprice - predicted_salesprice)^2))}

  ##Building the recommendation system
    
    ###Simple Model: Just the Average
    mu_hat <- mean(NYC_Train$SALEPRICE)
    
      ###How well does this model
      naive_rmse <- RMSE(mu_hat, NYC_Validation$SALEPRICE)
      naive_rmse

      ###Table that stores the result of the different model
      rmse_results <- data.frame(Method = "Just the average", RMSE = naive_rmse)
      rmse_results %>% knitr::kable()

    ###Model 1: Borough Effect
    mu <- mean(NYC_Train$SALEPRICE)
    borough_avgs <- NYC_Train %>% group_by(BOROUGH) %>% summarize(b_b = mean(SALEPRICE-mu))
    borough_avgs %>% qplot(b_b, geom ="histogram", bins = 10, data = ., color = I("black"))
    predicted_salesprice <- mu + NYC_Validation %>% left_join(borough_avgs, by='BOROUGH') %>% .$b_b
    model_1_rmse <- RMSE(NYC_Validation$SALEPRICE, predicted_salesprice)

      ###Update Table
      rmse_results <- bind_rows(rmse_results, data.frame(Method="BOROUGH Effect Model", RMSE = model_1_rmse))
      rmse_results %>% knitr::kable()

    ###Model 2: COMMERCIAL Effect
    NYC_Train %>% group_by(COMMERCIAL) %>% summarize(b_c = mean(SALEPRICE)) %>% filter(n()>=10) %>% ggplot(aes(b_c)) + geom_histogram(bins = 30, color = "black")
    commercial_avgs <- NYC_Train %>% left_join(borough_avgs, by='BOROUGH') %>% group_by(COMMERCIAL) %>% summarize(b_c = mean(SALEPRICE - mu - b_b))
    predicted_salesprice <- NYC_Validation %>% left_join(borough_avgs, by='BOROUGH') %>% left_join(commercial_avgs, by='COMMERCIAL') %>% mutate(pred = mu + b_b + b_c) %>% .$pred
    model_2_rmse <- RMSE(NYC_Validation$SALEPRICE, predicted_salesprice)

      ###Update Table
      rmse_results <- bind_rows(rmse_results, data.frame(Method="BOROUGH & COMMERCIAL Effects Model", RMSE = model_2_rmse))
      rmse_results %>% knitr::kable()

    ###Model 3: Building Effect
    NYC_Train %>% group_by(BUILDING) %>% summarize(b_u = mean(SALEPRICE)) %>% filter(n()>=10) %>% ggplot(aes(b_u)) + geom_histogram(bins = 30, color = "black")
    building_avgs <- NYC_Train %>% left_join(borough_avgs, by='BOROUGH') %>% left_join(commercial_avgs, by='COMMERCIAL') %>% group_by(BUILDING) %>% summarize(b_u = mean(SALEPRICE - mu - b_b - b_c))
    predicted_salesprice <- NYC_Validation %>% left_join(borough_avgs, by='BOROUGH') %>% left_join(commercial_avgs, by='COMMERCIAL') %>% left_join(building_avgs, by='BUILDING') %>% mutate(pred = mu + b_b + b_c + b_u) %>% .$pred
    model_3_rmse <- RMSE(NYC_Validation$SALEPRICE, predicted_salesprice)

      ###Update Table
      rmse_results <- bind_rows(rmse_results, data.frame(Method="BOROUGH & COMMERCIAL & BUILDING CLASS Effects Model", RMSE = model_3_rmse ))
      rmse_results %>% knitr::kable()

    ###Model 4: Zip Code Effect
    NYC_Train %>% group_by(ZIPCODE) %>% summarize(b_z = mean(SALEPRICE)) %>% ggplot(aes(b_z)) + geom_histogram(bins = 3, color = "black")
    zipcode_avgs <- NYC_Train %>% left_join(borough_avgs, by='BOROUGH') %>% left_join(commercial_avgs, by='COMMERCIAL') %>% left_join(building_avgs, by='BUILDING') %>% group_by(ZIPCODE) %>% summarize(b_z = mean(SALEPRICE - mu - b_b - b_c -b_u))
    predicted_salesprice <- NYC_Validation %>% left_join(borough_avgs, by='BOROUGH') %>% left_join(commercial_avgs, by='COMMERCIAL') %>% left_join(building_avgs, by='BUILDING') %>% left_join(zipcode_avgs, by='ZIPCODE') %>% mutate(pred = mu + b_b + b_c + b_u + b_z) %>% .$pred
    model_4_rmse <- RMSE(NYC_Validation$SALEPRICE, predicted_salesprice)
 
      ###Update Table
      rmse_results <- bind_rows(rmse_results, data.frame(Method="BOROUGH & COMMERCIAL & BUILDING CLASS & ZIPCODE Effects Model", RMSE = model_4_rmse ))
      rmse_results %>% knitr::kable()

    ###Model 5: Regularize the predicotr effects
    
      ###First we have to pick the optimal tuning parameter lambda. 
      lambdas <- seq(0, 50000, 250)

      rmses <- sapply(lambdas, function(l){
  
        mu <- mean(NYC_Train$SALEPRICE)
        b_b <- NYC_Train %>% group_by(BOROUGH) %>% summarize(b_b = sum(SALEPRICE-mu)/(n()+l))
        b_u <- NYC_Train %>% left_join(b_b, by="BOROUGH") %>% group_by(BUILDING) %>% summarize(b_u = sum(SALEPRICE - b_b - mu)/(n()+l))
        b_z <- NYC_Train %>% left_join(b_b, by="BOROUGH") %>% left_join(b_u, by="BUILDING") %>% group_by(ZIPCODE) %>% summarize(b_z = sum(SALEPRICE - b_u - b_b - mu)/(n()+l))
        b_c <- NYC_Train %>% left_join(b_b, by="BOROUGH") %>% left_join(b_u, by="BUILDING") %>% left_join(b_z, by="ZIPCODE") %>% group_by(COMMERCIAL) %>% summarize(b_c = sum(SALEPRICE - b_z - b_u - b_b - mu)/(n()+l))
  
        predicted_sales_price <- NYC_Validation %>% left_join(b_b, by="BOROUGH") %>% left_join(b_u, by = "BUILDING") %>% left_join(b_z, by="ZIPCODE") %>% left_join(b_c, by="COMMERCIAL") %>% mutate(pred = mu + b_b + b_u + b_z + b_c) %>% pull(pred)
        
        return(RMSE(NYC_Validation$SALEPRICE,predicted_sales_price))})
      
      qplot(lambdas, rmses)

      ###The optimal lambda is:
      lambda <- lambdas[which.min(rmses)]
      lambda

      ###Update Table
      model_5_rmse <- min(rmses)
      rmse_results <- bind_rows(rmse_results, data.frame(Method="Regularized BOROUGH & COMMERCIAL & BUILDING CLASS & ZIPCODE Effects Model", RMSE = min(rmses)))
      rmse_results %>% knitr::kable()

# Results Section
rmse_results %>% knitr::kable()