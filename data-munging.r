library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(zoo)
library(ggmap) # for visualization
library(leaflet) # for visualization
library(htmltools) # for visualization

estimate_roi_nyc<-function(file_listing,file_prop_price,file_cost){

  listing<-fread(file_listing) # Airbnb listing data
  prop_price<-fread(file_prop_price) # Zillow property price data
  cost<-fread(file_cost,select = c(2,6),skip=1,col.names = c('zipcode','monthly_cost')) # US gov't housing cost data

  # We will use these columns in the airbnb listing data.
  listing_col<-c("id","last_scraped","name","summary","description",
                 "neighbourhood_cleansed","neighbourhood_group_cleansed",
                 "state","zipcode","latitude","longitude","is_location_exact",
                 "property_type","room_type",
                 "accommodates","bathrooms","bedrooms","square_feet",
                 "price","weekly_price","monthly_price","cleaning_fee","extra_people",
                 "minimum_nights","maximum_nights",
                 "calendar_updated","availability_30","availability_60","availability_90" ,"availability_365","calendar_last_scraped",
                 "number_of_reviews","first_review","last_review",
                 "review_scores_rating","review_scores_location","review_scores_value",
                 "reviews_per_month")
  listing<-select(listing,listing_col)

  ## Data Cleaning:
  ### Airbnb Listing Data
  # 1.Zip code: 
  # a) Remove empty values, take 5 digtis and take the value within NYC.
  listing<-filter(listing,zipcode!='')%>%
    mutate(zipcode=as.integer(str_sub(zipcode,start = 1,end = 5)))%>%
    filter(!is.na(zipcode),zipcode>10000,zipcode<11437)

  # b) Take care of the zip code with multiple boroughs/neighbourhood.
  # List the zip code which have more than 1 boroughs and the most frequent boroughs
  mode_boroughs<-group_by(listing,zipcode,neighbourhood_group_cleansed)%>%
    summarise(n=n())%>%
    group_by(zipcode)%>%
    summarise(n=n(),nb=names(which.max(table(neighbourhood_group_cleansed))))%>%
    filter(n>1)
  # List the zip code which have more than 1 neghbourhood and the most frequent neighbourhood
  mode_neighbourhood<-group_by(listing,zipcode,neighbourhood_cleansed)%>%
    summarise(n=n())%>%
    group_by(zipcode)%>%
    summarise(n=n(),nb=names(which.max(table(neighbourhood_cleansed))))%>%
    filter(n>1)
  # Assignning the most frequent value of borough and neighbourhood to the zipcode which have multiple boroughs / neighbourhoods
  listing<-merge(listing,mode_boroughs,all.x = T)%>%
    mutate(boroughs=ifelse(!is.na(nb),nb,neighbourhood_group_cleansed))%>%select(-c(n,nb))%>%
    merge(mode_neighbourhood,all.x = T)%>%
    mutate(neighbourhood=ifelse(!is.na(nb),nb,neighbourhood_cleansed))%>%select(-c(n,nb))
  # Still, when checking the neighbourhood and boroughs, there are neigbourhood which has multiple boroughs. 
  listing$boroughs<-ifelse(floor(listing$zipcode/100)==100,'Manhattan', 
                    ifelse(floor(listing$zipcode/100)==112,'Brooklyn',
                    ifelse(floor(listing$zipcode/100)==104,'Bronx',
                    ifelse(floor(listing$zipcode/100)==103,'Staten Island',
                    'Queens'))))


  # 2.Number of bedrooms
  # a) Checking the median value of accommodates.
  group_by(listing, bedrooms)%>%
    summarise(median(accommodates))

  # b) Define a function : fill_bedrooms to fill the missing values
  fill_bedrooms<-function(x){
    n<-floor(x/2)
    return(ifelse(n==0,1,n))
  }

  # c) Update column:bedrooms, by filling missing value and setting 1 to those room type is private room.
  listing<-mutate(listing,bedrooms=ifelse(is.na(bedrooms),fill_bedrooms(accommodates),bedrooms))%>%
    mutate(bedrooms=ifelse(room_type=='Private room',1,bedrooms))

  # 3.Price
  # a) Change price data type to numeric
  price_col=c(19:23)
  listing[,price_col] <- data.frame(apply(select(listing,price_col), 2, 
                                       function(x) as.numeric(str_remove_all(x,'[,$]'))))
  # b) Fill missing value with 0
  listing$cleaning_fee<-na.fill(listing$cleaning_fee,0)

  # c) remove the listing which daily price is more than weekly/monthly price, and listing having daily price higher than $9000
  listing<-filter(listing,is.na(weekly_price) | price<weekly_price)%>%
    filter(is.na(monthly_price) | price<monthly_price)%>%
    filter(price<9000)


  # 4.Remove the listings which are not active in the coming year anymore
  listing<-filter(listing,availability_365!=0,calendar_updated!='never')

  # 5.Add three indicaters
  listing$two_bedroom<-as.numeric(listing$bedrooms==2)
  listing$entire<-as.numeric(listing$room_type=='Entire home/apt')
  listing$interest<-ifelse(listing$entire==1 & listing$two_bedroom==1,'2B Entire Home','Others')

  # 6.Longitude and Latitude
  # For those GPS is not correct, their location will be replaced by the mean value of longitudes and latitudes of the listings 
  # in the same zip code region assuming properties in the same zip code region are similar. 
  # location correction
  listing<-mutate(listing,
                  longitude=ifelse(is_location_exact=='f',NA,longitude),
                  latitude=ifelse(is_location_exact=='f',NA,latitude))%>%
    group_by(zipcode)%>%
    mutate(longitude=ifelse(is.na(longitude),mean(longitude,na.rm=T),longitude),
           latitude=ifelse(is.na(latitude),mean(latitude,na.rm=T),latitude))


  ### Property Price Data
  #Price informaiton in New York City is remained. 
  #Average annual appreciation is calculated from avearage increase rate since 2010 June. 
  #The transformed data has 4 colums: 1) zip code `RegionName`, 2) `CountyName`, 3) Property price`ppt_price` and 4) `annual_increase_rate`
  city_name<-'New York'
  prop_price<-filter(prop_price,City==city_name)
  prop_price<-select(prop_price,RegionName,CountyName,c(8:262))%>%
    gather(month,ppt_price,c(3:257))%>%
    mutate(month=as.yearmon(month))%>%
    filter(as.numeric(month)>as.numeric(as.yearmon('Jun 2010')))%>%
    group_by(RegionName,CountyName)%>%
    mutate(lag_ppt_price=shift(ppt_price,12),
           diff_price=(ppt_price-lag_ppt_price),
           increse_rate=diff_price/ppt_price)%>%
    summarise(ppt_price=last(ppt_price),
              annual_increase_rate=mean(increse_rate,na.rm = T))


  ### Property Cost Data
  #Data type should be numeric. Annual cost is calculated based on the monthly data.
  cost<-mutate(cost,
               annual_cost=as.numeric(str_replace_all(monthly_cost,'[/,/+\\s]',""))*12)%>%
    select(-monthly_cost)%>%drop_na()


  ### Income: Annual Revenue Estimation
  # Rent price consists of the rent price charged per day and the cleansing fee charged per stay. 
  # 1) We estimate the actual daily price for each listing as the average daily amount of a stay with minimum days. 
  # 2) The annual rent revenue for each listing is estimated by the actual daily price and a 75% occupancy rate in 365 days. 
  # 3) The estimated annual rent revenue for each zip code is estimated by the median value among the listings in that zip code.
  listing<-listing%>%
    mutate(revenue_per_stay=price*minimum_nights+cleaning_fee,
           revenue_per_stay_per_day=revenue_per_stay/minimum_nights,
           revenue_year=revenue_per_stay_per_day*365*0.75)

  agg<-group_by(listing,entire,two_bedroom,boroughs,neighbourhood,zipcode)%>%
    summarise(n=n(),
              latitude=mean(latitude),
              longitude=mean(longitude),
              rev_per_stay_daily_mean=mean(revenue_per_stay_per_day),
              rev_per_stay_daily_median=median(revenue_per_stay_per_day),
              revenue=median(revenue_year), 
              average_daily_available_365=sum(availability_365/365))

  ### ROI 
  # 1) Select the 2B entire listing, 2) merge property cost and 3)property price
  # 4) within same neighbourhood: impute missing value with median value for annual cost and property price, annual property appreciation rate
  # 5) within same borough: impute missing value with median value for annual cost and property price, annual property appreciation rate
  # 6) calculate return ratio and cash return ratio
  agg2<-filter(agg,entire==1,two_bedroom==1)%>%
    merge(cost,all.x = T)%>% 
    merge(prop_price,all.x = T,by.x = 'zipcode',by.y = 'RegionName')%>% 
    group_by(neighbourhood)%>% 
    mutate(ppt_price=ifelse(is.na(ppt_price),
                            median(ppt_price,na.rm = T),
                            ppt_price),
           annual_increase_rate=ifelse(is.na(annual_increase_rate),
                                       median(annual_increase_rate,na.rm = T),
                                       annual_increase_rate),
           annual_cost=ifelse(is.na(annual_cost),
                              median(annual_cost,na.rm = T),
                              annual_cost))%>%
    ungroup()%>%
    group_by(boroughs)%>% 
    mutate(ppt_price=ifelse(is.na(ppt_price),
                            median(ppt_price,na.rm = T),
                            ppt_price),
           annual_increase_rate=ifelse(is.na(annual_increase_rate),
                                       median(annual_increase_rate,na.rm = T),
                                       annual_increase_rate),
           annual_cost=ifelse(is.na(annual_cost),
                              median(annual_cost,na.rm = T),
                              annual_cost))%>%
    ungroup()%>%
    mutate(return_ratio=(ppt_price*annual_increase_rate-annual_cost+revenue)/ppt_price,
           cash_ratio=return_ratio-annual_increase_rate)

    return(agg2)
}


