library(tidyverse)
Airbnb_raw <- read_csv("C:\\Users\\Parham\\Desktop\\listings.csv")
Airbnb_DT <- Airbnb_raw %>%
  select(id, name, neighbourhood, last_scraped,
         host_name, host_location, host_id, host_since, host_listings_count, host_is_superhost,
         host_response_time, host_response_rate, host_acceptance_rate, host_verifications,
         latitude, longitude, property_type, room_type, accommodates, bedrooms, beds,price,
         bathrooms_text, amenities, number_of_reviews, first_review, last_review, review_scores_rating,
         review_scores_cleanliness, review_scores_location, review_scores_value,
         review_scores_communication, instant_bookable)
rm(Airbnb_raw)
glimpse(Airbnb_DT)





Airbnb_DT$amenities[1]
Airbnb_DT$amenities[2]

?str_split
x<-str_split(Airbnb_DT$amenities[1],",")

x<-gsub(" ", "", Airbnb_DT$amenities[1])
x<-strsplit(x, ",")[[1]]
x
z<-c()
for(i in x)
{
  print(gsub("[^0-9A-Za-z///' ]","'" , i))
  x<-gsub("[^0-9A-Za-z///' ]","'" , i)
  x<-gsub("'","",x)
  z = c(z,x)
}
z
table(z)

p<-""
k <- c()
for(j in 1:nrow(Airbnb_DT))
{
  x<-gsub(" ", "", Airbnb_DT$amenities[j])
  x<-strsplit(x, ",")[[1]]
  z<-c()
  for(i in x)
  {
    #print(gsub("[^0-9A-Za-z///' ]","'" , i))
    p<-gsub("[^0-9A-Za-z///' ]","'" , i)
    p<-gsub("'","",p)
    z = c(z,p)
  }
  k<-c(k,z)
}
tb<-as.tibble(table(k))
dim(as.matrix(k))
tb <- tb %>%
  arrange(desc(n))
View(tb)
tb%>%
  head(10)

write_csv(tb,"amenities.csv")
filter(tb,n==1)

#?grepl()
for (i in 1:length(tb$k))
{
  if(grepl("tv",tb$k[i],ignore.case = TRUE) == TRUE & i!=27 & i!=38)
  {
    tb[27,2] = tb[27,2] + tb[i,2]
    print(i)
    tb[i,2] = 0
  }
}

View(tb)
tb<-filter(tb,n!=0)
write_csv(tb,"amenities.csv")



f<-function(amint,j,data)
{
  for (i in 1:length(data$k))
  {
    if(grepl(amint,data$k[i],ignore.case = TRUE) == TRUE & i!=j)
    {
      data[j,2] = data[j,2] + data[i,2]
      #print(i)
      data[i,2] = 0
    }
  }
  return(data)
}
tb<-f("wifi",1,tb)
tb<-filter(tb,n!=0)
write_csv(tb,"amenities.csv")


tb<-f("shampoo",13,tb)
tb<-filter(tb,n!=0)
write_csv(tb,"amenities.csv")

arrange(tb,desc(n))



#for the next part
new<-Airbnb_DT%>%
  mutate(wifi = ifelse(grepl("wifi",amenities,ignore.case = TRUE) == TRUE,TRUE,FALSE))
new%>%
  relocate(wifi)
ggplot(new)+
  geom_boxplot(aes(wifi,review_scores_rating))

g<-function(amint,data)
{
  new<-data%>%
    mutate(x = ifelse(grepl(amint,amenities,ignore.case = TRUE) == TRUE,TRUE,FALSE))
  #new%>%
  #  relocate(amint)
  ggplot(new)+
    geom_boxplot(aes(x,review_scores_rating))
}

g("Smoke alarm",Airbnb_DT)
g("wifi",Airbnb_DT)
g("Kitchen",Airbnb_DT)

