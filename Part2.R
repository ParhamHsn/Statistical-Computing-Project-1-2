library(tidyverse)
library(data.table)
Airbnb_raw <- fread(paste0("C:/Users/Parham/Desktop/listings.csv"))
Airbnb_DT <- Airbnb_raw %>%
  select(id, name, neighbourhood, last_scraped,
         host_name, host_location, host_id, host_since, host_listings_count, host_is_superhost,
         host_response_time, host_response_rate, host_acceptance_rate, host_verifications,
         latitude, longitude, property_type, room_type, accommodates, bedrooms, beds,price,
         bathrooms_text, amenities,number_of_reviews, first_review, last_review, review_scores_rating,
         review_scores_cleanliness, review_scores_location, review_scores_value,
         review_scores_communication, instant_bookable)
rm(Airbnb_raw)
glimpse(Airbnb_DT)

str(Airbnb_DT$price)
Airbnb_DT <- Airbnb_DT %>%
  mutate(price = as.numeric(gsub("[\\$,]", "", price)) , 
         host_acceptance_rate = as.numeric(gsub("[\\%,]", "", host_acceptance_rate)),
         host_response_rate = as.numeric(gsub("[\\%,]", "", host_response_rate))
         )


Airbnb_DT %>%
  ggplot(aes(x = price)) +
  geom_histogram() +
  labs(title = "Histogram of the listings price")

summary(Airbnb_DT$price)
which(max(Airbnb_DT$price) == Airbnb_DT$price)
n = dim(Airbnb_DT)[1]
sum(Airbnb_DT$price > 1250)
indx <- which(Airbnb_DT$price > 1250) #1250 or 1000
indx
View(Airbnb_DT[indx,])

Airbnb_DT[-indx,] %>%
  ggplot(aes(x = price)) +
  geom_histogram() +
  labs(title = "Histogram of the listings price")

View(head(Airbnb_DT))





Airbnb_DT[-indx,] %>%
  ggplot(aes(x = beds , y = price)) +
  geom_point() +
  geom_jitter()+
  geom_smooth()
  labs(title = "Price vs Beds")

  

fit <- lm(price ~ beds ,data = Airbnb_DT)
summary(fit)
par(mfrow = c(2,2))
plot(fit)




Airbnb_DT[-indx,] %>%
  ggplot(aes(x = number_of_reviews , y = price)) +
  geom_point()+
  geom_smooth()
labs(title = "Price vs number_of_reviews")

fit <- lm(price ~ number_of_reviews ,data = Airbnb_DT)
summary(fit)
par(mfrow = c(2,2))
plot(fit)



Airbnb_DT[-indx,] %>%
  ggplot(aes(x = review_scores_rating , y = price)) +
  geom_point()+
  geom_smooth()+
labs(title = "Price vs review_scores_rating")

fit <- lm(price ~ review_scores_rating + I(review_scores_rating^2),data = Airbnb_DT)
summary(fit)
par(mfrow = c(2,2))
plot(fit)


Airbnb_DT[-indx,] %>%
  ggplot(aes(x = accommodates , y = log(price))) +
  geom_point()+
  geom_jitter()+
  geom_smooth()+
labs(title = "Price vs accommodates")

fit <- lm(log(price) ~ accommodates + I(accommodates^2),data = Airbnb_DT)
summary(fit)
par(mfrow = c(2,2))
plot(fit)


Airbnb_DT[-indx,] %>%
  ggplot(aes(x = host_acceptance_rate , y = price)) +
  geom_point()+
  geom_jitter()+
  labs(title = "Price vs host_acceptance_rate")

Airbnb_DT[-indx,] %>%
  ggplot(aes(x = host_response_rate , y = price)) +
  geom_point()+
  geom_jitter()+
  labs(title = "Price vs host_response_rate")


fit <- lm(log(price) ~ host_acceptance_rate,data = Airbnb_DT)
summary(fit)
par(mfrow = c(2,2))
plot(fit)
attach(Airbnb_DT)

fit <- lm(log2(price) ~ accommodates + bedrooms + review_scores_rating + number_of_reviews + longitude + latitude,data = Airbnb_DT[-indx,])
summary(fit)
d=cbind(price,host_response_rate,host_acceptance_rate,bedrooms,beds,host_listings_count,accommodates,review_scores_rating,number_of_reviews,longitude,latitude)
cor(d,use = "na.or.complete")
View(cor(d,use = "na.or.complete"))
par(mfrow = c(2,2))
plot(fit)


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
  k<-c(k,length(z))
}
Airbnb_DT <- Airbnb_DT%>%
  mutate(amenities_length = k)
fit<-lm(price ~ amenities_length ,data = Airbnb_DT[-indx,])
summary(fit)
Airbnb_DT[-indx,]%>%
  ggplot(aes(x = amenities_length , y = price))+
  geom_point()+
  geom_jitter()+
  geom_smooth()
par(mfrow=c(2,2))
plot(fit)

attach(Airbnb_DT)
fit <- lm(log(price) ~ accommodates + I(accommodates^2) + bedrooms + review_scores_rating + number_of_reviews + s(longitude) + s(latitude) + amenities_length,data = Airbnb_DT[-indx,])
summary(fit)

step(fit)
summary(fit)
anova(fit)
par(mfrow=c(2,2))
plot(fit)

library(MASS)
summary(m1 <- glm.nb(price ~ accommodates + I(accommodates^2) + bedrooms + review_scores_rating + number_of_reviews + longitude + latitude + amenities_length, data = Airbnb_DT))

fit2 <- glm(price ~ accommodates + bedrooms + review_scores_rating + number_of_reviews + longitude + latitude + amenities_length, data = Airbnb_DT, family = Gamma("log"))
summary(fit2)

par(mfrow=c(2,2))
plot(fit)
