# Read data and construct a simple model
adata<-read.csv("analysisData.csv",header=TRUE,stringsAsFactors=FALSE,na.strings = c("NA","N/A",""))
# Read scoring data and apply model to generate predictions
sdata = read.csv('scoringData.csv',header=TRUE,stringsAsFactors=FALSE,na.strings = c("NA","N/A",""))

#FOR EVERY DATA TRANSFORMATION I MADE IN THE TESTING DATA, I WILL ALSO APPLY THE TRANSFORMATION IN THE TESTING DATA.
#clean analysisData
adata$time_since_now <- as.Date(adata$host_since)
adata$time_since_now <- as.numeric(adata$time_since_now)
#vice versa
sdata$time_since_now <- as.Date(sdata$host_since)
sdata$time_since_now <- as.numeric(sdata$time_since_now)

#data cleaning to zeros
#cleaning_fee,beds,
adata$cleaning_fee[which(is.na(adata$cleaning_fee))] <- 0
adata$beds[which(is.na(adata$beds))] <- 0
adata$bed_type <- as.factor(adata$bed_type)
#vice versa
sdata$cleaning_fee[which(is.na(sdata$cleaning_fee))] <- 0
sdata$beds[which(is.na(sdata$beds))] <- 0
sdata$bed_type <- as.factor(sdata$bed_type)

#host response rate
adata$host_response_rate <- as.factor(adata$host_response_rate);
#vice versa
sdata$host_response_rate <- as.factor(sdata$host_response_rate);


#character count manipulation
adata$name <- nchar(adata$name)
adata$summary <- nchar(adata$summary)
adata$space <- nchar(adata$space)
adata$description <- nchar(adata$description)
#vice versa
sdata$name <- nchar(sdata$name)
sdata$summary <- nchar(sdata$summary)
sdata$space <- nchar(sdata$space)
sdata$description <- nchar(sdata$description)

#zipcode, theres na's
adata$zipcode[which(is.na(adata$zipcode))] <- 0
adata$zipcode <- as.factor(adata$zipcode)
#vice versa
sdata$zipcode[which(is.na(sdata$zipcode))] <- 0
sdata$zipcode <- as.factor(sdata$zipcode)

#instant_bookable manipulation; as.factor is better than as.numeric
adata$instant_bookable <-ifelse(adata$instant_bookable=="t",1, 0)
#vice versa
sdata$instant_bookable <-ifelse(sdata$instant_bookable=="t",1, 0)

#host_identity_verified
adata$host_identity_verified <-ifelse(adata$host_identity_verified=="t",1, 0)
#vice versa
sdata$host_identity_verified <-ifelse(sdata$host_identity_verified=="t",1, 0)

#host_verifications
adata$amenities_Wifi = grepl("Wifi", adata$amenities)
adata$amenities_Air_conditioning = grepl("Air conditioning", adata$amenities)
adata$amenities_Kitchen = grepl("Kitchen", adata$amenities)
adata$amenities_Elevator = grepl("Elevator", adata$amenities)
adata$amenities_Washer = grepl("Washer", adata$amenities)
adata$amenities_Dryer = grepl("Dryer", adata$amenities)
adata$amenities_TV = grepl("TV", adata$amenities)
adata$amenities_Wifi <-ifelse(adata$amenities_Wifi =="TRUE",1, 0)
adata$amenities_Air_conditioning <-ifelse(adata$amenities_Air_conditioning =="TRUE",1, 0)
adata$amenities_Kitchen <-ifelse(adata$amenities_Kitchen =="TRUE",1, 0)
adata$amenities_Elevator <-ifelse(adata$amenities_Elevator =="TRUE",1, 0)
adata$amenities_Washer <-ifelse(adata$amenities_Washer =="TRUE",1, 0)
adata$amenities_Dryer <-ifelse(adata$amenities_Dryer =="TRUE",1, 0)
adata$amenities_TV <-ifelse(adata$amenities_TV =="TRUE",1, 0)
#vice versa
sdata$amenities_Wifi = grepl("Wifi", sdata$amenities)
sdata$amenities_Air_conditioning = grepl("Air conditioning", sdata$amenities)
sdata$amenities_Kitchen = grepl("Kitchen", sdata$amenities)
sdata$amenities_Elevator = grepl("Elevator", sdata$amenities)
sdata$amenities_Washer = grepl("Washer", sdata$amenities)
sdata$amenities_Dryer = grepl("Dryer", sdata$amenities)
sdata$amenities_TV = grepl("TV", sdata$amenities)
sdata$amenities_Wifi <-ifelse(sdata$amenities_Wifi =="TRUE",1, 0)
sdata$amenities_Air_conditioning <-ifelse(sdata$amenities_Air_conditioning =="TRUE",1, 0)
sdata$amenities_Kitchen <-ifelse(sdata$amenities_Kitchen =="TRUE",1, 0)
sdata$amenities_Elevator <-ifelse(sdata$amenities_Elevator =="TRUE",1, 0)
sdata$amenities_Washer <-ifelse(sdata$amenities_Washer =="TRUE",1, 0)
sdata$amenities_Dryer <-ifelse(sdata$amenities_Dryer =="TRUE",1, 0)
sdata$amenities_TV <-ifelse(sdata$amenities_TV =="TRUE",1, 0)


#change house_rule to num variable
adata$house_rules = nchar(as.character(adata$house_rules))
adata$house_rules[which(is.na(adata$house_rules))] <- 0
#vice versa
sdata$house_rules = nchar(as.character(sdata$house_rules))
sdata$house_rules[which(is.na(sdata$house_rules))] <- 0

#property type
adata$property_type <- as.factor(adata$property_type)
#vice versa
sdata$property_type <- as.factor(sdata$property_type)

#property type
adata$room_type <- as.factor(adata$room_type)
#vice versa
sdata$room_type <- as.factor(sdata$room_type)

# is business travel read SUCKS no variation
adata$is_business_travel_ready <- as.factor(adata$is_business_travel_ready)
#vice versa
sdata$is_business_travel_ready <- as.factor(sdata$is_business_travel_ready)

#cancellation_policy 
adata$cancellation_policy <- as.factor(adata$cancellation_policy)
#vice versa sucks
sdata$cancellation_policy <- as.factor(sdata$cancellation_policy)


library(gbm)
set.seed(617)
boost = gbm(price~house_rules+property_type+room_type+is_business_travel_ready+
              amenities_Wifi+amenities_Air_conditioning+amenities_Kitchen+amenities_Elevator+amenities_Washer+amenities_Dryer+amenities_TV+
              instant_bookable+host_identity_verified+number_of_reviews+cancellation_policy+
              zipcode+time_since_now+cleaning_fee+host_response_rate+
              name+summary+space+description+
              accommodates+bathrooms+bedrooms+beds+bed_type+
              guests_included+extra_people+minimum_nights+maximum_nights+
              review_scores_rating+review_scores_accuracy+review_scores_cleanliness+
              review_scores_checkin+review_scores_communication+review_scores_location+review_scores_value,
            data=adata,distribution="gaussian",
            n.trees = 10000,interaction.depth = 30,shrinkage = 0.01)
predBoostTrain = predict(boost,n.trees = 1000)
rmseBoostTrain = sqrt(mean((predBoostTrain-adata$price)^2)); 
rmseBoostTrain

#10,000 tres is the best
#depth, the higher the better
#shrinkage the bigger the better, no larger than 1

pred_boost_test<-predict(boost,n.trees=10000,newdata=sdata)
submissionfile<-data.frame(id=sdata$id,price=pred_boost_test)
write.csv(submissionfile,"november191.csv",row.names=FALSE)


################################################
# THE FOLLOWING IS DISCARDED CODE
################################################
#library(gbm)
#set.seed(617)
# boost = gbm(earn~.,data=train,distribution="gaussian",
#             n.trees = 100000,interaction.depth = 3,shrinkage = 0.001)
# predBoostTrain = predict(boost,n.trees = 100000)
# rmseBoostTrain = sqrt(mean((predBoostTrain-train$earn)^2)); rmseBoostTrain

################################################
# Comparison of GBM and GBM with CV
# library(gbm)
# set.seed(617)
# boost = gbm(earn~.,data=train,distribution="gaussian",
#             n.trees = 100000,interaction.depth = 3,shrinkage = 0.001)
# predBoostTrain = predict(boost,n.trees = 100000)
# rmseBoostTrain = sqrt(mean((predBoostTrain-train$earn)^2)); rmseBoostTrain
# 
# library(caret)
# set.seed(617)
# trControl=trainControl(method="cv",number=10)
# tuneGrid=  expand.grid(n.trees = 1000, interaction.depth = c(1,2),
#                        shrinkage = (1:100)*0.001,n.minobsinnode=5)
# garbage = capture.output(cvBoost <- train(earn~.,data=train,method="gbm", 
#                                           trControl=trControl, tuneGrid=tuneGrid))
# boostCV = gbm(earn~.,data=train,distribution="gaussian",
#               n.trees=cvBoost$bestTune$n.trees,
#               interaction.depth=cvBoost$bestTune$interaction.depth,
#               shrinkage=cvBoost$bestTune$shrinkage,
#               n.minobsinnode = cvBoost$bestTune$n.minobsinnode)
# predBoostCV = predict(boostCV,test,n.trees=1000)
# rmseBoostCV = sqrt(mean((predBoostCV-test$earn)^2)); rmseBoostCV


#between review time has low variable importance
# adata$between_review_time <- as.Date(adata$last_review) - as.Date(adata$first_review)
# adata$between_review_time <- as.numeric(adata$between_review_time)
#host verifications has low variable importance
# adata$host_verifications <- nchar(adata$host_verifications)
#superhost has low variable importance
# adata$host_is_superhost <- as.factor(adata$host_is_superhost)
#is location exact has low variable importance
# adata$is_location_exact <- as.factor(adata$is_location_exact)
#host neighborhood has low variable importance
# adata$host_neighbourhood = as.factor(adata$host_neighbourhood)
#has_availability manipulation #this factor sucks
#adata$has_availability <- as.factor(adata$has_availability)#this factor sucks




