# loading libraries
library(readxl)
library(janitor)
library(plotly)
library(dplyr)
library(tidyr)
library(stringr)

#reading the dataset, the DoubleClick sheet only
airfrance <- read_excel("Air France Case Spreadsheet Supplement.xls", sheet = "DoubleClick")


#deleting the unused columns 
clean <- subset(airfrance, select = -c(8,9))

#applying janitor to edit column names to easier working format

clean <- clean_names(clean)
names(clean)

#cheching for NAs
sum(is.na(clean$match_type))

# R does not see N/A as NA
# manually getting the indeces for N/A rows
del <- which(clean$match_type == "N/A")
clean <- clean[-del,]


#creating additional columns for Brabded and Non-Branded Keywords 
clean$branded <- c()
brand_ind <- grep("AIRFRANCE|airfrance|Air France|air france", clean$keyword)
clean[brand_ind, "branded"] <- "Branded"
clean[-brand_ind, "branded"] <- "Non-branded"

#creating additional columns for US vs. Global Keywords 
clean$region <- c()
us_ind <- grep("US", clean$publisher_name)
gl_ind <- grep("Global", clean$publisher_name)

clean[us_ind, "region"] <- "US"
clean[gl_ind, "region"] <- "Global"

#creating additional columns for Publishers: Google, Yahoo, MSN, Overture
clean$publishers <-c()

ya_ind <- grep("Yahoo",clean$publisher_name)
go_ind <- grep("Google",clean$publisher_name)
msn_ind <- grep("MSN",clean$publisher_name)
over_ind <- grep("Overture",clean$publisher_name)

clean[ya_ind, "publishers"] <- "Yahoo"
clean[go_ind, "publishers"] <- "Google"
clean[msn_ind, "publishers"] <- "MSN"
clean[over_ind, "publishers"] <- "Overture"

#creating additional columns for Keywords: broad (advanced) vs. exact (standard)
clean$match <- c()

broad_ind <- which(clean$match_type == "Broad")
adv_ind <- which(clean$match_type == "Advanced")

clean[c(broad_ind,adv_ind),"match"] <- "Broad"
clean[-c(broad_ind,adv_ind),"match"] <- "Exact"


#creating ROA variable
#ROA = Amount/Total Cost

clean$ROA <- clean$amount/clean$total_cost
clean$ROA


#creating impression conversion variable
#Impression Conversion % = (Bookings/Impressions)*100

clean$Profit <- clean$amount-clean$total_cost

#there are not so many cases with resulting booking
#we will need to check click conversion as well

#creating binaries for publisher names

clean$Google <- c()

for(i in 1:length(clean$publishers)){
  if(clean$publishers[i] == "Google"){
    clean$Google[i] <- 1
  }else{
    clean$Google[i] <- 0
  }
}


clean$Yahoo <- c()

for(i in 1:length(clean$publishers)){
  if(clean$publishers[i] == "Yahoo"){
    clean$Yahoo[i] <- 1
  }else{
    clean$Yahoo[i] <- 0
  }
}

clean$MSN <- c()

for(i in 1:length(clean$publishers)){
  if(clean$publishers[i] == "MSN"){
    clean$MSN[i] <- 1
  }else{
    clean$MSN[i] <- 0
  }
}


clean$Overture <- c()

for(i in 1:length(clean$publishers)){
  if(clean$publishers[i] == "Overture"){
    clean$Overture[i] <- 1
  }else{
    clean$Overture[i] <- 0
  }
}

#creating binaries for region Us vs Global
clean$US <- c()

clean[us_ind,"US"] <- 1
clean[-us_ind,"US"] <- 0

clean$Global <- c()

clean[-us_ind,"Global"] <- 1
clean[us_ind,"Global"] <- 0

#creating binaries for branded vs. non-branved

clean$brand <- c()

clean[brand_ind,"brand"] <- 1
clean[-brand_ind,"brand"] <- 0

#creating binaries for broad vs. exact

clean$exact_match <- c()

clean[broad_ind, "exact_match"] <- 0
clean[-broad_ind, "exact_match"] <- 1

clean$broad_match <- c()

clean[broad_ind, "broad_match"] <- 1
clean[-broad_ind, "broad_match"] <- 0

#creating binaries column for booking success

clean$Booking_Success <- c()

for(i in 1:length(clean$total_volume_of_bookings)){
  if(clean$total_volume_of_bookings[i] > 0){
    clean$Booking_Success[i] <- 1
  }else{
    clean$Booking_Success[i] <- 0
  }
}

#Only observations with no bookings
zero_bookings <- clean[clean$total_volume_of_bookings== 0,]

#only observations with bookings
only_bookings <-  clean[clean$total_volume_of_bookings!= 0,]

#only observations with bookings in the US
only_bookings_US <- only_bookings[only_bookings$region == "US",]

#only observations with bookings global
only_bookings_global <- only_bookings[only_bookings$region == "Global",]

mean(only_bookings$ROA)

#decending bookings (overall data set)
decending_bookings<- only_bookings[order(-only_bookings$total_volume_of_bookings),]

#decending amount/revenue (only bookings data set)
decending_amount<- only_bookings[order(-only_bookings$amount),]

#only data from US
only_us <- clean[clean$region == "US",]

#only data from global
only_global <- clean[clean$region == "Global",]

#decending Profit (US)
decending_Profit_us<- only_us[order(-only_us$Profit),]

#decending ROA overall dataset
decending_ROA<- clean[order(-clean$ROA),]
nrow(decending_ROA)

#decending ROA only USA
decending_ROA_us<- only_us[order(-only_us$ROA),]

#decending ROA global 
decending_ROA_global <- only_global[order(-only_global$ROA),]

#decending transaction conversion (USA)
decending_trans_conv_us<- only_us[order(-only_us$trans_conv_percent),]

#decending transaction conversion (overall)
decending_trans_conv<- clean[order(-clean$trans_conv_percent),]

#creating profit (overall data set)
clean$Profit <- (as.numeric(clean$amount) - as.numeric(clean$total_cost))

#decending profit (overall data set)
decending_Profit<- clean[order(-clean$Profit),]

#decending profit (USA)
decending_Profit_us<- only_us[order(-only_us$Profit),]

#Testing aggregate function
dataSum <- aggregate(Profit ~ publishers + region, decending_Profit, FUN = 'mean')

#average booking per publisher from only_booked
average_booking_publisher <- aggregate(only_bookings[,20], list(only_bookings$publishers), mean)

#removing ROA outliers
clean <- subset(clean, ROA < 1007)

#################################################################################################################################################
# SUBSETTING CLEAN TO US ONLY
#################################################################################################################################################

clean_US <- subset(clean, US > 0)

#################################################################################################################################################
# SUBSETTING CLEAN TO GLOBAL ONLY
#################################################################################################################################################

clean_global <- subset(clean, Global > 0)

#################################################################################################################################################
# SUBSETTING CLEAN TO SUCCESSFUL BOOKINGS ONLY
#################################################################################################################################################

clean_success <- subset(clean, Booking_Success > 0)

#################################################################################################################################################
# SUBSETTING SUCCESSFUL BOOKINGS IN THE US ONLY
#################################################################################################################################################

clean_success_US <- subset(clean_success, US > 0)

#################################################################################################################################################
# SUBSETTING FAILED BOOKINGS ONLY
#################################################################################################################################################

clean_fail <- subset(clean, Booking_Success == 0)

#################################################################################################################################################
# SUBSETTING SUCCESSFUL BOOKINGS IN GLOBAL ONLY
#################################################################################################################################################

clean_success_global <- subset(clean_success, Global > 0)

########################################################THE END######################################################

# converting dataset back to a dataframe

clean <- as.data.frame(clean)

clean_fail <- as.data.frame(clean_fail)

clean_global <- as.data.frame(clean_global)

clean_US <- as.data.frame(clean_US)


##########################################################################################################################################
# Visual Exploration - PLOTTING CHARTS
##########################################################################################################################################


######### Region Profit & ROA

ggplot(clean, aes(x=region, y=Profit))+
  geom_col()

#proportion of profit coming from the global category
sum(clean_global$Profit)/sum(clean$Profit)

sum(clean_global$ROA)/sum(clean$ROA)

ggplot(clean, aes(x=region, y=ROA,fill= region))+
  stat_summary(fun.y= "mean", geom = "bar")

ggplot(clean, aes(x="Sum", y=Profit,fill= region))+
  stat_summary(fun.y= "sum", geom = "bar", position = "fill" )

ggplot(clean, aes(x="Average", y=Profit,fill= region))+
  stat_summary(fun.y= "mean", geom = "bar", position = "fill" )

ggplot(clean, aes(x="Average", y=ROA,fill= region))+
  stat_summary(fun.y= "mean", geom = "bar", position = "fill" )

########## Top Line Analysis


clean_success$publishers <- factor(clean_success$publishers, levels = c("Google","Yahoo", "MSN", "Overture"))
ggplot(clean_success, aes(x=publishers, y=trans_conv_percent, fill = publishers, fun.y= "mean"))+
  geom_violin()

clean_success_US$publishers <- factor(clean_success_US$publishers, levels = c("Google","Yahoo", "MSN", "Overture"))
ggplot(clean_success_US, aes(x=publishers, y=Booking_Success, fill = publishers))+
  stat_summary(fun.y= mean, geom = "bar")


ggplot(clean_US, aes(x=match, y=ROA, fill = exact_match))+
  stat_summary(fun.y= "mean", geom = "bar")


ggplot(clean_global, aes(x=match, y=ROA, fill = exact_match))+
  stat_summary(fun.y= "mean", geom = "bar",)

ggplot(clean, aes(x=match, y=ROA, fill = exact_match))+
  stat_summary(fun.y= "mean", geom = "bar")

ggplot(data = clean, aes(x = region, y= ROA, fill = publishers))+
  stat_summary(fun.y = mean, geom = "bar", position = "fill" )

clean_success$publishers <- factor(clean_success$publishers, levels = c("Yahoo", "Google", "Overture", "MSN"))
ggplot(clean_success, aes(x=publishers, y=ROA,fill= region, fun.y= "mean"))+
  geom_col()

########## Bottom line analysis

ggplot(clean_fail, aes(x=region, y=Profit,fill= publishers, fun.y= "mean"))+
  geom_col()

clean$publishers <- factor(clean$publishers, levels = c("MSN","Google", "Yahoo", "Overture"))
ggplot(clean, aes(x=publishers, y=avg_cost_per_click,fill= publishers))+
  stat_summary(fun.y= "mean", geom = "bar")


########## US Publishers

clean_US$publishers <- factor(clean_US$publishers, levels = c("Google", "Yahoo", "Overture", "MSN"))

ggplotly(ggplot(clean_US, aes(x=publishers, y=impressions,fill= publishers))+
           stat_summary(fun.y= "sum", geom = "bar"))

ggplotly(ggplot(clean_US, aes(x=publishers, y=trans_conv_percent,fill= publishers))+
           stat_summary(fun.y= "mean", geom = "bar"))

ggplotly(ggplot(clean_US, aes(x=publishers, y=amount,fill= publishers))+
           stat_summary(fun.y= "mean", geom = "bar"))

ggplotly(ggplot(clean_US, aes(x=publishers, y=ROA,fill= publishers))+
           stat_summary(fun.y= "mean", geom = "bar"))


ggplotly(ggplot(clean_US, aes(x=publishers, y=total_cost,fill= publishers))+
           stat_summary(fun.y= "mean", geom = "bar"))

ggplotly(ggplot(clean_US, aes(x=publishers, y=avg_cost_per_click,fill= publishers))+
           stat_summary(fun.y= "mean", geom = "bar"))

ggplotly(ggplot(clean_US, aes(x=publishers, y=total_volume_of_bookings,fill= publishers))+
           stat_summary(fun.y= "mean", geom = "bar"))

sum(clean_US$exact_match)/length(clean_US)

########## Global Publishers

clean_global$publishers <- factor(clean_global$publishers, levels = c("Google", "Yahoo", "Overture", "MSN"))

ggplotly(ggplot(clean_global, aes(x=publishers, y=impressions,fill= publishers))+
           stat_summary(fun.y= "sum", geom = "bar"))

ggplotly(ggplot(clean_global, aes(x=publishers, y=trans_conv_percent,fill= publishers))+
           stat_summary(fun.y= "mean", geom = "bar"))

ggplotly(ggplot(clean_global, aes(x=publishers, y=amount,fill= publishers))+
           stat_summary(fun.y= "mean", geom = "bar"))

ggplotly(ggplot(clean_global, aes(x=publishers, y=ROA,fill= publishers))+
           stat_summary(fun.y= "mean", geom = "bar"))

ggplotly(ggplot(clean_global, aes(x=publishers, y=total_cost,fill= publishers))+
           stat_summary(fun.y= "mean", geom = "bar"))

ggplotly(ggplot(clean_global, aes(x=publishers, y=total_volume_of_bookings,fill= publishers))+
           stat_summary(fun.y= "mean", geom = "bar"))

######### Analysis of outliers


Total_Bookings_per_publisher <- # Get a summary table with counts and sums of booking vol per publisher
  clean %>% #get the clean dataframe...
  group_by(publishers) %>% #Group by Publishers
  summarise(Sum.Bookings = sum(total_volume_of_bookings), # Create columns for sum of volume
            Amount.bookings = sum(amount), # Create columns for sum of amount
            Count.bookings = n()) # and another column for count of bookings

Total_Bookings_per_publisher


keyword_match <- # Get a summary table with counts and sums of booking vol per US-Global Scope
  clean %>% #get the clean dataframe...
  group_by(match) %>% #Group by match type 
  summarise(Sum_Bookings = sum(total_volume_of_bookings), # Create columns for sum of volume
            amount = sum(amount)) # Create columns for sum of amount

keyword_match


#Total positive Profit
positive_net <- subset(clean, clean$Profit>=0)
sum(positive_net$Profit)

#Total losses 
negative_net <- subset(clean, clean$Profit<0)
sum(negative_net$net_Profit)


#Box plot to look at outliers of net revenue

ggplot(decending_Profit, aes("", Profit)) + geom_boxplot()

#Box plot to look at outliers of ROA

ggplot(decending_ROA, aes(publishers, ROA)) + geom_boxplot()  

####We have decided to remove the two highest outliers of ROA belonging to Yahoo (USA) in almost all of our analysis


#bar graph of ROA vs Publisher (USA)
ggplot(decending_ROA_us, aes(x =reorder(publishers,-ROA), y = ROA  , fill =publishers))+ stat_summary(fun.y = "mean", geom = "bar")+
  labs(title = "ROA vs Publisher")+
  xlab("Publisher")+ylab("ROA") +theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8)) 



#bar graph of profit vs publisher (global)
ggplot(decending_ROA_global, aes(x =reorder(publishers,-Profit), y = Profit  , fill =publishers))+ stat_summary(fun.y = "mean", geom = "bar")+
  labs(title = "Profit vs Publisher")+
  xlab("Publisher")+ylab("Profit") + theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8)) 


#Plot of ROA vs Publisher (global) (USING PLOTLY TO LOOK AT values)
ggplotly(ggplot(decending_ROA_global, aes(x =reorder(publishers,-ROA), y = ROA  , fill =publishers))+ stat_summary(fun.y = "mean", geom = "bar")+
           labs(title = "ROA vs Publisher")+ xlab("Publisher")+ylab("Profit"))+ theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8)) 


#bar graph of ROA vs Region 
ggplot(decending_ROA, aes(x =reorder(region,-ROA), y = ROA , fill= region  )) +  stat_summary(fun.y = "mean", geom = "bar")+
  labs(title = "ROA vs Region")+
  xlab("Region")+ylab("Average ROAS")+ theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))


#bar graph of ROA vs Region broken down by publiser
ggplot(decending_ROA, aes(x =reorder(region,-Profit), y = Profit  , fill =publishers)) + stat_summary(fun.y="mean" , geom = "col", fun. = "mean")+labs(title = "Profit vs Region broken down by Publisher")+
  xlab("Region")+ylab("Profit")+ theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))


#bar graph of profit vs region
ggplot(decending_Profit, aes(x =reorder(region,-Profit), y = Profit)) + stat_summary(fun.y="mean" , geom = "bar")


#testing to plot aggregate function created in data "Rub" (Didn't work)
ggplot(dataSum, aes(x =reorder(region,-Profit), y = Profit , fill = publishers)) + geom_col()



#bar graph of ROA vs broad(overall dataset )
ggplot(decending_ROA, aes(x =reorder(branded,-ROA), y = ROA , fill= branded )) +  stat_summary(fun.y = "mean", geom = "bar")+
  labs(title = "ROA vs match type")+
  xlab("Branded")+ylab("Average ROAS") +theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))


#ROA vs broad (USA)
ggplot(decending_ROA_us, aes(x =reorder(match,-ROA), y = ROA , fill= match  )) +  stat_summary(fun.y = "mean", geom = "bar")+
  labs(title = "Average ROA vs Brand")+
  xlab("Branded")+ylab("Average ROAS") + theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))


#ROA vs brand or no brand (global)
ggplot(decending_ROA, aes(x =reorder(branded,ROA), y = ROA , fill= branded  )) +  stat_summary(fun.y = "mean", geom = "bar")+
  labs(title = "Average ROA vs Brand")+
  xlab("Branded")+ylab("Average ROAS")
theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))


#ROA vs publisher (Overall data set)
ggplot(decending_ROA, aes(x =reorder(publishers,-ROA), y = ROA , fill= publishers )) +  stat_summary(fun.y = "mean", geom = "bar")+
  labs(title = "ROA vs Publisher")+
  xlab("Branded")+ylab("Average ROAS")
theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))


#average ROA of bookings (USA)
mean(only_bookings$ROA)


#Publisher with highest number of bookings
ggplot(only_bookings, aes(x =reorder(publishers,-total_volume_of_bookings, FUN = sum), y = total_volume_of_bookings  , fill =campaign)) +geom_col(fill="salmon") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))

sum(only_bookings$total_volume_of_bookings)

#Publisher with highest ROA
ggplot(clean, aes(x =reorder(publishers,-ROA, FUN = sum), y = ROA  , fill =publishers)) +geom_bar(stat = "identity") + 
  labs(title = "ROA vs Publisher")+
  xlab("Publisher")+ylab("ROA")
theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))



#campaign with highest number of bookings
ggplot(only_bookings, aes(x =reorder(campaign,-total_volume_of_bookings, FUN = sum), y = total_volume_of_bookings  , fill =campaign)) +geom_col(fill="salmon") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))

#Campaigns with highest ROA from booked 
ggplot(only_bookings, aes( x = reorder(campaign,-ROA, FUN = sum), y= ROA)) +geom_col(fill="salmon")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))

#best performing keywords in terms of revenue from observations with bookings
ggplot(only_bookings, aes( x = reorder(keyword,-amount), y= amount)) +geom_col(fill="salmon")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))

#Worst performing keywords from bookings
nrow(decending_bookings)
ggplot(decending_bookings[355:365,], aes( x = reorder(keyword,-amount), y= amount)) +geom_col(fill="salmon")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))

# top  keywords in terms of amount
ggplot(decending_amount[1:30,], aes( x = reorder(keyword,-amount,FUN = sum), y= amount )) +geom_col(fill="salmon")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))

#worst keywords in terms of amount 
nrow(decending_amount)
ggplot(decending_amount[355:365,], aes( x = reorder(keyword,-amount), y= amount)) +geom_col(fill="salmon")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))


#top keywords in terms of profit (overall)
ggplot(decending_Profit[1:40,], aes( x = reorder(keyword,-Profit,FUN = sum), y= Profit )) +geom_col(fill="salmon")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))  + 
  labs(title = "Top Keywords in terms of Profit")+
  xlab("Keywords")+ylab("Profit ")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 10, face = "bold"))

#worst keywords in terms of profit (US + GLOBAL)
nrow(decending_Profit)
ggplot(decending_Profit[4450:4460,], aes( x = reorder(keyword,Profit,FUN = sum), y= Profit )) +geom_col(fill="salmon")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8, face = "bold")) + labs(title = "Worst Keywords in terms of Profit")+
  xlab("Keywords")+ylab("Profit ")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 10, face = "bold"))


#Best 20 keywords in terms of net revenue (US)
ggplot(decending_Profit_us[1:30,], aes( x = reorder(keyword,-Profit,FUN = sum), y= Profit )) +geom_col(fill="salmon")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))

#Worst 20 keywords US
nrow(decending_Profit_us)
ggplot(decending_Profit_us[3412:3422,], aes( x = reorder(keyword,Profit,FUN = sum), y= Profit )) +geom_col(fill="salmon")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))




sum(clean$Profit)

#worst key words in terms of profit
ggplot(decending_Profit_us[1:30,], aes( x = reorder(keyword,-Profit,FUN = sum), y= Profit )) +geom_col(fill="salmon")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))


#Worst 20 keywords US
nrow(decending_Profit_us)
ggplot(decending_Profit_us[3412:3422,], aes( x = reorder(keyword,Profit,FUN = sum), y= Profit )) +geom_col(fill="salmon")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))


#overview values
sum(clean$Profit)
sum(clean$total_volume_of_bookings)


#boxplot to indicate outlier data for yahoo. This time in the form of trans conversion
nrow(decending_trans_conv)
ggplot(decending_trans_conv[1:3422,], aes(publishers, trans_conv_percent)) + geom_boxplot()

#average cost per click vs publisher (overall)
ggplot(clean, aes(x =reorder(publishers,-avg_cost_per_click), y = avg_cost_per_click , fill =publishers)) + stat_summary(fun.y = "mean", geom = "bar", fill = "salmon")+
  labs(title = "Net revenue vs Region broken down by Publisher")+
  xlab("Region")+ylab("Net revenue")
theme(axis.text.x = element_text(angle = 90, hjust = 1,size = 8))


#############################



### Overall averages of the publishers to be used for comparing with Kayak.

mean(clean$trans_conv_percent)

mean(clean$avg_cost_per_click)

mean(clean$engine_click_thru_percent)

mean(clean$ROA)


#############################

#creating a linear regression model for explaining variables that cause ROA 

ROA_mod <- glm(ROA ~ Google + Yahoo + MSN + Overture, data=clean)
summary(ROA_mod)

ROA_mod <- glm(ROA ~ exact_match, data=clean)
summary(ROA_mod)

ROA_mod <- glm(ROA ~ brand, data=clean)
summary(ROA_mod)

#creating a linear regression model for explaining variables that cause booking success 

booking_success_mod <- glm(Booking_Success ~ Google + Yahoo + MSN + Overture, data=clean)
summary(booking_success_mod)

booking_success_mod <- glm(Booking_Success ~ exact_match, data=clean)
summary(booking_success_mod)

booking_success_mod <- glm(Booking_Success ~ brand, data=clean)
summary(booking_success_mod)

#creating a linear regression model for explaining variables that cause impression conversions 

impr_mod <- glm(impr_conv_percent ~ Google + Yahoo + MSN + US + exact_match + brand, data=clean)
summary(impr_mod)

#creating a linear regression model for explaining variables that cause Amount collected from marketing campaigns 

amount_mod <- glm(amount ~ Google + Yahoo + MSN, data=clean)
summary(amount_mod)

amount_mod <- glm(amount ~ US, data=clean)
summary(amount_mod)

amount_mod <- glm(amount ~ Global, data=clean)
summary(amount_mod)

amount_mod <- glm(amount ~ exact_match, data=clean)
summary(amount_mod)

amount_mod <- glm(amount ~ brand, data=clean)
summary(amount_mod)

#creating a linear regression model for explaining variables that cause click conversions 

click_mod <- glm(engine_click_thru_percent ~ Google + Yahoo + MSN + US + exact_match + brand, data=clean)
summary(click_mod)

#####################################################################################################################
# Probability model for booking success with brand mentioned in the US only
#####################################################################################################################

# user defined function for converting logit to odds and probability

logit2prob <- function(intercept, beta1, x){
  logit <- intercept + beta1*x
  odds <- exp(logit)
  prob <- odds/(1+odds)
  return(c(print("Your odds of success:"), odds,print("Your probability of success is:"), prob))
}

# impact of branded keywords on booking success

booking_success_mod <- glm(Booking_Success ~ brand, data = clean_US, family = "binomial")
summary(booking_success_mod)


logit2prob(intercept = -2.92153, beta1 = 2.0373, x = 1)

#####################################################################################################################
# Probability model for booking success with broad match-type in the US only
#####################################################################################################################

booking_success_mod <- glm(Booking_Success ~ broad_match, data = clean_US, family = "binomial")
summary(booking_success_mod)


logit2prob(intercept = -2.7268, beta1 = 0.2727, x = 25)

#####################################################################################################################
# Probability model for booking success with Exact Match-Type in the US only
#####################################################################################################################

booking_success_mod <- glm(Booking_Success ~ exact_match, data = clean_US, family = "binomial")
summary(booking_success_mod)


logit2prob(intercept = -2.73518, beta1 = 0.09154, x = 75)


#####################################################################################################################
# Probability model for booking success with brand mentioned in the Global only
#####################################################################################################################

booking_success_mod <- glm(Booking_Success ~ brand, data = clean_global, family = "binomial")
summary(booking_success_mod)



logit2prob(intercept = -2.0578, beta1 = 1.4049, x = 1)

#####################################################################################################################
# Probability model for booking success with broad match-type in the Global only
#####################################################################################################################

booking_success_mod <- glm(Booking_Success ~ broad, data = clean_global, family = "binomial")
summary(booking_success_mod)


logit2prob(intercept = -1.46397, beta1 = -1.77923, x = 0)

#####################################################################################################################
# Probability model for booking success with Exact Match-Type in the Global only
#####################################################################################################################

booking_success_mod <- glm(Booking_Success ~ exact_match, data = clean_global, family = "binomial")
summary(booking_success_mod)


logit2prob(intercept = --2.3593, beta1 = 0.9547, x = 1)

##################### THE END ###################################