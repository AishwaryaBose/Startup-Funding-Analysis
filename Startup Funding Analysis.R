################
#Start up funding
################

funding_data <- read.csv("startup_funding.csv",na.strings = c("","NA"),dec = ",")

#####Questions#####
#1. what are the top five start ups by investment amount  and location
#2. which are the top five investors by number of investments and their preferred investment type
#3. Which are the top five Industry verticals by number of investments
#4. Top 5 cities by number of investments
#5. which vertical has seen a rise in investment over the years and which has seen a decline over the years


funding_data$StartupName <- as.character(funding_data$StartupName)
funding_data$StartupName <- sapply(funding_data$StartupName, tolower)
funding_data$StartupName <- gsub("\\..*","",funding_data$StartupName)

funding_data$StartupName <- gsub("\\ola .*","olacabs",funding_data$StartupName)
funding_data$StartupName <- gsub("\\olac .*","olacabs",funding_data$StartupName)
funding_data$StartupName[funding_data$StartupName %like% "ola"] <- "olacabs"

funding_data$StartupName <- gsub("\\oyo .*","oyorooms",funding_data$StartupName)
funding_data$StartupName <- gsub("\\olay .*","olacabs",funding_data$StartupName)
funding_data$StartupName[funding_data$StartupName %like% "oyo"] <- "oyorooms"

funding_data$StartupName[funding_data$StartupName == "paytm marketplace"] <- "paytm"

funding_data$Amount <- gsub(",","",funding_data$AmountInUSD,fixed = TRUE)
funding_data$Amount <- as.numeric(funding_data$Amount)
funding_data$CityLocation[funding_data$StartupName=='paytm'] <- "New Delhi"
funding_data$CityLocation[funding_data$StartupName=='oyorooms'] <- "New Delhi"


Q1 <- funding_data %>% group_by(StartupName,CityLocation) %>% summarise(Total.Funding=sum(as.numeric(Amount)/1000000,na.rm=TRUE)) %>% ungroup() %>% arrange(desc(Total.Funding)) %>% top_n(5)

ggplot(Q1, aes(x=reorder(StartupName,-Total.Funding),y=Total.Funding,fill=CityLocation))+geom_bar(stat = "identity") + scale_y_continuous(labels = comma) +labs(y="Funding (in Million rupees)",x="Startup",fill="Location",title="Startup Funding")


####Investors and preferred investment type
ecom_strings <- c("ECommerce","Ecommerce","ecommerce","eCommerce")
logistics_strings <- c("Logistics","Logistics Tech")
education_strings <- c("Education","Online Education Platform")

levels(funding_data$InvestmentType)[levels(funding_data$InvestmentType)=="Crowd funding"]<-"Crowd Funding"
levels(funding_data$InvestmentType)[levels(funding_data$InvestmentType)=="SeedFunding"]<-"Seed Funding"
levels(funding_data$InvestmentType)[levels(funding_data$InvestmentType)=="PrivateEquity"]<-"Private Equity"
levels(funding_data$IndustryVertical)[levels(funding_data$IndustryVertical) %in% ecom_strings]<-"eCommerce"
levels(funding_data$IndustryVertical)[levels(funding_data$IndustryVertical) %in% logistics_strings]<-"Logistics"
levels(funding_data$IndustryVertical)[levels(funding_data$IndustryVertical) %in% education_strings]<-"Education"


Q2 <- funding_data %>% group_by(InvestmentType,IndustryVertical) %>% summarise(Count = n()) %>% arrange(desc(Count)) %>% na.omit() %>% top_n(5)
ggplot(Q2, aes(x=reorder(InvestmentType,-Count),y=Count,fill=IndustryVertical))+geom_bar(stat = "identity")


#3. Which are the top five Industry verticals by number of investments
Other_levels <- funding_data %>% group_by(IndustryVertical) %>% summarise(count=n()) %>% arrange(desc(count)) %>% top_n(-(length(levels(funding_data$IndustryVertical))-10))
levels(funding_data$IndustryVertical)[levels(funding_data$IndustryVertical)%in% Other_levels$IndustryVertical]<-"Other Verticals"
'%ni%' <- Negate('%in%')
Q3 <- funding_data %>% group_by(IndustryVertical) %>% filter(IndustryVertical %ni% c("Others","Other Verticals")) %>% summarise(Total.Funding=sum(as.numeric(Amount)/1000000,na.rm=TRUE)) %>% arrange(desc(Total.Funding)) %>% na.omit() %>% top_n(3)
Q3_C <- funding_data %>% group_by(IndustryVertical) %>% filter(IndustryVertical %ni% c("Others","Other Verticals")) %>% summarise(Count=n()) %>% arrange(desc(Count)) %>% na.omit() %>% top_n(3)
merged_Q3 <- merge(Q3,Q3_C,by = "IndustryVertical") %>% arrange(desc(Total.Funding))


ggplot(merged_Q3) + geom_bar(aes(x=reorder(IndustryVertical,-Total.Funding),y=Total.Funding),fill = "orange",stat = "identity")+
  geom_line(aes(x=IndustryVertical,y=Count*4,group=1),stat = "identity")+geom_text(aes(label=Count,x=IndustryVertical,y=Count*4)) + 
  scale_y_continuous(sec.axis = sec_axis(~./4,"Total Number of Investments made")) + labs(x="Industry Vertical",y="Total Funding in millions")




