library(ggplot2)
library(dplyr)






#Data Importing

data<-read.csv(file="E:/Project trimester 3/hmda_2017_nj.csv ")

View(data)



dataset<-head(data,20000)



#exploring data

glimpse(dataset)
names(dataset)
dim(dataset)
View(dataset)
str(dataset)
summary(dataset)

#chech NA values in every column

colSums(is.na(dataset)|dataset == 'NA')


#########################


dataset<-subset(dataset,select=-c(as_of_year,edit_status,edit_status_name,application_date_indicator,sequence_number,state_name,state_abbr,state_code))

str(dataset)

##############################################
dataset$applicant_income_000s<-as.numeric(dataset$applicant_income_000s)
median(dataset$applicant_income_000s,na.rm=TRUE)
median(dataset$loan_amount_000s, na.rm = TRUE)



################################################

#Loan Agency
counts <- table(dataset$agency_name)
barplot(counts, main="Loan_Agency",
        xlab="Agency", col=c("darkblue","red","cyan","brown","green"),
        legend = rownames(counts))
##################################################3
##Applcant_sex
  
countsf<- table(dataset$applicant_sex_name)
lbls<- c("Female", "Male", "Not applicable", "Information not provided")
pct<- round(countsf/sum(countsf)*100)
lbls<- paste(lbls, pct) # add percents to labels
lbls<- paste(lbls,"%",sep="") 

pie(countsf,labels =lbls,col=rainbow(length(countsf)),
    main="Pie Chart of Sex Name")

##################################
#MSAMD 

dataset%>%
  filter(!is.na(msamd_name)) %>%
  group_by(msamd_name) %>%
  summarise(Count= n() ) %>%
  mutate(percentage = ( Count/sum(Count) ) *100 ) %>%
  mutate(msamd_name= reorder(msamd_name, percentage)) %>%
  
  ggplot(aes(x = msamd_name,y = percentage)) +
  geom_bar(stat='identity',colour="white", fill ="red") +
  geom_text(aes(x = msamd_name, y = 1, label = paste0("( ",round(percentage),"% )",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'MSA/MD', y = 'Count', title = 'Metropolitian Name based on Loan Origination') + 
  theme_bw()+coord_flip()




####################################################################



######  EDA




################first chart


#hms--home mortgage status

#Loan Action


hms = dataset %>% group_by(action_taken_name) %>%
  summarise(CountOfActionTaken = n()) %>%
  mutate(PercentageActionTaken = CountOfActionTaken /sum(CountOfActionTaken) * 100) %>%
  arrange(desc(PercentageActionTaken))



ggplot(hms,aes(x = reorder(action_taken_name, PercentageActionTaken), 
       y = PercentageActionTaken)) +geom_bar(stat='identity',colour="black", fill = "cyan") +
  geom_text(aes(x = action_taken_name, y = 1, label = paste0("( ",round(PercentageActionTaken,2),"% )",sep="")),
    hjust=0, vjust=.5, size = 4, colour = 'black',fontface = 'bold') +
  labs(x = 'action_taken_name', y = '%age Count Of Action Taken', title = ' Loans Action') +
  coord_flip() 

str(dataset)

############Second Chart   


# Purpose Types distribution


dataset%>%
  filter(!is.na(loan_purpose_name)) %>%
  group_by(loan_purpose_name) %>%
  summarise(CountLoanPurpose = n() ) %>%
  mutate(percentage = ( CountLoanPurpose/sum(CountLoanPurpose) ) *100 ) %>%
  mutate(loan_purpose_name = reorder(loan_purpose_name, percentage)) %>%
  
  ggplot(aes(x = loan_purpose_name,y = percentage)) +
  geom_bar(stat='identity',colour="white", fill ="red") +
  geom_text(aes(x = loan_purpose_name, y = 1, label = paste0("( ",round(percentage),"% )",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Loan Purpose', y = 'Count', title = 'Loans Purpose Types') + 
  theme_bw()


#  Loans Purpose Types Distribution %

dataset %>%
  filter(!is.na(loan_purpose_name)) %>%
  group_by(loan_purpose_name,action_taken_name) %>%
  summarise(CountLoanPurpose = n() ) %>%
  
  ggplot(aes(x = loan_purpose_name,y = CountLoanPurpose,fill =(action_taken_name))) +
  geom_bar(stat='identity',colour="white") +
  labs(x = 'Loan Purpose', y = 'Count', title = 'Loans Purpose Types Distribution with Action Types') + coord_flip()


#################third chart

#County and Loans


dataset %>%
  filter(!is.na(county_name)) %>%
  group_by(county_name) %>%
  summarise(CountLoanPurpose = n() ) %>%
  mutate(percentage = ( CountLoanPurpose/sum(CountLoanPurpose) ) *100 ) %>%
  mutate(county_name = reorder(county_name, percentage)) %>%
  arrange(desc(percentage)) %>%
  head(120) %>%
  
  ggplot(aes(x = county_name,y = percentage)) +
  geom_bar(stat='identity',colour="black", fill ="pink") +
  geom_text(aes(x = county_name, y = 1, label = paste0("( ",round(percentage,2),"% )",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',fontface = 'bold') +
  labs(x = 'County Name', y = 'Count', title = 'County and Loans') +
  coord_flip()
  


############ fourth chart



Top10Counties = dataset %>%
  filter(!is.na(county_name)) %>%
  group_by(county_name) %>%
  summarise(CountLoanPurpose = n() ) %>%
  mutate(percentage = ( CountLoanPurpose/sum(CountLoanPurpose) ) *100 ) %>%
  mutate(county_name = reorder(county_name, percentage)) %>%
  arrange(desc(percentage)) %>%
  head(50)

dataset %>%
  filter(!is.na(county_name)) %>%
  filter(county_name %in% Top10Counties$county_name) %>%
  group_by(county_name,action_taken_name) %>%
  summarise(CountLoanPurpose = n() ) %>%
  
  
  ggplot(aes(x = county_name,y = CountLoanPurpose,fill = action_taken_name)) +
  geom_bar(stat='identity',colour="white") +labs(x = 'County Name', y = 'Count', title = 'County Distribution with Action Types') +
  theme_bw() + theme(legend.position="top") +coord_flip()

###############five chart
#Lien Status Distribution with Action Types

lienData = dataset%>% 
  group_by(action_taken_name,lien_status_name) %>%
  tally()

ggplot(lienData,aes(x = lien_status_name,y = n,fill = action_taken_name)) +
  geom_bar(stat='identity',colour="white") +
  labs(x = 'Lien Status', y = 'Count', title = 'Lien Status Distribution with Action Types')


########################## six chart


# Purchase Type Distribution with Action Type 

purchaseTypeData = dataset %>% group_by(action_taken_name,purchaser_type_name) %>%
  tally()

ggplot(purchaseTypeData, aes(x = purchaser_type_name,y = n,fill = action_taken_name)) +
  geom_bar(stat='identity',colour="white") +
  coord_flip() +
  labs(x = 'Purchase Type', y = 'Count', title = 'Purchase Type Distribution with Action Types')+ theme(legend.position="top")


hist(dataset$denial_reason_1)
#################################### seven chart 

##home mortgage race

### Action in Loan by race


hmr =dataset %>% group_by(action_taken_name,applicant_race_name_1) %>%
  summarise(CountOfActionTaken = n()) %>%
  arrange(desc(CountOfActionTaken))



hmr= dataset %>% group_by(applicant_race_name_1) %>%
  summarise(CountOfRace1 = n()) %>%
  arrange(desc(CountOfRace1))
ggplot(hmr, aes(x = reorder(applicant_race_name_1, CountOfRace1), 
                                         y = CountOfRace1)) +
  geom_bar(stat='identity',colour="white", fill ="orange") +
  geom_text(aes(x = applicant_race_name_1, y = 1, label = paste0("(",round(CountOfRace1),")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Race Name', y = 'Count Of Action Taken', title = 'Actions in Loans by Race') +
  coord_flip() + 
  theme_bw()
######################################################################Nine chart

#Loan distribution


actionStatus = "Loan originated"
breaks = seq(0,400,50)

actionStatus = "Loan originated"
breaks = seq(0,400,50)
dataset %>%
  filter(action_taken_name == actionStatus ) %>%
  ggplot(aes(applicant_income_000s)) +
  scale_x_continuous(limits = c(0,400),breaks=breaks ) +
  geom_histogram(binwidth = 10,fill = c("red")) +
  labs(x = 'Income in Thousands', y = 'Count', title = 'Loan Originated Applicant Income distribution') +  theme_bw()
################################ eight chart


#  Using Ethnicity Actions in Loans


homeMortgageStatus_ethnicity = dataset %>% group_by(action_taken_name,applicant_ethnicity_name) %>%
  summarise(CountOfActionTaken = n()) %>%
  arrange(desc(CountOfActionTaken))
homeMortgage_ethnicity = dataset %>% group_by(applicant_ethnicity_name) %>%
  summarise(CountOfEthnicity = n()) %>%
  arrange(desc(CountOfEthnicity))
ggplot(homeMortgage_ethnicity, aes(x = reorder(applicant_ethnicity_name, CountOfEthnicity), 
                                   y = CountOfEthnicity)) +
  geom_bar(stat='identity',colour="white", fill ="Blue") +
  geom_text(aes(x = applicant_ethnicity_name, y = 1, label = paste0("(",round(CountOfEthnicity),")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'applicant_ethnicity_name', y = '%age Count Of Action Taken', title = 'Using Ethnicity Actions in Loans') +
  coord_flip() + 
  theme_bw()
##############################################################

################################################################
#Data Splitting ino train and Test

sample <- sample.int(n = nrow(dataset), size = floor(.75*nrow(dataset)), replace = F)
train<- dataset[sample, ]
test<- dataset[-sample, ]



####################################



qqnorm(train$loan_purpose)
qqline(train$loan_purpose,col='red')
plot(density(train$loan_purpose),main='loan_purpose')

qqnorm(dataset$loan_amount_000s)
qqline(dataset$loan_amount_000s,col='blue')
plot(density(dataset$agency_code),main='agency_code')


qqnorm(dataset$loan_type)
qqline(dataset$loan_type,col='blue')
plot(density(dataset$loan_type),main='a')







##############Statistical Analysis



model<-lm(train$lien_status~train$applicant_income_000s+train$purchaser_type+train$loan_amount_000s,data=train)

summary(model)

plot(model)




model2<-lm(train$lien_status~train$applicant_income_000s+train$purchaser_type+train$loan_purpose+train$loan_amount_000s,data=train)

summary(model2)

plot(model2)


model3<-lm(train$lien_status~train$msamd+train$action_taken+train$applicant_ethnicity+       
                 train$applicant_income_000s+train$applicant_race_1+train$co_applicant_ethnicity+
           train$co_applicant_sex+train$county_code+train$hoepa_status +train$lien_status)

summary(model3)
plot(model3)




model4<-lm(train$lien_status~train$msamd+train$action_taken+train$applicant_ethnicity+       
             train$applicant_income_000s + train$applicant_race_1 + train$co_applicant_ethnicity+
             train$co_applicant_sex + train$county_code+train$hoepa_status +
             train$owner_occupancy+train$preapproval+train$property_type+train$purchaser_type+train$loan_amount_000s)

mod4<-lm(lien_status~msamd+action_taken+applicant_ethnicity+applicant_income_000s + applicant_race_1 +co_applicant_ethnicity+
                    co_applicant_sex + county_code+hoepa_status +owner_occupancy+preapproval+property_type
          +purchaser_type+loan_amount_000s,data=dataset)

summary(model4)

plot(model4)



coefficients(model4)

##################################################################################################



