library("ggcorrplot")
library("ggplot2")
df<-read.csv("D:/Advanced Stats/final_data_hour.csv")
df

#Descriptive analysis


head(df,2)
tail(df,2)
summary(df$temp)

table(df$season)
table(df$hr)
table(df$yr)
table(df$mnth)
table(df$holiday)
table(df$weekday)

summary(df$atemp)

library(e1071) 
skewness(df$seasons)

df$season <- factor(df$season)
df$mnth <- factor(df$mnth)
df$holiday <- factor(df$holiday)
df$weekday <- factor(df$weekday)
df$workingday <- factor(df$workingday)
df$weathersit <- factor(df$weathersit)
df$yr<-factor(df$yr)
df$hr<-factor(df$hr)
df$cnt<-factor(df$cnt)

summary(df$cnt)
str(df)
dim(df)

table(df$mnth)

summary(df$temp)

View(df)
##Univariate
barplot(table(df$holiday),
        main="Graph showing bike rental on Holidays ",
        xlab="Holiday",
        ylab="Number of Bike Rentals",
        ylim=c(0,5000),
        col=c("#429e9d","#941033"))




#1
barplot(table(df$workingday),
        main="Graph showing bike rental on Working days ",
        xlab="Working days",
        ylab="Number of Bike Rentals",
        ylim=c(0,4000),
        col="#8a67a1")
#Here we can observe that people tend to rent bikes on working days to commute to their respective destinations.

#2
hist(df$cnt,main="Histogram for count of users",xlab="users",col='steel blue')
# Here we can observe that Right skewed distribution also called a positively skewed distribution,and the density of values is more between 0 and 200.


#3
ggplot(data=df, aes(x=season, fill=season))+
  geom_bar(stat="count")+
  theme_classic()+
  labs(x="Season", y="Count of users", ylim=c(0,7000))
#The season does not actually affect the bike rental but we can see that summer season have a slightly higher amount of users


#4
ggplot(data=df, aes(weathersit,fill=weathersit))+
  geom_bar(stat="count")+
  theme_classic()+
  ggtitle("Number of Bike Rentals vs Type of Weather") +
  scale_fill_manual(values=c("#53ed6d","#82ebf5", "#203352","#52203f"), 
                    name="Weather Situation:")+
  labs(x="weather", y="Number of bike rentals")
#More people tend to rent bikes when the weather is  Clear, Few clouds, Partly cloudy or Partly cloudy 

#Box plot
#5
boxplot(df$cnt,
        main="Users that rented the bike",
        xlab='count of users',
        col="red",horizontal = TRUE)
#we can see that there are many outliers in the count of the users that have rented the bikes.



####Bivariate########

#6
#line plot of rentals v.s. hour of day
ggplot(data=df, aes(x =hr, y = cnt, color = as.factor(weekday))) +
  geom_smooth(method = "loess", fill = NA, linewidth= 1) +
  theme_light(base_size = 11) +
  xlab("Hour of the Day") +
  ylab("Number of Bike Rentals") +
  ggtitle("Number of Bike Rentals vs Hour of the day") +
  scale_color_discrete("") +
  theme(plot.title = element_text(size = 11, face="bold"))

###
ggplot(data=df, aes(x = hr, y = cnt, color = as.factor(weekday))) +
  geom_smooth(method = "loess", fill = NA, size = 1) +
  theme_light(base_size = 11) +
  xlab("Hour of the Day") +
  ylab("Number of Bike Rentals") +
  ggtitle("Number of Bike Rentals vs Hour of the day") +
  scale_color_discrete("") +
  theme(plot.title = element_text(size = 11, face="bold"))
##del


#The line plot of hour of the day against bike rental count categorize by day of the week shows the difference of rental demand for weekday and weekend in different hours. The rental count remains active later in the midnight during the weekend than weekday. We can also see that the bike rental count has a dip at around 12 P.M. during weekdays, whereas around the same time during weekends shows peak of demand of the day. The peak of demand during weekdays is around 4 to 5 P.M. in the afternoon, possibly due to people are done working and in need of transportation to go home.


ggplot(data=df, aes(x =hr, y = cnt, color = as.factor(weekday))) +
  geom_smooth(method = "loess", fill = NA, linewidth= 1) +
  theme_light(base_size = 11) +
  xlab("Hour of the Day") +
  ylab("Number of Bike Rentals") +
  ggtitle("Number of Bike Rentals vs Hour of the day") +
  scale_color_discrete("") +
  theme(plot.title = element_text(size = 11, face="bold"))





#7
#line plot of rentals v.s. temperature
ggplot(data=df, aes(x = temp, y = cnt, color = yr)) +
  geom_smooth(method = "loess", fill = NA, size = 1) +
  theme_light(base_size = 11) +
  xlab("temperature") +
  ylab("Number of Bike Rentals") +
  ggtitle("Number of bike rentals v.s. temperature") +
  scale_color_discrete("") +
  theme(plot.title = element_text(size = 11, face="bold"))


ggplot(data=df, aes(x = temp, y = cnt)) +
  geom_smooth(method = "loess", fill = NA, size = 1) +
  theme_light(base_size = 11) +
  xlab("temperature") +
  ylab("Number of Bike Rentals") +
  ggtitle("Number of bike rentals v.s. temperature") +
  scale_color_discrete("") +
  theme(plot.title = element_text(size = 11, face="bold"))



#as the temperature gets warmer the number of bikes rentals increases and also the number of bikes rented increased in 2012 compared to the year 2011.

#8
# boxplot of rental v.s. season 
ggplot(data=df, aes(x =season, y = cnt, fill = season)) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("Seasons") +
  ylab("Number of Bike Rentals") +
  ggtitle("Number of Bike rentals vs.Season") +
  scale_fill_manual(values=c("#D6EAF8", "#2ECC71", "#E74C3C", "#F39C12"), 
                    name="Season:",
                    
                    labels=c("Winter", "Spring", "Summer", "Fall")) +
  theme(plot.title = element_text(size = 11, face="bold"))
#The boxplot of different seasons against bike rental count reveals that there is a seasonal trend with the rental count. Rental count is generally low in Winter and it peaks in Summer. Season can be one of the determining factors that affects bike rental count.


# boxplot of rental v.s. month 
ggplot(data=df, aes(x =mnth, y = cnt, fill = mnth)) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("Months") +
  ylab("Number of Bike Rentals") +
  ggtitle("rental v.s. month ") +
  scale_fill_manual(values=c("#D6EAF8", "#2ECC71", "#E74C3C", "#F39C12","red","blue","darkgreen","brown","orange","pink","blue","yellow"), labels=c("Jan", "Feb", "Mar", "APr","May","June","July","aug","sept","Oct","Nov","Dec")) +
  theme(plot.title = element_text(size = 11, face="bold"))
#The box plot shows that the number of bikes rented by the users are more in July,October and August than in comparison to other months.


# boxplot of rental v.s. weather
ggplot(data=df, aes(x =weathersit, y = cnt, fill = weathersit)) +
  geom_boxplot(outlier.color = adjustcolor("black", alpha.f = 0), na.rm = TRUE) +
  theme_light(base_size = 11) +
  xlab("Type of Weather") +
  ylab("Number of Bike Rentals") +
  ggtitle("rental v.s. month ") +
  scale_fill_manual(values=c("#D6EAF8", "#2ECC71", "#E74C3C", "#F39C12","red","blue","darkgreen","brown","orange","pink","blue","yellow"), labels=c("Jan", "Feb", "Mar", "APr","May","June","July","aug","sept","Oct","Nov","Dec")) +
  theme(plot.title = element_text(size = 11, face="bold"))
table(df$weathersit)

#The boxplot of different seasons against bike rental count reveals that there is a seasonal trend with the rental count.
# Rental count is generally low in Winter and it peaks in Summer.
# Season can be one of the determining factors that affects bike rental count.




#9
ggplot(df, aes(x = casual, y = registered, color =workingday))+
  geom_point()+
  labs(title = "Relation Between Bike counts(casual& registered) vs Working, Non working")+
  scale_color_manual(values=c("#ff12a0", "#815e91"),
                     name="workingday:",
                     labels=c("non working", "working")
  ) +
  xlab("Casual Bike Counts") +
  ylab("Registered Bike Counts")
#The graph shows that mostly working people are registered and use bikes mainly on weekdays. On the other hand, mostly non-working people are casual bikers and prefer to ride on weekends and holidays.

#10
ggplot(df,aes(x =temp , y = cnt,color=holiday)) +
  geom_jitter() +
  theme_minimal() +
  theme(legend.position="bottom", plot.title = element_text(vjust = 0.5)) + 
  labs(title = "Number of Bikes Rented in relation to temperature based on holidays",
       x = "Temperature",
       y = "count of users", 
       color = "")+
  facet_grid(rows=df$holiday)
#Number of bikes rented on working days is more irrespective of the temperature compared to holidays.Whereas on holidays users tend to rent bikes as the temperature gets warmer.

str(df)
df$hrs=as.integer(df$hr)
df$mnths=as.integer(df$mnth)
df$seasons=as.integer(df$season)


correlation<-cor(df[,c("hrs","temp","mnths","atemp","cnt","windspeed","hum","seasons")])


ggcorrplot(correlation,hc.order=TRUE,
           type='full',
           lab=TRUE,
           lab_size=3,
           method="square",
           title="Correlogram ",
           ggtheme=theme_bw)
#We can clearly see from the matrix that hour and temperature has the strongest correlation to bike rental count while all variables considered. However, hour and temperature has a relatively high correlation between each other. We can disregard the significantly high correlation between season and month since it is only natural for them to have high correlation.




##########################################################

######   Hypothesis Testing

#obj 1 : Is there an increase in the number of bikes rented on holidays?

#H0: There is no increase in the number of bikes rented on holidays
#H1: There is an increase in the number of bikes rented on holidays

cnt=df$cnt
holiday=df$holiday
chisq.test(holiday,cnt)

#table(df$holiday)

#p-value= 0.957
#p-value>0.05, Accept H0.
# According to the chi-square test we observe that the number of bike rentals do not increase on holidays.

# Obj 2 : Is there a significant difference in the number of bike rentals due to seasons?
  
#H0: There is no significant difference between  the number of bike rentals due to seasons.
#H1:There is a significant difference between the number of bike rentals due to seasons.

season=df$season
chisq.test(season,cnt)

#p-value = (< 2.2e-16)
#p-value<0.05, Reject H0.
# The test reveals that there is a significant difference between the number of bike rentals due to seasons.

#Obj 3:  Is there a significant difference in the number of bike rentals according to months?

#H0: There is no significant difference in the number of bike rentals according to months
#H1: There is a significant difference in the number of bike rentals according to months

mnth=df$mnth
chisq.test(cnt,mnth)

#p-value= (< 2.2e-16)
#p-value<0.05,Reject H0
# By applying the chi-square test we can conclude that there is a significant difference in the number of bike rentals according to months.

# Obj 4: Is there an increase in the number of bike rentals in the year 2012 compared to 2011?
  
#H0:There is no increase in the number of bike rentals in the year 2012 compared to 2011.
#H1: There is an increase in the number of bike rentals in the year 2012 compared to 2011.

yr=df$yr
chisq.test(cnt,yr)

#p-value= (< 2.2e-16)
#p-value<0.05,Reject H0.
# As the p-value is less than 0.05, we can conclude that there is an increase in the number of bike rentals in the year 2012 compared to 2011.

#Obj 5: Is there an association between temperature and bike rentals?
#H0:There is no association between temperature and bike rentals
#H1:There is an association between temperature and bike rentals

temp=df$temp
cnt=factor(df$cnt)
result=aov(temp,cnt)
summary(result)

#Pr(>F)= (<2e-16)
# Pr(>F)<0.05,Reject H0.
# From anova test between temperature and bike rentals we observe there is an association between temperature and bike rentals

#Obj 6: Is there an association between hours and bike rentals?
  
#H0:There is no association between the hour and bike rentals
#H1:There is an association between the hour and bike rentals.

chisq.test(cnt,hr)

#p-value< 2.2e-16
#p-value<0.05,Reject H0.
# From the chi-square test we can conclude that there is an association between the hour and bike rentals.
