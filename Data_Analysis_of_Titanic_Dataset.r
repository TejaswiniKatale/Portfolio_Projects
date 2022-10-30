library(ggplot2)

#Importing file 
titanic <- read.csv("D:\\Excel\\titanic4.csv", stringsAsFactors = FALSE)

View(titanic)


#Converting Character to numeric values


titanic$pclass <- as.factor(titanic$pclass)
titanic$survived <- as.factor(titanic$survived)
titanic$sex <- as.factor(titanic$sex)
titanic$embarked <- as.factor(titanic$embarked)


titanic$sex=as.factor(titanic$sex)
titanic_drop = titanic[rowSums(is.na(titanic)) <= 0,]
titanic_survivor =titanic[titanic$survived==1,]
titanic_nonsurvivor =titanic[titanic$survived ==0,]


#Survival Rate 

ggplot(titanic, aes(x=survived))  + theme_bw()+geom_bar(fill="red")+ labs(y="Passenger Count", x= "Titanic Survival")



# Probability Table of Survived people
prop.table(table(titanic$survived))



#Survival Rate by Gender

ggplot(titanic, aes(x = sex, fill = survived)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates by Sex")

#Survival Rate by Class of Ticket

ggplot(titanic, aes(x = pclass, fill = survived)) + 
  theme_bw() +
  geom_bar() +
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates by Pclass")


#Survival Rate by Class of Ticket and Gender

ggplot(titanic, aes(x = sex, fill = survived)) + 
  theme_bw() +
  facet_wrap(~ pclass) +
  geom_bar() +
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates by Pclass and Sex")

#Survival Rate by Age and Gender

ggplot(titanic, aes(x = sex, fill = survived)) + 
  theme_bw() +
  facet_wrap(~ age) +
  geom_bar() +
  labs(y = "Passenger Count",
       title = "Titanic Survival Rates by Age and Sex")


#Barplot Actual Survivors vs Sex

barplot(table(titanic_survivor$sex),xlab="Genders", ylab="Frequency of Survivors", col ="green", main="Barplot(Survivors vs Gender)")

#Barplot Actual Non Survivors vs Sex
barplot(table(titanic_nonsurvivor$sex),xlab="Genders", ylab="Frequency of Non Survivors", col ="yellow", main="Barplot(Non Survivors vs Gender)")




#Histogram of Sex frequency
ggplot(titanic, aes(x = age)) +
  theme_bw() +
  geom_histogram(aes(color = sex, fill = sex),binwidth = 5) +scale_fill_manual(values = c("#00AFBB", "#E7B800")) +   scale_color_manual(values = c("#E7B800", "#00AFBB"))
  labs(y = "Passenger Count",
       x = "Age (binwidth = 5)",
       title = "Titanic Age Distribtion")



#Histograms  age Frequency 
ggplot(titanic, aes(x = age, fill = survived)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Passenger Count",
       x = "Age (binwidth = 5)",
       title = "Titanic Survival Rates by Age")


#Histogram of Actual Survived ones with Age
hist(titanic_survivor$age,breaks = 22,xlab="AGE",xlim=c(0,100),ylab="Frequency of Survived Ones",col="darkmagenta")

#Histogram of Actual Non Survived ones with Age
hist(titanic_nonsurvivor$age,breaks = 22,xlab="AGE",xlim=c(0,100),ylab="Frequency of Non Survived Ones",col="darkmagenta")




#Boxplot- Survived and Age
ggplot(titanic, aes(x = survived, y = age,fill=survived)) +
  theme_bw() +
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=3) +
  labs(y = "Age",
       x = "Survived",
       title = "Titanic Survival Rates by Age")



#Boxplot of Age vs Sex

boxplot(age ~ sex, data = titanic, xlab = "Genders",
        ylab = "Age", main = "Gender vs Age",notch = TRUE, 
        varwidth = TRUE,col = c("green","purple"),
        names = c("NA","Female","Male"))


#survival rates by age when segmented by gender and class of ticket?


ggplot(titanic, aes(x = age, fill = survived)) +
  theme_bw() +
  facet_wrap(sex ~ pclass) +
  geom_density(alpha = 0.5) +
  labs(y = "Age",
       x = "Survived",
       title = "Titanic Survival Rates by Age, Pclass and Sex")

# Histogram survival rates by age when segmented by gender and class of ticket?

ggplot(titanic, aes(x = age, fill = survived)) +
  theme_bw() +
  facet_wrap(sex ~ pclass) +
  geom_histogram(binwidth = 5) +
  labs(y = "Age",
       x = "Survived",
       title = "Titanic Survival Rates by Age, Pclass and Sex")


#################################################################################

























