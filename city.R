library(dplyr)
library(readr)
library(tidyverse)
blackfriday <- read_csv("C:/blackfriday.csv")

blackfriday %>%
  select(Gender,User_ID,Purchase) %>%
  group_by(User_ID, Gender) %>%
  summarise(avg_purchase =mean(Purchase),
            Sales_frequency=n()) %>%
  ggplot(aes(Sales_frequency,avg_purchase,
             colour = Gender))+
  geom_point()+
  labs(title = "relationship between purchase and gender")


blackfriday %>%
  select(Occupation,User_ID,Purchase) %>%
  group_by(User_ID, Occupation) %>%
  summarise(avg_purchase =mean(Purchase),
            Sales_frequency=n()) %>%
  ggplot(aes(Sales_frequency,avg_purchase,
             colour = Occupation))+
  geom_point()+
  labs(title = "relationship between purchase and occupation")


blackfriday %>%
  select(Age,User_ID,Purchase) %>%
  group_by(User_ID, Age) %>%
  summarise(avg_purchase =mean(Purchase),
            Sales_frequency=n()) %>%
  ggplot(aes(Sales_frequency,avg_purchase,
             colour = Age))+
  geom_point()+
  labs(title = "relationship between purchase and age")

blackfriday %>%
  select(Marital_Status,User_ID,Purchase) %>%
  group_by(User_ID, Marital_Status) %>%
  summarise(avg_purchase =mean(Purchase),
            Sales_frequency=n()) %>%
  ggplot(aes(Sales_frequency,avg_purchase,
             colour = Marital_Status))+
  geom_point()+
  scale_colour_gradientn(colours=rainbow(4))+
  labs(title = "relationship between purchase and marital status")



a <-blackfriday %>%
  select(Marital_Status
         ,Product_Category_1
         ,Product_Category_2
         ,Product_Category_3)%>%
  group_by(yes = Marital_Status > 0)%>%
  summarise(P1 =sum(Product_Category_1)
            ,P2 =sum(Product_Category_2,na.rm = TRUE)
            ,P3=sum(Product_Category_3,na.rm = TRUE))

blackfriday %>%
  select(Age,User_ID,Occupation) %>%
  group_by_all() %>%
  summarise(Sales_frequency=n(), O =$) %>%
  ggplot(aes(Sales_frequency,Occupation,
             colour = Age))+
  geom_point()+
  labs(title = "relationship between purchase and gender")

