
Sales <- read.csv("C:/Users/Bhavya Tandon/Documents/Analytics R/Assignments R/R case study 3/SalesData.csv")

#QUE 1
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

Sal <- Sales %>% dplyr::group_by(Region) %>% 
  summarise(Sales2015=sum(Sales2015),Sales2016= sum(Sales2016)) %>% 
  gather(key= "Sales",value= "Value",-Region)

ggplot2::ggplot(data = Sal) + 
  aes(x = Value, y =  Region, fill=Sales ) + 
  geom_bar(stat = 'identity', position = 'dodge' ,color = 'red')+
  scale_x_continuous(labels=comma)+
  geom_text(aes(label = Value), size = 3,hjust = 1,position=position_dodge(width=1),color='yellow')+
  labs(x="Sales")

#QUE 2
D1 <- Sales %>% dplyr::group_by(Region) %>% summarise(Sales2016= sum(Sales2016))

PC <- ggplot2::ggplot(data = D1) + 
  aes(x = '', y =Sales2016,fill = Region) + 
  geom_bar(stat = 'identity', width=1) +scale_y_continuous(labels=comma)+
  coord_polar(theta = 'y', start = 0) +
  geom_text(aes(label = paste0(round((Sales2016 *100)/sum(Sales2016),2),'%')), position = position_stack(vjust = 0.3))+
  theme(plot.title = element_text(hjust = 0.5, color = 'black')) +
  ggtitle("Pie Chart of Sales 2016")
plot(PC)

#QUE 3

Sal2 <- Sales %>% dplyr::group_by(Region,Tier) %>% 
  summarise(Sales2015=sum(Sales2015),Sales2016= sum(Sales2016)) %>% 
  gather(key= "Sales",value= "Value",-Region,-Tier)

ggplot2::ggplot(data = Sal2) + 
  aes(x = Tier, y = Value, fill=Sales ) + 
  geom_bar(stat = 'identity', position = 'dodge', width=1,color = 'red')+ facet_grid(.~Region)+
  scale_y_continuous(labels=comma)+
    theme(axis.text.x =element_text(angle = 90))+labs(y="Sales")

  
#QUE 4
Sal3 <- Sales %>% filter(Region =="East") %>% 
    dplyr::group_by(State) %>% 
    summarise(Sales2015 = sum(Sales2015),Sales2016 = sum(Sales2016))%>% 
    filter(Sales2015>Sales2016)

# ANS: NY state registered a decline in 2016 as compared to 2015

Sal3 <- Sales %>% filter(Region =="East") %>% 
  dplyr::group_by(State) %>% 
  summarise(Sales2015 = sum(Sales2015),Sales2016 = sum(Sales2016))%>% 
  gather(key= "Sales",value= "Value",-State)

ggplot2::ggplot(data = Sal3) + 
  aes(x = State, y = Value, fill=Sales ) + 
  geom_bar(stat = 'identity', position = 'dodge', width=1,color = 'red')+
  scale_y_continuous(labels=comma)+
  theme(axis.text.x =element_text(angle = 90))+labs(y="Sales")


#QUE5
Sal4 <- Sales %>% filter(Tier =="High") %>% 
  dplyr::group_by(Division) %>% 
  summarise(Units2015 = sum(Units2015),Units2016 = sum(Units2016))%>%  
  filter(Units2015>Units2016)

#ANS: No division saw a decline in number of units sold in 2016 compared to 2015

Sal4 <- Sales %>% filter(Tier =="High") %>% 
  dplyr::group_by(Division) %>% 
  summarise(Units2015 = sum(Units2015),Units2016 = sum(Units2016))%>%
  gather(key= "Sales",value= "Value",-Division)

ggplot2::ggplot(data = Sal4) + 
  aes(x = Division , y = Value, fill=Sales ) + 
  geom_bar(stat = 'identity', position = 'dodge', width=1,color = 'red')+
  scale_y_continuous(labels=comma)+
  theme(axis.text.x =element_text(angle = 90,size=7))+labs(y="Sales")

#QUE6

Mon<-Sales$Month
Sales$Qtr <- ifelse(Mon %in% c("Jan","Feb","Mar") ,"Q1",
                      ifelse(Mon %in% c("Apr","May","Jun"),"Q2",
                             ifelse(Mon %in% c("Jul","Aug","Sep"),"Q3","Q4")))


#QUE7

Sal5 <- Sales %>% 
  dplyr::group_by(Qtr) %>% 
  summarise(Sales2015 = sum(Sales2015),Sales2016 = sum(Sales2016)) %>% 
  gather(key= "Sales",value= "Value",-Qtr)

ggplot2::ggplot(data = Sal5) + 
  aes(x =Qtr , y =  Value, fill=Sales ) + 
  geom_bar(stat = 'identity', position = 'dodge' ,color = 'red')+
  scale_y_continuous(labels=comma) + labs(y="Sales")

#QUE 8

Qtr <- Sales %>%
  dplyr::group_by(Qtr,Tier) %>% 
  summarise(Sales2015 = sum(Sales2015))

par(mfrow=c(1,4))
pie(Qtr$Sales2015[Qtr$Qtr=='Q1'],labels = paste(Qtr$Tier),xlab="Qtr1")
pie(Qtr$Sales2015[Qtr$Qtr=='Q2'],labels = paste(Qtr$Tier),xlab="Qtr2")
pie(Qtr$Sales2015[Qtr$Qtr=='Q3'],labels = paste(Qtr$Tier),xlab="Qtr3")
pie(Qtr$Sales2015[Qtr$Qtr=='Q4'],labels = paste(Qtr$Tier),xlab="Qtr4")

