library(ineq)
library(tidyverse)
library(dplyr)

gini.years = df %>%
  select(HHID,HHINCPC_CPI,WAVE) %>%
  arrange(HHINCPC_CPI) %>%
  transform(HHINCPC_CPI = ifelse(HHINCPC_CPI < 0, 0, HHINCPC_CPI)) %>%
  group_by(WAVE) %>%
  summarise(index = ineq(HHINCPC_CPI,type = c("Gini")))

##Visulization 1
ggplot(gini.years,aes(x = WAVE,y = index)) +
  geom_line() +
  geom_point()+
  geom_text(aes(label = round(index, 3),hjust = -0.3,vjust = 0.5)) +
  ggtitle("Gini Index Over Year") +
  xlab("Year") + ylab("Gini Index") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

##Visulization 2
years = c(1989,1991,1993,1997,2000,2004,2006,2009,2011,2015)
plot(Lc(df[df$WAVE ==1989,]$HHINCPC_CPI),col="darkred",lwd=2)
for (i in 2:length(years)){
  lines(Lc(df[df$WAVE ==years[i],]$HHINCPC_CPI),col="darkred",lwd=2)
}
lines(Lc(df[df$WAVE ==1991,]$HHINCPC_CPI),col="darkred",lwd=2)

#inequality over province
gini.province = df %>%
  select(HHID,HHINCPC_CPI,T1) %>%
  arrange(HHINCPC_CPI) %>%
  transform(HHINCPC_CPI = ifelse(HHINCPC_CPI < 0, 0, HHINCPC_CPI)) %>%
  group_by(T1) %>%
  summarise(index = ineq(HHINCPC_CPI,type = c("Gini")))


ggplot(gini.province,aes(x = as.numeric(T1),y = index)) +
  geom_line() +
  geom_point()+
  geom_text(aes(label = round(index, 3),hjust = -0.3,vjust = 0.5)) +
  ggtitle("Gini Index Over Year") +
  xlab("Year") + ylab("Gini Index") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

#inequlaity over province over time
gini.years.province = df %>%
  select(HHID,HHINCPC_CPI,T1,WAVE) %>%
  arrange(HHINCPC_CPI) %>%
  transform(HHINCPC_CPI = ifelse(HHINCPC_CPI < 0, 0, HHINCPC_CPI)) %>%
  group_by(T1,WAVE) %>%
  summarise(index = ineq(HHINCPC_CPI,type = c("Gini")))

ggplot(gini.years.province,aes(x = WAVE,y = index)) +
  geom_line() +
  geom_point()+
  geom_text(aes(label = round(index, 3),hjust = -0.3,vjust = 0.5)) +
  #ggtitle("Gini Index Over Year") +
  xlab("Year") + ylab("Gini Index") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))+
  facet_wrap(~T1)

#inequlaity  income source
gini = function(x){
  x = ifelse(x < 0, 0, x)
  return(ineq(x,type = c("Gini")))
}
gini.income <- lapply(df[,c("HHBUS","HHFARM","HHFISH","HHGARD","HHLVST","HHNRWAGE","HHOTHR","HHRETIRE","HHSUB")], gini)

plot(Lc(df$HHBUS),col="darkred",lwd=2)
