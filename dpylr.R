### Applied Modern Statistical Learning Methods


library(dplyr)
library(hflights)

View(hflights)


#Q2:

hflights <- tbl_df(hflights)

g <- select(hflights, ActualElapsedTime, AirTime, ArrDelay, DepDelay)

g[1,]

#q3:

select(hflights, Origin:Cancelled )


#q4

select(hflights, c(Year:DayOfWeek, ArrDelay:Diverted))


#q6

select(hflights, UniqueCarrier,
       ends_with("Num"),
       starts_with("Cancel"))


#q7

f=mutate(hflights, GroundTime= TaxiIn + TaxiOut)

#q8

#checking if it's in the data

"a" %in% c("b", "c")

f1 <- filter(hflights, Distance >3000)


#q9

f3 <- filter(hflights, TaxiIn +TaxiOut > AirTime)

#or

filter(f, GroundTime > AirTime)


#q10
f4 <- filter(hflights, DayOfWeek %in% c(6,7), Cancelled==1)


#q11

arrange(hflights, UniqueCarrier,
        desc(DepDelay))

#q12


arrange(hflights, ArrDelay + DepDelay)

#q13

tr <- filter(hflights, Dest =="DFW", DepTime < 800)
arrange(tr, desc(AirTime))

##orr

hflights %>%
  filter(Dest == "DFW", DepTime<800) %>%
  arrange(desc(AirTime))

#q14

min_dist = min(hflights$Distance)
max_dist = max(hflights$Distance)
grouped = group_by(min_dist, max_dist)
summarise(grouped)  


#q15

hflights %>%
  filter(Diverted ==1) %>%
  summarize(max_div= max(Distance))


#q16

hflights %>%
  summarize(n_obs = n(),
            n_carriers = n_distinct(UniqueCarrier),
            n_dest = n_distinct(Dest),
            dest100 = nth(Dest, 100))


#q17

hflights %>%
  mutate(diff= TaxiOut-TaxiIn) %>%
  filter(diff != "NA") %>%
  summarize(avg= mean(diff))

#q18


hflights %>%
  select(Dest, UniqueCarrier, Distance, ActualElapsedTime) %>%
  mutate(RealTime = ActualElapsedTime + 100)

#q19

hflights %>%
  group_by(UniqueCarrier)%>%
  summarize(n_flights = n(),
            n_canc = sum(Cancelled == 1),
            avg_delay = mean(ArrDelay, na.rm = T)) %>%
  arrange(avg_delay, n_canc)

