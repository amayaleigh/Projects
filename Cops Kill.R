copskill<-read.csv("Downloads/FATAL ENCOUNTERS DOT ORG SPREADSHEET (See Read me tab) - Form Responses-2.csv")

#i want to use unique id, age, gender, race with imputations, date of injury, 
#location of death city, location of death state, agency responsible for death, cause of death


copskill2<-data.frame(copskill$Unique.ID,copskill$Subject.s.age,copskill$Subject.s.gender,
                      copskill$Subject.s.race.with.imputations, copskill$Date.of.injury.resulting.in.death..month.day.year.
                      ,copskill$Cause.of.death, copskill$Date..Year.,copskill$Latitude,
                      copskill$Longitude, copskill$Location.of.death..county.,copskill$Location.of.death..state.)
summary(copskill2)
str(copskill2)


levels(copskill2$copskill.Subject.s.race.with.imputations)

library(plyr)

#Death frequency by age, gender, race, city, state, cause
#count(copskill2$copskill.Subject.s.age)
#count(copskill2$copskill.Subject.s.gender)
count(copskill2$copskill.Subject.s.race.with.imputations)
#count(copskill2$copskill.Cause.of.death)

nrow(copskill2)
#Percent Black Deaths
#7670/28294
#Percent White Deaths
#13395/28294
#Percent Hispanic Deaths
#4576/28294

copskill2$copskill.Date.of.injury.resulting.in.death..month.day.year. <- as.Date(copskill2$copskill.Date.of.injury.resulting.in.death..month.day.year., format="%m/%d/%Y")

#deaths by month and year
tab <- table(cut(copskill2$copskill.Date.of.injury.resulting.in.death..month.day.year., 'month'))
data.frame(Date=format(as.Date(names(tab)), '%m/%Y'),
           Frequency=as.vector(tab))
#deaths per year
tab2 <- table(cut(copskill2$copskill.Date.of.injury.resulting.in.death..month.day.year., 'year'))
data.frame(Date=format(as.Date(names(tab2)), '%Y'),
           Frequency=as.vector(tab2))

#clean age and bin
copskill2$copskill.Subject.s.age<-as.character((copskill2$copskill.Subject.s.age))
copskill2$copskill.Subject.s.age<-as.numeric((copskill2$copskill.Subject.s.age))



#copskill2$copskill.Location.of.death..zip.code. <- as.numeric(copskill2$copskill.Location.of.death..zip.code.)

replace(copskill2$copskill.Subject.s.age,c("18-25","20s","20s-30s","25-35","25'","30s",
                                           "40s","40-50","45 or 49", "46/53",
                                           "50s","60s","70s"),c(18,20,25,25,25,30,40,40,45,50,50,60,70))

replace(copskill2$copskill.Subject.s.age, c("10 months","11 mon", "18 months","2 months",
                                            "3 months","4 months","6 months", "7 mon",
                                            "7 months","8 months", "9 months"),c(0.5,0.5,1,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5))

labs <- c(paste(seq(0, 70, by = 10), seq(0 + 10 - 1, 80 - 1, by = 10),
                sep = "-"), paste(80, "+", sep = ""))
labs
copskill2$agegroups <- cut(copskill2$copskill.Subject.s.age, breaks = c(seq(0, 80, by = 10), Inf), labels = labs, right = FALSE)

count(copskill2$agegroups)
table(copskill2$agegroups)
length(copskill2$agegroups)
percentages<-table(copskill2$agegroups)/length(copskill2$agegroups)*100
percentages


#Remove races with low values Middle Eastern,"Native American/Alaskan","Other Race","Race Unspecified","NA"," ","Asian/Pacific Islander")
copskill2<-copskill2[!(copskill2$copskill.Subject.s.race.with.imputations=="Middle Eastern"),]
copskill2<-copskill2[!(copskill2$copskill.Subject.s.race.with.imputations=="Native American/Alaskan"),]
copskill2<-copskill2[!(copskill2$copskill.Subject.s.race.with.imputations=="Other Race"),]
copskill2<-copskill2[!(copskill2$copskill.Subject.s.race.with.imputations=="Race unspecified"),]
copskill2<-copskill2[!(copskill2$copskill.Subject.s.race.with.imputations=="NA"),]
copskill2<-copskill2[!(copskill2$copskill.Subject.s.race.with.imputations==""),]
copskill2<-copskill2[!(copskill2$copskill.Subject.s.race.with.imputations=="Asian/Pacific Islander"),]
copskill2<-copskill2[!(copskill2$copskill.Subject.s.race.with.imputations=="European American/White"),]
copskill2<-copskill2[!(copskill2$copskill.Subject.s.race.with.imputations=="HIspanic/Latino"),]



library(ggplot2)
#age and cause of death
ggplot(copskill2, aes(x=copskill2$agegroups ,fill=copskill2$copskill.Cause.of.death)) + geom_bar()+labs(y="Number of Victims", x="Age Range")
#age and race
ggplot(copskill2, aes(x=copskill2$agegroups ,fill=copskill2$copskill.Subject.s.race.with.imputations)) + geom_bar()+labs(y="Number of Victims", x="Age Range")



data.frame(sort(table(copskill$Location.of.death..zip.code.), decreasing=TRUE))
countydeaths<-data.frame(sort(table(copskill$Location.of.death..county.), decreasing=TRUE))
#60 counties have more than 100 death by police in 20 years

copskill2

colnames(copskill2) <- c("Unique ID", "age","gender", "race", "Date","Cause of Death","year","latitude","longitude","subregion","region","age group")
colnames(countydeaths)<-c("subregion","freq")


us_map <- map_data("county")
str(us_map)
us_map
us_map$subregion
str(copskill2)
killsmap <- join(copskill2, us_map, by = "subregion", type="left")
killsmap<-join(copskill2,countydeaths,by="subregion")
# Create the map
killsmap



countypop<-read.csv("Downloads/cc-est2018-alldata.csv")
countypop$YEAR[countypop$YEAR == "1"] <- "2010"
countypop$YEAR[countypop$YEAR == "2"] <- "2011"
countypop$YEAR[countypop$YEAR == "3"] <- "2012"
countypop$YEAR[countypop$YEAR == "4"] <- "2013"
countypop$YEAR[countypop$YEAR == "5"] <- "2014"
countypop$YEAR[countypop$YEAR == "6"] <- "2015"
countypop$YEAR[countypop$YEAR == "7"] <- "2016"
countypop$YEAR[countypop$YEAR == "8"] <- "2017"
countypop$YEAR[countypop$YEAR == "9"] <- "2018"
countypop$YEAR[countypop$YEAR == "10"] <- "2019"
countypop$YEAR[countypop$YEAR == "11"] <- "2020"

countypop$YEAR<-as.integer(countypop$YEAR)


states<-state.abb[match(countypop$STATE,state.name)]
str(countypop)

popbycounty<-data.frame(countypop$CITY,states,countypop$YEAR,countypop$WHITE.MALE,countypop$WHITE.FEMALE,countypop$BLACK.MALE,countypop$BLACK.FEMALE,countypop$HISPANIC.MALE,countypop$HISPANIC.FEMALE,countypop$TOTAL.POPULATION)
popbycounty$countypop.CITY<-gsub(paste0("County",collapse = "|"),"", popbycounty$countypop.CITY)
popbycounty$countypop.CITY<-gsub(paste0("Municipality",collapse = "|"),"", popbycounty$countypop.CITY)
popbycounty$countypop.CITY<-gsub(paste0("Borough",collapse = "|"),"", popbycounty$countypop.CITY)
popbycounty$countypop.CITY<-gsub(paste0("Census Area",collapse = "|"),"", popbycounty$countypop.CITY)
popbycounty$countypop.CITY<-gsub(paste0("Parish",collapse = "|"),"", popbycounty$countypop.CITY)

popbycounty<-aggregate(popbycounty[,4:10], popbycounty[,1:3], FUN = sum)

str(popbycounty)
colnames(popbycounty) <- c("subregion","state", "year","white male", "white female", "black male","black female","hispanic male","hispanic female","total population")

popbycounty


library(readr)
write_csv(popbycounty,path="populationbycounty.csv")

