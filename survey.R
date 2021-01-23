haikou <- read.csv("huayi_haikou.csv")
library(psych)
library(GPArotation)
library(ggplot2)
library(readxl)
library(dplyr)

#for haikou offline factor analysis
location_ds <- haikou[,c(15:18)]
fa.parallel(location_ds, fm = 'pa', fa = 'fa')
location_factor <- fa(location_ds,nfactors = 1,rotate = "oblimin",fm="pa")
cor(location_ds)

picutre_movietheme_ds <- haikou [,c(23:28,36:39)] #36:39
fa.parallel(picutre_movietheme_ds, fm = 'pa', fa = 'fa')
picture_movie_factor <- fa(picutre_movietheme_ds,nfactors = 2,rotate = "oblimin",fm="pa")

feeling_ds <- haikou[,c(70:74)]
fa.parallel(feeling_ds, fm = 'pa', fa = 'fa')
feeling_factor <- fa(feeling_ds,nfactors = 1,rotate = "oblimin",fm="pa")

operation_ds <- haikou[,c(29,30,31,32,33,44:58)]
fa.parallel(operation_ds, fm = 'pa', fa = 'fa')
operation_factor <- fa(operation_ds,nfactors = 2,rotate = "oblimin",fm="pa")

tme_ds <- haikou[,c(62:74)]
fa.parallel(tme_ds, fm = 'pa', fa = 'fa')
tme_factor <- fa(tme_ds,nfactors = 2,rotate = "oblimin",fm="pa")

outcome_ds <- haikou[,c(75:78)]
fa.parallel(outcome_ds, fm = 'pa', fa = 'fa')
outcome_factor <- fa(outcome_ds,nfactors = 2,rotate = "oblimin",fm="pa")

################################
########    off-line survey
## read in data from both cities
########################################
ds <- read.csv("/home/rstudio/huayi/huayi.csv", header = T)
ds <- ds[,c(7:87)]

table(ds$gender)
t.test(gender ~ submission_location, data = ds)
t.test(age ~ submission_location, data = ds)
t.test(edu ~ submission_location, data = ds)
t.test(income ~ submission_location, data = ds)
t.test(movie_freq ~ submission_location, data = ds)

try <- ds %>% filter(submission_location == "zhengzhou") %>% select(age)

#visualization - age
to_string <- as_labeller(c(`haikou` = "海口", `zhengzhou` = "郑州"))
ggplot(ds, aes(x=age)) + geom_histogram(binwidth=1, color = "black", fill = "white")+
  facet_wrap(~submission_location, labeller = to_string, nrow = 2) +
  scale_x_continuous(breaks=seq(0,70,10)) +
  theme(
        axis.text=element_text(size=14), 
        plot.title  =element_text(size=14),
        strip.text.x = element_text(size=12))+
  labs(title="两地年龄分布",x="年龄", y = "频数")

#visualization - gender
ds$gender <- as.factor(ds$gender)

ggplot(data.frame(ds), aes(x=gender)) +
  scale_x_discrete(labels=c("1" = "男", "2" = "女"))+
  geom_bar(width = 0.4) +  facet_wrap(~submission_location, labeller = to_string, nrow = 1) +
  theme(aspect.ratio = 2/1, 
        axis.text=element_text(size=14), 
        plot.title  =element_text(size=16),
        strip.text.x = element_text(size=14))+
  labs(title="两地游客性别分布",x="性别", y = "频数")

#dplyr for quick cross-tab
table(ds %>% filter(submission_location == "haikou") %>% select(gender) 
table(ds %>% filter(submission_location == "郑州") %>% select(fav_type_1))
table(ds %>% filter(submission_location == "haikou") %>% select())
table(ds %>% select(movie_freq))

#summarize each movie type and to get an overall sum
ds %>% filter(submission_location == "zhengzhou") %>%select(fav_type_1,fave_type_2,
                                                                  fave_type_3, fave_type_4,
                                                                  fave_type5, fave_type_6,
                                                                  fave_type7, fave_type8,
                                                                  fave_type9, fave_type10,
                                                                  fave_type_11) %>%summarise_all(sum)

##########################################
# survey scale analysis
# aggregate multi-items to be same scale

location <- ds %>% select(starts_with('location')) %>%
  transmute(location = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
photo <-  ds %>% select(starts_with('photo')) %>%
  transmute(photo = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
movietheme<- ds %>% select(starts_with('movietheme')) %>%
  transmute(movietheme = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
service<- ds %>% select(starts_with('service')) %>%
  transmute(service = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
show<- ds %>% select(starts_with('show')) %>%
  transmute(show = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
likeIP<- ds %>% select(starts_with('likeIP')) %>%
  transmute(likeIP = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
cn_culture <- ds %>% select(starts_with('cn_culture')) %>%
  transmute(cn_culture = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
food <- ds %>% select(starts_with('food')) %>%
  transmute(food = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
facility <- ds %>% select(starts_with('facility')) %>%
  transmute(facility = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
product <- ds %>% select(starts_with('product')) %>%
  transmute(product = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
fame <- ds %>% select(starts_with('fame')) %>%
  transmute(fame = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
tme <- ds %>% select(starts_with('tme')) %>%
  transmute(tme = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
value <- ds %>% select(starts_with('value')) %>%
  transmute(value = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
feel <- ds %>% select(starts_with('feel')) %>%
  transmute(feel = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
outcome <- ds %>% select(starts_with('outcome')) %>%
  transmute(outcome = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()

ds_survey <- cbind(location, movietheme, photo, service,show, likeIP,
                   cn_culture, food, facility, product, fame, tme, value, feel, outcome)

#check each scale's scores
order(unlist(lapply(ds_survey[1:11], mean)))

#regression
m <- lm(data = ds_survey, outcome ~ . -tme - value - feel)
m2 <- lm(data = ds_survey, tme ~ . - value - feel - outcome)
m3 <- lm(data = ds_survey, feel ~ . - value - tme - outcome)
m4 <- lm(data = ds_survey, value ~ . - feel - tme - outcome)

############################
## load online data n = 100
###########################

ds0118 <- read.csv("online0118.csv", header = T)
survey0118 <- ds0118[,c(24:82)]

fa.parallel(survey0118, fm = 'pa', fa = 'fa')
survey0118_factor <- fa(survey0118,nfactors = 5,rotate = "oblimin",fm="pa")
library(stats)

fit <- factanal(survey0118, 
                5,                # number of factors to extract
                scores=c("regression"),
                rotation="varimax")

print(fit, digits=2, cutoff=.45, sort=TRUE)

################################
############################
## load online data n = 1000, 1/21/2021
###########################
ds_online <- read_excel("onlinesurvey0121.xlsx")
my_data <- read_excel("my_file.xlsx")

#change english names
names(ds)
colnames(ds_online) <- colnames(ds[,-c(44:47)])

#visualization - age
# remove outlier
ds_online_ageclean <- ds_online %>% filter(age < 68) %>% select(age) 
ds_online %>% filter(17.9 < age & age < 35.1) %>% select(age) 
982/1163

ggplot(ds_online_ageclean, aes(x=age)) + geom_histogram(binwidth=5, color = "black", fill = "white")+
  scale_x_continuous(breaks=seq(0,70,5)) +
  theme(aspect.ratio = 1/2,
    axis.text=element_text(size=14), 
    plot.title  =element_text(size=14),
    strip.text.x = element_text(size=12))+
  labs(title="网络问卷年龄分布",x="年龄", y = "频数")


#visualization - gender
ds_online$gender <- as.factor(ds_online$gender)


ggplot(data.frame(ds_online), aes(x=gender)) +
  scale_x_discrete(labels=c("1" = "男", "2" = "女"))+
  geom_bar(width = 0.4) + 
  theme(aspect.ratio = 1/2, 
        axis.text=element_text(size=14), 
        plot.title  =element_text(size=16),
        strip.text.x = element_text(size=14))+
  labs(title="网络问卷性别分布",x="性别", y = "频数")

#crosstabs - gender
table(ds_online%>% select(gender) )

#crosstabs - income
ds_online$income <- as.factor(ds_online$income)

ds_online %>% select(income) %>%group_by(income) %>%
  summarise(proportion = n()) %>% 
  mutate(Perc = cumsum(100*proportion/sum(proportion))) 

#crosstab -movie freq
ds_online$movie_freq<- as.factor(ds_online$movie_freq)

ds_online %>% select(movie_freq) %>%group_by(movie_freq) %>%
  summarise(proportion = n()) %>% 
  mutate(CumPerc = cumsum(100*proportion/sum(proportion)),
         Perc = 100*proportion/sum(proportion) ) 

#summarize each movie type and to get an overall sum

ds_online %>%select(fav_type_1,fave_type_2,fave_type_3, fave_type_4,
                                                          fave_type5, fave_type_6,
                                                            fave_type7, fave_type8,
                                                            fave_type9, fave_type10,
                                                            fave_type_11)%>%
  summarise_all(sum) 


# survey scale analysis
# aggregate multi-items to be same scale

location <- ds_online %>% select(starts_with('location')) %>%
  transmute(location = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
photo <-  ds_online %>% select(starts_with('photo')) %>%
  transmute(photo = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
movietheme<- ds_online %>% select(starts_with('movietheme')) %>%
  transmute(movietheme = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
service<- ds_online %>% select(starts_with('service')) %>%
  transmute(service = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
show<- ds_online %>% select(starts_with('show')) %>%
  transmute(show = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
likeIP<- ds_online %>% select(starts_with('likeIP')) %>%
  transmute(likeIP = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()

food <- ds_online %>% select(starts_with('food')) %>%
  transmute(food = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
facility <- ds_online %>% select(starts_with('facility')) %>%
  transmute(facility = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
product <- ds_online %>% select(starts_with('product')) %>%
  transmute(product = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
fame <- ds_online %>% select(starts_with('fame')) %>%
  transmute(fame = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
tme <- ds_online %>% select(starts_with('tme')) %>%
  transmute(tme = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
value <- ds_online %>% select(starts_with('value')) %>%
  transmute(value = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
feel <- ds_online %>% select(starts_with('feel')) %>%
  transmute(feel = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()
outcome <- ds_online %>% select(starts_with('outcome')) %>%
  transmute(outcome = rowMeans(., na.rm = TRUE))%>% 
  as.data.frame()

ds_online_survey <- cbind(location, movietheme, photo, service,show, likeIP,
                    food, facility, product, fame, tme, value, feel, outcome)

#check each scale's scores
order(unlist(lapply(ds_online_survey[1:10], mean)))

#regression
m <- lm(data = ds_online_survey, outcome ~ . -tme - value - feel)
m2 <- lm(data = ds_online_survey, tme ~ . - value - feel - outcome)
m3 <- lm(data = ds_online_survey, feel ~ . - value - tme - outcome)
m4 <- lm(data = ds_online_survey, value ~ . - feel - tme - outcome)

#factor loadings
survey0121 <- ds_online[,c(19:60)]

fa.parallel(survey0121, fm = 'pa', fa = 'fa')
survey0121factor <- fa(survey0121,nfactors = 8,rotate = "oblimin",fm="pa")
library(stats)

fit <- factanal(survey0121, 
                11,                # number of factors to extract
                scores=c("regression"),
                rotation="varimax")

print(fit, digits=2, cutoff=.45, sort=TRUE)

