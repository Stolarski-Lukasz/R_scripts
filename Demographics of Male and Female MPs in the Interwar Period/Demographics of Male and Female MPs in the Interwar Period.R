################
# Load libraries
################
library(ggplot2)


########################
# Load private libraries
########################
source("ggplot2_functions.R")


###########
# Load data
###########
MPs <- read.delim2("MPs.tsv")


###############
# Subgroup data
###############

#### all women in the interwar period ####
female <- MPs[MPs$verified_gender == "f", ]
female_1939 <- female[female$entire_service_beginning <= 1939, ]
nrow(female_1939)

#### the comparison corpus - men in commons who started in the interwar period ####
male <- MPs[MPs$verified_gender == "m", ]
male_1918_1939 <- male[male$entire_service_beginning >= 1918 & male$entire_service_beginning <= 1939, ]
male_1918_1939_commons <- male_1918_1939[male_1918_1939$house == "commons" | male_1918_1939$house == "commons_lords", ]
nrow(male_1918_1939_commons)


################
# House analysis
################

#### women ####
table(female_1939$house)
prop.table(table(female_1939$house))

#### men ####
table(male_1918_1939$house)
prop.table(table(male_1918_1939$house))


###################################
# Commons service duration analysis
###################################

#### women in Commons ####
female_1939$commons_service_span <- as.character(female_1939$commons_service_span)
female_1939$commons_service_span <- as.numeric(female_1939$commons_service_span)

mean(female_1939$commons_service_span)
median(female_1939$commons_service_span)

hist(female_1939$commons_service_span)

#### men in Commons ####
class(male_1918_1939_commons$commons_service_span)
male_1918_1939_commons$commons_service_span <- as.character(male_1918_1939_commons$commons_service_span)
male_1918_1939_commons$commons_service_span <- as.numeric(male_1918_1939_commons$commons_service_span)

mean(male_1918_1939_commons$commons_service_span)
median(male_1918_1939_commons$commons_service_span)

hist(male_1918_1939_commons$commons_service_span)

#### violin plots comparing men and women in Commons ####
women_serviceduration <- ggplot(female_1939,
                                aes(x = verified_gender, y = commons_service_span)) +
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=3, color="grey10") +
  coord_cartesian(ylim =  c(0, 50)) +
  labs(x = "Female MPs",
       y = "Years of Service in the House of Commons") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_text(size=12),
        axis.title = element_text(size=14))

men_serviceduration <- ggplot(male_1918_1939_commons,
                              aes(x = verified_gender, y = commons_service_span)) +
  geom_violin() +
  stat_summary(fun.y=median, geom="point", size=3, color="grey10")+
  coord_cartesian(ylim =  c(0, 50)) +
  labs(x = "Male MPs",
       y = "") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_text(size=12),
        axis.title = element_text(size=14))

multiplot(women_serviceduration, men_serviceduration, cols=2)


##################################
# Entire service duration analysis
##################################

#### women entire service ####
entire_service_duration_vector <- c()
counter <- 1
for (duration in female_1939$commons_service_span) {
  if (female_1939$lords_service_beginning[counter] != "unknown") {
    entire_service_duration <- female_1939$commons_service_span[counter] + (as.numeric(as.character(female_1939$lords_service_ending[counter])) - as.numeric(as.character(female_1939$lords_service_beginning[counter])))
    entire_service_duration_vector <- c(entire_service_duration_vector, entire_service_duration)
    counter <- counter + 1
  } else {
    entire_service_duration_vector <- c(entire_service_duration_vector, female_1939$commons_service_span[counter])
    counter <- counter + 1
  }
}
female_1939$entire_service_duration <- entire_service_duration_vector

mean(female_1939$entire_service_duration)
median(female_1939$entire_service_duration)

#### men entire service ####
entire_service_duration_vector <- c()
counter <- 1
for (duration in male_1918_1939_commons$commons_service_span) {
  if (male_1918_1939_commons$lords_service_beginning[counter] == "unknown") {
    entire_service_duration_vector <- c(entire_service_duration_vector, male_1918_1939_commons$commons_service_span[counter])
    counter <- counter + 1
  } else if (male_1918_1939_commons$commons_service_span[counter] == "unknown"){
    entire_service_duration_vector <- c(entire_service_duration_vector, as.numeric(as.character(male_1918_1939_commons$lords_service_ending[counter])) - as.numeric(as.character(male_1918_1939_commons$lords_service_beginning[counter])))
    counter <- counter + 1
  } else {
    entire_service_duration <- male_1918_1939_commons$commons_service_span[counter] + (as.numeric(as.character(male_1918_1939_commons$lords_service_ending[counter])) - as.numeric(as.character(male_1918_1939_commons$lords_service_beginning[counter])))
    entire_service_duration_vector <- c(entire_service_duration_vector, entire_service_duration)
    counter <- counter + 1
  }
}
male_1918_1939_commons$entire_service_duration <- entire_service_duration_vector

mean(male_1918_1939_commons$entire_service_duration)
median(male_1918_1939_commons$entire_service_duration)
