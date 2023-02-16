################
# Load libraries
################
library(ggplot2)
library(moments)


########################
# Load private libraries
########################
source("ggplot2_functions.R")


####################
# Set global options
####################
options(scipen=5)
set.seed(17)


###########
# Load data
###########
data <- read.delim("data_balanced.tsv")


##############
# Test balance
##############
gender_count <- table(data_balanced$author_gender)
gender_count

dialect_count <- table(data_balanced$author_dialect_broad)
dialect_count

century_count <- table(data_balanced$novel_period)
century_count

ggplot(data_balanced,
       aes(x = novel_date)) +
  geom_histogram(bins = 20, color=border_color, fill=fill_color) +
  xlab("Publication Year") +
  ylab("Count") +
  grey_theme


###############
# Subgroup data
###############
data_balanced_male <- data_balanced[data_balanced$author_gender == "male", ]
data_balanced_female <- data_balanced[data_balanced$author_gender == "female", ]
data_balanced_ame <- data_balanced[data_balanced$author_dialect_broad == "American", ]
data_balanced_bre <- data_balanced[data_balanced$author_dialect_broad == "British", ]
data_balanced_male_ame <- data_balanced[data_balanced$author_gender == "male" & data_balanced$author_dialect_broad == "American", ]
data_balanced_female_ame <- data_balanced[data_balanced$author_gender == "female" & data_balanced$author_dialect_broad == "American", ]
data_balanced_male_bre <- data_balanced[data_balanced$author_gender == "male" & data_balanced$author_dialect_broad == "British", ]
data_balanced_female_bre <- data_balanced[data_balanced$author_gender == "female" & data_balanced$author_dialect_broad == "British", ]
data_balanced_19century <- data_balanced[data_balanced$novel_date <= 1900, ]
data_balanced_20century <- data_balanced[data_balanced$novel_date > 1900, ]
data_balanced_19century_ame <- data_balanced_19century[data_balanced_19century$author_dialect_broad == "American", ]
data_balanced_19century_bre <- data_balanced_19century[data_balanced_19century$author_dialect_broad == "British", ]
data_balanced_20century_ame <- data_balanced_20century[data_balanced_20century$author_dialect_broad == "American", ]
data_balanced_20century_bre <- data_balanced_20century[data_balanced_20century$author_dialect_broad == "British", ]


################################
# Effects of text length on MTLD
################################
#### testing normality ####
ggplot(data_balanced,
       aes(x = tokens)) +
  geom_histogram(bins = 20, color=border_color, fill=fill_color) +
  xlab("Word Tokens") +
  ylab("Count") +
  grey_theme

shapiro.test(data_balanced$tokens)
skewness(data_balanced$tokens)
kurtosis(data_balanced$tokens)

ggplot(data_balanced,
       aes(x = regular_mtld)) +
  geom_histogram(bins = 20, color=border_color, fill=fill_color) +
  xlab("MTLD") +
  ylab("Count") +
  grey_theme

shapiro.test(data_balanced$regular_mtld)
skewness(data_balanced$regular_mtld)
kurtosis(data_balanced$regular_mtld)

#### correlation tests ####
cor.test(data_balanced$tokens, data_balanced$regular_mtld)
cor.test(data_balanced$tokens, data_balanced$regular_mtld, method = "spearman")

ggplot(data = data_balanced,
       mapping = aes(x = tokens,
                     y = regular_mtld)) + 
  geom_point(size = 2, colour = "#4f5458") +
  xlab("Word Tokens") +
  ylab("MTLD") + 
  geom_smooth(method = "lm", color = "#4f5458", se = FALSE) +
  theme(
    panel.background = element_rect(fill = '#F5F5F5'),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "#C8C8C8"),
    panel.grid.minor = element_line(size = 0.5, linetype = 'solid',
                                    colour = "#C8C8C8"))


##################################
# Effects of independent variables
##################################

#### MTLD~gender in the whole sample ####
# tests
tapply(data_balanced$regular_mtld, data_balanced$author_gender, mean)
tapply(data_balanced$regular_mtld, data_balanced$author_gender, sd)
tapply(data_balanced$regular_mtld, data_balanced$author_gender, shapiro.test)
t.test(data_balanced$regular_mtld ~ data_balanced$author_gender)

# calculating power of the t.test
# calculating pooled sd
# find sample standard deviation of each sample
data_balanced_male <- data_balanced[data_balanced$author_gender == "male", ]
data_balanced_female <- data_balanced[data_balanced$author_gender == "female", ]
s1 <- sd(data_balanced_male$regular_mtld)
s2 <- sd(data_balanced_female$regular_mtld)
# find sample size of each sample
n1 <- length(data_balanced_male$regular_mtld)
n2 <- length(data_balanced_female$regular_mtld)
# calculate pooled standard deviation
pooled <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2) / (n1+n1-2))
# power t-test
power.t.test(n = 150, delta = 4.86908, sd = pooled, sig.level = 0.05, type ="two.sample", alternative = "two.sided")

gender_boxplots <- ggplot(data_balanced,
                          aes(x = author_gender, y = regular_mtld)) +
  geom_boxplot(color=boxplot_border_color, fill=boxplot_fill_color) +
  stat_summary(fun=mean, geom="point", shape=15, size=4, color=boxplot_border_color) +
  labs(x = "Gender",
       y = "MTLD") +
  grey_theme

#### MTLD~dialect in the whole sample ####
tapply(data_balanced$regular_mtld, data_balanced$author_dialect_broad, mean)
tapply(data_balanced$regular_mtld, data_balanced$author_dialect_broad, sd)
tapply(data_balanced$regular_mtld, data_balanced$author_dialect_broad, shapiro.test)
t.test(data_balanced$regular_mtld ~ data_balanced$author_dialect_broad)

dialect_boxplots <- ggplot(data_balanced,
                           aes(x = author_dialect_broad, y = regular_mtld)) +
  geom_boxplot(color=boxplot_border_color, fill=boxplot_fill_color) +
  stat_summary(fun=mean, geom="point", shape=15, size=4, color=boxplot_border_color) +
  labs(x = "Dialect",
       y = "MTLD") +
  grey_theme

#### MTLD~time in the whole sample ####
#### testing normality ####
ggplot(data_balanced,
       aes(x = novel_date)) +
  geom_histogram(bins = 20, color=border_color, fill=fill_color) +
  xlab("Word Tokens") +
  ylab("Count") +
  grey_theme

shapiro.test(data_balanced$novel_date)
skewness(data_balanced$novel_date)
kurtosis(data_balanced$novel_date)

#### correlation tests ####
cor.test(data_balanced$novel_date, data_balanced$regular_mtld)
cor.test(data_balanced$novel_date, data_balanced$regular_mtld, method = "spearman")

date_scatterplot <- ggplot(data = data_balanced,
                           mapping = aes(x = novel_date,
                                         y = regular_mtld)) + 
  geom_point(size = 2, colour = fill_color) +
  xlab("Years") +
  ylab("MTLD") + 
  geom_smooth(method = "lm", color = fill_color, se = FALSE) +
  grey_theme

multiplot(gender_boxplots, dialect_boxplots, date_scatterplot, cols=3)


###########################################
# Combined effects of independent variables
###########################################

#### MTLD~dialect~gender ####
dialect_gender_anova <- aov(data_balanced$regular_mtld ~ data_balanced$author_dialect * data_balanced$author_gender)
summary(dialect_gender_anova)
# Details 
tapply(data_balanced_ame$regular_mtld, data_balanced_ame$author_gender, mean)
tapply(data_balanced_ame$regular_mtld, data_balanced_ame$author_gender, sd)
t.test(data_balanced_ame$regular_mtld ~ data_balanced_ame$author_gender)

dialect_gender_ame <- ggplot(data_balanced_ame,
                             aes(x = author_gender, y = regular_mtld)) +
  geom_boxplot(color=boxplot_border_color, fill=boxplot_fill_color) +
  stat_summary(fun=mean, geom="point", shape=15, size=4, color=boxplot_border_color) +
  labs(title = "American English",
       x = "Gender",
       y = "MTLD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  grey_theme

tapply(data_balanced_bre$regular_mtld, data_balanced_bre$author_gender, mean)
tapply(data_balanced_bre$regular_mtld, data_balanced_bre$author_gender, sd)
t.test(data_balanced_bre$regular_mtld ~ data_balanced_bre$author_gender)

dialect_gender_bre <- ggplot(data_balanced_bre,
                             aes(x = author_gender, y = regular_mtld)) +
  geom_boxplot(color=boxplot_border_color, fill=boxplot_fill_color) +
  stat_summary(fun=mean, geom="point", shape=15, size=4, color=boxplot_border_color) +
  labs(title = "British English",
       x = "Gender",
       y = "MTLD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  grey_theme

multiplot(dialect_gender_ame, dialect_gender_bre, cols=2)

#### MTLD~gender~dialect ####
# male
tapply(data_balanced_male$regular_mtld, data_balanced_male$author_dialect_broad, mean)
tapply(data_balanced_male$regular_mtld, data_balanced_male$author_dialect_broad, sd)
t.test(data_balanced_male$regular_mtld ~ data_balanced_male$author_dialect_broad)

ggplot(data_balanced_male,
       aes(x = author_dialect_broad, y = regular_mtld)) +
  geom_boxplot(color=boxplot_border_color, fill=boxplot_fill_color) +
  labs(x = "Dialect",
       y = "MTLD") +
  grey_theme

# female
tapply(data_balanced_female$regular_mtld, data_balanced_female$author_dialect_broad, mean)
tapply(data_balanced_female$regular_mtld, data_balanced_female$author_dialect_broad, sd)
t.test(data_balanced_female$regular_mtld ~ data_balanced_female$author_dialect_broad)

ggplot(data_balanced_female,
       aes(x = author_dialect_broad, y = regular_mtld)) +
  geom_boxplot(color=boxplot_border_color, fill=boxplot_fill_color) +
  labs(x = "Dialect",
       y = "MTLD") +
  grey_theme

#### MTLD~gender~time ####
# MTLD~time male
cor.test(data_balanced_male$novel_date, data_balanced_male$regular_mtld)
ggplot(data = data_balanced_male,
       mapping = aes(x = novel_date,
                     y = regular_mtld)) + 
  geom_point(size = 2, colour = fill_color) +
  xlab("Years") +
  ylab("MTLD") + 
  geom_smooth(method = "lm", color = fill_color, se = FALSE) +
  grey_theme

# MTLD~time female
cor.test(data_balanced_female$novel_date, data_balanced_female$regular_mtld)
ggplot(data = data_balanced_female,
       mapping = aes(x = novel_date,
                     y = regular_mtld)) + 
  geom_point(size = 2, colour = fill_color) +
  xlab("Years") +
  ylab("MTLD") + 
  geom_smooth(method = "lm", color = fill_color, se = FALSE) +
  grey_theme

# MTLD~time male and female in one scatterplot
ggplot(data = data_balanced,
       mapping = aes(x = novel_date,
                     y = regular_mtld,
                     shape = author_gender)) + 
  geom_point(size = 2, colour = fill_color) +
  scale_shape_manual(values=c(19, 6)) +
  xlab("Years") +
  ylab("MTLD") + 
  geom_smooth(method = "lm", color = fill_color, se = FALSE) +
  grey_theme

# MTLD~gender separately in 19th and 20th century
tapply(data_balanced_19century$regular_mtld, data_balanced_19century$author_gender, mean)
tapply(data_balanced_19century$regular_mtld, data_balanced_19century$author_gender, sd)
t.test(data_balanced_19century$regular_mtld ~ data_balanced_19century$author_gender)

dialect_gender_19century <- ggplot(data_balanced_19century,
                                   aes(x = author_gender, y = regular_mtld)) +
  geom_boxplot(color=boxplot_border_color, fill=boxplot_fill_color) +
  stat_summary(fun=mean, geom="point", shape=15, size=4, color=boxplot_border_color) +
  labs(title = "19th century",
       x = "Gender",
       y = "MTLD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  grey_theme

tapply(data_balanced_20century$regular_mtld, data_balanced_20century$author_gender, mean)
tapply(data_balanced_19century$regular_mtld, data_balanced_19century$author_gender, sd)
t.test(data_balanced_20century$regular_mtld ~ data_balanced_20century$author_gender)

dialect_gender_20century <- ggplot(data_balanced_20century,
                                   aes(x = author_gender, y = regular_mtld)) +
  geom_boxplot(color=boxplot_border_color, fill=boxplot_fill_color) +
  stat_summary(fun=mean, geom="point", shape=15, size=4, color=boxplot_border_color) +
  labs(title = "20th century",
       x = "Gender",
       y = "MTLD") +
  theme(plot.title = element_text(hjust = 0.5)) +
  grey_theme

multiplot(dialect_gender_19century, dialect_gender_20century, cols=2)

#### MTLD~dialect~time ####
# MTLD~time American
cor.test(data_balanced_ame$novel_date, data_balanced_ame$regular_mtld)
ggplot(data = data_balanced_ame,
       mapping = aes(x = novel_date,
                     y = regular_mtld)) + 
  geom_point(size = 2, colour = fill_color) +
  xlab("Years") +
  ylab("MTLD") + 
  geom_smooth(method = "lm", color = fill_color, se = FALSE) +
  grey_theme

# MTLD~time British
cor.test(data_balanced_bre$novel_date, data_balanced_bre$regular_mtld)
ggplot(data = data_balanced_bre,
       mapping = aes(x = novel_date,
                     y = regular_mtld)) + 
  geom_point(size = 2, colour = fill_color) +
  xlab("Years") +
  ylab("MTLD") + 
  geom_smooth(method = "lm", color = fill_color, se = FALSE) +
  grey_theme

# MTLD~time American and British in one scatterplot
ggplot(data = data_balanced,
       mapping = aes(x = novel_date,
                     y = regular_mtld,
                     shape = author_dialect_broad)) + 
  geom_point(size = 2, colour = fill_color) +
  scale_shape_manual(values=c(19, 6)) +
  xlab("Years") +
  ylab("MTLD") + 
  geom_smooth(method = "lm", color = fill_color, se = FALSE) +
  grey_theme

# MTLD~dialect separately in 19th and 20th century
tapply(data_balanced_19century$regular_mtld, data_balanced_19century$author_dialect_broad, mean)
t.test(data_balanced_19century$regular_mtld ~ data_balanced_19century$author_dialect_broad)

ggplot(data_balanced_19century,
       aes(x = author_dialect_broad, 
           y = regular_mtld)) +
  geom_boxplot(color=boxplot_border_color, fill=boxplot_fill_color) +
  labs(x = "Gender",
       y = "MTLD") +
  grey_theme

tapply(data_balanced_20century$regular_mtld, data_balanced_20century$author_dialect_broad, mean)
t.test(data_balanced_20century$regular_mtld ~ data_balanced_20century$author_dialect_broad)

ggplot(data_balanced_20century,
       aes(x = author_dialect_broad, 
           y = regular_mtld)) +
  geom_boxplot(color=boxplot_border_color, fill=boxplot_fill_color) +
  labs(x = "Gender",
       y = "MTLD") +
  grey_theme

#### MTLD~gender~dialect~time ####
tapply(data_balanced_19century_ame$regular_mtld, data_balanced_19century_ame$author_gender, mean)
tapply(data_balanced_19century_ame$regular_mtld, data_balanced_19century_ame$author_gender, sd)
t.test(data_balanced_19century_ame$regular_mtld ~ data_balanced_19century_ame$author_gender)

tapply(data_balanced_19century_bre$regular_mtld, data_balanced_19century_bre$author_gender, mean)
tapply(data_balanced_19century_bre$regular_mtld, data_balanced_19century_bre$author_gender, sd)
t.test(data_balanced_19century_bre$regular_mtld ~ data_balanced_19century_bre$author_gender)

gender_19century_ame <- ggplot(data_balanced_19century_ame,
                               aes(x = author_gender, y = regular_mtld)) +
  geom_boxplot(color=boxplot_border_color, fill=boxplot_fill_color) +
  stat_summary(fun=mean, geom="point", shape=15, size=4, color=boxplot_border_color) +
  labs(title = "19th century American English",
       x = "Gender",
       y = "MTLD") +
  ylim(50, 140) +
  theme(plot.title = element_text(hjust = 0.5)) +
  grey_theme

gender_19century_bre <- ggplot(data_balanced_19century_bre,
                               aes(x = author_gender, y = regular_mtld)) +
  geom_boxplot(color=boxplot_border_color, fill=boxplot_fill_color) +
  stat_summary(fun=mean, geom="point", shape=15, size=4, color=boxplot_border_color) +
  labs(title = "19th century British English",
       x = "Gender",
       y = "MTLD") +
  ylim(50, 140) +
  theme(plot.title = element_text(hjust = 0.5)) +
  grey_theme

tapply(data_balanced_20century_ame$regular_mtld, data_balanced_20century_ame$author_gender, mean)
tapply(data_balanced_20century_ame$regular_mtld, data_balanced_20century_ame$author_gender, sd)
t.test(data_balanced_20century_ame$regular_mtld ~ data_balanced_20century_ame$author_gender)

tapply(data_balanced_20century_bre$regular_mtld, data_balanced_20century_bre$author_gender, mean)
tapply(data_balanced_20century_bre$regular_mtld, data_balanced_20century_bre$author_gender, sd)
t.test(data_balanced_20century_bre$regular_mtld ~ data_balanced_20century_bre$author_gender)

gender_20century_ame <- ggplot(data_balanced_20century_ame,
                               aes(x = author_gender, y = regular_mtld)) +
  geom_boxplot(color=boxplot_border_color, fill=boxplot_fill_color) +
  stat_summary(fun=mean, geom="point", shape=15, size=4, color=boxplot_border_color) +
  labs(title = "20th century American English",
       x = "Gender",
       y = "MTLD") +
  ylim(50, 140) +
  theme(plot.title = element_text(hjust = 0.5)) +
  grey_theme

gender_20century_bre <- ggplot(data_balanced_20century_bre,
                               aes(x = author_gender, y = regular_mtld)) +
  geom_boxplot(color=boxplot_border_color, fill=boxplot_fill_color) +
  stat_summary(fun=mean, geom="point", shape=15, size=4, color=boxplot_border_color) +
  labs(title = "20th century British English",
       x = "Gender",
       y = "MTLD") +
  ylim(50, 140) +
  theme(plot.title = element_text(hjust = 0.5)) +
  grey_theme

multiplot(gender_19century_ame, gender_19century_bre, gender_20century_ame, gender_20century_bre, cols=4)

#### time as continuous variable ####
cor.test(data_balanced_female_bre$regular_mtld, data_balanced_female_bre$novel_date)
cor.test(data_balanced_male_bre$regular_mtld, data_balanced_male_bre$novel_date)
ggplot(data = data_balanced_bre,
       mapping = aes(x = novel_date,
                     y = regular_mtld,
                     shape = author_gender)) + 
  geom_point(size = 2, colour = fill_color) +
  scale_shape_manual(values=c(19, 6)) +
  xlab("Years") +
  ylab("MTLD") + 
  geom_smooth(method = "lm", color = fill_color, se = FALSE) +
  grey_theme

cor.test(data_balanced_female_ame$regular_mtld, data_balanced_female_ame$novel_date)
cor.test(data_balanced_male_ame$regular_mtld, data_balanced_male_ame$novel_date)
ggplot(data = data_balanced_ame,
       mapping = aes(x = novel_date,
                     y = regular_mtld,
                     shape = author_gender)) + 
  geom_point(size = 2, colour = fill_color) +
  scale_shape_manual(values=c(19, 6)) +
  xlab("Years") +
  ylab("MTLD") + 
  geom_smooth(method = "lm", color = "#4f5458", se = FALSE) +
  grey_theme
