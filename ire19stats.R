# ----------------------------------------------------------------------------------
# | AN INTRODUCTION TO STATISTICS IN R
# | STEVE REILLY (sreilly@usatoday.com, @BySteveReilly)
# | IRE 2019, Houston, Texas
# --------------------------------------------------------------------------------
# | GOALS FOR SESSION
# | 
# | 1. Introduction and workspace setup
# | 2. Using R for descriptive and summary statistics
# | 3. Linear regression
# | 4. Multiple regression
# | 5. Basic visualization
# -------------------------------------------------------------------------------
# | CONTENTS
# | 
# | We will use this R script to analyze the following datasets:
# | 
# | 1. Diamond prices (pre-loaded practice dataset)
# | 2. Massachusetts school test scores (pre-loaded practice dataset)
# | 3. California school test scores (pre-loaded practice dataset)
# | 4. NFL game TV ratings and 2016 presidential election results, 2016-2017 (USA Today analysis)
# -------------------------------------------------------------------------------
# | CLASS MATERIALS:
# | 
# | - GitHub repo: https://tinyurl.com/ire19statscode
# | - Slides: tinyurl.com/ire19stats
# -------------------------------------------------------------------------------

## Load libraries
library("Ecdat")
library("ggplot2")

#############################
# Analysis 1: Diamond Prices #
#############################

#Load and explore data 
data(Diamond)           # load the data
View(Diamond)           # view the data 
?Diamond
head(Diamond,5)         # view first five rows
tail(Diamond)           # view last five rows
names(Diamond)          # view column names
str(Diamond)            # view structure of the data
summary(Diamond)        # view descriptive statistics

# Basic visualization with trendline
plot(Diamond$carat, Diamond$price, main = "Diamond Carats vs. price", xlab = 'Carat', ylab = 'Price', pch = 21, bg = 'gold', ylim = c(0,16000))
abline(lm(Diamond$price~Diamond$carat))

# Visualize with ggplot
ggplot(Diamond,aes(x=carat,y=price))+ 
  geom_point(colour="red",
             alpha = 0.2,
             size = 2) + 
  geom_smooth(method="lm") +
  labs(x= "Carat Size",
       y = "Price ($)",
       title = "Diamond Carats vs. price")

# Linear regresion analysis
lm.out <- lm(Diamond$price~Diamond$carat)
summary(lm.out)

# Interpret the results
# - For every one-carat increase in the diamond, its value increases $11,599.
# - This model is statistically significant at the 99.9% confidence level (p value < 0.001).
# - There is a statistically significant relationship between the number of number of carats and price of a diamond.
# - The adjusted R-squared is 0.89, meaning 89% of the variation in price is explained by carat.


########################################## 
## Analysis 2: Massachusetts Test Results #
##########################################

### Explore the data frame
data(MCAS)
?MCAS
View(MCAS)
head(MCAS)
tail(MCAS)
names(MCAS)
summary(MCAS) 
str(MCAS)

### Visualize the data 

# Create a histogram
hist(MCAS$totsc4, main="Fourth grade test scores")

# View scatterplot
ggplot(MCAS,aes(x=tchratio, y=totsc4))+ geom_point() + geom_smooth(method="lm") +
  labs(y= "Fourth-grade test scores",
       x = "Student-teacher ratio",
       title = "Fourth-grade test scores and student-teacher ratio")

### Regression analysis

# Bivariate regression: Test scores and student-teacher ratio
Model1 <-lm(MCAS$totsc4~MCAS$tchratio)
summary(Model1)

#Interpret the results
# - A one-unit increase in the student-teacher ratio in this model decreases the average test score by 1.7 points.
# - The relationship between student-teacher ratio and test scores is statistically significant at the 99.9% confidence level.
# - However, this model only explains 7 percent of the variation in test scores.

# Multiple linear regression: Test scores and student-teacher ratio+spending per pupil
Model2 <-lm(MCAS$totsc4~MCAS$tchratio+MCAS$regday)
summary(Model2)

#Interpret the results
# -This model is also statistically significant, but still only explains about 6 percent of the variation in test scores.

# Multiple linear regression: Test scores and student-teacher ratio+spending per pupil+percent of students receiving free lunch
Model3 <-lm(MCAS$totsc4~MCAS$tchratio+MCAS$percap+MCAS$lnchpct)
summary(Model3)

# Interpret the results
# -There is a statistically significant relationship between all variables included in this model and test scores.
# -Model 3 is a much better fit than Models 1 and 2. This model explains 67 percent of the variation in test scores.
# -In this model, a one-unit increase in student-teacher ratio decreases the average test score by 0.7 points after controlling per-capita income and students eligible for free/reduced lunch.

#######################################
## Analysis 3: California test results#
#######################################

### Explore the data frame
data(Caschool)
View(Caschool)
names(Caschool)
str(Caschool)
summary(Caschool)
head(Caschool)

### Visualize the data

# Visualize individual variables with histograms
hist(Caschool$testscr, main="Test Scores")
hist(Caschool$avginc, main="Average Income")
hist(Caschool$compstu, main="Computers Per Student")
hist(Caschool$str, main="Student Teacher Ratio")

### Regression analysis 

# Linear regression: Test scores and student-teacher ratio
Model4 <-lm(Caschool$testscr~Caschool$str)
summary(Model4)

# Interpret the results
# -There is a statistically significant relationship between student-teacher ratio and test scores.
# -However, this model only explains roughly 5 percent of the variation in test scores.
# -A one-unit increase in the student-teacher ratio in this model decreases the average test score by 2.3 points.

# Multiple regression: Test scores and student-teacher ratio + computers-per-student
Model5 <-lm(Caschool$testscr~Caschool$str+Caschool$compstu)
summary(Model5)

# Interpret the results
# -This model is less statistically significant, and still only explains about 9 percent of the variation in test scores.
# -After controlling for computers-per-student, in this model a one-unit increase in student-teacher-ratio equals a 0.2 point decrease in test scores.

# Multiple regression analysis: Test scores and student-teacher ratio + computers-per-student + average income
Model6 <- lm(Caschool$testscr~Caschool$str+Caschool$compstu+Caschool$avginc)
summary(Model6)

# Interpret the results
# -After controlling for both computers-per-student and average income, student-teacher ratio is no longer staistically significat at th 95 percent confidence level.
# -This does not mean there isn’t a real-world relationship between student-teacher ratio and student test scores.
# -Importantly, note that this model still only accounts for 52% of the variation in student test scores. There must be omitted variables.
# -This analysis shows there is need for further inquiry. We do some reporting and analysis to find what other omitted variables need to be factored into the analysis.

##########################################
# Analysis 4: NFL TV Ratings and politics#
##########################################

# ----------------------------------------------------------
### Sidenote on social science methods: Hypothesis testing###


# "Ratings for NFL football are way down except before game starts, when people tune in to see whether or not our country will be disrespected!"
# - President Donald Trump, 9/26/2017: https://twitter.com/realDonaldTrump/status/912624892239077376

# Null hypothesis: There is no stastically significant relationship between political support for President Trump and NFL ratings.
#
# Alternative hypothesis: There is a statistically significant relationship between political support for President Trump and NFL ratings.
# ----------------------------------------------------------

### Import data
NFL.ratings <- read.csv("https://raw.githubusercontent.com/steve-reilly/ire19/master/nfl_ratings.csv")

#### View summary statistics
View(NFL.ratings)
str(NFL.ratings)
summary(NFL.ratings)

#### Visualize data
ggplot(NFL.ratings,aes(y=NFL.ratings$ratings.change, x=NFL.ratings$trump.percent))+ geom_point() + geom_smooth(method="lm") + 
  labs(x= "Percent of vote won by Trump (%)",
       y = "Change in NFL ratings 2016-2017",
       title = "Trump vote vs. change in NFL ratings")

### Linear regresion analysis: NFL ratings change vs. Trump voting percentage
lm.out <- lm(NFL.ratings$ratings.change~NFL.ratings$trump.percent)
summary(lm.out)

# Interpret the results
# - A one-percentage point increase in the percentage of a TV market which voted for President Trump is associated with a change of 6-unit decrease in the Sunday Night Football rating
# - However, the relationship between votes for Trump and is not statistically significant at the 95% percent confidence level, so we cannot reject the null hypothesis.
# -In this model, political support for President Trump explains only 8 percent of variation in Sunday Night Football TV rating changes.