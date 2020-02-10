# Read in file
library(readxl)
library(radiant)

#Indicate the file to be used
Bakery <- read_excel("Bakery.xlsx")
View(Bakery)

#There's no missing data here, but if there is, please handle it first.

#Conjoint analysis data needs to be oragnised in a certain way.
#Normally we have one row per respondent
#Remember to factorize variables
Bakery <- Bakery %>%
  gather(respondent, rating, starts_with("Individual")) %>%  # respondent keeps track of the respondent, rating will store the respondent's ratings, and we want to stack every variable that starts with Individual
  mutate(Observations = factor(Observations), respondent = factor(respondent),  
        Price = factor(Price), Crust = factor(Crust), Toppings = factor(Toppings), Organic = factor(Organic))

# Conjoint analysis with 1 respondent -> Individual 1
respondent1 <- Bakery %>% filter(respondent == "Individual 1")

# Perform conjoint analysis
conjoint_respondent1 <- conjoint(respondent1, rvar = "rating", evar = c("Price", "Crust", "Toppings", "Organic"))
summary(conjoint_respondent1)

#Plot the results
plot(conjoint_respondent1)

# Run this regression if you're interested in learning which predictor is significant or what the R-squared of the overall model is.
# We tend not to consider this because it's expected to be non-significant
summary(lm(rating ~ Price + Crust + Toppings + Organic, data = respondent1))

# Predicting preference
# first, you need to establish how many profiles are tested
profiles <- Bakery %>% 
  filter(respondent == "Individual 1") %>% 
  select(Price,Crust,Toppings,Organic)

profiles

#Now you're ready to predict. You are predicting the ratings for the profiles based on the conjoint analysis result of that one individual
predict(conjoint_respondent1, profiles)

#This prediction is based on what this person had answered, which is not so interesting
# We can also predict the unknown profiles

#This is to see all the possible combinations
#First, you list out all the possible combinations
Price <- c("625","425","225")
Crust <- c("Regular","Medium","Large")
Toppings <- c("crisp Capsicum","Baby Corn","Lpaneer","Fresh Tomato")
Organic <- c("Organic","Not organic")

expand.grid(Price, Crust, Toppings, Organic)

# Sometimes, there are a lot of attributes and a lot of levels.
# There is an easier way to get attribute levels than creating the vectors manually:
# Make sure all the attributes are factorized.
# Remember to rename the variables created by expand.grid, so the predict function can read the data

profiles.all <- expand.grid(levels(Bakery$Price),levels(Bakery$Crust),levels(Bakery$Toppings),levels(Bakery$Organic)) %>% 
  rename("Price" = "Var1", "Crust" = "Var2", "Toppings" = "Var3", "Organic" = "Var4") 

# predict the ratings of all profiles
predict(conjoint_respondent1, profiles.all) %>% 
  arrange(desc(Prediction)) # show the pizzas with the highest predicted rating on top

#Let's do the conjoint for all the respondents
# same as before, but the whole dataset
conjoint_allrespondents <- conjoint(Bakery, rvar = "rating", evar = c("Price","Crust","Toppings","Organic")) 

summary(conjoint_allrespondents) 

#Plot it
plot(conjoint_allrespondents)

# Predict ratings for all possible combinations based on all participants
predict(conjoint_allrespondents, profiles.all) %>% 
  arrange(desc(Prediction)) # show the pizzas with the highest predicted rating on top

#Let's predict the market share for different options
#You can use the formula shown in the slides. Or you can see proportionally, how many respondents preferred a certain option.
# use slice() to select rows
market_profiles <- profiles.all %>% 
  slice(c(3, 21, 45, 71)) # from profiles.all, select rows 3, 21, 45, 71 as the four profiles. I choose this randomly here. You should choose the combination that interests you.

market_profiles

# We already know how to predict the ratings
predict(conjoint_allrespondents, market_profiles) %>%
  arrange(desc(Prediction))

#This tell us the overall rating but not the marketshare. To know the share, you need to know how every single respondents would react
# same model as before, but now add by = "respondent"
conjoint_perrespondent <- conjoint(Bakery, rvar = "rating", evar = c("Price","Crust","Toppings","Organic"), by = "respondent")

predict(conjoint_perrespondent, market_profiles) %>% 
  arrange(respondent, desc(Prediction)) # sort by respondent and then by predicted rating

#Retain for each individual only his or her highest rated profile
highest_rated <- predict(conjoint_perrespondent, market_profiles) %>% 
  group_by(respondent) %>% 
  mutate(ranking = rank(Prediction))
# have a look
highest_rated %>% 
  arrange(respondent, ranking)
# we need to retain only the highest ranked pizza
highest_rated <- highest_rated %>% 
  arrange(respondent, ranking) %>% 
  filter(ranking == 4)

highest_rated

#Now you're finally ready to estimate the market share
market_share <- highest_rated %>% 
  group_by(Price, Crust, Toppings, Organic) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

market_share #you can see 425, regular, crisp Capsicum Organic, is prefered by 16 out of 22 respondents
