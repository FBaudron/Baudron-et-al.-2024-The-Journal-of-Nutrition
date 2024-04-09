#' ---
#' title: "Minimum cost of diets adequate in vitamin A"
#' author: "Frédéric Baudron"
#' date: "April 9th, 2024"
#' ---


rm(list=ls())


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(openxlsx)
library(dplyr)
library(lpSolve)
library(lpSolveAPI)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(egg)
library(cowplot)
library(grid)


# LOADING THE DATA--------------------------------------------------------------

rec = read.xlsx("Data\\Recommendations.xlsx", sheet = 2)
qty1 = read.xlsx("Data\\Daily quantities of food items consumed.xlsx", sheet = 2)
qty2 = read.xlsx("Data\\Daily quantities of food items consumed.xlsx", sheet = 3)
food = read.xlsx("Data\\Food composition.xlsx", sheet = 2)
group = read.xlsx("Data\\Food items by food groups.xlsx", sheet = 2) 
cost1 = read.xlsx("Data\\Prices by food items.xlsx", sheet = 2)
cost2 = read.xlsx("Data\\Prices by food items.xlsx", sheet = 3)


# DATA MANIPULATION-------------------------------------------------------------

food = unique(food)

group = unique(group)

food = food[which(food$food_item != "beans_fresh"), ]
food = food[which(food$food_item != "beef_dried"), ]
food = food[which(food$food_item != "blackjack_dried"), ]
food = food[which(food$food_item != "cabbage_dried"), ]
food = food[which(food$food_item != "chicken_dried"), ]
food = food[which(food$food_item != "cooking_oil_reused"), ]
food = food[which(food$food_item != "covo_dried"), ]
food = food[which(food$food_item != "cowpea_fresh"), ]
food = food[which(food$food_item != "fish_dried"), ]
food = food[which(food$food_item != "fish_small"), ]
food = food[which(food$food_item != "goat_meat_dried"), ]
food = food[which(food$food_item != "groundnuts_fresh"), ]
food = food[which(food$food_item != "groundnuts_unshelled"), ]
food = food[which(food$food_item != "jam"), ]
food = food[which(food$food_item != "maize_boiled"), ]
food = food[which(food$food_item != "maize_boiled_orange"), ]
food = food[which(food$food_item != "maize_dried_roasted"), ]
food = food[which(food$food_item != "milk_powdered"), ]
food = food[which(food$food_item != "mince_meat_dried"), ]
food = food[which(food$food_item != "okra_dried"), ]
food = food[which(food$food_item != "onions_shallots"), ]
food = food[which(food$food_item != "pigeon"), ]
food = food[which(food$food_item != "popcorn"), ]
food = food[which(food$food_item != "pumkin_seeds"), ]
food = food[which(food$food_item != "pumpkin_small"), ]
food = food[which(food$food_item != "rape_dried"), ]
food = food[which(food$food_item != "roasted_nuts_and_maize"), ]
food = food[which(food$food_item != "sugar"), ]
food = food[which(food$food_item != "tomato_medium"), ]
food = food[which(food$food_item != "tomato_powder"), ]
food = food[which(food$food_item != "tomatoes_small"), ]
food = food[which(food$food_item != "tomatoes_very_small"), ]
food = food[which(food$food_item != "zapnacks"), ]
food = food[which(food$food_item != "pasta_cooked"), ]
food = food[which(food$food_item != "mince_meat_moist"), ]
food = food[which(food$food_item != "maize_dried"), ]


qty1$quantity = ifelse(qty1$quantity > 0, 1, 0)

freq1 = qty1[, c(2:3)] %>%
  group_by(food_item) %>%
  summarise_each(funs(sum))

freq1$quantity = ifelse(freq1$quantity > 1, 1, 0)

names(freq1)[2] = "availability"


qty2$quantity = ifelse(qty2$quantity > 0, 1, 0)

freq2 = qty2[, c(2:3)] %>%
  group_by(food_item) %>%
  summarise_each(funs(sum))

freq2$quantity = ifelse(freq2$quantity > 1, 1, 0)

names(freq2)[2] = "availability"

freq1$availability = ifelse(freq1$food_item == "milk_powdered", 0, freq1$availability)

freq2$availability = ifelse(freq2$food_item == "covo_dried", 0, freq2$availability)
freq2$availability = ifelse(freq2$food_item == "groundnuts_unshelled", 0, freq2$availability)
freq2$availability = ifelse(freq2$food_item == "milk_powdered", 0, freq2$availability)
freq2$availability = ifelse(freq2$food_item == "rape_dried", 0, freq2$availability)

food = merge(food, group, by = "food_item", all.x = TRUE)

food$cereal = ifelse(food$group == "Cereals", (100 - food$Water..g.)/100, 0)
food$dark_green_veg = ifelse(food$group == "Dark green leafy vegetables", (100 - food$Water..g.)/100, 0)
food$egg = ifelse(food$group == "Eggs", (100 - food$Water..g.)/100, 0)
food$fish = ifelse(food$group == "Fish and seafood", (100 - food$Water..g.)/100, 0)
food$flesh = ifelse(food$group == "Flesh meat", (100 - food$Water..g.)/100, 0)
food$legume = ifelse(food$group == "Legumes nuts and seeds", (100 - food$Water..g.)/100, 0)
food$milk = ifelse(food$group == "Milk and milk products", (100 - food$Water..g.)/100, 0)
food$oil = ifelse(food$group == "Oils and fats", (100 - food$Water..g.)/100, 0)
food$organ = ifelse(food$group == "Organ meat", (100 - food$Water..g.)/100, 0)
food$other_fruit = ifelse(food$group == "Other fruits", (100 - food$Water..g.)/100, 0)
food$other_veg = ifelse(food$group == "Other vegetables", (100 - food$Water..g.)/100, 0)
food$vita_fruit = ifelse(food$group == "Vitamin A rich fruits", (100 - food$Water..g.)/100, 0)
food$vita_veg = ifelse(food$group == "Vitamin A rich vegetables and tubers", (100 - food$Water..g.)/100, 0)
food$root = ifelse(food$group == "White roots and tubers", (100 - food$Water..g.)/100, 0)

food1 = merge(food, freq1, by = "food_item", all.x = TRUE)
food1$availability[is.na(food1$availability)] = 0

food1 = unique(food1)
food1 = food1[which(food1$availability == "1"), ]

food2 = merge(food, freq2, by = "food_item", all.y = TRUE)
food2$availability[is.na(food2$availability)] = 0

food2 = unique(food2)
food2 = food2[which(food2$availability == "1"), ]

food1 = merge(food1, cost1, by = "food_item")
food2 = merge(food2, cost2, by = "food_item")


# OPTIMIZATION CURRENT----------------------------------------------------------

# Wet season

N = nrow(rec)

meal = c()
comp = c()
cost = c()

for(i in 1:N) {
  lprec = make.lp(0, nrow(food1))
  set.objfn(lprec, food1$cost_per_g)
  add.constraint(lprec, food1$energy/100, ">=", 14971.4)
  add.constraint(lprec, food1$Vitamin.A..RAE..ug./100, ">=", 570)
  add.constraint(lprec, food1$Protein..g./100, ">=", 19.3)
  add.constraint(lprec, food1$Vitamin.C..total.ascorbic.acid..mg./100, ">=", 90.0)
  add.constraint(lprec, food1$Vitamin.E..alpha.tocopherol...mg./100, ">=", 12.0)
  add.constraint(lprec, food1$Riboflavin..mg./100, ">=", 1.0)
  add.constraint(lprec, food1$Vitamin.B.12..ug./100, ">=", 1.0)
  add.constraint(lprec, food1$Choline..total..mg./100, ">=", 25.6)
  add.constraint(lprec, food1$Calcium..Ca..mg./100, ">=", 453.1)
  add.constraint(lprec, food1$Iron..Fe..mg./100, ">=", 19.2)
  add.constraint(lprec, food1$Zinc..Zn..mg./100, ">=", 11.0)
  add.constraint(lprec, food1$cereal, "<=", 792)
  add.constraint(lprec, food1$dark_green_veg, "<=", 16.6)
  add.constraint(lprec, food1$egg, "<=", 5.3)
  add.constraint(lprec, food1$fish, "<=", 13.6)
  add.constraint(lprec, food1$flesh, "<=", 19.8)
  add.constraint(lprec, food1$legume, "<=", 320.8)
  add.constraint(lprec, food1$milk, "<=", 11.6)
  add.constraint(lprec, food1$oil, "<=", 24)
  add.constraint(lprec, food1$other_fruit, "<=", 17.8)
  add.constraint(lprec, food1$other_veg, "<=", 31.6)
  add.constraint(lprec, food1$vita_veg, "<=", 18.7)
  add.constraint(lprec, food1$root, "<=", 124.6)
  add.constraint(lprec, food1$cereal, ">=", 792/2)
  add.constraint(lprec, food1$dark_green_veg, ">=", 16.6/2)
  add.constraint(lprec, food1$egg, ">=", 5.3/2)
  add.constraint(lprec, food1$fish, ">=", 13.6/2)
  add.constraint(lprec, food1$flesh, ">=", 19.8/2)
  add.constraint(lprec, food1$legume, ">=", 320.8/2)
  add.constraint(lprec, food1$milk, ">=", 11.6/2)
  add.constraint(lprec, food1$oil, ">=", 24/2)
  add.constraint(lprec, food1$other_fruit, ">=", 17.8/2)
  add.constraint(lprec, food1$other_veg, ">=", 31.6/2)
  add.constraint(lprec, food1$root, ">=", 124.6/2)
  RowNames = c("energy", "vitamin_a", "protein", "vitamin_c","vitamin_e",
                "vitamin_b12", "riboflavin", "choline", "calcium", "iron", "zinc",
                "cereal", "dark_green_veg", "egg", "fish", "flesh", "legume",  "milk",
                "oil", "other_fruit", "other_veg", "vita_veg", "root",
                "cereal", "dark_green_veg", "egg",  "fish", "flesh" , "legume", "milk",
                "oil", "other_fruit", "other_veg", "root")
  ColNames = food1$food_item
  dimnames(lprec) = list(RowNames, ColNames)
  solve(lprec)
  print(get.variables(lprec))
  print(get.constraints(lprec))
  print(get.objective(lprec))
  meal = cbind(meal, get.variables(lprec))
  comp = cbind(comp, get.constraints(lprec))
  cost = cbind(cost, get.objective(lprec))
}

rownames(meal) = food1$food_item
rownames(comp) = RowNames
rownames(cost) = c('Cost')

opti_cur1 = rbind(meal, comp, cost)
opti_cur1 = as.data.frame(opti_cur1)
names(opti_cur1) = rec$category

opti_cur1$X = row.names(opti_cur1)


# Dry season

N = nrow(rec)

meal = c()
comp = c()
cost = c()

for(i in 1:N) {
  lprec = make.lp(0, nrow(food2))
  set.objfn(lprec, food2$cost_per_g)
  add.constraint(lprec, food2$energy/100, ">=", 13369.7)
  add.constraint(lprec, food2$Vitamin.A..RAE..ug./100, ">=", 570)
  add.constraint(lprec, food2$Protein..g./100, ">=", 19.6)
  add.constraint(lprec, food2$Vitamin.C..total.ascorbic.acid..mg./100, ">=", 79.1)
  add.constraint(lprec, food2$Vitamin.E..alpha.tocopherol...mg./100, ">=", 12.0)
  add.constraint(lprec, food2$Riboflavin..mg./100, ">=", 0.9)
  add.constraint(lprec, food2$Vitamin.B.12..ug./100, ">=", 0.7)
  add.constraint(lprec, food2$Choline..total..mg./100, ">=", 47.7)
  add.constraint(lprec, food2$Calcium..Ca..mg./100, ">=", 340.7)
  add.constraint(lprec, food2$Iron..Fe..mg./100, ">=", 19.2)
  add.constraint(lprec, food2$Zinc..Zn..mg./100, ">=", 11.0)
  add.constraint(lprec, food2$cereal, "<=", 1018.2)
  add.constraint(lprec, food2$dark_green_veg, "<=", 17.1)
  add.constraint(lprec, food2$egg, "<=", 9.9)
  add.constraint(lprec, food2$fish, "<=", 14.8)
  add.constraint(lprec, food2$flesh, "<=", 23.2)
  add.constraint(lprec, food2$legume, "<=", 147.2)
  add.constraint(lprec, food2$milk, "<=", 13.2)
  add.constraint(lprec, food2$oil, "<=", 13.1)
  add.constraint(lprec, food2$organ, "<=", 7.5)
  add.constraint(lprec, food2$other_fruit, "<=", 24.8)
  add.constraint(lprec, food2$other_veg, "<=", 22.4)
  add.constraint(lprec, food2$vita_fruit, "<=", 7.8)
  add.constraint(lprec, food2$vita_veg, "<=", 39.4)
  add.constraint(lprec, food2$root, "<=", 72.8)
  add.constraint(lprec, food2$cereal, ">=", 1018.2/2)
  add.constraint(lprec, food2$dark_green_veg, ">=", 17.1/2)
  add.constraint(lprec, food2$egg, ">=", 9.9/2)
  add.constraint(lprec, food2$fish, ">=", 14.8/2)
  add.constraint(lprec, food2$flesh, ">=", 23.2/2)
  add.constraint(lprec, food2$legume, ">=", 147.2/2)
  add.constraint(lprec, food2$milk, ">=", 13.2/2)
  add.constraint(lprec, food2$oil, ">=", 13.1/2)
  add.constraint(lprec, food2$other_fruit, ">=", 24.8/2)
  add.constraint(lprec, food2$other_veg, ">=", 22.4/2)
  add.constraint(lprec, food2$root, ">=", 72.8/2)
  RowNames = c("energy", "vitamin_a", "protein", "vitamin_c", "vitamin_e",
                "vitamin_b12", "riboflavin", "choline", "calcium", "iron", "zinc",
                "cereal", "dark_green_veg", "egg", "fish", "flesh", "legume",  "milk",
                "oil", "organ", "other_fruit", "other_veg", "vita_fruit", "vita_veg",
                "root", "cereal", "dark_green_veg", "egg", "fish", "flesh", "legume", 
                "milk", "oil", "other_fruit", "other_veg", "root")
  ColNames = food2$food_item
  dimnames(lprec) = list(RowNames, ColNames)
  solve(lprec)
  print(get.variables(lprec))
  print(get.constraints(lprec))
  print(get.objective(lprec))
  meal = cbind(meal, get.variables(lprec))
  comp = cbind(comp, get.constraints(lprec))
  cost = cbind(cost, get.objective(lprec))
}

rownames(meal) = food2$food_item
rownames(comp) = RowNames
rownames(cost) = c('Cost')

opti_cur2 = rbind(meal, comp, cost)
opti_cur2 = as.data.frame(opti_cur2)
names(opti_cur2) = rec$category

opti_cur2$X = row.names(opti_cur2)


# OPTIMIZATION PVA28------------------------------------------------------------

food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "green_mealies", 9.0528, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "mahewu", 2.5461, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "maize_boiled", 7.15737, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "maize_boiled_orange", 6.7896, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "maize_dried", 25.20639, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "maize_dried_roasted", 25.20639, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "mealie_meal", 24.6123, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "popcorn", 27.41301, food1$Vitamin.A..RAE..ug.)

food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "green_mealies", 9.0528, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "mahewu", 2.5461, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "maize_boiled", 7.15737, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "maize_boiled_orange", 6.7896, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "maize_dried", 25.20639, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "maize_dried_roasted", 25.20639, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "mealie_meal", 24.6123, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "popcorn", 27.41301, food2$Vitamin.A..RAE..ug.)


# Wet season

N = nrow(rec)

meal = c()
comp = c()
cost = c()

for(i in 1:N) {
  lprec = make.lp(0, nrow(food1))
  set.objfn(lprec, food1$cost_per_g)
  add.constraint(lprec, food1$energy/100, ">=", 14971.4)
  add.constraint(lprec, food1$Vitamin.A..RAE..ug./100, ">=", 570)
  add.constraint(lprec, food1$Protein..g./100, ">=", 19.3)
  add.constraint(lprec, food1$Vitamin.C..total.ascorbic.acid..mg./100, ">=", 90.0)
  add.constraint(lprec, food1$Vitamin.E..alpha.tocopherol...mg./100, ">=", 12.0)
  add.constraint(lprec, food1$Riboflavin..mg./100, ">=", 1.0)
  add.constraint(lprec, food1$Vitamin.B.12..ug./100, ">=", 1.0)
  add.constraint(lprec, food1$Choline..total..mg./100, ">=", 25.6)
  add.constraint(lprec, food1$Calcium..Ca..mg./100, ">=", 453.1)
  add.constraint(lprec, food1$Iron..Fe..mg./100, ">=", 19.2)
  add.constraint(lprec, food1$Zinc..Zn..mg./100, ">=", 11.0)
  add.constraint(lprec, food1$cereal, "<=", 792)
  add.constraint(lprec, food1$dark_green_veg, "<=", 16.6)
  add.constraint(lprec, food1$egg, "<=", 5.3)
  add.constraint(lprec, food1$fish, "<=", 13.6)
  add.constraint(lprec, food1$flesh, "<=", 19.8)
  add.constraint(lprec, food1$legume, "<=", 320.8)
  add.constraint(lprec, food1$milk, "<=", 11.6)
  add.constraint(lprec, food1$oil, "<=", 24)
  add.constraint(lprec, food1$other_fruit, "<=", 17.8)
  add.constraint(lprec, food1$other_veg, "<=", 31.6)
  add.constraint(lprec, food1$vita_veg, "<=", 18.7)
  add.constraint(lprec, food1$root, "<=", 124.6)
  add.constraint(lprec, food1$cereal, ">=", 792/2)
  add.constraint(lprec, food1$dark_green_veg, ">=", 16.6/2)
  add.constraint(lprec, food1$egg, ">=", 5.3/2)
  add.constraint(lprec, food1$fish, ">=", 13.6/2)
  add.constraint(lprec, food1$flesh, ">=", 19.8/2)
  add.constraint(lprec, food1$legume, ">=", 320.8/2)
  add.constraint(lprec, food1$milk, ">=", 11.6/2)
  add.constraint(lprec, food1$oil, ">=", 24/2)
  add.constraint(lprec, food1$other_fruit, ">=", 17.8/2)
  add.constraint(lprec, food1$other_veg, ">=", 31.6/2)
  add.constraint(lprec, food1$root, ">=", 124.6/2)
  RowNames = c("energy", "vitamin_a", "protein", "vitamin_c","vitamin_e",
               "vitamin_b12", "riboflavin", "choline", "calcium", "iron", "zinc",
               "cereal", "dark_green_veg", "egg", "fish", "flesh", "legume",  "milk",
               "oil", "other_fruit", "other_veg", "vita_veg", "root",
               "cereal", "dark_green_veg", "egg",  "fish", "flesh" , "legume", "milk",
               "oil", "other_fruit", "other_veg", "root")
  ColNames = food1$food_item
  dimnames(lprec) = list(RowNames, ColNames)
  solve(lprec)
  print(get.variables(lprec))
  print(get.constraints(lprec))
  print(get.objective(lprec))
  meal = cbind(meal, get.variables(lprec))
  comp = cbind(comp, get.constraints(lprec))
  cost = cbind(cost, get.objective(lprec))
}

rownames(meal) = food1$food_item
rownames(comp) = RowNames
rownames(cost) = c('Cost')

opti_pva281 = rbind(meal, comp, cost)
opti_pva281 = as.data.frame(opti_pva281)
names(opti_pva281) = rec$category

opti_pva281$X = row.names(opti_pva281)


# Dry season

N = nrow(rec)

meal = c()
comp = c()
cost = c()

for(i in 1:N) {
  lprec = make.lp(0, nrow(food2))
  set.objfn(lprec, food2$cost_per_g)
  add.constraint(lprec, food2$energy/100, ">=", 13369.7)
  add.constraint(lprec, food2$Vitamin.A..RAE..ug./100, ">=", 570)
  add.constraint(lprec, food2$Protein..g./100, ">=", 19.6)
  add.constraint(lprec, food2$Vitamin.C..total.ascorbic.acid..mg./100, ">=", 79.1)
  add.constraint(lprec, food2$Vitamin.E..alpha.tocopherol...mg./100, ">=", 12.0)
  add.constraint(lprec, food2$Riboflavin..mg./100, ">=", 0.9)
  add.constraint(lprec, food2$Vitamin.B.12..ug./100, ">=", 0.7)
  add.constraint(lprec, food2$Choline..total..mg./100, ">=", 47.7)
  add.constraint(lprec, food2$Calcium..Ca..mg./100, ">=", 340.7)
  add.constraint(lprec, food2$Iron..Fe..mg./100, ">=", 19.2)
  add.constraint(lprec, food2$Zinc..Zn..mg./100, ">=", 11.0)
  add.constraint(lprec, food2$cereal, "<=", 1018.2)
  add.constraint(lprec, food2$dark_green_veg, "<=", 17.1)
  add.constraint(lprec, food2$egg, "<=", 9.9)
  add.constraint(lprec, food2$fish, "<=", 14.8)
  add.constraint(lprec, food2$flesh, "<=", 23.2)
  add.constraint(lprec, food2$legume, "<=", 147.2)
  add.constraint(lprec, food2$milk, "<=", 13.2)
  add.constraint(lprec, food2$oil, "<=", 13.1)
  add.constraint(lprec, food2$organ, "<=", 7.5)
  add.constraint(lprec, food2$other_fruit, "<=", 24.8)
  add.constraint(lprec, food2$other_veg, "<=", 22.4)
  add.constraint(lprec, food2$vita_fruit, "<=", 7.8)
  add.constraint(lprec, food2$vita_veg, "<=", 39.4)
  add.constraint(lprec, food2$root, "<=", 72.8)
  add.constraint(lprec, food2$cereal, ">=", 1018.2/2)
  add.constraint(lprec, food2$dark_green_veg, ">=", 17.1/2)
  add.constraint(lprec, food2$egg, ">=", 9.9/2)
  add.constraint(lprec, food2$fish, ">=", 14.8/2)
  add.constraint(lprec, food2$flesh, ">=", 23.2/2)
  add.constraint(lprec, food2$legume, ">=", 147.2/2)
  add.constraint(lprec, food2$milk, ">=", 13.2/2)
  add.constraint(lprec, food2$oil, ">=", 13.1/2)
  add.constraint(lprec, food2$other_fruit, ">=", 24.8/2)
  add.constraint(lprec, food2$other_veg, ">=", 22.4/2)
  add.constraint(lprec, food2$root, ">=", 72.8/2)
  RowNames = c("energy", "vitamin_a", "protein", "vitamin_c", "vitamin_e",
               "vitamin_b12", "riboflavin", "choline", "calcium", "iron", "zinc",
               "cereal", "dark_green_veg", "egg", "fish", "flesh", "legume",  "milk",
               "oil", "organ", "other_fruit", "other_veg", "vita_fruit", "vita_veg",
               "root", "cereal", "dark_green_veg", "egg", "fish", "flesh", "legume", 
               "milk", "oil", "other_fruit", "other_veg", "root")
  ColNames = food2$food_item
  dimnames(lprec) = list(RowNames, ColNames)
  solve(lprec)
  print(get.variables(lprec))
  print(get.constraints(lprec))
  print(get.objective(lprec))
  meal = cbind(meal, get.variables(lprec))
  comp = cbind(comp, get.constraints(lprec))
  cost = cbind(cost, get.objective(lprec))
}

rownames(meal) = food2$food_item
rownames(comp) = RowNames
rownames(cost) = c('Cost')

opti_pva282 = rbind(meal, comp, cost)
opti_pva282 = as.data.frame(opti_pva282)
names(opti_pva282) = rec$category

opti_pva282$X = row.names(opti_pva282)


# OPTIMIZATION PVA40------------------------------------------------------------

food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "green_mealies", 12.928, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "mahewu", 3.636, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "maize_boiled", 10.2212, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "maize_boiled_orange", 9.696, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "maize_dried", 35.9964, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "maize_dried_roasted", 35.9964, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "mealie_meal", 24.6123, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "popcorn", 39.1476, food1$Vitamin.A..RAE..ug.)

food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "green_mealies", 12.928, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "mahewu", 3.636, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "maize_boiled", 10.2212, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "maize_boiled_orange", 9.696, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "maize_dried", 35.9964, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "maize_dried_roasted", 35.9964, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "mealie_meal", 24.6123, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "popcorn", 39.1476, food2$Vitamin.A..RAE..ug.)


# Wet season

N = nrow(rec)

meal = c()
comp = c()
cost = c()

for(i in 1:N) {
  lprec = make.lp(0, nrow(food1))
  set.objfn(lprec, food1$cost_per_g)
  add.constraint(lprec, food1$energy/100, ">=", 14971.4)
  add.constraint(lprec, food1$Vitamin.A..RAE..ug./100, ">=", 570)
  add.constraint(lprec, food1$Protein..g./100, ">=", 19.3)
  add.constraint(lprec, food1$Vitamin.C..total.ascorbic.acid..mg./100, ">=", 90.0)
  add.constraint(lprec, food1$Vitamin.E..alpha.tocopherol...mg./100, ">=", 12.0)
  add.constraint(lprec, food1$Riboflavin..mg./100, ">=", 1.0)
  add.constraint(lprec, food1$Vitamin.B.12..ug./100, ">=", 1.0)
  add.constraint(lprec, food1$Choline..total..mg./100, ">=", 25.6)
  add.constraint(lprec, food1$Calcium..Ca..mg./100, ">=", 453.1)
  add.constraint(lprec, food1$Iron..Fe..mg./100, ">=", 19.2)
  add.constraint(lprec, food1$Zinc..Zn..mg./100, ">=", 11.0)
  add.constraint(lprec, food1$cereal, "<=", 792)
  add.constraint(lprec, food1$dark_green_veg, "<=", 16.6)
  add.constraint(lprec, food1$egg, "<=", 5.3)
  add.constraint(lprec, food1$fish, "<=", 13.6)
  add.constraint(lprec, food1$flesh, "<=", 19.8)
  add.constraint(lprec, food1$legume, "<=", 320.8)
  add.constraint(lprec, food1$milk, "<=", 11.6)
  add.constraint(lprec, food1$oil, "<=", 24)
  add.constraint(lprec, food1$other_fruit, "<=", 17.8)
  add.constraint(lprec, food1$other_veg, "<=", 31.6)
  add.constraint(lprec, food1$vita_veg, "<=", 18.7)
  add.constraint(lprec, food1$root, "<=", 124.6)
  add.constraint(lprec, food1$cereal, ">=", 792/2)
  add.constraint(lprec, food1$dark_green_veg, ">=", 16.6/2)
  add.constraint(lprec, food1$egg, ">=", 5.3/2)
  add.constraint(lprec, food1$fish, ">=", 13.6/2)
  add.constraint(lprec, food1$flesh, ">=", 19.8/2)
  add.constraint(lprec, food1$legume, ">=", 320.8/2)
  add.constraint(lprec, food1$milk, ">=", 11.6/2)
  add.constraint(lprec, food1$oil, ">=", 24/2)
  add.constraint(lprec, food1$other_fruit, ">=", 17.8/2)
  add.constraint(lprec, food1$other_veg, ">=", 31.6/2)
  add.constraint(lprec, food1$root, ">=", 124.6/2)
  RowNames = c("energy", "vitamin_a", "protein", "vitamin_c","vitamin_e",
               "vitamin_b12", "riboflavin", "choline", "calcium", "iron", "zinc",
               "cereal", "dark_green_veg", "egg", "fish", "flesh", "legume",  "milk",
               "oil", "other_fruit", "other_veg", "vita_veg", "root",
               "cereal", "dark_green_veg", "egg",  "fish", "flesh" , "legume", "milk",
               "oil", "other_fruit", "other_veg", "root")
  ColNames = food1$food_item
  dimnames(lprec) = list(RowNames, ColNames)
  solve(lprec)
  print(get.variables(lprec))
  print(get.constraints(lprec))
  print(get.objective(lprec))
  meal = cbind(meal, get.variables(lprec))
  comp = cbind(comp, get.constraints(lprec))
  cost = cbind(cost, get.objective(lprec))
}

rownames(meal) = food1$food_item
rownames(comp) = RowNames
rownames(cost) = c('Cost')

opti_pva401 = rbind(meal, comp, cost)
opti_pva401 = as.data.frame(opti_pva401)
names(opti_pva401) = rec$category

opti_pva401$X = row.names(opti_pva401)


# Dry season

N = nrow(rec)

meal = c()
comp = c()
cost = c()

for(i in 1:N) {
  lprec = make.lp(0, nrow(food2))
  set.objfn(lprec, food2$cost_per_g)
  add.constraint(lprec, food2$energy/100, ">=", 13369.7)
  add.constraint(lprec, food2$Vitamin.A..RAE..ug./100, ">=", 570)
  add.constraint(lprec, food2$Protein..g./100, ">=", 19.6)
  add.constraint(lprec, food2$Vitamin.C..total.ascorbic.acid..mg./100, ">=", 79.1)
  add.constraint(lprec, food2$Vitamin.E..alpha.tocopherol...mg./100, ">=", 12.0)
  add.constraint(lprec, food2$Riboflavin..mg./100, ">=", 0.9)
  add.constraint(lprec, food2$Vitamin.B.12..ug./100, ">=", 0.7)
  add.constraint(lprec, food2$Choline..total..mg./100, ">=", 47.7)
  add.constraint(lprec, food2$Calcium..Ca..mg./100, ">=", 340.7)
  add.constraint(lprec, food2$Iron..Fe..mg./100, ">=", 19.2)
  add.constraint(lprec, food2$Zinc..Zn..mg./100, ">=", 11.0)
  add.constraint(lprec, food2$cereal, "<=", 1018.2)
  add.constraint(lprec, food2$dark_green_veg, "<=", 17.1)
  add.constraint(lprec, food2$egg, "<=", 9.9)
  add.constraint(lprec, food2$fish, "<=", 14.8)
  add.constraint(lprec, food2$flesh, "<=", 23.2)
  add.constraint(lprec, food2$legume, "<=", 147.2)
  add.constraint(lprec, food2$milk, "<=", 13.2)
  add.constraint(lprec, food2$oil, "<=", 13.1)
  add.constraint(lprec, food2$organ, "<=", 7.5)
  add.constraint(lprec, food2$other_fruit, "<=", 24.8)
  add.constraint(lprec, food2$other_veg, "<=", 22.4)
  add.constraint(lprec, food2$vita_fruit, "<=", 7.8)
  add.constraint(lprec, food2$vita_veg, "<=", 39.4)
  add.constraint(lprec, food2$root, "<=", 72.8)
  add.constraint(lprec, food2$cereal, ">=", 1018.2/2)
  add.constraint(lprec, food2$dark_green_veg, ">=", 17.1/2)
  add.constraint(lprec, food2$egg, ">=", 9.9/2)
  add.constraint(lprec, food2$fish, ">=", 14.8/2)
  add.constraint(lprec, food2$flesh, ">=", 23.2/2)
  add.constraint(lprec, food2$legume, ">=", 147.2/2)
  add.constraint(lprec, food2$milk, ">=", 13.2/2)
  add.constraint(lprec, food2$oil, ">=", 13.1/2)
  add.constraint(lprec, food2$other_fruit, ">=", 24.8/2)
  add.constraint(lprec, food2$other_veg, ">=", 22.4/2)
  add.constraint(lprec, food2$root, ">=", 72.8/2)
  RowNames = c("energy", "vitamin_a", "protein", "vitamin_c", "vitamin_e",
               "vitamin_b12", "riboflavin", "choline", "calcium", "iron", "zinc",
               "cereal", "dark_green_veg", "egg", "fish", "flesh", "legume",  "milk",
               "oil", "organ", "other_fruit", "other_veg", "vita_fruit", "vita_veg",
               "root", "cereal", "dark_green_veg", "egg", "fish", "flesh", "legume", 
               "milk", "oil", "other_fruit", "other_veg", "root")
  ColNames = food2$food_item
  dimnames(lprec) = list(RowNames, ColNames)
  solve(lprec)
  print(get.variables(lprec))
  print(get.constraints(lprec))
  print(get.objective(lprec))
  meal = cbind(meal, get.variables(lprec))
  comp = cbind(comp, get.constraints(lprec))
  cost = cbind(cost, get.objective(lprec))
}

rownames(meal) = food2$food_item
rownames(comp) = RowNames
rownames(cost) = c('Cost')

opti_pva402 = rbind(meal, comp, cost)
opti_pva402 = as.data.frame(opti_pva402)
names(opti_pva402) = rec$category

opti_pva402$X = row.names(opti_pva402)


# OPTIMIZATION PVA95------------------------------------------------------------

food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "green_mealies", 30.4, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "mahewu", 8.55, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "maize_boiled", 24.035, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "maize_boiled_orange", 22.8, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "maize_dried", 84.645, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "maize_dried_roasted", 84.645, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "mealie_meal", 82.65, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "popcorn", 92.055, food1$Vitamin.A..RAE..ug.)

food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "green_mealies", 30.4, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "mahewu", 8.55, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "maize_boiled", 24.035, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "maize_boiled_orange", 22.8, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "maize_dried", 84.645, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "maize_dried_roasted", 84.645, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "mealie_meal", 82.65, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "popcorn", 92.055, food2$Vitamin.A..RAE..ug.)


# Wet season

N = nrow(rec)

meal = c()
comp = c()
cost = c()

for(i in 1:N) {
  lprec = make.lp(0, nrow(food1))
  set.objfn(lprec, food1$cost_per_g)
  add.constraint(lprec, food1$energy/100, ">=", 14971.4)
  add.constraint(lprec, food1$Vitamin.A..RAE..ug./100, ">=", 570)
  add.constraint(lprec, food1$Protein..g./100, ">=", 19.3)
  add.constraint(lprec, food1$Vitamin.C..total.ascorbic.acid..mg./100, ">=", 90.0)
  add.constraint(lprec, food1$Vitamin.E..alpha.tocopherol...mg./100, ">=", 12.0)
  add.constraint(lprec, food1$Riboflavin..mg./100, ">=", 1.0)
  add.constraint(lprec, food1$Vitamin.B.12..ug./100, ">=", 1.0)
  add.constraint(lprec, food1$Choline..total..mg./100, ">=", 25.6)
  add.constraint(lprec, food1$Calcium..Ca..mg./100, ">=", 453.1)
  add.constraint(lprec, food1$Iron..Fe..mg./100, ">=", 19.2)
  add.constraint(lprec, food1$Zinc..Zn..mg./100, ">=", 11.0)
  add.constraint(lprec, food1$cereal, "<=", 792)
  add.constraint(lprec, food1$dark_green_veg, "<=", 16.6)
  add.constraint(lprec, food1$egg, "<=", 5.3)
  add.constraint(lprec, food1$fish, "<=", 13.6)
  add.constraint(lprec, food1$flesh, "<=", 19.8)
  add.constraint(lprec, food1$legume, "<=", 320.8)
  add.constraint(lprec, food1$milk, "<=", 11.6)
  add.constraint(lprec, food1$oil, "<=", 24)
  add.constraint(lprec, food1$other_fruit, "<=", 17.8)
  add.constraint(lprec, food1$other_veg, "<=", 31.6)
  add.constraint(lprec, food1$vita_veg, "<=", 18.7)
  add.constraint(lprec, food1$root, "<=", 124.6)
  add.constraint(lprec, food1$cereal, ">=", 792/2)
  add.constraint(lprec, food1$dark_green_veg, ">=", 16.6/2)
  add.constraint(lprec, food1$egg, ">=", 5.3/2)
  add.constraint(lprec, food1$fish, ">=", 13.6/2)
  add.constraint(lprec, food1$flesh, ">=", 19.8/2)
  add.constraint(lprec, food1$legume, ">=", 320.8/2)
  add.constraint(lprec, food1$milk, ">=", 11.6/2)
  add.constraint(lprec, food1$oil, ">=", 24/2)
  add.constraint(lprec, food1$other_fruit, ">=", 17.8/2)
  add.constraint(lprec, food1$other_veg, ">=", 31.6/2)
  add.constraint(lprec, food1$root, ">=", 124.6/2)
  RowNames = c("energy", "vitamin_a", "protein", "vitamin_c","vitamin_e",
               "vitamin_b12", "riboflavin", "choline", "calcium", "iron", "zinc",
               "cereal", "dark_green_veg", "egg", "fish", "flesh", "legume",  "milk",
               "oil", "other_fruit", "other_veg", "vita_veg", "root",
               "cereal", "dark_green_veg", "egg",  "fish", "flesh" , "legume", "milk",
               "oil", "other_fruit", "other_veg", "root")
  ColNames = food1$food_item
  dimnames(lprec) = list(RowNames, ColNames)
  solve(lprec)
  print(get.variables(lprec))
  print(get.constraints(lprec))
  print(get.objective(lprec))
  meal = cbind(meal, get.variables(lprec))
  comp = cbind(comp, get.constraints(lprec))
  cost = cbind(cost, get.objective(lprec))
}

rownames(meal) = food1$food_item
rownames(comp) = RowNames
rownames(cost) = c('Cost')

opti_pva951 = rbind(meal, comp, cost)
opti_pva951 = as.data.frame(opti_pva951)
names(opti_pva951) = rec$category

opti_pva951$X = row.names(opti_pva951)


# Dry season

N = nrow(rec)

meal = c()
comp = c()
cost = c()

for(i in 1:N) {
  lprec = make.lp(0, nrow(food2))
  set.objfn(lprec, food2$cost_per_g)
  add.constraint(lprec, food2$energy/100, ">=", 13369.7)
  add.constraint(lprec, food2$Vitamin.A..RAE..ug./100, ">=", 570)
  add.constraint(lprec, food2$Protein..g./100, ">=", 19.6)
  add.constraint(lprec, food2$Vitamin.C..total.ascorbic.acid..mg./100, ">=", 79.1)
  add.constraint(lprec, food2$Vitamin.E..alpha.tocopherol...mg./100, ">=", 12.0)
  add.constraint(lprec, food2$Riboflavin..mg./100, ">=", 0.9)
  add.constraint(lprec, food2$Vitamin.B.12..ug./100, ">=", 0.7)
  add.constraint(lprec, food2$Choline..total..mg./100, ">=", 47.7)
  add.constraint(lprec, food2$Calcium..Ca..mg./100, ">=", 340.7)
  add.constraint(lprec, food2$Iron..Fe..mg./100, ">=", 19.2)
  add.constraint(lprec, food2$Zinc..Zn..mg./100, ">=", 11.0)
  add.constraint(lprec, food2$cereal, "<=", 1018.2)
  add.constraint(lprec, food2$dark_green_veg, "<=", 17.1)
  add.constraint(lprec, food2$egg, "<=", 9.9)
  add.constraint(lprec, food2$fish, "<=", 14.8)
  add.constraint(lprec, food2$flesh, "<=", 23.2)
  add.constraint(lprec, food2$legume, "<=", 147.2)
  add.constraint(lprec, food2$milk, "<=", 13.2)
  add.constraint(lprec, food2$oil, "<=", 13.1)
  add.constraint(lprec, food2$organ, "<=", 7.5)
  add.constraint(lprec, food2$other_fruit, "<=", 24.8)
  add.constraint(lprec, food2$other_veg, "<=", 22.4)
  add.constraint(lprec, food2$vita_fruit, "<=", 7.8)
  add.constraint(lprec, food2$vita_veg, "<=", 39.4)
  add.constraint(lprec, food2$root, "<=", 72.8)
  add.constraint(lprec, food2$cereal, ">=", 1018.2/2)
  add.constraint(lprec, food2$dark_green_veg, ">=", 17.1/2)
  add.constraint(lprec, food2$egg, ">=", 9.9/2)
  add.constraint(lprec, food2$fish, ">=", 14.8/2)
  add.constraint(lprec, food2$flesh, ">=", 23.2/2)
  add.constraint(lprec, food2$legume, ">=", 147.2/2)
  add.constraint(lprec, food2$milk, ">=", 13.2/2)
  add.constraint(lprec, food2$oil, ">=", 13.1/2)
  add.constraint(lprec, food2$other_fruit, ">=", 24.8/2)
  add.constraint(lprec, food2$other_veg, ">=", 22.4/2)
  add.constraint(lprec, food2$root, ">=", 72.8/2)
  RowNames = c("energy", "vitamin_a", "protein", "vitamin_c", "vitamin_e",
               "vitamin_b12", "riboflavin", "choline", "calcium", "iron", "zinc",
               "cereal", "dark_green_veg", "egg", "fish", "flesh", "legume",  "milk",
               "oil", "organ", "other_fruit", "other_veg", "vita_fruit", "vita_veg",
               "root", "cereal", "dark_green_veg", "egg", "fish", "flesh", "legume", 
               "milk", "oil", "other_fruit", "other_veg", "root")
  ColNames = food2$food_item
  dimnames(lprec) = list(RowNames, ColNames)
  solve(lprec)
  print(get.variables(lprec))
  print(get.constraints(lprec))
  print(get.objective(lprec))
  meal = cbind(meal, get.variables(lprec))
  comp = cbind(comp, get.constraints(lprec))
  cost = cbind(cost, get.objective(lprec))
}

rownames(meal) = food2$food_item
rownames(comp) = RowNames
rownames(cost) = c('Cost')

opti_pva952 = rbind(meal, comp, cost)
opti_pva952 = as.data.frame(opti_pva952)
names(opti_pva952) = rec$category

opti_pva952$X = row.names(opti_pva952)


# OPTIMIZATION PVA125------------------------------------------------------------

food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "green_mealies", 40, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "mahewu", 11.25, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "maize_boiled", 31.625, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "maize_boiled_orange", 30, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "maize_dried", 111.375, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "maize_dried_roasted", 111.375, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "mealie_meal", 108.75, food1$Vitamin.A..RAE..ug.)
food1$Vitamin.A..RAE..ug. = ifelse(food1$food_item == "popcorn", 121.125, food1$Vitamin.A..RAE..ug.)

food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "green_mealies", 40, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "mahewu", 11.25, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "maize_boiled", 31.625, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "maize_boiled_orange", 30, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "maize_dried", 111.375, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "maize_dried_roasted", 111.375, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "mealie_meal", 108.75, food2$Vitamin.A..RAE..ug.)
food2$Vitamin.A..RAE..ug. = ifelse(food2$food_item == "popcorn", 121.125, food2$Vitamin.A..RAE..ug.)


# Wet season

N = nrow(rec)

meal = c()
comp = c()
cost = c()

for(i in 1:N) {
  lprec = make.lp(0, nrow(food1))
  set.objfn(lprec, food1$cost_per_g)
  add.constraint(lprec, food1$energy/100, ">=", 14971.4)
  add.constraint(lprec, food1$Vitamin.A..RAE..ug./100, ">=", 570)
  add.constraint(lprec, food1$Protein..g./100, ">=", 19.3)
  add.constraint(lprec, food1$Vitamin.C..total.ascorbic.acid..mg./100, ">=", 90.0)
  add.constraint(lprec, food1$Vitamin.E..alpha.tocopherol...mg./100, ">=", 12.0)
  add.constraint(lprec, food1$Riboflavin..mg./100, ">=", 1.0)
  add.constraint(lprec, food1$Vitamin.B.12..ug./100, ">=", 1.0)
  add.constraint(lprec, food1$Choline..total..mg./100, ">=", 25.6)
  add.constraint(lprec, food1$Calcium..Ca..mg./100, ">=", 453.1)
  add.constraint(lprec, food1$Iron..Fe..mg./100, ">=", 19.2)
  add.constraint(lprec, food1$Zinc..Zn..mg./100, ">=", 11.0)
  add.constraint(lprec, food1$cereal, "<=", 792)
  add.constraint(lprec, food1$dark_green_veg, "<=", 16.6)
  add.constraint(lprec, food1$egg, "<=", 5.3)
  add.constraint(lprec, food1$fish, "<=", 13.6)
  add.constraint(lprec, food1$flesh, "<=", 19.8)
  add.constraint(lprec, food1$legume, "<=", 320.8)
  add.constraint(lprec, food1$milk, "<=", 11.6)
  add.constraint(lprec, food1$oil, "<=", 24)
  add.constraint(lprec, food1$other_fruit, "<=", 17.8)
  add.constraint(lprec, food1$other_veg, "<=", 31.6)
  add.constraint(lprec, food1$vita_veg, "<=", 18.7)
  add.constraint(lprec, food1$root, "<=", 124.6)
  add.constraint(lprec, food1$cereal, ">=", 792/2)
  add.constraint(lprec, food1$dark_green_veg, ">=", 16.6/2)
  add.constraint(lprec, food1$egg, ">=", 5.3/2)
  add.constraint(lprec, food1$fish, ">=", 13.6/2)
  add.constraint(lprec, food1$flesh, ">=", 19.8/2)
  add.constraint(lprec, food1$legume, ">=", 320.8/2)
  add.constraint(lprec, food1$milk, ">=", 11.6/2)
  add.constraint(lprec, food1$oil, ">=", 24/2)
  add.constraint(lprec, food1$other_fruit, ">=", 17.8/2)
  add.constraint(lprec, food1$other_veg, ">=", 31.6/2)
  add.constraint(lprec, food1$root, ">=", 124.6/2)
  RowNames = c("energy", "vitamin_a", "protein", "vitamin_c","vitamin_e",
               "vitamin_b12", "riboflavin", "choline", "calcium", "iron", "zinc",
               "cereal", "dark_green_veg", "egg", "fish", "flesh", "legume",  "milk",
               "oil", "other_fruit", "other_veg", "vita_veg", "root",
               "cereal", "dark_green_veg", "egg",  "fish", "flesh" , "legume", "milk",
               "oil", "other_fruit", "other_veg", "root")
  ColNames = food1$food_item
  dimnames(lprec) = list(RowNames, ColNames)
  solve(lprec)
  print(get.variables(lprec))
  print(get.constraints(lprec))
  print(get.objective(lprec))
  meal = cbind(meal, get.variables(lprec))
  comp = cbind(comp, get.constraints(lprec))
  cost = cbind(cost, get.objective(lprec))
}

rownames(meal) = food1$food_item
rownames(comp) = RowNames
rownames(cost) = c('Cost')

opti_pva1251 = rbind(meal, comp, cost)
opti_pva1251 = as.data.frame(opti_pva1251)
names(opti_pva1251) = rec$category

opti_pva1251$X = row.names(opti_pva1251)


# Dry season

N = nrow(rec)

meal = c()
comp = c()
cost = c()

for(i in 1:N) {
  lprec = make.lp(0, nrow(food2))
  set.objfn(lprec, food2$cost_per_g)
  add.constraint(lprec, food2$energy/100, ">=", 13369.7)
  add.constraint(lprec, food2$Vitamin.A..RAE..ug./100, ">=", 570)
  add.constraint(lprec, food2$Protein..g./100, ">=", 19.6)
  add.constraint(lprec, food2$Vitamin.C..total.ascorbic.acid..mg./100, ">=", 79.1)
  add.constraint(lprec, food2$Vitamin.E..alpha.tocopherol...mg./100, ">=", 12.0)
  add.constraint(lprec, food2$Riboflavin..mg./100, ">=", 0.9)
  add.constraint(lprec, food2$Vitamin.B.12..ug./100, ">=", 0.7)
  add.constraint(lprec, food2$Choline..total..mg./100, ">=", 47.7)
  add.constraint(lprec, food2$Calcium..Ca..mg./100, ">=", 340.7)
  add.constraint(lprec, food2$Iron..Fe..mg./100, ">=", 19.2)
  add.constraint(lprec, food2$Zinc..Zn..mg./100, ">=", 11.0)
  add.constraint(lprec, food2$cereal, "<=", 1018.2)
  add.constraint(lprec, food2$dark_green_veg, "<=", 17.1)
  add.constraint(lprec, food2$egg, "<=", 9.9)
  add.constraint(lprec, food2$fish, "<=", 14.8)
  add.constraint(lprec, food2$flesh, "<=", 23.2)
  add.constraint(lprec, food2$legume, "<=", 147.2)
  add.constraint(lprec, food2$milk, "<=", 13.2)
  add.constraint(lprec, food2$oil, "<=", 13.1)
  add.constraint(lprec, food2$organ, "<=", 7.5)
  add.constraint(lprec, food2$other_fruit, "<=", 24.8)
  add.constraint(lprec, food2$other_veg, "<=", 22.4)
  add.constraint(lprec, food2$vita_fruit, "<=", 7.8)
  add.constraint(lprec, food2$vita_veg, "<=", 39.4)
  add.constraint(lprec, food2$root, "<=", 72.8)
  add.constraint(lprec, food2$cereal, ">=", 1018.2/2)
  add.constraint(lprec, food2$dark_green_veg, ">=", 17.1/2)
  add.constraint(lprec, food2$egg, ">=", 9.9/2)
  add.constraint(lprec, food2$fish, ">=", 14.8/2)
  add.constraint(lprec, food2$flesh, ">=", 23.2/2)
  add.constraint(lprec, food2$legume, ">=", 147.2/2)
  add.constraint(lprec, food2$milk, ">=", 13.2/2)
  add.constraint(lprec, food2$oil, ">=", 13.1/2)
  add.constraint(lprec, food2$other_fruit, ">=", 24.8/2)
  add.constraint(lprec, food2$other_veg, ">=", 22.4/2)
  add.constraint(lprec, food2$root, ">=", 72.8/2)
  RowNames = c("energy", "vitamin_a", "protein", "vitamin_c", "vitamin_e",
               "vitamin_b12", "riboflavin", "choline", "calcium", "iron", "zinc",
               "cereal", "dark_green_veg", "egg", "fish", "flesh", "legume",  "milk",
               "oil", "organ", "other_fruit", "other_veg", "vita_fruit", "vita_veg",
               "root", "cereal", "dark_green_veg", "egg", "fish", "flesh", "legume", 
               "milk", "oil", "other_fruit", "other_veg", "root")
  ColNames = food2$food_item
  dimnames(lprec) = list(RowNames, ColNames)
  solve(lprec)
  print(get.variables(lprec))
  print(get.constraints(lprec))
  print(get.objective(lprec))
  meal = cbind(meal, get.variables(lprec))
  comp = cbind(comp, get.constraints(lprec))
  cost = cbind(cost, get.objective(lprec))
}

rownames(meal) = food2$food_item
rownames(comp) = RowNames
rownames(cost) = c('Cost')

opti_pva1252 = rbind(meal, comp, cost)
opti_pva1252 = as.data.frame(opti_pva1252)
names(opti_pva1252) = rec$category

opti_pva1252$X = row.names(opti_pva1252)


# MERGING RESULTS FROM ALL SCENARIOS--------------------------------------------

opti_cur1 = opti_cur1[, c(24, 9)]
opti_cur2 = opti_cur2[, c(24, 9)]
opti_pva281 = opti_pva281[, c(24, 9)]
opti_pva282 = opti_pva282[, c(24, 9)]
opti_pva401 = opti_pva401[, c(24, 9)]
opti_pva402 = opti_pva402[, c(24, 9)]
opti_pva951 = opti_pva951[, c(24, 9)]
opti_pva952 = opti_pva952[, c(24, 9)]
opti_pva1251 = opti_pva1251[, c(24, 9)]
opti_pva1252 = opti_pva1252[, c(24, 9)]

names(opti_cur1)[2] = "Current - Season 1"
names(opti_cur2)[2] = "Current - Season 2"
names(opti_pva281)[2] = "PVA 28 - Season 1"
names(opti_pva282)[2] = "PVA 28 - Season 2"
names(opti_pva401)[2] = "PVA 40 - Season 1"
names(opti_pva402)[2] = "PVA 40 - Season 2"
names(opti_pva951)[2] = "PVA 95 - Season 1"
names(opti_pva952)[2] = "PVA 95 - Season 2"
names(opti_pva1251)[2] = "PVA 125 - Season 1"
names(opti_pva1252)[2] = "PVA 125 - Season 2"

wet = merge(opti_cur1, opti_pva281, by = "X", all = TRUE)
wet = merge(wet, opti_pva401, by = "X", all = TRUE)
wet = merge(wet, opti_pva951, by = "X", all = TRUE)
wet = merge(wet, opti_pva1251, by = "X", all = TRUE)

dry = merge(opti_cur2, opti_pva282, by = "X", all = TRUE)
dry = merge(dry, opti_pva402, by = "X", all = TRUE)
dry = merge(dry, opti_pva952, by = "X", all = TRUE)
dry = merge(dry, opti_pva1252, by = "X", all = TRUE)

opt = merge(wet, dry, by = "X", all = TRUE)

opt[is.na(opt)] = 0


cst1 = round(opt[17, 2:6], 3)
cst2 = round(opt[17, 7:11], 3)

ing = opt[c(1:8, 10, 13, 15:16, 18:21, 26, 28:29, 32, 35:38, 40:41, 44:50,
            53:54, 57:60, 66:70, 72:74, 76, 79:88, 95), ]

names(ing)[1] = "food_item"

ing1 = ing[, c(1:6)]
ing2 = ing[, c(1, 7:11)]

ing1 = merge(ing1, cost1, by = "food_item", all.x = TRUE)
ing2 = merge(ing2, cost2, by = "food_item", all.x = TRUE)

names(ing1)

ing1$`Current - Season 1` = ing1$`Current - Season 1` * ing1$cost_per_g
ing1$`PVA 28 - Season 1` = ing1$`PVA 28 - Season 1` * ing1$cost_per_g
ing1$`PVA 40 - Season 1` = ing1$`PVA 40 - Season 1` * ing1$cost_per_g
ing1$`PVA 95 - Season 1` = ing1$`PVA 95 - Season 1` * ing1$cost_per_g
ing1$`PVA 125 - Season 1` = ing1$`PVA 125 - Season 1` * ing1$cost_per_g

ing2$`Current - Season 2` = ing2$`Current - Season 2` * ing2$cost_per_g
ing2$`PVA 28 - Season 2` = ing2$`PVA 28 - Season 2` * ing2$cost_per_g
ing2$`PVA 40 - Season 2` = ing2$`PVA 40 - Season 2` * ing2$cost_per_g
ing2$`PVA 95 - Season 2` = ing2$`PVA 95 - Season 2` * ing2$cost_per_g
ing2$`PVA 125 - Season 2` = ing2$`PVA 125 - Season 2` * ing2$cost_per_g


ing1[is.na(ing1)] = 0
ing2[is.na(ing2)] = 0


ing1 = merge(ing1, group, by = "food_item")
ing2 = merge(ing2, group, by = "food_item")

ing1 = ing1[, -c(7, 9)]
ing2 = ing2[, -c(7, 9)]

names(ing1) = c("food_item", "5. Current", "4. Mean on-farm content", "3. Maximum on-farm content", "2. Maximum on-station content", "1. Target content", "group")
names(ing2) = c("food_item", "5. Current", "4. Mean on-farm content", "3. Maximum on-farm content", "2. Maximum on-station content", "1. Target content", "group")

ing1 = gather(ing1, scenario, value, "5. Current":"1. Target content")
ing2 = gather(ing2, scenario, value, "5. Current":"1. Target content")


ing1 = ing1[,c(2:4)] %>%
  group_by(scenario, group) %>%
  summarise_each(funs(sum))

ing2 = ing2[,c(2:4)] %>%
  group_by(scenario, group) %>%
  summarise_each(funs(sum))

ing1[nrow(ing1) + 1,] = list("1. Target content", "Organ meat", 0)
ing1[nrow(ing1) + 1,] = list("1. Target content", "Vitamin A rich fruits", 0)

ing2[nrow(ing2) + 1,] = list("1. Target content", "Organ meat", 0)
ing2[nrow(ing2) + 1,] = list("1. Target content", "Vitamin A rich fruits", 0)


theme_set(theme_few())  

pcst1 = ing1 %>%
  ggplot(aes(x = scenario, y = value, fill = as.factor(group))) +
  geom_bar(stat = 'identity', width=.5, position = position_stack(reverse = TRUE), color = "black") +
  ggthemes::scale_fill_tableau(palette = "Tableau 20", name = "Food group", direction = 1) +
  xlab("") + ylab("") + 
  ggtitle("A. Cost - wet season") +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "none",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  scale_x_discrete(labels = c("5. Current" = "Current",
                              "4. Mean on-farm content" = "Mean on-farm content",
                              "3. Maximum on-farm content" = "Maximum on-farm content",
                              "2. Maximum on-station content" = "Maximum on-station content",
                              "1. Target content" = "Target content")) +
  annotate(size = 4, "text", x = 5:1, y = as.numeric(cst1) + 0.09, label = gsub(" ", "", cst1)) + 
  ylim(0, max(as.numeric(cst1)) + 0.12) +
  coord_flip()


pcst2 = ing2 %>%
  ggplot(aes(x = scenario, y = value, fill = as.factor(group))) +
  geom_bar(stat = 'identity', width=.5, position = position_stack(reverse = TRUE), color = "black") +
  ggthemes::scale_fill_tableau(palette = "Tableau 20", name = "Food group", direction = 1) +
  xlab("") + ylab("") + 
  ggtitle("B. Cost - dry season") +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "right",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 10,  face = "bold"),
        axis.text.y = element_blank(),
        axis.title = element_blank()) +
  scale_x_discrete(labels = c("5. Current" = "Current",
                              "4. Mean on-farm content" = "Mean on-farm content",
                              "3. Maximum on-farm content" = "Maximum on-farm content",
                              "2. Maximum on-station content" = "Maximum on-station content",
                              "1. Target content" = "Target content")) +
  annotate(size = 4, "text", x = 5:1, y = as.numeric(cst2) + 0.07, label = gsub(" ", "", cst2)) + 
  ylim(0, max(as.numeric(cst2)) + 0.1) +
  coord_flip()

FIGURE5 = ggarrange(pcst1, pcst2,
                   ncol = 2, nrow = 1, widths = c(1, 1), heights = c(1))

x.grob <- textGrob("Cost of optimized diets (USD per adult equivalent and per day)", gp = gpar(fontface = "bold", fontsize = 12))

ggdraw(arrangeGrob(FIGURE5, bottom = x.grob)) +  theme(plot.margin = margin(10, 10, 10, 10))

ggsave("Output\\Figure 5.jpeg", units = "cm", width = 40, height = 15, dpi = 320)
