#' ---
#' title: "Description of participants and current diets"
#' author: "Frédéric Baudron"
#' date: "April 9th, 2024"
#' ---


rm(list=ls())


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(openxlsx)
library(dplyr)
library(tidyr)
library(gtsummary)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(egg)
library(cowplot)
library(grid)


# LOADING THE DATA--------------------------------------------------------------

hh1 = read.xlsx("Data\\Household individuals.xlsx", sheet = 2)
hh2 = read.xlsx("Data\\Household individuals.xlsx", sheet = 3)
mealsuppsourceday1 = read.xlsx("Data\\Daily intakes by meals vs snacks.xlsx", sheet = 2)
mealsuppsourceday2 = read.xlsx("Data\\Daily intakes by meals vs snacks.xlsx", sheet = 3)
sourceday1 = read.xlsx("Data\\Daily intakes by sources.xlsx", sheet = 2)
sourceday2 = read.xlsx("Data\\Daily intakes by sources.xlsx", sheet = 3)
sourcegroup1 = read.xlsx("Data\\Daily intakes by food groups.xlsx", sheet = 2)
sourcegroup2 = read.xlsx("Data\\Daily intakes by food groups.xlsx", sheet = 3)
intake1 = read.xlsx("Data\\Daily intakes current and PVA scenarios.xlsx", sheet = 2)
intake2 = read.xlsx("Data\\Daily intakes current and PVA scenarios.xlsx", sheet = 3)
qty1 = read.xlsx("Data\\Daily quantities of food items consumed.xlsx", sheet = 2)
qty2 = read.xlsx("Data\\Daily quantities of food items consumed.xlsx", sheet = 3)
pr1 = read.xlsx("Data\\Prices by food items.xlsx", sheet = 2)
pr2 = read.xlsx("Data\\Prices by food items.xlsx", sheet = 3)
cor = read.xlsx("Data\\Correspondance.xlsx", sheet = 2) 
group = read.xlsx("Data\\Food items by food groups.xlsx", sheet = 2) 
compo = read.xlsx("Data\\Food composition.xlsx", sheet = 2) 


# DATA MANIPULATION-------------------------------------------------------------

names(cor)[8] = c("Type")

cor$Type = ifelse(cor$Type == "1", "Type 1",
                            ifelse(cor$Type == "2", "Type 2", 
                                   ifelse(cor$Type == "3", "Type 3", "Type 4")))

hh1$cat3 = ifelse(hh1$cat2 == "I1", "Infants",
                  ifelse(hh1$cat2 == "I2", "Infants",
                         hh1$cat2))

hh1$cat3 = ifelse(hh1$cat3 == "C1", "Children",
                  ifelse(hh1$cat3 == "C2", "Children",
                         ifelse(hh1$cat3 == "C3", "Children",
                                ifelse(hh1$cat3 == "C4M", "Children",
                                       ifelse(hh1$cat3 == "C4F", "Children",
                                              ifelse(hh1$cat3 == "C5M", "Children",
                                                     ifelse(hh1$cat3 == "C5F", "Children",
                                                            ifelse(hh1$cat3 == "C5FP", "Children",
                                                                   ifelse(hh1$cat3 == "C5FL", "Children",
                                                                          hh1$cat3)))))))))

hh1$cat3 = ifelse(hh1$cat3 == "A1M", "Young male adults",
                  ifelse(hh1$cat3 == "A2M", "Young male adults",
                         hh1$cat3))

hh1$cat3 = ifelse(hh1$cat3 == "A3M", "Older male adults",
                  ifelse(hh1$cat3 == "A4M", "Older male adults",
                         hh1$cat3))

hh1$cat3 = ifelse(hh1$cat3 == "A1FP", "Young female adults",
                  ifelse(hh1$cat3 == "A1FL", "Young female adults",
                         ifelse(hh1$cat3 == "A1F", "Young female adults",
                                ifelse(hh1$cat3 == "A2FP", "Young female adults",
                                       ifelse(hh1$cat3 == "A2FL", "Young female adults",
                                              ifelse(hh1$cat3 == "A2F", "Young female adults",
                                                     hh1$cat3))))))

hh1$cat3 = ifelse(hh1$cat3 == "A3F", "Older female adults",
                  ifelse(hh1$cat3 == "A4F", "Older female adults",
                         hh1$cat3))

hh2$cat3 = ifelse(hh2$cat2 == "I1", "Infants",
                  ifelse(hh2$cat2 == "I2", "Infants",
                         hh2$cat2))

hh2$cat3 = ifelse(hh2$cat3 == "C1", "Children",
                  ifelse(hh2$cat3 == "C2", "Children",
                         ifelse(hh2$cat3 == "C3", "Children",
                                ifelse(hh2$cat3 == "C4M", "Children",
                                       ifelse(hh2$cat3 == "C4F", "Children",
                                              ifelse(hh2$cat3 == "C5M", "Children",
                                                     ifelse(hh2$cat3 == "C5F", "Children",
                                                            ifelse(hh2$cat3 == "C5FP", "Children",
                                                                   ifelse(hh2$cat3 == "C5FL", "Children",
                                                                          hh2$cat3)))))))))

hh2$cat3 = ifelse(hh2$cat3 == "A1M", "Young male adults",
                  ifelse(hh2$cat3 == "A2M", "Young male adults",
                         hh2$cat3))

hh2$cat3 = ifelse(hh2$cat3 == "A3M", "Older male adults",
                  ifelse(hh2$cat3 == "A4M", "Older male adults",
                         hh2$cat3))

hh2$cat3 = ifelse(hh2$cat3 == "A1FP", "Young female adults",
                  ifelse(hh2$cat3 == "A1FL", "Young female adults",
                         ifelse(hh2$cat3 == "A1F", "Young female adults",
                                ifelse(hh2$cat3 == "A2FP", "Young female adults",
                                       ifelse(hh2$cat3 == "A2FL", "Young female adults",
                                              ifelse(hh2$cat3 == "A2F", "Young female adults",
                                                     hh2$cat3))))))

hh2$cat3 = ifelse(hh2$cat3 == "A3F", "Older female adults",
                  ifelse(hh2$cat3 == "A4F", "Older female adults",
                         hh2$cat3))


hh1$n = rep(1, nrow(hh1))

hhtot1 = hh1[, c(2, 13)] %>%
  group_by(HH_ID) %>%
  summarise_each(funs(sum))

hhcomp1 = hh1[, c(2, 12, 13)] %>%
  group_by(HH_ID, cat3) %>%
  summarise_each(funs(sum))

hhcomp1 = spread(hhcomp1, cat3, n)

hhcomp1[is.na(hhcomp1)] = 0

hhcomp1 = merge(hhtot1, hhcomp1, by = "HH_ID")

hhcomp1 = hhcomp1[, c(1, 2, 4, 3, 8, 7, 6, 5)]

names(hhcomp1)[2] = c("Total")

hhcomp1 = merge(hhcomp1, cor[, c(2, 8)], by = "HH_ID")


hh2$n = rep(1, nrow(hh2))

hhtot2 = hh2[, c(2, 13)] %>%
  group_by(HH_ID) %>%
  summarise_each(funs(sum))

hhcomp2 = hh2[, c(2, 12, 13)] %>%
  group_by(HH_ID, cat3) %>%
  summarise_each(funs(sum))

hhcomp2 = spread(hhcomp2, cat3, n)

hhcomp2[is.na(hhcomp2)] = 0

hhcomp2 = merge(hhtot2, hhcomp2, by = "HH_ID")

hhcomp2 = hhcomp2[, c(1, 2, 4, 3, 8, 7, 6, 5)]

names(hhcomp2)[2] = c("Total")

hhcomp2 = merge(hhcomp2, cor[, c(2, 8)], by = "HH_ID")


# TABLE 1 - MEAN HOUSEHOLD SIZE AND COMPOSITION--------------------------------- 

hhcomp1[, -c(1)] %>%
  tbl_summary(
    by = Type,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    missing = "no",
    type = list(Infants ~ "continuous",
                Children ~ "continuous",
                `Young male adults` ~ "continuous",
                `Young female adults` ~ "continuous",
                `Older male adults` ~ "continuous",
                `Older female adults` ~ "continuous"
    ),
    digits = all_continuous() ~ 1,
  )%>%
  add_overall()


hhcomp2[, -c(1)] %>%
  tbl_summary(
    by = Type,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    missing = "no",
    type = list(Infants ~ "continuous",
                Children ~ "continuous",
                `Young male adults` ~ "continuous",
                `Young female adults` ~ "continuous",
                `Older male adults` ~ "continuous",
                `Older female adults` ~ "continuous"
    ),
    digits = all_continuous() ~ 1,
  )%>%
  add_overall()


# TESTS OF DIFFERENCES BETWEEN TYPES IN HOUSEHOLD SIZE AND COMPOSITION---------- 

# Wet season

aov_tot1 = aov(Total ~ Type,
               data = hhcomp1)
summary(aov_tot1)

aov_inf1 = aov(Infants ~ Type,
               data = hhcomp1)
summary(aov_inf1)

aov_chd1 = aov(Children ~ Type,
               data = hhcomp1)
summary(aov_chd1)
TukeyHSD(aov_chd1)
plot(TukeyHSD(aov_chd1))

aov_yml1 = aov(`Young male adults` ~ Type,
               data = hhcomp1)
summary(aov_yml1)

aov_yfm1 = aov(`Young female adults` ~ Type,
               data = hhcomp1)
summary(aov_yfm1)

aov_oml1 = aov(`Older male adults` ~ Type,
               data = hhcomp1)
summary(aov_oml1)

aov_ofm1 = aov(`Older female adults` ~ Type,
               data = hhcomp1)
summary(aov_ofm1)

# Dry season

aov_tot2 = aov(Total ~ Type,
               data = hhcomp2)
summary(aov_tot2)

aov_inf2 = aov(Infants ~ Type,
               data = hhcomp2)
summary(aov_inf2)

aov_chd2 = aov(Children ~ Type,
               data = hhcomp2)
summary(aov_chd2)
TukeyHSD(aov_chd2)
plot(TukeyHSD(aov_chd2))

aov_yml2 = aov(`Young male adults` ~ Type,
               data = hhcomp2)
summary(aov_yml2)

aov_yfm2 = aov(`Young female adults` ~ Type,
               data = hhcomp2)
summary(aov_yfm2)

aov_oml2 = aov(`Older male adults` ~ Type,
               data = hhcomp2)
summary(aov_oml2)

aov_ofm2 = aov(`Older female adults` ~ Type,
               data = hhcomp2)
summary(aov_ofm2)


# TABLE 2 - MEAN QUANTITIES OF FOOD GROUPS CONSUMED IN CURRENT DIETS------------ 

compo = compo[, c(1, 33)]
compo = unique(compo)

qty1 = merge(qty1, compo, by = "food_item")
qty2 = merge(qty2, compo, by = "food_item")

group = unique(group)

qty1 = merge(qty1, group, by = "food_item")
qty2 = merge(qty2, group, by = "food_item")

qty1$quantity = ((100 - qty1$Water..g.)/100) * qty1$quantity
qty2$quantity = ((100 - qty2$Water..g.)/100) * qty2$quantity

qtygroupsum1 <- qty1[, c(2:3, 5)] %>%
  group_by(HH_ID, group) %>%
  summarise_each(funs(sum))

meanqtygroup1 = qtygroupsum1[, c(2:3)]%>%
  group_by(group) %>%
  summarise_each(funs(mean))

meanqtygroup1$quantity = round(meanqtygroup1$quantity, 1)

meanqtygroup1

qtygroupsum2 <- qty2[, c(2:3, 5)] %>%
  group_by(HH_ID, group) %>%
  summarise_each(funs(sum))

meanqtygroup2 = qtygroupsum2[, c(2:3)]%>%
  group_by(group) %>%
  summarise_each(funs(mean))

meanqtygroup2$quantity = round(meanqtygroup2$quantity, 1)

meanqtygroup2


# TABLE 3 - MEAN INTAKE PER NUTRIENT-------------------------------------------- 

intake1 = merge(intake1, cor[, c(1:2, 8)], by = "HH_ID")
intake2 = merge(intake2, cor[, c(1:2, 8)], by = "HH_ID")

# Wet season

intake1[, c(38, 2, 36, 4:7, 9:18, 27, 20, 25, 23,  24, 29)] %>%
  tbl_summary(
    by = Type,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    missing = "no",
    digits = all_continuous() ~ 1,
    label = list(energy ~ "Energy (kJ",
                 prot ~ "Protein (g)",
                 vit_a ~ "Vitamin A (µg RAE)",
                 vit_c ~ "Vitamin C (mg)",
                 vit_d ~ "Vitamin D (µg)",
                 vit_e ~ "Vitamin E (mg)",
                 thia ~ "Thiamin (mg)",
                 ribo ~ "Riboflavin (mg)",
                 niac ~ "Niacin (mg)",
                 vit_b6 ~ "Vitamin B6 (mg)",
                 fol ~ "Folate (µg)",
                 vit_b12 ~ "Vitamin B12 (µg)",
                 pant ~ "Pantothenic acid (mg)",
                 biot ~ "Biotin (mg)",
                 chol ~ "Cholin (mg)",
                 ca ~ "Calcium (mg)",
                 ph ~ "Phosphorus (mg)",
                 cu ~ "Copper (µg)",
                 mn ~ "Manganese (mg)",
                 fe ~ "Iron (mg)",
                 mg ~ "Magnesium (mg)",
                 zn ~ "Zinc (mg)"))%>%
  add_overall()

# Dry season

intake2[, c(38, 2, 36, 4:7, 9:18, 27, 20, 25, 23,  24, 29)] %>%
  tbl_summary(
    by = Type,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%"),
    missing = "no",
    digits = all_continuous() ~ 1,
    label = list(energy ~ "Energy (kJ",
                 prot ~ "Protein (g)",
                 vit_a ~ "Vitamin A (µg RAE)",
                 vit_c ~ "Vitamin C (mg)",
                 vit_d ~ "Vitamin D (µg)",
                 vit_e ~ "Vitamin E (mg)",
                 thia ~ "Thiamin (mg)",
                 ribo ~ "Riboflavin (mg)",
                 niac ~ "Niacin (mg)",
                 vit_b6 ~ "Vitamin B6 (mg)",
                 fol ~ "Folate (µg)",
                 vit_b12 ~ "Vitamin B12 (µg)",
                 pant ~ "Pantothenic acid (mg)",
                 biot ~ "Biotin (mg)",
                 chol ~ "Cholin (mg)",
                 ca ~ "Calcium (mg)",
                 ph ~ "Phosphorus (mg)",
                 cu ~ "Copper (µg)",
                 mn ~ "Manganese (mg)",
                 fe ~ "Iron (mg)",
                 mg ~ "Magnesium (mg)",
                 zn ~ "Zinc (mg)"))%>%
  add_overall()


# TESTING DIFFERENCES BETWEEN TYPES---------------------------------------------

# Wet season

aov_energy1 = aov(energy ~ Type,
                data = intake1)
summary(aov_energy1)

aov_prot1 = aov(prot ~ Type,
                  data = intake1)
summary(aov_prot1)
TukeyHSD(aov_prot1)
plot(TukeyHSD(aov_prot1))

aov_vita1 = aov(vit_a ~ Type,
               data = intake1)
summary(aov_vita1)

aov_vitc1 = aov(vit_c ~ Type,
                data = intake1)
summary(aov_vitc1)

aov_vitd1 = aov(vit_d ~ Type,
                data = intake1)
summary(aov_vitd1)
TukeyHSD(aov_vitd1)
plot(TukeyHSD(aov_vitd1))

aov_vite1 = aov(vit_e ~ Type,
                data = intake1)
summary(aov_vite1)

aov_ribo1 = aov(ribo ~ Type,
                data = intake1)
summary(aov_ribo1)

aov_vit_b121 = aov(vit_b12 ~ Type,
                data = intake1)
summary(aov_vit_b121)

aov_chol1 = aov(chol ~ Type,
                data = intake1)
summary(aov_chol1)
TukeyHSD(aov_chol1)
plot(TukeyHSD(aov_chol1))

aov_ca1 = aov(ca ~ Type,
                data = intake1)
summary(aov_ca1)

aov_fe1 = aov(fe ~ Type,
              data = intake1)
summary(aov_fe1)

aov_zn1 = aov(zn ~ Type,
              data = intake1)
summary(aov_zn1)

# Dry season

aov_energy2 = aov(energy ~ Type,
                  data = intake2)
summary(aov_energy2)

aov_prot2 = aov(prot ~ Type,
                data = intake2)
summary(aov_prot2)
TukeyHSD(aov_prot2)
plot(TukeyHSD(aov_prot2))

aov_vita2 = aov(vit_a ~ Type,
                data = intake2)
summary(aov_vita2)

aov_vitc2 = aov(vit_c ~ Type,
                data = intake2)
summary(aov_vitc2)

aov_vitd2 = aov(vit_d ~ Type,
                data = intake2)
summary(aov_vitd2)

aov_vite2 = aov(vit_e ~ Type,
                data = intake2)
summary(aov_vite2)

aov_ribo2 = aov(ribo ~ Type,
                data = intake2)
summary(aov_ribo2)

aov_vit_b122 = aov(vit_b12 ~ Type,
                   data = intake2)
summary(aov_vit_b122)

aov_chol2 = aov(chol ~ Type,
                data = intake2)
summary(aov_chol2)

aov_ca2 = aov(ca ~ Type,
              data = intake2)
summary(aov_ca2)

aov_fe2 = aov(fe ~ Type,
              data = intake2)
summary(aov_fe2)

aov_zn2 = aov(zn ~ Type,
              data = intake2)
summary(aov_zn2)


# FIGURE 1 – VITAMIN A INTAKE--------------------------------------------------- 

mealsuppsourceday1 = merge(mealsuppsourceday1, cor, by = "HH_ID")

sourceday1 = merge(sourceday1, cor, by = "HH_ID")

sourcegroup1 = merge(sourcegroup1, cor, by = "HH_ID")


theme_set(theme_few())  

p1.1 = mealsuppsourceday1 %>%
  ggplot(aes(x = reorder(ID, vit_a, sum), y = vit_a, fill = as.factor(meal_snack))) +
  geom_bar(stat = 'identity', width=.5, position = position_stack(reverse = TRUE), color = "black") +
  geom_hline(yintercept = 570, linewidth = 0.75, color = "grey30", linetype = 2) +
  geom_hline(yintercept = 570/2, linewidth = 0.75, color = "grey30", linetype = 2) +
  ggthemes::scale_fill_tableau(name = "Source", direction = 1) +
  xlab("") + ylab("") + 
  ggtitle("A. Intake per meal vs. snack - wet season") +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "none",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  coord_flip()
  
p2.1 = sourceday1 %>%
  ggplot(aes(x = reorder(ID, vit_a, sum), y = vit_a, fill = as.factor(source))) +
  geom_bar(stat = 'identity', width=.5, position = position_stack(reverse = TRUE), color = "black") +
  geom_hline(yintercept = 570, linewidth = 0.75, color = "grey30", linetype = 2) +
  geom_hline(yintercept = 570/2, linewidth = 0.75, color = "grey30", linetype = 2) +
  ggthemes::scale_fill_tableau(name = "Source", direction = 1) +
  xlab("") + ylab("") + 
  ggtitle("C. Intake per food source - wet season") +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "none",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  coord_flip()

sourcegroup1[nrow(sourcegroup1) + 1,] = list("Chikosha HH1", "Organ meat", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
sourcegroup1[nrow(sourcegroup1) + 1,] = list("Chikosha HH1", "Vitamin A rich fruits", 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

p3.1 = sourcegroup1 %>%
  ggplot(aes(x = reorder(ID, vit_a, sum), y = vit_a, fill = as.factor(group))) +
  geom_bar(stat = 'identity', width=.5, position = position_stack(reverse = TRUE), color = "black") +
  geom_hline(yintercept = 570, linewidth = 0.75, color = "grey30", linetype = 2) +
  geom_hline(yintercept = 570/2, linewidth = 0.75, color = "grey30", linetype = 2) +
  ggthemes::scale_fill_tableau(palette = "Tableau 20", name = "Source", direction = 1) +
  xlab("") + ylab("") + 
  ggtitle("E. Intake per food group - wet season") +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "none",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  coord_flip()

p4.1 = intake1 %>%
  ggplot(aes(x = reorder(ID, vit_a, sum), y = vit_a, fill = as.factor(Type))) +
  geom_bar(stat = 'identity', width=.5, position = position_stack(reverse = TRUE), color = "black") +
  geom_hline(yintercept = 570, linewidth = 0.75, color = "grey30", linetype = 2) +
  geom_hline(yintercept = 570/2, linewidth = 0.75, color = "grey30", linetype = 2) +
  ggthemes::scale_fill_tableau(name = "Source", direction = 1) +
  xlab("") + ylab("") + 
  ggtitle("G. Intake per farm type - wet season") +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "none",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  coord_flip()


mealsuppsourceday2 = merge(mealsuppsourceday2, cor, by = "HH_ID")

sourceday2 = merge(sourceday2, cor, by = "HH_ID")

sourcegroup2 = merge(sourcegroup2, cor, by = "HH_ID")

p1.2 = mealsuppsourceday2 %>%
  ggplot(aes(x = reorder(ID, vit_a, sum), y = vit_a, fill = as.factor(meal_snack))) +
  geom_bar(stat = 'identity', width=.5, position = position_stack(reverse = TRUE), color = "black") +
  geom_hline(yintercept = 570, linewidth = 0.75, color = "grey30", linetype = 2) +
  geom_hline(yintercept = 570/2, linewidth = 0.75, color = "grey30", linetype = 2) +
  ggthemes::scale_fill_tableau(name = "Source", direction = 1,
                               labels = c("Meal", "Snack")) +
  xlab("") + ylab("") + 
  ggtitle("B. Intake per meal vs. snack - dry season") +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "right",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  coord_flip()

p2.2 = sourceday2 %>%
  ggplot(aes(x = reorder(ID, vit_a, sum), y = vit_a, fill = as.factor(source))) +
  geom_bar(stat = 'identity', width=.5, position = position_stack(reverse = TRUE), color = "black") +
  geom_hline(yintercept = 570, linewidth = 0.75, color = "grey30", linetype = 2) +
  geom_hline(yintercept = 570/2, linewidth = 0.75, color = "grey30", linetype = 2) +
  ggthemes::scale_fill_tableau(name = "Source", direction = 1,
                               labels = c("Food production", "Purchase", "Gift", "Hunting/gathering", "Other")) +
  xlab("") + ylab("") + 
  ggtitle("D. Intake per food source - dry season") +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "right",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  coord_flip()

p3.2 = sourcegroup2 %>%
  ggplot(aes(x = reorder(ID, vit_a, sum), y = vit_a, fill = as.factor(group))) +
  geom_bar(stat = 'identity', width=.5, position = position_stack(reverse = TRUE), color = "black") +
  geom_hline(yintercept = 570, linewidth = 0.75, color = "grey30", linetype = 2) +
  geom_hline(yintercept = 570/2, linewidth = 0.75, color = "grey30", linetype = 2) +
  ggthemes::scale_fill_tableau(palette = "Tableau 20", name = "Group", direction = 1) +
  xlab("") + ylab("") + 
  ggtitle("F. Intake per food group - dry season") +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "right",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  coord_flip()

p4.2 = intake2 %>%
  ggplot(aes(x = reorder(ID, vit_a, sum), y = vit_a, fill = as.factor(Type))) +
  geom_bar(stat = 'identity', width=.5, position = position_stack(reverse = TRUE), color = "black") +
  geom_hline(yintercept = 570, linewidth = 0.75, color = "grey30", linetype = 2) +
  geom_hline(yintercept = 570/2, linewidth = 0.75, color = "grey30", linetype = 2) +
  ggthemes::scale_fill_tableau(name = "Type",
                               labels = c("Type 1", "Type 2", "Type 3", "Type 4"), direction = 1) +
  xlab("") + ylab("") + 
  ggtitle("H. Intake per farm type - dry season") +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "right",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  coord_flip()


FIGURE1 = ggarrange(p1.1, p1.2, p2.1, p2.2, p3.1, p3.2, p4.1, p4.2,
                   ncol = 2, nrow = 4, widths = c(1, 1), heights = c(1, 1, 1, 1))

x.grob <- textGrob("Vitamin A (\u00b5g RAE per adult male equivalent and per day)", gp = gpar(fontface = "bold", fontsize = 14))
y.grob <- textGrob("Farm ID", gp = gpar(fontface = "bold", fontsize = 14), rot = 90)

ggdraw(arrangeGrob(FIGURE1, bottom = x.grob, left = y.grob)) +  theme(plot.margin = margin(10, 10, 10, 10))

ggsave("Output\\Figure 1.jpeg", units = "cm", width = 40, height = 60, dpi = 320)
