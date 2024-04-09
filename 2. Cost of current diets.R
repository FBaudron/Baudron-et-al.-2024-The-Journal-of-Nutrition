#' ---
#' title: "Cost of current diets"
#' author: "Frédéric Baudron"
#' date: "April 9th, 2024"
#' ---


rm(list=ls())


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(openxlsx)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(egg)
library(cowplot)
library(grid)


# LOADING THE DATA--------------------------------------------------------------

qty1 = read.xlsx("Data\\Daily quantities of food items consumed.xlsx", sheet = 2)
qty2 = read.xlsx("Data\\Daily quantities of food items consumed.xlsx", sheet = 3)
pr1 = read.xlsx("Data\\Prices by food items.xlsx", sheet = 2)
pr2 = read.xlsx("Data\\Prices by food items.xlsx", sheet = 3)
cor = read.xlsx("Data\\Correspondance.xlsx", sheet = 2) 
group = read.xlsx("Data\\Food items by food groups.xlsx", sheet = 2) 
compo = read.xlsx("Data\\Food composition.xlsx", sheet = 2) 

# hh1 = read.csv("Data\\Household composition - Season 1.csv")
# hh2 = read.csv("Data\\Household composition - Season 2.csv")
# mealsuppsourceday1 = read.csv("Data\\Meals & snacks - Season 1.csv")
# mealsuppsourceday2 = read.csv("Data\\Meals & snacks - Season 2.csv")
# sourceday1 = read.csv("Data\\Sources - Season 1.csv")
# sourceday2 = read.csv("Data\\Sources - Season 2.csv")
# sourcegroup1 = read.csv("Data\\Groups - Season 1.csv")
# sourcegroup2 = read.csv("Data\\Groups - Season 2.csv")
# intake1 = read.csv("Data\\Intakes - Season 1.csv")
# intake2 = read.csv("Data\\Intakes - Season 2.csv")



# DATA MANIPULATION-------------------------------------------------------------

qty1 = merge(qty1, group, by = "food_item", all.x = TRUE)
qty1 = merge(qty1, pr1, by = "food_item", all.x = TRUE)
qty1$cost = qty1$quantity * qty1$cost_per_g

qty2 = merge(qty2, group, by = "food_item", all.x = TRUE)
qty2 = merge(qty2, pr2, by = "food_item", all.x = TRUE)
qty2$cost = qty2$quantity * qty2$cost_per_g

qtysum1 = na.omit(qty1[, c(2, 4, 7)]) %>%
  group_by(HH_ID, group) %>%
  summarise_each(funs(sum))

qtysum2 = na.omit(qty2[, c(2, 4, 7)]) %>%
  group_by(HH_ID, group) %>%
  summarise_each(funs(sum))

qtysum1 = merge(qtysum1, cor[, c(1:2)], by = "HH_ID")
qtysum2 = merge(qtysum2, cor[, c(1:2)], by = "HH_ID")

qtysumsum1 = na.omit(qty1[, c(2, 7)]) %>%
  group_by(HH_ID) %>%
  summarise_each(funs(sum))

qtysumsum2 = na.omit(qty2[, c(2, 7)]) %>%
  group_by(HH_ID) %>%
  summarise_each(funs(sum))

qtymean1 = na.omit(qtysum1[, c(2, 3)]) %>%
  group_by(group) %>%
  summarise_each(funs(mean))

sum(qtymean1$cost)

qtymean1$cost = round((qtymean1$cost * 100)/ sum(qtymean1$cost), 1)

qtymean2 = na.omit(qtysum2[, c(2, 3)]) %>%
  group_by(group) %>%
  summarise_each(funs(mean))

sum(qtymean2$cost)

qtymean2$cost = round((qtymean2$cost * 100)/ sum(qtymean2$cost), 1)

round(mean(qtysumsum1$cost), 3)
round(sd(qtysumsum1$cost), 3)
round(mean(qtysumsum2$cost), 3)
round(sd(qtysumsum2$cost), 3)

HH_ID = c("Chikosha HH1", "Chikosha HH1")
group = c("Organ meat", "Vitamin A rich fruits")
cost = c(0, 0)
ID = c(279, 279)

qtyother = data.frame(HH_ID, group, cost, ID)

qtysum1 = rbind(qtysum1, qtyother)


compo = compo[which(compo$food_item != "covo_dried"), ]
compo = compo[which(compo$food_item != "rape_dried"), ]
compo = compo[which(compo$food_item != "pumpkin_small"), ]

pr1 = merge(pr1, compo, by  = "food_item")
pr1$cost_vit_a = pr1$Vitamin.A..RAE..ug. / (pr1$cost_per_g * 100)

pr2 = merge(pr2, compo, by  = "food_item")
pr2$cost_vit_a = pr2$Vitamin.A..RAE..ug. / (pr2$cost_per_g * 100)

# FIGURE 2 – COST OF CURRENT DIETS---------------------------------------------- 

theme_set(theme_few())  

pcst1 = qtysum1 %>%
  ggplot(aes(x = reorder(ID, cost, sum), y = cost, fill = as.factor(group))) +
  geom_bar(stat = 'identity', width=.5, position = position_stack(reverse = TRUE), color = "black") +
  geom_hline(yintercept = mean(qtysumsum1$cost), linewidth = 0.75, color = "grey30", linetype = 2) +
  ggthemes::scale_fill_tableau(palette = "Tableau 20", name = "Food group", direction = 1) +
  xlab("") + ylab("") + 
  ggtitle("A. Cost - wet season") +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "none",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  coord_flip()
  
pcst2 = qtysum2 %>%
  ggplot(aes(x = reorder(ID, cost, sum), y = cost, fill = as.factor(group))) +
  geom_bar(stat = 'identity', width=.5, position = position_stack(reverse = TRUE), color = "black") +
  geom_hline(yintercept = mean(qtysumsum2$cost), linewidth = 0.75, color = "grey30", linetype = 2) +
  ggthemes::scale_fill_tableau(palette = "Tableau 20", name = "Food group", direction = 1) +
  xlab("") + ylab("") + 
  ggtitle("B. Cost - dry season") +
  scale_y_continuous(breaks = c(0, 1, 2)) +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "right",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  coord_flip()

FIGURE2 = ggarrange(pcst1, pcst2,
                   ncol = 2, nrow = 1, widths = c(1, 1), heights = c(1))

x.grob <- textGrob("Cost of current diets (USD per adult equivalent and per day)", gp = gpar(fontface = "bold", fontsize = 14))
y.grob <- textGrob("Farm ID", gp = gpar(fontface = "bold", fontsize = 14), rot = 90)

ggdraw(arrangeGrob(FIGURE2, bottom = x.grob, left = y.grob)) +  theme(plot.margin = margin(10, 10, 10, 10))

ggsave("Output\\Figure 2.jpeg", units = "cm", width = 40, height = 20, dpi = 320)


# FIGURE 3 – COST OF LEAST EXPENSIVE VITAMIN A RICH FOOD INTEMS----------------- 

theme_set(theme_few()) 

pr1_vit_a = pr1[order(pr1$cost_vit_a, decreasing = TRUE), ]
pr1_vit_a$score = 1:nrow(pr1_vit_a)
pr1_vit_a = pr1_vit_a[pr1_vit_a$score < 11,]

pr1_vit_a$cost_vit_a = as.numeric(pr1_vit_a$cost_vit_a)

pvit_a1 = pr1_vit_a %>%
  arrange(cost_vit_a) %>%
  mutate(order = factor(food_item, food_item)) %>%
  ggplot(aes(x = order, y = cost_vit_a)) +
  geom_point(color = "#4E79A7", size = 6, alpha = 0.75) +
  geom_segment(aes(x = order, xend = food_item, y = 0, yend = cost_vit_a), color = "#4E79A7",
               size = 1.2, alpha = 0.75) +
  scale_x_discrete(labels = c("kale" = "Kale",
                              "rape" = "Rape",
                              "covo" = "Covo",
                              "butternut" = "Butternut",
                              "pumpkin" = "Pumpkin",
                              "milk_powdered" = "Powdered milk",
                              "margarine" = "Magarine",
                              "pumpkin_leaves" = "Pumpkin leaves",
                              "milk_sour" = "Sour milk",
                              "amaranthus" = "Amaranthus")) +
  ggtitle("A. Wet season") +
  theme(plot.title = element_text(size = 16, face = 'bold', margin = margin (0, 0, 10, 0)),
        legend.position = "none",
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  coord_flip()

pr2_vit_a = pr2[order(pr2$cost_vit_a, decreasing = TRUE), ]
pr2_vit_a$score = 1:nrow(pr2_vit_a)
pr2_vit_a = pr2_vit_a[pr2_vit_a$score < 11,]

pr2_vit_a$cost_vit_a = as.numeric(pr2_vit_a$cost_vit_a)

pvit_a2 = pr2_vit_a %>%
  arrange(cost_vit_a) %>%
  mutate(order = factor(food_item, food_item)) %>%
  ggplot(aes(x = order, y = cost_vit_a)) +
  geom_point(color = "#F28E2B", size = 6, alpha = 0.75) +
  geom_segment(aes(x = order, xend = food_item, y = 0, yend = cost_vit_a), color = "#F28E2B",
               size = 1.2, alpha = 0.75) +
  scale_x_discrete(labels = c("carrots" = "Carrots",
                              "kale" = "Kale",
                              "rape" = "Rape",
                              "covo" = "Covo",
                              "spinach" = "Spinach",
                              "margarine" = "Magarine",
                              "pumpkin" = "Pumpkin",
                              "mango" = "Mango",
                              "butternut" = "Butternut",
                              "cleome_gynandra" = "Cleome")) +
  ggtitle("B. Dry season") +
  theme(plot.title = element_text(size = 16, face = 'bold', margin = margin (0, 0, 10, 0)),
        legend.position = "none",
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  coord_flip()

FIGURE3 = ggarrange(pvit_a1, pvit_a2, 
                   ncol = 2, nrow = 1, widths = c(1, 1), heights = c(1))

x.grob <- textGrob("Vitamin A (\u00b5g RAE/USD)", gp = gpar(fontface = "bold", fontsize = 14))

ggdraw(arrangeGrob(FIGURE3, bottom = x.grob)) +  theme(plot.margin = margin(10, 10, 10, 10))

ggsave("Output\\Figure 3.jpeg", units = "cm", width = 25, height = 15, dpi = 320)
