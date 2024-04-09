#' ---
#' title: "Projected impact of the adoption of large-scale PVA maize adoption"
#' author: "Frédéric Baudron"
#' date: "April 9th, 2024"
#' ---


rm(list=ls())


# LOADING NECESSARY PACKAGES----------------------------------------------------

library(openxlsx)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(egg)
library(cowplot)
library(grid)


# LOADING THE DATA--------------------------------------------------------------

intnopva1 = read.xlsx("Data\\Daily intakes current and PVA scenarios.xlsx", sheet = 2)
intnopva2 = read.xlsx("Data\\Daily intakes current and PVA scenarios.xlsx", sheet = 3)
intpva28_1 = read.xlsx("Data\\Daily intakes current and PVA scenarios.xlsx", sheet = 4)
intpva28_2 = read.xlsx("Data\\Daily intakes current and PVA scenarios.xlsx", sheet = 5)
intpva40_1 = read.xlsx("Data\\Daily intakes current and PVA scenarios.xlsx", sheet = 6)
intpva40_2 = read.xlsx("Data\\Daily intakes current and PVA scenarios.xlsx", sheet = 7)
intpva95_1 = read.xlsx("Data\\Daily intakes current and PVA scenarios.xlsx", sheet = 8)
intpva95_2 = read.xlsx("Data\\Daily intakes current and PVA scenarios.xlsx", sheet = 9)
intpva125_1 = read.xlsx("Data\\Daily intakes current and PVA scenarios.xlsx", sheet = 10)
intpva125_2 = read.xlsx("Data\\Daily intakes current and PVA scenarios.xlsx", sheet = 11)
cor = read.xlsx("Data\\Correspondance.xlsx", sheet = 2)


# DATA MANIPULATION-------------------------------------------------------------

intnopva1 = intnopva1[, c(1, 4)]
intnopva2 = intnopva2[, c(1, 4)]


intpva28_1 = intpva28_1[, c(1, 4)]
intpva28_2 = intpva28_2[, c(1, 4)]

int28_1 = merge(intnopva1, intpva28_1, by = "HH_ID")
int28_2 = merge(intnopva2, intpva28_2, by = "HH_ID")

int28_1$"Extra from PVA maize" = int28_1$Vitamin.A..RAE..ug. - int28_1$vit_a
names(int28_1)[2] = "Current"
int28_1 = merge(int28_1, cor[, c(1:2)], by = "HH_ID")
int28_1 = int28_1[, c(5, 2, 4)]
int28_1 = gather(int28_1, scenario, intake, "Current":"Extra from PVA maize")

int28_2$"Extra from PVA maize" = int28_2$Vitamin.A..RAE..ug. - int28_2$vit_a
names(int28_2)[2] = "Current"
int28_2 = merge(int28_2, cor[, c(1:2)], by = "HH_ID")
int28_2 = int28_2[, c(5, 2, 4)]
int28_2 = gather(int28_2, scenario, intake, "Current":"Extra from PVA maize")


intpva40_1 = intpva40_1[, c(1, 4)]
intpva40_2 = intpva40_2[, c(1, 4)]

int40_1 = merge(intnopva1, intpva40_1, by = "HH_ID")
int40_2 = merge(intnopva2, intpva40_2, by = "HH_ID")

int40_1$"Extra from PVA maize" = int40_1$Vitamin.A..RAE..ug. - int40_1$vit_a
names(int40_1)[2] = "Current"
int40_1 = merge(int40_1, cor[, c(1:2)], by = "HH_ID")
int40_1 = int40_1[, c(5, 2, 4)]
int40_1 = gather(int40_1, scenario, intake, "Current":"Extra from PVA maize")

int40_2$"Extra from PVA maize" = int40_2$Vitamin.A..RAE..ug. - int40_2$vit_a
names(int40_2)[2] = "Current"
int40_2 = merge(int40_2, cor[, c(1:2)], by = "HH_ID")
int40_2 = int40_2[, c(5, 2, 4)]
int40_2 = gather(int40_2, scenario, intake, "Current":"Extra from PVA maize")


intpva95_1 = intpva95_1[, c(1, 4)]
intpva95_2 = intpva95_2[, c(1, 4)]

int95_1 = merge(intnopva1, intpva95_1, by = "HH_ID")
int95_2 = merge(intnopva2, intpva95_2, by = "HH_ID")

int95_1$"Extra from PVA maize" = int95_1$Vitamin.A..RAE..ug. - int95_1$vit_a
names(int95_1)[2] = "Current"
int95_1 = merge(int95_1, cor[, c(1:2)], by = "HH_ID")
int95_1 = int95_1[, c(5, 2, 4)]
int95_1 = gather(int95_1, scenario, intake, "Current":"Extra from PVA maize")

int95_2$"Extra from PVA maize" = int95_2$Vitamin.A..RAE..ug. - int95_2$vit_a
names(int95_2)[2] = "Current"
int95_2 = merge(int95_2, cor[, c(1:2)], by = "HH_ID")
int95_2 = int95_2[, c(5, 2, 4)]
int95_2 = gather(int95_2, scenario, intake, "Current":"Extra from PVA maize")


intpva125_1 = intpva125_1[, c(1, 4)]
intpva125_2 = intpva125_2[, c(1, 4)]

int125_1 = merge(intnopva1, intpva125_1, by = "HH_ID")
int125_2 = merge(intnopva2, intpva125_2, by = "HH_ID")

int125_1$"Extra from PVA maize" = int125_1$Vitamin.A..RAE..ug. - int125_1$vit_a
names(int125_1)[2] = "Current"
int125_1 = merge(int125_1, cor[, c(1:2)], by = "HH_ID")
int125_1 = int125_1[, c(5, 2, 4)]
int125_1 = gather(int125_1, scenario, intake, "Current":"Extra from PVA maize")

int125_2$"Extra from PVA maize" = int125_2$Vitamin.A..RAE..ug. - int125_2$vit_a
names(int125_2)[2] = "Current"
int125_2 = merge(int125_2, cor[, c(1:2)], by = "HH_ID")
int125_2 = int125_2[, c(5, 2, 4)]
int125_2 = gather(int125_2, scenario, intake, "Current":"Extra from PVA maize")


# FIGURE 4 – VITAMIN A INTAKE IN CURRENT DIETS AND IN MODELLED DIETS------------ 

p1 = int28_1 %>%
  ggplot(aes(x = reorder(ID, intake, sum), y = intake, fill = scenario)) +
  geom_bar(stat = 'identity', width=.5, position = position_stack(reverse = TRUE), color = "black") +
  ggthemes::scale_fill_tableau() +
  xlab("") + ylab("") + theme_few() +
  geom_hline(yintercept = 570, linetype = 2, size = 1, color = "grey30") +
  geom_hline(yintercept = 570/2, linetype = 2, size = 1, color = "grey30") +
  ggtitle("A. Mean on-farm content - wet season") +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "none",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  coord_flip()

p2 = int28_2 %>%
  ggplot(aes(x = reorder(ID, intake, sum), y = intake, fill = scenario)) +
  geom_bar(stat = 'identity', width=.5, position = position_stack(reverse = TRUE), color = "black") +
  ggthemes::scale_fill_tableau() +
  xlab("") + ylab("") + theme_few() +
  geom_hline(yintercept = 570, linetype = 2, size = 1, color = "grey30") +
  geom_hline(yintercept = 570/2, linetype = 2, size = 1, color = "grey30") +
  ggtitle("B. Mean on-farm content - dry season") +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "none",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  coord_flip()

p3 = int40_1 %>%
  ggplot(aes(x = reorder(ID, intake, sum), y = intake, fill = scenario)) +
  geom_bar(stat = 'identity', width=.5, position = position_stack(reverse = TRUE), color = "black") +
  ggthemes::scale_fill_tableau() +
  xlab("") + ylab("") + theme_few() +
  geom_hline(yintercept = 570, linetype = 2, size = 1, color = "grey30") +
  geom_hline(yintercept = 570/2, linetype = 2, size = 1, color = "grey30") +
  ggtitle("C. Maximum on-farm content - wet season") +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "none",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  coord_flip()

p4 = int40_2 %>%
  ggplot(aes(x = reorder(ID, intake, sum), y = intake, fill = scenario)) +
  geom_bar(stat = 'identity', width=.5, position = position_stack(reverse = TRUE), color = "black") +
  ggthemes::scale_fill_tableau() +
  xlab("") + ylab("") + theme_few() +
  geom_hline(yintercept = 570, linetype = 2, size = 1, color = "grey30") +
  geom_hline(yintercept = 570/2, linetype = 2, size = 1, color = "grey30") +
  ggtitle("D. Maximum on-farm content - dry season") +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "none",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  coord_flip()

p5 = int95_1 %>%
  ggplot(aes(x = reorder(ID, intake, sum), y = intake, fill = scenario)) +
  geom_bar(stat = 'identity', width=.5, position = position_stack(reverse = TRUE), color = "black") +
  ggthemes::scale_fill_tableau() +
  xlab("") + ylab("") + theme_few() +
  geom_hline(yintercept = 570, linetype = 2, size = 1, color = "grey30") +
  geom_hline(yintercept = 570/2, linetype = 2, size = 1, color = "grey30") +
  ggtitle("E. Maximum on-station content - wet season") +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "none",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  coord_flip()

p6 = int95_2 %>%
  ggplot(aes(x = reorder(ID, intake, sum), y = intake, fill = scenario)) +
  geom_bar(stat = 'identity', width=.5, position = position_stack(reverse = TRUE), color = "black") +
  ggthemes::scale_fill_tableau() +
  xlab("") + ylab("") + theme_few() +
  geom_hline(yintercept = 570, linetype = 2, size = 1, color = "grey30") +
  geom_hline(yintercept = 570/2, linetype = 2, size = 1, color = "grey30") +
  ggtitle("F. Maximum on-station content - dry season") +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "none",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  coord_flip()

p7 = int125_1 %>%
  ggplot(aes(x = reorder(ID, intake, sum), y = intake, fill = scenario)) +
  geom_bar(stat = 'identity', width=.5, position = position_stack(reverse = TRUE), color = "black") +
  ggthemes::scale_fill_tableau() +
  xlab("") + ylab("") + theme_few() +
  geom_hline(yintercept = 570, linetype = 2, size = 1, color = "grey30") +
  geom_hline(yintercept = 570/2, linetype = 2, size = 1, color = "grey30") +
  ggtitle("G. Target content - wet season") +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        legend.position = "none",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  coord_flip()

p8 = int125_2 %>%
  ggplot(aes(x = reorder(ID, intake, sum), y = intake, fill = scenario)) +
  geom_bar(stat = 'identity', width=.5, position = position_stack(reverse = TRUE), color = "black") +
  ggthemes::scale_fill_tableau() +
  xlab("") + ylab("") + theme_few() +
  geom_hline(yintercept = 570, linetype = 2, size = 1, color = "grey30") +
  geom_hline(yintercept = 570/2, linetype = 2, size = 1, color = "grey30") +
  ggtitle("H. Target content - dry season") +
  theme(plot.title = element_text(size = 16, face = 'bold'),
        legend.position = c(0.95, 0.05), legend.justification = c(0.95, 0.05),
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 10,  face = "bold"),
        axis.title = element_blank()) +
  coord_flip()

FIGURE4 = ggarrange(p1, p2, p3, p4, p5, p6, p7, p8,
                   ncol = 2, nrow = 4, widths = c(1, 1), heights = c(1, 1, 1, 1))

x.grob <- textGrob("Vitamin A (\u00b5g RAE per adult male equivalent and per day)", gp = gpar(fontface = "bold", fontsize = 14))
y.grob <- textGrob("Farm ID", gp = gpar(fontface = "bold", fontsize = 14), rot = 90)

ggdraw(arrangeGrob(FIGURE4, bottom = x.grob, left = y.grob)) +  theme(plot.margin = margin(10, 10, 10, 10))

ggsave("Output\\Figure 4.jpeg", units = "cm", width = 32, height = 60, dpi = 320)
