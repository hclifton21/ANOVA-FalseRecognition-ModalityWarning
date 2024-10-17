# PSYX 225 - Project Code
# Harley Clifton
# 2023-04-29


library(tidyverse)
library(mosaic)
library(plyr)
library(gt)
library(gridExtra)


## Data Importing and Wrangling

mem.dat <- read_csv("data/PSYX225_ProjectData.csv")

mem.dat$Modality <- as.factor(mem.dat$Modality)
mem.dat$Warning <- factor(mem.dat$Warning,
                          levels = c("NoWarning", "Warning"),
                          labels = c("No Warning", "Warning"))

summary(mem.dat)



## Anova & Descriptive Statistics

table(mem.dat$Modality, mem.dat$Warning)


crit.val <- qt(0.975, df = 19)



cr.table <- ddply(mem.dat, c("Modality", "Warning"), summarise,
                  Mean = round(mean(Correct), 3),
                  Std.Dev = round(sd(Correct), 3),
                  Lower.CI = sprintf("%.3f", round(mean(Correct) - (crit.val*sd(Correct)/sqrt(20)), 3)),
                  Upper.CI = round(mean(Correct) + (crit.val*sd(Correct)/sqrt(20)), 3),
                  CI = paste0("(", Lower.CI, ",", " ", Upper.CI, ")" )
)

cl.table <- ddply(mem.dat, c("Modality", "Warning"), summarise,
                  Mean = round(mean(CL), 3),
                  Std.Dev = round(sd(CL), 3),
                  Lower.CI = round(mean(CL) - (crit.val*sd(CL)/sqrt(20)), 3),
                  Upper.CI = round(mean(CL) + (crit.val*sd(CL)/sqrt(20)), 3),
                  CI = paste0("(", Lower.CI, ",", " ", Upper.CI, ")" )
)


### Correct Recognition
cr <- aov(Correct ~ Modality * Warning, data = mem.dat)
summary(cr)

### Critical Lures
cl <- aov(CL ~ Modality * Warning, data = mem.dat)
summary(cl)




## Tables

cr.table[, c("Modality", "Warning", "Mean", "Std.Dev", "CI")] %>% 
  gt() %>%
  tab_options(
    table.border.top.width = "NULL"  ) %>%
  # set table style
  tab_style(
    style = list(
      # remove horizontal lines
      cell_borders(
        sides = c("top", "bottom"),
        color = "white",
        weight = px(1)
      ),
      # remove row striping in Markdown documents
      cell_fill(color = "white", alpha = NULL)
    ),
    #do this for all columns and rows
    locations = cells_body(
      columns = everything(),
      rows = everything()    )) %>%
  cols_align(
    align = "left",
    columns = c(Modality, Warning)) %>%
  cols_align(
    align = "center",
    columns = c(Mean, Std.Dev, CI)) %>%
  tab_header(
    title = "Proportion of Correctly Recognized List Items by Condition") %>%
  cols_label(Modality = md("**Modality <br> Condition**"),
             Warning = md("**Warning <br> Condition**"),
             Mean = md("**Mean**"),
             Std.Dev = md("**Standard <br> Deviation**"),
             CI = md("**95% <br> Confidence Interval**"))


cl.table[, c("Modality", "Warning", "Mean", "Std.Dev", "CI")] %>% 
  gt() %>%
  tab_options(
    table.border.top.width = "NULL"  ) %>%
  # set table style
  tab_style(
    style = list(
      # remove horizontal lines
      cell_borders(
        sides = c("top", "bottom"),
        color = "white",
        weight = px(1)
      ),
      # remove row striping in Markdown documents
      cell_fill(color = "white", alpha = NULL)
    ),
    #do this for all columns and rows
    locations = cells_body(
      columns = everything(),
      rows = everything()    )) %>%
  cols_align(
    align = "left",
    columns = c(Modality, Warning)) %>%
  cols_align(
    align = "center",
    columns = c(Mean, Std.Dev, CI)) %>%
  tab_header(
    title = "Proportion of Falsely Recognized Critical Lures by Condition") %>%
  cols_label(Modality = md("**Modality <br> Condition**"),
             Warning = md("**Warning <br> Condition**"),
             Mean = md("**Mean**"),
             Std.Dev = md("**Standard <br> Deviation**"),
             CI = md("**95% <br> Confidence Interval**"))




## Figures

### Correct Recognition

colors <- c("No Warning" = "mediumturquoise", 
            "Warning" = "orange1")

cr.line <- cr.table %>%
  ggplot(aes(y = Mean, x = Modality, group = Warning, color = Warning)) +
  geom_line(lwd = 1.5)  +
  ylim(0, 1) +
  labs(title = "Modality & Warning \n on Correct Recognition",
       y = "Proportion of \n Correctly Recognized List Items",
       x = "Modality",
       color = "Warning") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(vjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = colors)



cr.bar <- cr.table %>%
  ggplot(aes(y = Mean, x = Modality, fill = Warning)) +
  geom_bar(stat = 'identity', position = 'dodge')  +
  ylim(0, 1) +
  labs(title = "Modality & Warning \n on Correct Recognition",
       y = "Proportion of \n Correctly Recognized List Items",
       x = "Modality",
       color = "Warning") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(vjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  scale_fill_manual(values = c("mediumturquoise", "orange1"))


grid.arrange(cr.line, cr.bar, ncol = 2)



### Critical Lures

colors <- c("No Warning" = "lightgreen" ,
            "Warning" = "maroon1")


cl.line <- cl.table %>%
  ggplot(aes(y = Mean, x = Modality, group = Warning, color = Warning)) +
  geom_line(lwd = 1.5)  +
  ylim(0, 1) +
  labs(title = "Modality & Warning \n on False Recognition",
       y = "Proportion of \n Falsely Recognized Critical Lures",
       x = "Modality",
       color = "Warning") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(vjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = colors)



cl.bar <- cl.table %>%
  ggplot(aes(y = Mean, x = Modality, fill = Warning)) +
  geom_bar(stat = 'identity', position = 'dodge')  +
  ylim(0, 1) +
  labs(title = "Modality & Warning \n on False Recognition",
       y = "Proportion of \n Falsely Recognized Critical Lures",
       x = "Modality",
       color = "Warning") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(vjust = 0.5),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  scale_fill_manual(values = c("lightgreen", "maroon1"))


grid.arrange(cl.line, cl.bar, ncol = 2)