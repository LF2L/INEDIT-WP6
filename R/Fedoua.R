# Data de Fedoua
## Date: 07/12/2022

## Load Libraries -----
library(tidyverse)
library(here)
library(ggpubr)
library(usethis)
library(readxl)


# Loading the data
Fedoua <- read_excel(path = "data/Resultats-fedoua.xls",
                   range = "A6:EX26")
## Arranging the factors 
Fedoua <-
   Fedoua %>% 
   mutate(Effective = factor(Effective, 
                             levels = c("0 – 10 salariés", "11 à 49 salariés", "50 à 199 salariés", "200 à 999 salariés" ),
                             labels = c("0 - 10", "11 - 49", "50 - 199", "200 - 999" )))

# ECOSYSTEM  MOBILISED -----
## Graphique 1: 

data <- 
   Fedoua %>% 
   group_by(Effective, Type) %>% 
   summarise(Total = n())


data %>% 
   ggplot() +
   aes(x = Effective, y = Total , fill = Type) +
   geom_bar(stat = 'identity') +
   #coord_flip() +
   scale_fill_brewer(palette="Set2") +
   theme_minimal(base_size = 18, base_family = "Palatino") +
   labs(title = "Stakeholders interviewed for the local ecosystem",
        subtitle = "3D Printing Recycled Plastic Demonstrator",
        fill='Type of Stakeholder',
        x='Size of collaborators/employees',
        y = "Total of stakeholders enquired",
        caption = paste("Data Source update by UL on",
                        format(Sys.Date(), "%b/%y"),
                        sep = ":  "
        )) +
   theme(legend.position = "bottom")  

ggsave(filename = "figures/fedoua/Ecosystem-01.pdf",  width = 9, height = 9, dpi = "print")


# Acceptability -----
## Graphique 1: 


## Graphique Acceptability: 
data <- 
   Fedoua %>% 
   group_by(Comportement1, Type) %>% 
   summarise(Total = n()) %>% 
   mutate(Comportement1 = factor(Comportement1, 
                          levels = c("Faible", "Moyen" ,  "Fort"),
                          labels = c( "Low", "Medium", "High")))

data %>% 
   ggplot() +
   aes(x = Comportement1, y = Total, fill = Type) +
   geom_bar(stat = 'identity') +
   coord_flip() +
   scale_fill_brewer(palette="Set2") +
   theme_minimal(base_size = 18, base_family = "Palatino") +
   labs(title = "How would you rate the level of involvement ",
        subtitle = "in plastic sorting and collection?",
        fill='Type of Stakeholder',
        x='',
        y = "",
        caption = paste("Data Source update by UL on",
                        format(Sys.Date(), "%b/%y"),
                        sep = ":  "
        )) +
   theme(legend.position = "bottom")  

ggsave(filename = "figures/fedoua/Acceptability-01.pdf",  width = 9, height = 5, dpi = "print")


## Acceptability 2

data <- 
   Fedoua %>% 
   group_by(SC2, Type) %>% 
   summarise(Total = n()) %>% 
   mutate(SC2 = factor(SC2, 
                                 levels = c("Oui", "Non" ),
                                 labels = c( "Yes", "No")))


data %>% 
   ggplot() +
   aes(x = SC2, y = Total, fill = Type) +
   geom_bar(stat = 'identity') +
   coord_flip() +
   scale_fill_brewer(palette="Set2") +
   theme_minimal(base_size = 18, base_family = "Palatino") +
   labs(title = "Would you be interested in using a ",
        subtitle = "smart collector to help manage in your plastic waste?",
        fill='Type of Stakeholder',
        x='',
        y = "",
        caption = paste("Data Source update by UL on",
                        format(Sys.Date(), "%b/%y"),
                        sep = ":  "
        )) +
   theme(legend.position = "bottom")  

ggsave(filename = "figures/fedoua/Acceptability-02.pdf",  width = 9, height = 5, dpi = "print")



data %>% 
   ggplot()+
   aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Effective)) +
   




# Compute percentages
data <- 
   data %>% 
   mutate(fraction = Total / sum(Total))

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
#data$label <- paste0(data$Effective, ":\n ", paste(round(data$fraction*100,2), '%'))
data$label <- paste(round(data$fraction*100,1), '%')



# Make the plot 1
stakeholder <- 
   ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Effective)) +
   geom_rect() +
   geom_text(x=2, aes(y=labelPosition, label=label, color=Effective), size=6) + # x here controls label position (inner / outer)
   scale_fill_brewer(palette="Set2") +
   scale_color_brewer(palette="Set2") +
   coord_polar(theta="y") +
   xlim(c(-1, 4)) +
   theme_void(base_size = 15, base_family = "Palatino") +
   labs(title = "Stakeholders interviewed for the Recovery of Plastic of wastes",
        subtitle = "3D Printing Recycled Plastic Demonstrator",
        fill='Employees range',
        color='Employees range',
        #x = "Months",
        #y = "Plastic collected [kilograms]",
        caption = paste("Data Source update by UL on",
                        format(Sys.Date(), "%b/%y"),
                        sep = ":  "
        )
   ) +
   #theme(legend.position = "none")  
   annotate("text", x = 0, y = 0, label = paste("Stakeholders\nInterviewed: ", sum(data$Total), sep = '\n'), size=9)


ggsave(filename = "figures/fedoua/Stakeholder.jpg", plot = stakeholder, width = 9, height = 9, dpi = "print")
