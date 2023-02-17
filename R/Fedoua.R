# Data de Fedoua
## Date: 07/12/2022

## Load Libraries -----
library(tidyverse)
library(here)
library(ggpubr)
library(usethis)
library(readxl)


# Loading the data
data <- read_excel(path = "data/Resultats-fedoua.xls",
                   range = "A5:EW26")