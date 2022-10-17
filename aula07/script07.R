# Carregando os pacotes necessáros
library(tidyverse)
library(agricolae)
library(readxl)

# DBA - Blocos aumentados Simples -----------------------------------------
dba_simles <- read_xlsx("data/aula7-DBA-simples.xlsx")

# Blocos aumentados contraste ---------------------------------------------
dba_contraste <- read_xlsx("data/aula7-DBA-contraste.xlsx")

# Blocos incompletos ------------------------------------------------------
dbib <- read_xlsx("data/aula7-BIB.xlsx")

# LATTICE -----------------------------------------------------------------
d_lattice <- read_xlsx("data/aula7-lattice.xlsx")

# LATTICE RESUMO -----------------------------------------------------------------
d_resumo <- read_xlsx("data/aula7-lattice-resumo.xlsx")
