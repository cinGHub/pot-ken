library(readr)
library(dplyr)
library(here)

#Path to input files
# Ruta a los archivos de entrada
# The code assumes that all files are stored in the "Example" folder at the root of the repository
# El código asume que todos los archivos están en la carpeta "Determinations" en la raíz del repositorio

path_ <- here("Determinations") 

#title templates/plantilla de titulos

template_path <- file.path(path_, "RunDataTemplate.csv") 
header <- read_csv(template_path, n_max = 1) |> names() 

# List .csv files, only determinations
# Listar los archivos .csv SOLO de las determinaciones

Determinations <- list.files(path_, pattern = "\\.csv", full.names = TRUE)
Determinations <- Determinations[!grepl("RunDataTemplate|Ashes", Determinations)]

Determinations_sh <- function(path) {read_csv(path, col_names = FALSE, skip = 0) %>% 
    setNames(header) %>% 
    mutate(archivo = basename(path))}

# Delete unnecessary columns and redefne binary values from the equipment output
# Borra columnas innecesarias y reasigna valores binarios de la salida del equipo

Determinations_ <- Determinations %>%  
  lapply(Determinations_sh) %>%  
  bind_rows() %>% 
  select(-(c("Sulfur", "SulfurFinal","Hydrogen", "HydrogenFinal","MAD", "MADFinal","UnitMultIfOther",
             "NetHOC", "<blank>","DryNetHOC","Oxygen", "OxygenFinal","Nitrogen", 
             "NitrogenFinal","MAR", "MARFinal","ARNetHOC", "FuseFinal", "AcidFinal", "BombID"))) %>%  
  mutate(Mode = case_when(Mode == 1 ~ "Determination",
                          Mode == 2 ~ "Standarization",
                          TRUE ~ as.character(Mode)),
         Method = case_when(Method == 1 ~ "Combustion",
                            TRUE ~ as.character(Method)),
         State = case_when(State == 0 ~ "Prepesado",
                           State == 1 ~ "Preliminar",
                           State == 2 ~ "Final",
                           TRUE ~ as.character(State)),
         Units = case_when(Units == 1 ~ "Btu/lb",
                           Units == 2 ~ "cal/g", 
                           Units == 3 ~ "J/g",TRUE ~ as.character(Units))) 


#Step to correct by ashes (kJ/g AFDM)
#Para corregir el valor de DE por cenizas y obtener kJ/g AFDM 

Ashes <- read_delim(file.path(path_, "Ashes.csv"), delim = ";")
Determinations_ <- Determinations_%>%  
  left_join(Ashes %>% 
              select(SampleID, Ashes, Spike_Ashes), by = "SampleID")


Determinations__AshFree <- Determinations_ %>%
  mutate(DE_kJ_g_DM = as.numeric(HOC) * 0.004186) %>%
  mutate(HOC_corr = case_when(
    SpikeWt == 0 ~ ((BombEE * DeltaT) - (23 - (Fuse * 23 / 0.0145)) - Acid) / (SampleWt - Ashes),
    SpikeWt != 0 ~ ((((((BombEE * DeltaT) - (23 - (Fuse * 23 / 0.0145)) - Acid) / (SampleWt))*(SampleWt-Ashes))-(3186.128*(SpikeWt-Spike_Ashes)))/(SampleWt - SpikeWt-Ashes)))) %>%
  mutate(DE_kJ_g_AFDM = as.numeric(HOC_corr) * 0.004186)

Determinations__AshFree <-Determinations__AshFree %>% 
  select(SampleID, archivo, Timestamp, BombEE, BombName, Mode, Method, State, Units, 
         SpikeWt, SampleWt, JacketTemp, InitTemp, DeltaT, HOC, Fuse, Acid, Ashes, Spike_Ashes, HOC_corr, DE_kJ_g_DM, DE_kJ_g_AFDM, everything())
View(Determinations__AshFree)