# ============================================================
# POT-KEN workflow
# Energy density estimation – Parr 6725 calorimeter
# ============================================================

library(readr)
library(dplyr)
library(here)

# ------------------------------------------------------------
# 1. Define path to input files
# ------------------------------------------------------------
# All input files must be stored in the "Determinations" folder
# located at the root of the repository

path_ <- here("Example")

# ------------------------------------------------------------
# 2. Read column names from RunDataTemplate.csv
# ------------------------------------------------------------
template_path <- file.path(path_, "RunDataTemplate.csv")
header <- read_csv(template_path, n_max = 1) |> names()

# ------------------------------------------------------------
# 3. List determination files (.det.finl.csv), excluding template and ashes files
# ------------------------------------------------------------
Determinations <- list.files(
  path_,
  pattern = "\\.csv",
  full.names = TRUE
)

Determinations <- Determinations[!grepl("RunDataTemplate|Ashes", Determinations)]

# ------------------------------------------------------------
# 4. Read and bind all determinations
# ------------------------------------------------------------
Determinations_ <- Determinations %>%
  lapply(function(path) {
    read_csv(path, col_names = FALSE) %>%
      setNames(header) %>%
      mutate(archivo = basename(path))
  }) %>%
  bind_rows()

# ------------------------------------------------------------
# 5. Remove non-essential variables
# ------------------------------------------------------------
Determinations_ <- Determinations_ %>%
  select(
    -c(
      "Sulfur", "SulfurFinal",
      "Hydrogen", "HydrogenFinal",
      "MAD", "MADFinal",
      "UnitMultIfOther",
      "NetHOC", "<blank>", "DryNetHOC",
      "Oxygen", "OxygenFinal",
      "Nitrogen", "NitrogenFinal",
      "MAR", "MARFinal",
      "ARNetHOC",
      "FuseFinal", "AcidFinal",
      "BombID"
    )
  )

# ------------------------------------------------------------
# 6. Recode binary and numeric variables
# ------------------------------------------------------------
Determinations_ <- Determinations_ %>%
  mutate(
    Mode = case_when(
      Mode == 1 ~ "Determination",
      Mode == 2 ~ "Standarization",
      TRUE ~ as.character(Mode)
    ),
    Method = case_when(
      Method == 1 ~ "Combustion",
      TRUE ~ as.character(Method)
    ),
    State = case_when(
      State == 0 ~ "Prepesado",
      State == 1 ~ "Preliminar",
      State == 2 ~ "Final",
      TRUE ~ as.character(State)
    ),
    Units = case_when(
      Units == 1 ~ "Btu/lb",
      Units == 2 ~ "cal/g",
      Units == 3 ~ "J/g",
      TRUE ~ as.character(Units)
    )
  )

# ------------------------------------------------------------
# 7. Read ashes data and join
# ------------------------------------------------------------
Ashes <- read_delim(
  file.path(path_, "Ashes.csv"),
  delim = ";"
)

Determinations_ <- Determinations_ %>%
  left_join(
    Ashes %>% select(SampleID, Ashes, Spike_Ashes),
    by = "SampleID"
  )

# ------------------------------------------------------------
# 8. Calculate energy density (DM and AFDM)
# ------------------------------------------------------------
Determinations__AshFree <- Determinations_ %>%
  mutate(
    DE_kJ_g_DM = as.numeric(HOC) * 0.004186,
    HOC_corr = case_when(
      SpikeWt == 0 ~
        ((BombEE * DeltaT) -
           (23 - (Fuse * 23 / 0.0145)) -
           Acid) /
        (SampleWt - Ashes),
      
      SpikeWt != 0 ~
        (
          (
            (((BombEE * DeltaT) -
                (23 - (Fuse * 23 / 0.0145)) -
                Acid) / SampleWt) *
              (SampleWt - Ashes)
          ) -
            (3186.128 * (SpikeWt - Spike_Ashes))
        ) /
        (SampleWt - SpikeWt - Ashes)
    ),
    DE_kJ_g_AFDM = as.numeric(HOC_corr) * 0.004186
  )

# ------------------------------------------------------------
# 9. Reorder columns for final output
# ------------------------------------------------------------
Determinations__AshFree <- Determinations__AshFree %>%
  select(
    SampleID, archivo, Timestamp,
    BombEE, BombName,
    Mode, Method, State, Units,
    SpikeWt, SampleWt,
    JacketTemp, InitTemp, DeltaT,
    HOC, Fuse, Acid,
    Ashes, Spike_Ashes,
    HOC_corr, DE_kJ_g_DM, DE_kJ_g_AFDM,
    everything()
  )

# ------------------------------------------------------------
# 10. Inspect final dataset
# ------------------------------------------------------------
View(Determinations__AshFree)