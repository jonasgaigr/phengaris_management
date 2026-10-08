#----------------------------------------------------------#
#
#
#       Phengaris spp. management in Czechia
#
#      Step 03 - Identify sites inside protected areas
#
#
#----------------------------------------------------------#
#
# Flags every monitoring site by the protection it falls under:
#
#   EVL_any     inside any Natura 2000 site of Community importance
#   EVL_target  inside a Natura 2000 site designated for a Phengaris species;
#               PA_SPECIES says which one
#   MZCHU       inside a small-scale specially protected area
#
# Reads:  Data/Processed/common/records.csv, sites.gpkg   (step 01)
#         Data/Input/reference/evl, mzchu, n2k_site_subjects
# Writes: Data/Processed/common/protected_area_id.csv
#
#----------------------------------------------------------#

if (!exists("PIPELINE")) source("R/00_setup.R", encoding = "UTF-8")
step_begin("03")

phengaris_lokal_new <- read_records_with_sites()

evl <-
  read_reference_layer("evl") %>%
  sf::st_transform(CRS_SJTSK)

mzchu <-
  read_reference_layer("mzchu") %>%
  sf::st_transform(CRS_SJTSK)

# Which Natura 2000 site was designated for which feature.
sites_subjects <- read_n2k_site_subjects()

report_start(
  "03",
  "Protected area membership",
  paste(
    "Monitoring sites intersected with Natura 2000 sites and small-scale",
    "specially protected areas. A site counts as EVL_target when the Natura",
    "2000 site it falls in was designated for that particular species."
  )
)

#----------------------------------------------------------#
# Protected area membership -----
#----------------------------------------------------------#

protected_area_id <- protected_area_membership(
  phengaris_lokal_new,
  evl,
  mzchu,
  sites_subjects
)

#----------------------------------------------------------#
# Results -----
#----------------------------------------------------------#

protection_summary <- summarise_protection_membership(protected_area_id)

report_table(
  protection_summary,
  "Records and distinct sites by protection category",
  "03_protection_summary"
)

#----------------------------------------------------------#
# Export -----
#----------------------------------------------------------#

readr::write_csv(
  protected_area_id,
  processed_path("protected_area_id.csv", "common")
)

report_finish()

message("  ", nrow(protected_area_id), " protected-area assignments")
step_end("03")

#----------------------------------------------------------#
# End script -----
#----------------------------------------------------------#
