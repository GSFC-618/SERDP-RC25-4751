# 1. Extract the NLCD codes
cover_codes <- lc.list[[1]]

# 2. Create an empty vector for the initial PFT names
pft_values <- rep(NA_character_, length(cover_codes))

# 3. Apply the Ecoregion 10 (Kirtland AFB) rules
pft_values[cover_codes == 41] <- "deciduous"
pft_values[cover_codes == 42] <- "conifer"
pft_values[cover_codes == 43] <- "mixed forest"
pft_values[cover_codes %in% c(21, 22, 23, 24)] <- "developed"
pft_values[cover_codes == 95] <- "mesic grassland"
pft_values[cover_codes == 90] <- "woody wetland"
pft_values[cover_codes %in% c(52, 71, 81, 82)] <- "arid grassland"

# 4. Apply the temporal renaming for HPDA models
pft_values[pft_values == 'mesic grassland'] <- 'semiarid.grassland_HPDA'
pft_values[pft_values == 'woody wetland'] <- 'semiarid.grassland_HPDA'
pft_values[pft_values == 'mixed forest'] <- 'temperate.deciduous.HPDA'
pft_values[pft_values == 'deciduous'] <- 'temperate.deciduous.HPDA'
pft_values[pft_values == 'conifer'] <- 'boreal.coniferous'
pft_values[pft_values == 'arid grassland'] <- 'semiarid.grassland_HPDA'

# 5. Bring in your Site IDs
# IMPORTANT: lc.list does not contain site IDs. You must replace `1:length(pft_values)` 
# with your actual vector of site IDs from your PEcAn settings or BETY database.
site_ids <- 1:length(pft_values) # <--- REPLACE THIS WITH YOUR ACTUAL SITE IDs

# 6. Create the final data frame PEcAn expects
out_csv <- data.frame(
  site = site_ids,
  pft = pft_values
)

# 5. Write out the CSV file to your settings output directory
write.csv(out_csv, file = "/shared/users-bucket/mhayden/obs_prep/SDA_disturbance/example_polygon/site_pft.csv", row.names = FALSE)

# Print the first few rows to verify
head(out_csv)