#----------------------------------------------------------#
# Load data -----
#----------------------------------------------------------#

#----------------------------------------------------------#
# Plot maps -----
#----------------------------------------------------------#
#----------------------------------------------------------#
## Mapping fields -----
#----------------------------------------------------------#
hypso_read <- RCzechia::vyskopis(format = "rayshaded", cropped = FALSE)
hypso_df <- as.data.frame(hypso_read, xy = TRUE)
colnames(hypso_df) <- c("x", "y", "elevation")

data_map <- ggplot2::ggplot(
  data = phengaris_lokal_new %>%
    dplyr::filter(ZDROJ %in% target_mon_zdroj) %>%
    dplyr::mutate(geometry = sf::st_centroid(geometry))
) +
  ggplot2::geom_raster(
    data = hypso_df,
    aes(x = x, y = y, fill = elevation)
  ) +
  ggplot2::geom_sf(
    aes(color = as.factor(NEGATIV)),
    size = 0.5,
    show.legend = FALSE
  ) +
  ggplot2::geom_sf(
    data = czechia_border,
    fill = NA
  ) +
  ggplot2::scale_y_continuous(expand = ggplot2::expand_scale(mult = c(0.05, 0.05))) +
  ggplot2::scale_x_continuous(expand = ggplot2::expand_scale(mult = c(0.05, 0.05))) +
  ggplot2::theme_void()

data_map

phenau_dist <- data %>%
  filter(DRUH == "Phengaris nausithous" & POSITIVE == 1) %>%
  pull(SITMAP) %>%
  unique()
phetel_dist <- data %>%
  filter(DRUH == "Phengaris teleius" & POSITIVE == 1) %>%
  pull(SITMAP) %>%
  unique()
sample_dist <- data %>%
  pull(SITMAP) %>%
  unique()

data_dist_map <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = czechia, fill = NA, size = 1.5) +
  ggplot2::geom_sf(data = sitmap %>%
                     dplyr::filter(POLE %in% sample_dist),
                   fill = "light grey") +
  ggplot2::geom_sf(data = sitmap %>%
                     dplyr::filter(POLE %in% phenau_dist),
                   fill = "blue",
                   alpha = .5) +
  ggplot2::geom_sf(data = sitmap %>%
                     dplyr::filter(POLE %in% phetel_dist),
                   fill = "red",
                   alpha = .5) +
  ggplot2::scale_y_continuous(expand = expand_scale(mult = c(0.05, 0.05))) +
  ggplot2::scale_x_continuous(expand = expand_scale(mult = c(0.05, 0.05))) +
  ggplot2::theme_void()
data_dist_map

phetel_dist

both_dist

#----------------------------------------------------------#
## Occurrences -----
#----------------------------------------------------------#
