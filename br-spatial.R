## -----------------------------------------------------------------------------
#| code-fold: false
# Try this out for yourself!

N <- 100
x <- runif(n = N, min = 0, max = 1)
head(x)  # Show the first 6 elements
mean(x)  # Calculate the mean of this vector
run_x <- rep(NA, length(x))
for (i in seq_along(x)) {
  run_x[i] <- sum(x[1:i])
}
head(run_x)


## -----------------------------------------------------------------------------
#| code-fold: false
#| eval: false
## install.packages("tidyverse")


## -----------------------------------------------------------------------------
#| label: setup
library(kernlab)       # for GPR
library(ggrepel)       # nice labels in plots
library(osrm)          # OSM route calculator
library(spdep)         # Moran's test and more
library(CARBayes)      # spatial CAR models
library(mapview)       # interactive plots
library(scales)        # pretty formats
library(tidyverse)
theme_set(theme_bw())  # set the theme


## -----------------------------------------------------------------------------
#| code-fold: false
#| eval: false
## remotes::install_github("propertypricebn/bruneimap")


## -----------------------------------------------------------------------------
#| code-fold: false
library(bruneimap)


## -----------------------------------------------------------------------------
#| label: fig-hsp-spatial
#| layout-ncol: 2
#| fig-cap: Spatial distribution of house prices in Brunei
#| fig-subcap:
#|   - Original distribution
#|   - Random shuffle
load("data/artificial_hsp.RData")

# forest_col <- adjustcolor("forestgreen", alpha = 0.9)
forest_col <- "grey"

plot_hsp <- function(x) {
  left_join(kpg_sf, x, by = join_by(id, kampong, mukim)) |>
    ggplot() +
    geom_sf(aes(fill = price, col = "")) +
    scale_fill_viridis_c(
      labels = scales::dollar, 
      name = "Predicted price", 
      alpha = 0.9, 
      na.value = forest_col
    ) +
    scale_colour_manual(values = "grey30") +              
    guides(colour = guide_legend(
      "No property\nsales", 
      override.aes = list(fill = forest_col, colour = forest_col)
    ))
}

p1 <- plot_hsp(hsp_bn)
p2 <- 
  hsp_bn |> 
  mutate(price = sample(price)) |> 
  plot_hsp()

p1
p2


## -----------------------------------------------------------------------------
#| label: moran

# Prepare neighbourhood strucuture
hsp_sf <- left_join(kpg_sf, hsp_bn, by = join_by(id, kampong, mukim))
nb <- poly2nb(hsp_sf, queen = FALSE)
Wlist <- nb2listw(nb, zero.policy = TRUE, style = "B")

# Moran's test
moran.test(hsp_sf$price, listw = Wlist, zero.policy = TRUE, na.action = na.omit)


## -----------------------------------------------------------------------------
left_join(
  kpg_sf, 
  bn_census2021, 
  by = join_by(id, kampong, mukim, district)
) |>
  select(
    kampong, population, geometry
  ) |>
  slice_sample(n = 10)


## -----------------------------------------------------------------------------
## Load the data sets
soil_gps <- read_csv(
  "data/8389823/GPS - Revised.csv", 
  # IMPORTANT!!! The csv file has latin1 encoding as opposed to UTF-8
  locale = readr::locale(encoding = "latin1")
)
  
soil_physico <- read_csv("data/8389823/Soil physicochemical properties.csv")
soil_texture <- read_csv("data/8389823/Soil texture classification.csv")


## -----------------------------------------------------------------------------
#| code-fold: false
glimpse(soil_gps)


## -----------------------------------------------------------------------------
#| code-fold: false
x <- soil_gps$Latitude[1]
x

# convert it using sp::char2dms() function
x <- sp::char2dms(x, chd = "°")
x
str(x)


## -----------------------------------------------------------------------------
#| code-fold: false
as.numeric(x)


## -----------------------------------------------------------------------------
soil_gps <-
  soil_gps |>
  mutate(
    Latitude = as.numeric(sp::char2dms(Latitude, chd = "°")),
    Longitude = as.numeric(sp::char2dms(Longitude, chd = "°"))
  )
soil_gps


## -----------------------------------------------------------------------------
ggplot(brn_sf) +
  geom_sf() +
  geom_point(data = soil_gps, aes(Longitude, Latitude)) 


## -----------------------------------------------------------------------------
ggplot(mkm_sf) +
  geom_sf() +
  geom_sf(data = dis_sf, fill = NA, col = "black", linewidth = 1) +
  geom_point(data = soil_gps, aes(Longitude, Latitude)) +
  geom_text_repel(
    data = soil_gps,
    aes(Longitude, Latitude, label = Plot_name),
    box.padding = 0.5,
    max.overlaps = 30
  ) +
  coord_sf(
    xlim = c(114.4, 114.6),
    ylim = c(4.5, 4.7)
  )


## -----------------------------------------------------------------------------
#| code-fold: false
glimpse(soil_physico)
glimpse(soil_texture)


## -----------------------------------------------------------------------------
# Actually I just want to merge these two together
soil_df <- left_join(
  soil_physico,
  soil_texture,
  by = join_by(Habitat_type, Plot_name, Subplot_name, Soil_depth)
)
soil_df


## -----------------------------------------------------------------------------
soil_df <- left_join(
  soil_df, 
  soil_gps,
  by = join_by(Habitat_type, Plot_name)
)


## -----------------------------------------------------------------------------
ggplot(kpg_sf) +
  geom_sf(fill = NA) +
  geom_jitter(
    data = soil_df, 
    aes(Longitude, Latitude, col = Nitrogen, size = Nitrogen, 
        shape = Habitat_type),
    width = 0.001, height = 0.001, alpha = 0.7
  ) +
  coord_sf(
    xlim = c(114.46, 114.54),
    ylim = c(4.58, 4.64)
  ) +
  scale_color_viridis_c() +
  guides(size = "none")


## -----------------------------------------------------------------------------
x <- seq(-4, 4, length = 100)
y <- exp(-x^2)
tibble(d = x, Kxx = y) |>
  ggplot(aes(d, Kxx)) +
  geom_line() 


## -----------------------------------------------------------------------------
# Build a model to predict Nitrogen from all numeric variables. This is
# definitely not theory based, so just want to show the code.
soil_df <-
  soil_df |>
  select(where(is.numeric)) 
mod <- gausspr(Nitrogen ~ ., data = soil_df)
mod


## -----------------------------------------------------------------------------
xr <- c(114.4, 114.6)
xx <- seq(xr[1] - 0.01, xr[2] + 0.01, length = 100)

yr <- c(4.5, 4.7)
yy <- seq(yr[1] - 0.01, yr[2] + 0.01, length = 100)

mean_X <- 
  soil_df |>
  summarise(across(everything(), mean)) |>
  select(-Longitude, -Latitude)

pred_df <-
  expand_grid(
    Longitude = xx,
    Latitude = yy
  ) |>
  bind_cols(mean_X)

pred_df$ypred <- predict(mod, newdata = pred_df)

# Additional step: filter points that are outside of the Brunei land area.
pred_sf <- 
  pred_df |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>
  st_filter(y = brn_sf[1, ])

ggplot() +
  geom_raster(
    data = pred_sf,
    aes(fill = ypred, geometry = geometry),
    stat = "sf_coordinates",
    alpha = 0.8
  ) +
  # geom_raster(data = pred_df, aes(Longitude, Latitude, fill = ypred),
  #             alpha = 0.8) +
  geom_sf(data = kpg_sf, fill = NA, inherit.aes = FALSE, col = "black") +
  geom_sf(data = dis_sf, fill = NA, col = "black", linewidth = 1) +
  geom_point(data = soil_gps, aes(Longitude, Latitude, 
                                  shape = Habitat_type)) +
  geom_text_repel(
    data = soil_gps,
    aes(Longitude, Latitude, label = Plot_name),
    box.padding = 0.5,
    max.overlaps = 30
  ) +
  scale_fill_viridis_c() +
  scale_colour_viridis_c() +
  coord_sf(xlim = xr, ylim = yr)


## -----------------------------------------------------------------------------
brd <- 
  read_sf("data/hotosm_brn_roads_lines_geojson/hotosm_brn_roads_lines_geojson.geojson") |>
  sf::st_transform(4326)  # SET THE CRS!!! (WGS84)
glimpse(brd)


## -----------------------------------------------------------------------------
table(brd$highway)


## -----------------------------------------------------------------------------
brd_mjr <- 
  brd |>
  filter(highway %in% c("motorway", "trunk", "primary", "secondary")) 
brd_mjr


## -----------------------------------------------------------------------------
ggplot() +
  geom_sf(data = brn_sf) +
  geom_sf(data = brd_mjr, aes(col = highway), size = 0.5) +
  # scale_colour_viridis_d(option = "turbo")
  ggsci::scale_colour_npg()


## -----------------------------------------------------------------------------
brd_belait <- 
  brd |>
  st_intersection(filter(dis_sf, name == "Belait"))

ggplot(brd_belait) +
  geom_sf() +
  geom_sf(data = filter(dis_sf, name == "Belait"), fill = NA)


## -----------------------------------------------------------------------------
get_random_point <- function(linestring) {
  coords <- st_coordinates(linestring)
  samp_coord <- coords[sample(nrow(coords), 1), , drop = FALSE]
  samp_coord[, 1:3]
}
get_random_point(brd_belait$geometry[1])


## -----------------------------------------------------------------------------
random_points <-
  map(brd_belait$geometry, get_random_point) |>
  bind_rows() |>
  slice_sample(n = 100)


## -----------------------------------------------------------------------------
suriseri <- c(114.198778, 4.583444)

res <- list()
for (i in 1:100) {
  res[[i]] <- osrmRoute(src = random_points[i, 1:2], dst = suriseri, overview = "full")
}
res <- 
  bind_rows(res) |>
  as_tibble() |>
  st_as_sf()
res


## -----------------------------------------------------------------------------
ggplot(res) +
  # geom_point(data = random_points, aes(x = X, y = Y), col = "red") +
  geom_sf(data = filter(kpg_sf, district == "Belait"), fill = NA) +
  geom_sf(aes(col = duration), linewidth = 1.2, alpha = 0.7) +
  geom_point(x = suriseri[1], y = suriseri[2], col = "red3", pch = "X", 
             size = 3) +
  scale_colour_viridis_c() 


## -----------------------------------------------------------------------------
glimpse(bn_census2021)


## -----------------------------------------------------------------------------
bn_pop_sf <- 
  left_join(
    kpg_sf, 
    bn_census2021, 
    by = join_by(id, kampong, mukim, district)
  )


## -----------------------------------------------------------------------------
ggplot(bn_pop_sf) +
  geom_sf(aes(fill = population)) +
  scale_fill_viridis_c(na.value = NA)


## -----------------------------------------------------------------------------
ggplot(bn_pop_sf) +
  geom_histogram(aes(population), binwidth = 100)


## -----------------------------------------------------------------------------
kpg_labels_sf <-
  bn_pop_sf |>
  arrange(desc(population)) |>
  slice_head(n = 10)

bn_pop_sf |>
  # filter(population > 50) |>
  ggplot() +
  geom_sf(aes(fill = population), col = NA, alpha = 0.8) +
  geom_sf(data = kpg_sf, fill = NA, col = "black") +
  ggrepel::geom_label_repel(
    data = kpg_labels_sf,
    aes(label = kampong, geometry = geometry),
    stat = "sf_coordinates",
    inherit.aes = FALSE,
    box.padding = 1,
    size = 2,
    max.overlaps = Inf
  ) +
  scale_fill_viridis_b(
    name = "Population",
    na.value = NA,
    labels = scales::comma,
    breaks = c(0, 100, 1000, 10000, 20000)
    # limits = c(0, 12000)
  ) +
  theme_bw()


## -----------------------------------------------------------------------------
glimpse(hsp_bn)


## -----------------------------------------------------------------------------
dat_sp <-
  hsp_bn |>
  group_by(mukim) |>
  summarise(mukim = first(mukim)) |>
  left_join(mkm_sf) |>
  st_as_sf() |>
  as("Spatial")
nb_mkm <- poly2nb(dat_sp, row.names = dat_sp$mukim, queen = FALSE)

# Plot
nb_sf <- nb2lines(nb_mkm, coords = sp::coordinates(dat_sp), as_sf = TRUE)
nb_sf <- st_set_crs(nb_sf, st_crs(dat_sp))
ggplot() +
  geom_sf(data = mkm_sf) +
  geom_sf(data = nb_sf, col = "red3")



## -----------------------------------------------------------------------------
i <- which(attr(nb_mkm, "region.id") == "Mukim Kota Batu")
j <- which(attr(nb_mkm, "region.id") == "Mukim Bangar")
nb_mkm[[i]] <- c(nb_mkm[[i]], j)
nb_mkm[[j]] <- c(nb_mkm[[j]], i)
nb_mkm


## -----------------------------------------------------------------------------
#| output: false
mod1 <- CARBayes::S.CARmultilevel(
  formula = I(price/1000) ~ I(built_up/1000) + beds + baths + land_size,
  data = hsp_bn,
  family = "gaussian",
  burnin = 1000,
  n.sample = 2000,
  ind.area = match(hsp_bn$mukim, dat_sp$mukim),
  W = nb2mat(nb_mkm, style = "B")
)


## -----------------------------------------------------------------------------
mod1


## -----------------------------------------------------------------------------
phi <- apply(mod1$samples$phi, 2, mean)
names(phi) <- dat_sp$mukim
head(phi)


## -----------------------------------------------------------------------------
hsp_mapview <-
  left_join(mkm_sf, tibble(
    mukim = names(phi),
    phi = round(phi * 1000, -3)
  ))
mapview(
  hsp_mapview,
  zcol = "phi",
  layer.name = "Spatial effects (BND)",
  label = "mukim",
  alpha.regions = 0.9
)

