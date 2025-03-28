## ----setup, include = FALSE---------------------------------------------------
# # jss style
# knitr::opts_chunk$set(prompt=TRUE, echo = TRUE, highlight = FALSE, continue = " + ", comment = "")
# options(replace.assign=TRUE, width=90, prompt="R> ")


# rmd style
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.pos = "H"
)

## ----Load_SSNbler-------------------------------------------------------------
library(SSNbler)

## ----Copy_Local---------------------------------------------------------------
copy_streams_to_temp()
path <- paste0(tempdir(), "/streamsdata")

## ----Import_Data, results = "hide"--------------------------------------------
library(sf)
MF_streams <- st_read(paste0(path, "/MF_streams.gpkg"))
MF_obs <- st_read(paste0(path, "/MF_obs.gpkg"))
MF_pred1km <- st_read(paste0(path, "/MF_pred1km.gpkg"))
MF_CapeHorn <- st_read(paste0(path, "/MF_CapeHorn.gpkg"))

## ----Plot_MF, fig.alt = "The Middle Fork Stream Network.", fig.align = "center", out.width = "75%"----
library(ggplot2)
ggplot() +
  geom_sf(data = MF_streams) +
  geom_sf(data = MF_CapeHorn, color = "gold", size = 1.7) +
  geom_sf(data = MF_pred1km, colour = "purple", size = 1.7) +
  geom_sf(data = MF_obs, color = "blue", size = 2) +
  coord_sf(datum = st_crs(MF_streams))

## ----valid_nodes_fig, fig.cap = "A landscape network (LSN). Nodes are denoted by blue circles, with the node category labelled. Edges are denoted by black arrows, with the arrow indicating flow direction (i.e., digitized direction).", fig.alt = "Valid Nodes", fig.align = "center",echo =FALSE, out.width = "50%"----
knitr::include_graphics("valid_nodes.png")

## ----lines_to_lsn-------------------------------------------------------------
## Set path for new folder for lsn
lsn.path <- paste0(tempdir(), "/mf04")

edges <- lines_to_lsn(
  streams = MF_streams,
  lsn_path = lsn.path,
  check_topology = TRUE,
  snap_tolerance = 0.05,
  topo_tolerance = 20,
  overwrite = TRUE
)

## ----sites_to_lsn_obs---------------------------------------------------------
obs <- sites_to_lsn(
  sites = MF_obs,
  edges = edges,
  lsn_path = lsn.path,
  file_name = "obs",
  snap_tolerance = 100,
  save_local = TRUE,
  overwrite = TRUE
)

## ----sites_to_lsn_preds-------------------------------------------------------
preds <- sites_to_lsn(
  sites = MF_pred1km,
  edges = edges,
  save_local = TRUE,
  lsn_path = lsn.path,
  file_name = "pred1km.gpkg",
  snap_tolerance = 100,
  overwrite = TRUE
)

capehorn <- sites_to_lsn(
  sites = MF_CapeHorn,
  edges = edges,
  save_local = TRUE,
  lsn_path = lsn.path,
  file_name = "CapeHorn.gpkg",
  snap_tolerance = 100,
  overwrite = TRUE
)

## ----updist_edges-------------------------------------------------------------
edges <- updist_edges(
  edges = edges,
  save_local = TRUE,
  lsn_path = lsn.path,
  calc_length = TRUE
)

names(edges) ## View edges column names

## ----updist_sites-------------------------------------------------------------
site.list <- updist_sites(
  sites = list(
    obs = obs,
    pred1km = preds,
    CapeHorn = capehorn
  ),
  edges = edges,
  length_col = "Length",
  save_local = TRUE,
  lsn_path = lsn.path
)

names(site.list) ## View output site.list names
names(site.list$obs) ## View column names in obs

## ----plot_updist, fig.alt = "Upstream Distance.", fig.align = "center", out.width = "75%"----
ggplot() +
  geom_sf(data = edges, aes(color = upDist)) +
  geom_sf(data = site.list$obs, aes(color = upDist)) +
  coord_sf(datum = st_crs(MF_streams)) +
  scale_color_viridis_c()

## ----summarise_h2oAreaKm2-----------------------------------------------------
summary(edges$h2oAreaKm2) ## Summarize and check for zeros

edges <- afv_edges(
  edges = edges,
  infl_col = "h2oAreaKm2",
  segpi_col = "areaPI",
  afv_col = "afvArea",
  lsn_path = lsn.path
)

names(edges) ## Look at edges column names
summary(edges$afvArea) ## Summarize the AFV column

## ----afv_sites----------------------------------------------------------------
site.list <- afv_sites(
  sites = site.list,
  edges = edges,
  afv_col = "afvArea",
  save_local = TRUE,
  lsn_path = lsn.path
)

names(site.list$pred1km) ## View column names in pred1km
summary(site.list$pred1km$afvArea) ## Summarize AFVs in pred1km and look for zeros

## ----ssn_assemble-------------------------------------------------------------
mf04_ssn <- ssn_assemble(
  edges = edges,
  lsn_path = lsn.path,
  obs_sites = site.list$obs,
  preds_list = site.list[c("pred1km", "CapeHorn")],
  ssn_path = paste0(path, "/MiddleFork04.ssn"),
  import = TRUE,
  check = TRUE,
  afv_col = "afvArea",
  overwrite = TRUE
)

class(mf04_ssn) ## Get class
names(mf04_ssn) ## print names of SSN object
names(mf04_ssn$preds) ## print names of prediction datasets

## ----plot_SSN, fig.cap = "Mean summer stream temperature (Temperature) and cumulative watershed area (WS AREA) for the Middle Fork stream network. Prediction locations are white circles.", fig.alt = "Mean summer stream temperature and cumulative watershed area.", fig.align = "center", out.width = "75%"----
ggplot() +
  geom_sf(
    data = mf04_ssn$edges,
    color = "medium blue",
    aes(linewidth = h2oAreaKm2)
  ) +
  scale_linewidth(range = c(0.1, 2.5)) +
  geom_sf(
    data = mf04_ssn$preds$pred1km,
    size = 1.5,
    shape = 21,
    fill = "white",
    color = "dark grey"
  ) +
  geom_sf(
    data = mf04_ssn$obs,
    size = 1.7,
    aes(color = Summer_mn)
  ) +
  coord_sf(datum = st_crs(MF_streams)) +
  scale_color_viridis_c() +
  labs(color = "Temperature", linewidth = "WS Area") +
  theme(
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10)
  )

## ----SSN2_modeling------------------------------------------------------------
library(SSN2)

## Generate hydrologic distance matrices
ssn_create_distmat(mf04_ssn)

## Fit the model
ssn_mod <- ssn_lm(
  formula = Summer_mn ~ ELEV_DEM + AREAWTMAP,
  ssn.object = mf04_ssn,
  tailup_type = "exponential",
  taildown_type = "spherical",
  euclid_type = "gaussian",
  additive = "afvArea"
)
summary(ssn_mod)

## ----get-labels, echo = FALSE-------------------------------------------------
labs <- knitr::all_labels()
labs <- setdiff(labs, c("setup", "get-labels"))

## ----all-code, ref.label=labs, eval = FALSE-----------------------------------
# library(SSNbler)
# copy_streams_to_temp()
# path <- paste0(tempdir(), "/streamsdata")
# library(sf)
# MF_streams <- st_read(paste0(path, "/MF_streams.gpkg"))
# MF_obs <- st_read(paste0(path, "/MF_obs.gpkg"))
# MF_pred1km <- st_read(paste0(path, "/MF_pred1km.gpkg"))
# MF_CapeHorn <- st_read(paste0(path, "/MF_CapeHorn.gpkg"))
# library(ggplot2)
# ggplot() +
#   geom_sf(data = MF_streams) +
#   geom_sf(data = MF_CapeHorn, color = "gold", size = 1.7) +
#   geom_sf(data = MF_pred1km, colour = "purple", size = 1.7) +
#   geom_sf(data = MF_obs, color = "blue", size = 2) +
#   coord_sf(datum = st_crs(MF_streams))
# knitr::include_graphics("valid_nodes.png")
# ## Set path for new folder for lsn
# lsn.path <- paste0(tempdir(), "/mf04")
# 
# edges <- lines_to_lsn(
#   streams = MF_streams,
#   lsn_path = lsn.path,
#   check_topology = TRUE,
#   snap_tolerance = 0.05,
#   topo_tolerance = 20,
#   overwrite = TRUE
# )
# obs <- sites_to_lsn(
#   sites = MF_obs,
#   edges = edges,
#   lsn_path = lsn.path,
#   file_name = "obs",
#   snap_tolerance = 100,
#   save_local = TRUE,
#   overwrite = TRUE
# )
# preds <- sites_to_lsn(
#   sites = MF_pred1km,
#   edges = edges,
#   save_local = TRUE,
#   lsn_path = lsn.path,
#   file_name = "pred1km.gpkg",
#   snap_tolerance = 100,
#   overwrite = TRUE
# )
# 
# capehorn <- sites_to_lsn(
#   sites = MF_CapeHorn,
#   edges = edges,
#   save_local = TRUE,
#   lsn_path = lsn.path,
#   file_name = "CapeHorn.gpkg",
#   snap_tolerance = 100,
#   overwrite = TRUE
# )
# edges <- updist_edges(
#   edges = edges,
#   save_local = TRUE,
#   lsn_path = lsn.path,
#   calc_length = TRUE
# )
# 
# names(edges) ## View edges column names
# site.list <- updist_sites(
#   sites = list(
#     obs = obs,
#     pred1km = preds,
#     CapeHorn = capehorn
#   ),
#   edges = edges,
#   length_col = "Length",
#   save_local = TRUE,
#   lsn_path = lsn.path
# )
# 
# names(site.list) ## View output site.list names
# names(site.list$obs) ## View column names in obs
# ggplot() +
#   geom_sf(data = edges, aes(color = upDist)) +
#   geom_sf(data = site.list$obs, aes(color = upDist)) +
#   coord_sf(datum = st_crs(MF_streams)) +
#   scale_color_viridis_c()
# summary(edges$h2oAreaKm2) ## Summarize and check for zeros
# 
# edges <- afv_edges(
#   edges = edges,
#   infl_col = "h2oAreaKm2",
#   segpi_col = "areaPI",
#   afv_col = "afvArea",
#   lsn_path = lsn.path
# )
# 
# names(edges) ## Look at edges column names
# summary(edges$afvArea) ## Summarize the AFV column
# site.list <- afv_sites(
#   sites = site.list,
#   edges = edges,
#   afv_col = "afvArea",
#   save_local = TRUE,
#   lsn_path = lsn.path
# )
# 
# names(site.list$pred1km) ## View column names in pred1km
# summary(site.list$pred1km$afvArea) ## Summarize AFVs in pred1km and look for zeros
# mf04_ssn <- ssn_assemble(
#   edges = edges,
#   lsn_path = lsn.path,
#   obs_sites = site.list$obs,
#   preds_list = site.list[c("pred1km", "CapeHorn")],
#   ssn_path = paste0(path, "/MiddleFork04.ssn"),
#   import = TRUE,
#   check = TRUE,
#   afv_col = "afvArea",
#   overwrite = TRUE
# )
# 
# class(mf04_ssn) ## Get class
# names(mf04_ssn) ## print names of SSN object
# names(mf04_ssn$preds) ## print names of prediction datasets
# ggplot() +
#   geom_sf(
#     data = mf04_ssn$edges,
#     color = "medium blue",
#     aes(linewidth = h2oAreaKm2)
#   ) +
#   scale_linewidth(range = c(0.1, 2.5)) +
#   geom_sf(
#     data = mf04_ssn$preds$pred1km,
#     size = 1.5,
#     shape = 21,
#     fill = "white",
#     color = "dark grey"
#   ) +
#   geom_sf(
#     data = mf04_ssn$obs,
#     size = 1.7,
#     aes(color = Summer_mn)
#   ) +
#   coord_sf(datum = st_crs(MF_streams)) +
#   scale_color_viridis_c() +
#   labs(color = "Temperature", linewidth = "WS Area") +
#   theme(
#     legend.text = element_text(size = 8),
#     legend.title = element_text(size = 10)
#   )
# library(SSN2)
# 
# ## Generate hydrologic distance matrices
# ssn_create_distmat(mf04_ssn)
# 
# ## Fit the model
# ssn_mod <- ssn_lm(
#   formula = Summer_mn ~ ELEV_DEM + AREAWTMAP,
#   ssn.object = mf04_ssn,
#   tailup_type = "exponential",
#   taildown_type = "spherical",
#   euclid_type = "gaussian",
#   additive = "afvArea"
# )
# summary(ssn_mod)

