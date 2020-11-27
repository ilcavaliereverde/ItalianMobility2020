#Packages for other tabs
require(geojsonio)
require(broom)
require(maptools)

source("global.R")
memory.limit(size = 50000)

#Loading geojson polygons for Municipalities, Provinces and Regions
# SpMun <- geojson_read("limits_IT_Municipalities.geojson", what = "sp")
SpPro <- geojson_read("data/limits_IT_Provinces.geojson", what = "sp")
# SpReg <- geojson_read("limits_IT_Regions.geojson", what = "sp")

#Transforming SPDFs into dataframes for ggplot keeping named index
# Mun <- tidy(SpMun, region = "name")
Pro <- tidy(SpPro, region = "prov_name")
# Reg <- tidy(SpReg, region = "reg_name")

#Keeping names as dataframes
# NmMun <- as.data.frame(SpMun@data)
NmPro <- as.data.frame(SpPro@data)
# NmReg <- as.data.frame(SpReg@data)

#Merging geospatial province dataframe with its names
NmPro$prov_acr <- paste0("IT-", NmPro$prov_acr)
NmPro <- NmPro[ , c(1, 3)]
Pro <- left_join(Pro, NmPro, by = c("id" = "prov_name"))

#Merging geospatial province dataframe with Google Mobility data
int <- dfr %>% select(-c("country", "iso")) %>% mutate_all(na_if,"") %>% drop_na(iso31662)
rm(list = c("dfr", "nam", "NmPro", "SpPro", "prre", "pro", "reg", "vis"))
int2 <- pivot_longer(int, 5:10)
int3 <- split(int2, int2$name)
groc <- int3$grocery_pharmacy %>% select(-c(region, name, province))
groc <- left_join(Pro, groc, by = c("prov_acr" = "iso31662"))

#Plotting and saving to a file for convenience
# PlMun <- ggplot() +
#   geom_polygon(data = Mun, aes(
#     x = long,
#     y = lat,
#     group = group,
#     fill = id)) +
#   theme_void() + coord_map()

#Setting min/max for the legend
minb <- min(groc$value, na.rm = T)
maxb <- max(groc$value, na.rm = T)

#Setting data range
a <- min(groc$date, na.rm = T)
b <- as.Date("2020-05-14", "%Y-%m-%d") #max(groc$date, na.rm = T)
dates <- seq(as.Date(a), as.Date(b), by = 1)

#for loop that prints images for every day
plotlist <- list()
for (i in dates) {
  ggplot() +
    geom_polygon(data = groc %>% filter(date == i),
                 aes(
                   x = long,
                   y = lat,
                   group = group,
                   fill = value)) + scale_fill_viridis_c(limits = c(minb, maxb)) +
    theme_void() + coord_map()
  ggsave(paste0("plots/grocerypharmacy/", as.Date(i, origin = "1970-01-01"), ".svg"),
         device = "svg")}



#original working plot & save
PlPro <- ggplot() +
  geom_polygon(data = groc %>% filter(date == a),
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = value)
               ) + scale_fill_viridis_c(limits = c(minb, maxb)) + theme_void() + coord_map() + theme(legend.position = 'none')

ggsave("pro.svg",
       plot = PlPro,
       device = "svg")

# PlReg <- ggplot() +
#   geom_polygon(data = Reg, aes(
#     x = long,
#     y = lat,
#     group = group, fill = id)) +
#   theme_void() + coord_map()

# ggsave("mun.pdf",
#        plot = PlMun,
#        device = "pdf",
#        width = 30,
#        height = 30)



# ggsave("reg.pdf",
#        plot = PlReg,
#        device = "pdf",
#        width = 30,
#        height = 30)


