library(data.table)
library(dplyr)
library(raster)
library(ggmap)

# save.image()

ff <- list.files("data")

for (i in 1:length(ff)) {

  f <- stringi::stri_sub(ff[i], 10, 19)
  print(f)
  
  fname <- paste0("data/listings-", f, ".csv")
  
  # read all records and give names to the coloums
  df <- fread(fname, header = T)
  
  # Remove duplicate rows of the dataframe
  df <- distinct(df)
  
  # assign correct attribuates to the data frame
  df <- df %>% mutate(lats = latitude, lons = longitude)
  
  summary(df)
  
  # combine to get the results
  bjdf <- df
  
  esa <- c(min(bjdf$lons), min(bjdf$lats), max(bjdf$lons), max(bjdf$lats))
  
  shp <- getData("GADM", country="China", level=1)
  bj.shp <- shp[which(shp$NAME_1 == "Beijing"),]
  plot(bj.shp)
  
  p <- get_stamenmap(bbox(bj.shp), zoom = 10, maptype = "toner-lite") %>% ggmap() +
    theme(panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "#0d1329")) +
    # geom_polygon(data = bj.shp, aes(x = long, y = lat, group = group), color = 'white', alpha=0.5) +
    geom_point(data=bjdf, aes(x=lons, y=lats), pch=16, fill = "red", color = 'red', alpha=0.2, size = .5)+
    theme(legend.position="none",
          text = element_text(family = 'Hiragino Sans GB W6'),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    ggtitle(paste0("北京Airbnb民宿分布图（更新时间：", f, "） \n")) +
    labs(x = 'Longitude', y = 'Latitude')
  
  ggsave(paste0("figs/city/beijing_pic", f, ".png"), plot = p, dpi = 300, width = 8, height = 8)
  
  
  
  esa <- c(116, 39.65, 116.8, 40.3)
  
  p <- get_stamenmap(esa, zoom = 11, maptype = "toner-lite") %>% ggmap() +
    theme(panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "#0d1329")) +
    # geom_polygon(data = thailand, aes(x = long, y = lat, group = group), color = 'white', alpha=0.5) +
    geom_point(data=bjdf, aes(x=lons, y=lats), pch=16, fill = "blue", color = 'blue', alpha=0.2, size = 1)+
    theme(legend.position="none",
          text = element_text(family = 'Hiragino Sans GB W6'),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    ggtitle(paste0("北京Airbnb民宿分布局部放大图（更新时间：", f, "） \n")) +
    labs(x = 'Longitude', y = 'Latitude')
  
  ggsave(paste0("figs/zoom/beijing_zoom", f, ".png"), plot = p, dpi = 300, width = 8, height = 8)
  
  
  # density maps
  p <- get_stamenmap(esa, zoom = 12, maptype = "toner-lite") %>% ggmap() +
    stat_density2d(
      aes(x = lons, y = lats, fill = ..level.., alpha = .1),
      size = 0.001, bins = 150, data = bjdf,
      geom = "polygon"
    ) +
    # geom_density2d(data = df, aes(x = lons, y = lats), size = 0.3) +
    scale_fill_continuous(low = "blue", high = "red")+
    # geom_point(data=top.places, aes(x=lons, y=lats, shape = pname), size = 5, fill = "red", color = 'red', alpha=1)+
    # geom_point(data=top.places, aes(x=lons, y=lats), size = 5, fill = "black", color = 'black', alpha=1)+
    # geom_label_repel(data = top.places, aes(lons, lats, label = pname), box.padding = 0.2, point.padding = 0.3, segment.color = 'grey50')+
    # coord_cartesian() +
    theme(legend.position="none",
          text = element_text(family = 'Hiragino Sans GB W6'),
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    ggtitle(paste0("北京Airbnb民宿分布密度图（更新时间：", f, "） \n")) +
    labs(x = 'Longitude', y = 'Latitude')
  
  ggsave(paste0("figs/density/beijing_density", f, ".png"), plot = p, dpi = 300, width = 8, height = 8)
  
  
}

