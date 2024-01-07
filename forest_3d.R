libs <- c("tidyverse", "sf", "geodata", "terra", "classInt", "rayshader")

installed_libs <- libs %in% rownames(installed.packages())

if (any(!installed_libs)) {
  install.packages(libs[!installed_libs])
}

invisible(lapply(libs, library, character.only = TRUE))

urls <- c(
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N09E078_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N09E081_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N06E078_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N06E081_Map.tif",
  "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files=ETH_GlobalCanopyHeight_10m_2020_N03E078_Map.tif"
)
for (url in urls) {
  # Replace special characters with underscores
  safe_filename <- gsub("[^A-Za-z0-9._-]", "_", basename(url))
  
  # Check if the file already exists
  if (file.exists(safe_filename)) {
    cat("File", safe_filename, "already exists. Skipping download.\n")
  } else {
    # Download the file
    download.file(url, destfile = safe_filename, mode = "wb")
    cat("Downloaded", safe_filename, "\n")
  }
}


raster_files <- list.files(
  path = getwd(),
  pattern = "ETH",
  full.names = T
)

print(raster_files)
list.files(
  path=getwd(),
)

get_country_borders<-function(){
  main_path<-getwd()
  country_borders<-geodata::gadm(
    country = "LKA",
    level=1,
    path=main_path
  )|>
    sf::st_as_sf()
  return(country_borders)
}
country_borders<-get_country_borders()
sri_sf <- country_borders|>
  sf::st_union()

plot(sri_sf)

forest_height_list<-lapply(
  raster_files,
  terra::rast
)
forest_height_rasters <- lapply(
  forest_height_list,
  function(x) {
    terra::crop(
      x,
      terra::vect(sri_sf),
      snap = "in",
      mask = TRUE
    )
    
  }
)
forest_height_mosaic<-do.call(
  terra::mosaic,
  forest_height_rasters
)
print(forest_height_mosaic)

forest_height_sri<-forest_height_mosaic|>
  terra::aggregate(
    fact=7
  )
forest_height_sri_df<-forest_height_sri|>
  as.data.frame(
    xy=T
  )
head(forest_height_sri_df)
names(forest_height_sri_df)[3]<-"height"

subset_data <- forest_height_sri_df[sample(nrow(forest_height_sri_df), 1000), ]

breaks <- classInt::classIntervals(
  subset_data$height,
  n = 5,
  style = "fisher"
)$brks

print(breaks)

cols<-
  c(
    "white","#ffd3af","#fbe06e","#6daa55","#205544"
  )
texture<-colorRampPalette(
  cols,
  bias=2
)(6)
p<-ggplot(
  forest_height_sri_df
)+
  geom_raster(
    aes(
      x=x,
      y=y,
      fill=height
    )
  )+
  scale_fill_gradientn(
    name="height(m)",
    colors=texture,
    breaks=round(breaks,0)
  )+
  coord_sf(crs=4326)+
  guides(
    fill=guide_legend(
      direction="vertical",
      keyheight=unit(5,"mm"),
      keywidth=unit(5,"mm"),
      title.position="top",
      label.position="right",
      title.hjust=.5,
      label.hjust=.5,
      ncol=1,
      byrow=F
    )
  )+
  theme_minimal()+
  theme(
    axis.line=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    legend.position="right",
    legend.title=element_text(
      size=11,color="grey10"
    ),
    legend.text=element_text(
      size=10,color="grey10"
    ),
    panel.grid.major=element_line(
      color="white"
    ),
    panel.grid.minor=element_line(
      color="white"
    ),
    plot.background=element_rect(
      fill="white",color=NA
    ),
    panel.border=element_rect(
      fill=NA,color="white"
    ),
    plot.margin=unit(
      c(
        t=0,r=0,
        b=0,l=0
      ),"lines"
    )
    
  )
print(p)


h<-nrow(forest_height_sri)
w<-ncol(forest_height_sri)

rayshader::plot_gg(
  ggobj = p,
  width=w/1000,
  height=h/1000,
  scale=150,
  solid=F,
  soilddepth=0,
  shadow=T,
  shadow_intensity = .99,
  offset_edges = F,
  sunangle=315,
  window.size=c(800,800),
  zoom=.4,
  phi=30,
  theta=-30,
  multicore = T
)
rayshader::render_camera(
  phi=50,
  zoom = .7,
  theta=45
)

rayshader::render_highquality(
  filename = "sri_forest_height-2020_01.png",
  preview=T,
  interactive=F,
  light=T,
  lightdirection = c(
    315,310,315,310
  ),
  lightintensity = c(
    1000,1500,150,100
  ),
  lightaltitude = c(
    15,15,80,80 
  ),
  ground_material = 
  rayrender::microfacet(
        roughness=.6
  ),
  width = 4000,
  height = 4000
)








