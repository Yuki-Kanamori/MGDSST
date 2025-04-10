require(tidyverse)
require(stringr)
require(fuzzyjoin)

# make dataframe ----------------------------------------------------------
setwd("/Users/Yuki/Dropbox/SST/all")
dir = "/Users/Yuki/Dropbox/SST/all"
widths = rep(3, each = 1440)


fl = list.files(path = dir, full.names = T, pattern = "txt") #読み込むファイル名のリスト化
length(fl)


length(fl)
fl[1]
fl[length(fl)]

#####データの緯度経度を指定する#####
#MGDSSTのlat
lat = data.frame(seq(89.875, -89.875, -0.25))
colnames(lat) = "lat"
lat$tag = rep(1:720)
#欲しい緯度範囲
lat2 = lat[lat$lat >= 24.75 & lat$lat <= 48.75, ]


#MGDSSTのlon
lon = data.frame(seq(359.875, 0, -0.25))
colnames(lon) = "lon"
lon$tag = rep(1:1440)
#欲しい経度範囲
lon2 = lon[lon$lon >= 122.25 & lon$lon <= 150.25, ] # 小さくした


# ディレクトリの作成 ---------------------------------------------------------------
for(i in 1982:2024){
  setwd(dir = "/Users/Yuki/Dropbox/SST/all/japan/")
  dir.create(paste(i))
}



# データフレームの作成 --------------------------------------------------------------
setwd("/Users/Yuki/Dropbox/SST/all")
for(i in 1:1){
  df = read_fwf(file = fl[i], fwf_widths(col_names = T, widths = widths), skip = 1)
  df2 = df[min(lat2$tag):max(lat2$tag), min(lon2$tag):max(lon2$tag)]
  colnames(df2) = lon2$lon
  df2$lat = rep(lat2$lat)
  df2$ymd = rep(str_sub(fl[i], (nchar(fl[i])-11), (nchar(fl[i])-4)), each = nrow(df2))
  
  year = str_sub(fl[i], -12, -9)
  # dir_write = paste("/Users/Yuki/Dropbox/SST/all/japan/", year, "/", sep = "", collapse ="")
  # setwd(dir_write)
  # write.csv(df2, paste0(str_sub(fl[i], -25, -5), "_japan.csv"), row.names = FALSE)
}

df3 = df2 %>% tidyr::gather(key = lon, value = SST, -c(ymd, lat))
sst_lonlat = df3 %>% select(lon, lat) %>% mutate(lon = as.numeric(lon))

setwd("/Users/Yuki/MGDSST/MHWs")
plot = read.csv("doto_lonlat.csv")
# plot = plot[c(1, 6, 11, 16, 21), ]
summary(plot)
summary(sst_lonlat)

near = NULL
for(i in 1:nrow(plot)){
  temp_plot = plot[i, ]
  pairs = temp_plot %>%
    geo_left_join(sst_lonlat, max_dist = 50, unit = "km", distance_col = "distance") %>% filter(distance == min(distance))
  near = rbind(near, pairs)
}
write.csv(near, "near.csv", row.names = FALSE)
