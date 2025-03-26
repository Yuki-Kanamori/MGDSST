require(tidyverse)
require(stringr)

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
for(i in 1:length(fl)){
  df = read_fwf(file = fl[i], fwf_widths(col_names = T, widths = widths), skip = 1)
  df2 = df[min(lat2$tag):max(lat2$tag), min(lon2$tag):max(lon2$tag)]
  colnames(df2) = lon2$lon
  df2$lat = rep(lat2$lat)
  df2$ymd = rep(str_sub(fl[i], (nchar(fl[i])-11), (nchar(fl[i])-4)), each = nrow(df2))
  
  year = str_sub(fl[i], -12, -9)
  dir_write = paste("/Users/Yuki/Dropbox/SST/all/japan/", year, "/", sep = "", collapse ="")
  setwd(dir_write)
  write.csv(df2, paste0(str_sub(fl[i], -25, -5), "_japan.csv"), row.names = FALSE)
}











