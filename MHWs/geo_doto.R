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
# write.csv(near, "near.csv", row.names = FALSE)

ap_mz = near %>% filter(plot == "AP1") %>% mutate(lon = lon.y, lat = lat.y) %>% select(lon, lat)
mb = near %>% filter(plot == "MB1") %>% mutate(lon = lon.y, lat = lat.y) %>% select(lon, lat)
nn = near %>% filter(plot == "NN1") %>% mutate(lon = lon.y, lat = lat.y) %>% select(lon, lat)
mc = near %>% filter(plot == "MC1") %>% mutate(lon = lon.y, lat = lat.y) %>% select(lon, lat)

# sstデータ ------------------------------------------------------------------
dir_sst = "/Users/Yuki/Library/CloudStorage/Dropbox/SST/all/"
setwd(dir = dir_sst)

sst1 = read_csv("sst_1982-1991.csv")
ap_mz1 = sst1 %>% filter(lon == ap_mz$lon, lat == ap_mz$lat)
mb1 = sst1 %>% filter(lon == mb$lon, lat == mb$lat)
nn1 = sst1 %>% filter(lon == nn$lon, lat == nn$lat)
mc1 = sst1 %>% filter(lon == mc$lon, lat == mc$lat)

sst2 = read_csv("sst_1992-2001.csv")
ap_mz2 = sst2 %>% filter(lon == ap_mz$lon, lat == ap_mz$lat)
mb2 = sst2 %>% filter(lon == mb$lon, lat == mb$lat)
nn2 = sst2 %>% filter(lon == nn$lon, lat == nn$lat)
mc2 = sst2 %>% filter(lon == mc$lon, lat == mc$lat)

sst3 = read_csv("sst_2002-2011.csv")
ap_mz3 = sst3 %>% filter(lon == ap_mz$lon, lat == ap_mz$lat)
mb3 = sst3 %>% filter(lon == mb$lon, lat == mb$lat)
nn3 = sst3 %>% filter(lon == nn$lon, lat == nn$lat)
mc3 = sst3 %>% filter(lon == mc$lon, lat == mc$lat)

sst4 = read_csv("sst_2012-2021.csv")
ap_mz4 = sst4 %>% filter(lon == ap_mz$lon, lat == ap_mz$lat)
mb4 = sst4 %>% filter(lon == mb$lon, lat == mb$lat)
nn4 = sst4 %>% filter(lon == nn$lon, lat == nn$lat)
mc4 = sst4 %>% filter(lon == mc$lon, lat == mc$lat)

sst5 = read_csv("sst_2022-2024.csv")
ap_mz5 = sst5 %>% filter(lon == ap_mz$lon, lat == ap_mz$lat)
mb5 = sst5 %>% filter(lon == mb$lon, lat == mb$lat)
nn5 = sst5 %>% filter(lon == nn$lon, lat == nn$lat)
mc5 = sst5 %>% filter(lon == mc$lon, lat == mc$lat)

sst_APMZ = rbind(ap_mz1, ap_mz2, ap_mz3, ap_mz4, ap_mz5)
summary(sst_APMZ)
sst_MB = rbind(mb1, mb2, mb3, mb4, mb5)
summary(sst_MB)
sst_NN = rbind(nn1, nn2, nn3, nn4, nn5)
summary(sst_NN)
sst_MC = rbind(mc1, mc2, mc3, mc4, mc5)
summary(sst_MC)

write.csv(sst_APMZ, "sst_APMZ.csv", row.names = FALSE)
write.csv(sst_MB, "sst_MB.csv", row.names = FALSE)
write.csv(sst_NN, "sst_NN.csv", row.names = FALSE)
write.csv(sst_MC, "sst_MC.csv", row.names = FALSE)
