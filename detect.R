require(tidyverse)
require(heatwaveR)
require(maps)
require(mapdata)
require(mapproj)


# directories -------------------------------------------------------------
dir = "/Users/Yuki/Dropbox/SST/all/SSTまとめ"
dir_output = "/Users/Yuki/Dropbox/SST/MHWs"


# sst data ----------------------------------------------------------------
setwd(dir)
sst = read_csv("sst_1982-2023.csv")
summary(sst)


# 異常値の検出 ------------------------------------------------------------------
head(sst)
lonlat = data_frame(lon = sst$lon, lat = sst$lat) %>% mutate(tag = paste(lon, lat, sep = "_")) %>% distinct(tag, .keep_all = TRUE) %>% mutate(tag_no = 1:1248)

mhws = NULL
for(i in 1:nrow(lonlat)){
  tag = lonlat[i, ]
  tag_lon = tag$lon
  tag_lat = tag$lat
  tag_no = tag$tag_no
  df = sst %>% filter(lon == tag_lon, lat == tag_lat)
  df = df %>% mutate(temp = paste(year, month, day, sep = "-")) %>% mutate(t= as.Date(temp)) %>% select(-temp) %>% rename(temp = SST)
  
  if(max(df$year) != 2023) next
  
  res = detect_event(ts2clm(data = df,
                             climatologyPeriod = c("1982-01-01", "2012-12-31")))
  cate = category(res, S = FALSE, name = "doto") %>% select(event_no, category, season)
  df_event = left_join(res_doto[["event"]], cate, by = "event_no") %>% mutate(lon = tag$lon, lat = tag$lat, tag = tag$tag, tag_no = paste(tag_no))
   
  mhws = rbind(mhws, df_event)
 }

mcss = NULL
for(i in 1:nrow(lonlat)){
  tag = lonlat[i, ]
  tag_lon = tag$lon
  tag_lat = tag$lat
  tag_no = tag$tag_no
  df = sst %>% filter(lon == tag_lon, lat == tag_lat)
  df = df %>% mutate(temp = paste(year, month, day, sep = "-")) %>% mutate(t = as.Date(temp)) %>% select(-temp) %>% rename(temp = SST)
  
  if(max(df$year) != 2023) next
  
  res = detect_event(ts2clm(data = df,
                            pctile = 10,
                            climatologyPeriod = c("1982-01-01", "2012-12-31")))
  cate = category(res, S = FALSE, name = "doto") %>% select(event_no, category, season)
  df_event = left_join(res_doto[["event"]], cate, by = "event_no") %>% mutate(lon = tag$lon, lat = tag$lat, tag = tag$tag, tag_no = paste(tag_no))
  
  mcss = rbind(mcss, df_event)
}


# setwd(dir_output)
# write.csv(mhws, "mhws.csv")
# write.csv(mcss, "mcss.csv")


# 発生率と強度の算出 ---------------------------------------------------------------
sum_h = mhws %>% mutate(year = str_sub(date_start, 1, 4)) 
sum_h = sum_h %>% group_by(year, tag_no) %>% summarize(occu_rate = sum(duration), intensity = sum(intensity_cumulative))
sum_h = left_join(sum_h, lonlat %>% mutate(tag_no = as.character(tag_no)), by = "tag_no")

sum_c = mcss %>% mutate(year = str_sub(date_start, 1, 4)) 
sum_c = sum_c %>% group_by(year, tag_no) %>% summarize(occu_rate = sum(duration), intensity = sum(intensity_cumulative))
sum_c = left_join(sum_c, lonlat %>% mutate(tag_no = as.character(tag_no)), by = "tag_no")


# 地図 ----------------------------------------------------------------------
# 日本地図の設定（変更しない）
japan = map_data("japan") %>% mutate(long = long - 0.01, lat = lat - 0.01)

g = ggplot() + 
  geom_polygon(data = japan, aes(x = long, y = lat, group = group), colour = "gray 50", fill = "gray 50")

# sstデータの地点
p = geom_point(data = lonlat, aes(x = lon, y = lat), shape = 15, size = 1)
labs = labs(title = "", x = "", y = "")
fig = g+p+labs+theme_bw()


p = geom_point(data = sum_h, aes(x = lon, y = lat, color = occu_rate), shape = 15, size = 1)
c = scale_color_gradientn(colours = c("blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
f = facet_wrap(~ year, ncol = 6)
labs = labs(title = "", x = "", y = "", fill = "days")

fig = g+p+c+f+labs+theme_bw()
ggsave(filename = "mhws_days.pdf", plot = fig, units = "in", width = 11.69, height = 8.27)
