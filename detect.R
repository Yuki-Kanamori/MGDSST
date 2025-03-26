require(tidyverse)
require(heatwaveR)


# directories -------------------------------------------------------------
dir = "/Users/Yuki/Dropbox/SST/all/SSTまとめ"
dir_output = "/Users/Yuki/Dropbox/SST/MHWs"


# sst data ----------------------------------------------------------------
setwd(dir)
sst = read_csv("sst_1982-2023.csv")


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
sum = mhws %>% mutate(year = str_sub(date_start, 1, 4)) 
sum_h = sum %>% group_by(year, tag_no) %>% summarize(occu_rate = sum(duration), intensity = sum(intensity_cumulative))

sum = mhws %>% mutate(year = str_sub(date_start, 1, 4)) 
sum_h = sum %>% group_by(year, tag_no) %>% summarize(occu_rate = sum(duration), intensity = sum(intensity_cumulative))
