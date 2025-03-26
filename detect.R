require(tidyverse)
require(heatwaveR)

dir = "/Users/Yuki/Dropbox/SST/all/SSTまとめ"
dir_output = "/Users/Yuki/Dropbox/SST/MHWs"
setwd(dir)
sst = read_csv("sst_1982-2023.csv")

head(sst)
lonlat = data_frame(lon = sst$lon, lat = sst$lat) %>% mutate(tag = paste(lon, lat, sep = "_")) %>% distinct(tag, .keep_all = TRUE) %>% mutate(tag_no = 1:1248)

mhws = NULL
for(i in 1153:nrow(lonlat)){
  tag = lonlat[i, ]
  tag_lon = tag$lon
  tag_lat = tag$lat
  df = sst %>% filter(lon == tag_lon, lat == tag_lat)
  df = df %>% mutate(temp = paste(year, month, day, sep = "-")) %>% mutate(t= as.Date(temp)) %>% select(-temp) %>% rename(temp = SST)
  
  if(max(df$year) != 2023) next
  
  res = detect_event(ts2clm(data = df,
                             climatologyPeriod = c("1982-01-01", "2012-12-31")))
  cate = category(res, S = FALSE, name = "doto") %>% select(event_no, category, season)
  df_event = left_join(res_doto[["event"]], cate, by = "event_no") %>% mutate(lon = tag$lon, lat = tag$lat, tag = tag$tag, tag_no = tag$tag_no)
   
  mhws = rbind(mhws, df_event)
 }

mcss = NULL
for(i in 1:nrow(lonlat)){
  tag = lonlat[i, ]
  tag_lon = tag$lon
  tag_lat = tag$lat
  df = sst %>% filter(lon == tag_lon, lat == tag_lat)
  df = df %>% mutate(temp = paste(year, month, day, sep = "-")) %>% mutate(t = as.Date(temp)) %>% select(-temp) %>% rename(temp = SST)
  
  if(max(df$year) != 2023) next
  
  res = detect_event(ts2clm(data = df,
                            pctile = 10,
                            climatologyPeriod = c("1982-01-01", "2012-12-31")))
  cate = category(res, S = FALSE, name = "doto") %>% select(event_no, category, season)
  df_event = left_join(res_doto[["event"]], cate, by = "event_no") %>% mutate(lon = tag$lon, lat = tag$lat, tag = tag$tag)
  
  mcss = rbind(mcss, df_event)
}


# setwd(dir_output)
# write.csv(mhws, "mhws.csv")
# write.csv(mcss, "mcss.csv")
