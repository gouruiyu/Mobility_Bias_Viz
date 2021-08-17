library(plyr)
library(nngeo)

# Load data
cams <- read.csv("data/surrey_desc.csv")
cams_data <- read.csv("data/surrey_data.csv")
cams_merged <- merge(cams, cams_data, by.x = "station_name", by.y = "station")
INTERSECTION_TEST_TYPE <- 0 #same intersection

categorize_ampm <- function(df, dateRange) {
  df$time <- as.POSIXct(df$time)
  df_ampm <- df %>%
    filter(time %within% interval(dateRange[1], dateRange[2])) %>%
    mutate(rush_hour = case_when(hour(time) >= 7 & hour(time) <= 10 ~ "AM",
                                 hour(time) >= 16 & hour(time) <= 19 ~ "PM")) %>%
    drop_na(rush_hour)
  return(df_ampm)
}

find_nn <- function(curr_camid) {
  curr_intersection <- unique(cams[which(cams$station_name == curr_camid),]$street_address)
  other_cams <- cams[which(cams$street_address != curr_intersection),]
  curr_cam_sf <- cams[which(cams$station_name == curr_camid),] %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  other_cams_sf <- other_cams %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  nn <- st_nn(curr_cam_sf, other_cams_sf, maxdist=1000, returnDist = FALSE)
  nn <- lapply(nn, function(x) other_cams_sf[x,])
  nn_df <- ldply(nn, data.frame)
  print(nn_df$station_name)
  return(nn_df$station_name)
}

validate_pair <- function(sel_cams) {
  if (!length(sel_cams$ids) == 2) return(FALSE)
  street_addresses <- cams[which(cams$station_name %in% sel_cams$ids),]$street_address
  same_intersection <- length(unique(street_addresses)) == 1
  if (same_intersection == TRUE) {
    INTERSECTION_TEST_TYPE <- 0
    return(TRUE)
  }
  nn_intersection <- unique(find_nn(sel_cams$ids[2]) == sel_cams$ids[1])
  if (nn_intersection == TRUE) {
    INTERSECTION_TEST_TYPE <- 1
    return(TRUE)
  }
  return(FALSE)
}

weekday_picker <- function(df) {
  df$time <- as.POSIXct(df$time)
  df$date <- as.POSIXct(format(df$time, "%Y-%m-%d"))
  df <- df[which(wday(df$date) %notin% c(6, 7)),]
  return(df)
}

validate_existing_data <- function(camid) {
  camid_data <- cams_data %>% filter(station == camid) 
  return(nrow(camid_data) > 0)
}

pair_analyze_am <- function(sel_cams, dateRange, weekdayOnly) {
  if (!validate_pair(sel_cams)) return()
  sel_cams_data_exist <- lapply(sel_cams$ids, validate_existing_data)
  if (FALSE %in% sel_cams_data_exist) return()
  paired_df <- sel_cams$data
  paired_df$station <- as.factor(paired_df$station)
  paired_am_df <- categorize_ampm(paired_df, dateRange) %>%
    filter(rush_hour == "AM")
  if (weekdayOnly == TRUE) paired_am_df <- weekday_picker(paired_am_df)
  pair_levels <- unique(paired_am_df$station)
  paired_am_df_1 <- paired_am_df %>% filter(station == pair_levels[1])
  paired_am_df_2 <- paired_am_df %>% filter(station == pair_levels[2])
  if (INTERSECTION_TEST_TYPE == 0) {
    paired_am_test <- t.test(paired_am_df_1$car_count, paired_am_df_2$car_count, alternative = "two.sided", paired = TRUE)
  } else {
    paired_am_test <- t.test(paired_am_df_1$car_count, paired_am_df_2$car_count, alternative = "two.sided", paired = FALSE)
  }
  paired_am_stat <- round(paired_am_test$statistic, 4)
  paired_am_pval <- round(paired_am_test$p.value, 4)
  paired_am_sig <- paired_am_pval < 0.05
  return(list(rush_hour = "AM", t = paired_am_stat, pval = paired_am_pval, isSignificant = paired_am_sig))
}

pair_analyze_pm <- function(sel_cams, dateRange, weekdayOnly) {
  if (!validate_pair(sel_cams)) return()
  sel_cams_data_exist <- lapply(sel_cams$ids, validate_existing_data)
  if (FALSE %in% sel_cams_data_exist) return()
  paired_df <- sel_cams$data
  paired_df$station <- as.factor(paired_df$station)
  paired_pm_df <- categorize_ampm(paired_df, dateRange) %>%
    filter(rush_hour == "PM")
  if (weekdayOnly == TRUE) paired_pm_df <- weekday_picker(paired_pm_df)
  pair_levels <- unique(paired_pm_df$station)
  paired_pm_df_1 <- paired_pm_df %>% filter(station == pair_levels[1])
  paired_pm_df_2 <- paired_pm_df %>% filter(station == pair_levels[2])
  if (INTERSECTION_TEST_TYPE == 0) {
    paired_pm_test <- t.test(paired_pm_df_1$car_count, paired_pm_df_2$car_count, alternative = "two.sided", paired = TRUE)
  } else {
    paired_pm_test <- t.test(paired_pm_df_1$car_count, paired_pm_df_2$car_count, alternative = "two.sided", paired = FALSE)
  }
  paired_pm_stat <- round(paired_pm_test$statistic, 4)
  paired_pm_pval <- round(paired_pm_test$p.value, 4)
  paired_pm_sig <- paired_pm_pval < 0.05
  return(list(rush_hour = "PM", t = paired_pm_stat, pval = paired_pm_pval, isSignificant = paired_pm_sig))
}

