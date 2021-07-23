# Load data
cams <- read.csv("data/surrey_desc.csv")
cams_data <- read.csv("data/surrey_data.csv")
cams_merged <- merge(cams, cams_data, by.x = "station_name", by.y = "station")

categorize_ampm <- function(df, dateRange) {
  df$time <- as.POSIXct(df$time)
  df_ampm <- df %>%
    filter(time %within% interval(dateRange[1], dateRange[2])) %>%
    mutate(rush_hour = case_when(hour(time) >= 7 & hour(time) <= 10 ~ "AM",
                                 hour(time) >= 16 & hour(time) <= 19 ~ "PM")) %>%
    drop_na(rush_hour)
  return(df_ampm)
}

validate_pair <- function(sel_cams) {
  street_addresses <- cams[which(cams$station_name %in% sel_cams$ids),]$street_address
  print(length(sel_cams$ids) == 2 & length(unique(street_addresses)) == 1)
  return (length(sel_cams$ids) == 2 & length(unique(street_addresses)) == 1)
}

weekday_picker <- function(df) {
  df$time <- as.POSIXct(df$time)
  df$date <- as.POSIXct(format(df$time, "%Y-%m-%d"))
  df <- df[which(wday(df$date) %notin% c(6, 7)),]
  return(df)
}

pair_analyze_am <- function(sel_cams, dateRange, weekdayOnly) {
  if (!validate_pair(sel_cams)) return()
  paired_df <- sel_cams$data
  paired_df$station <- as.factor(paired_df$station)
  paired_am_df <- categorize_ampm(paired_df, dateRange) %>%
    filter(rush_hour == "AM")
  if (weekdayOnly == TRUE) paired_am_df <- weekday_picker(paired_am_df)
  pair_levels <- unique(paired_am_df$station)
  paired_am_df_1 <- paired_am_df %>% filter(station == pair_levels[1])
  paired_am_df_2 <- paired_am_df %>% filter(station == pair_levels[2])
  paired_am_test <- t.test(paired_am_df_1$car_count, paired_am_df_2$car_count, alternative = "two.sided", paired = TRUE)
  paired_am_stat <- paired_am_test$statistic
  paired_am_pval <- paired_am_test$p.value
  return(list(rush_hour = "AM", t = paired_am_stat, pval = paired_am_pval))
}

pair_analyze_pm <- function(sel_cams, dateRange, weekdayOnly) {
  if (!validate_pair(sel_cams)) return()
  paired_df <- sel_cams$data
  paired_df$station <- as.factor(paired_df$station)
  paired_pm_df <- categorize_ampm(paired_df, dateRange) %>%
    filter(rush_hour == "PM")
  if (weekdayOnly == TRUE) paired_pm_df <- weekday_picker(paired_pm_df)
  pair_levels <- unique(paired_pm_df$station)
  paired_pm_df_1 <- paired_pm_df %>% filter(station == pair_levels[1])
  paired_pm_df_2 <- paired_pm_df %>% filter(station == pair_levels[2])
  paired_pm_test <- t.test(paired_pm_df_1$car_count, paired_pm_df_2$car_count, alternative = "two.sided", paired = TRUE)
  paired_pm_stat <- paired_pm_test$statistic
  paired_pm_pval <- paired_pm_test$p.value
  return(list(rush_hour = "PM", t = paired_pm_stat, pval = paired_pm_pval))
}

