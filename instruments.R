# sum of characteristics for products by the same firm
vars_to_include <- c("dpm", "door3", "door4", "door5", "at", "ps", "air","drv" , "wt", "hp2wt", "hp", "euro", "japan", "size", "wb")
# for(var in vars_to_include){
#   print(class(data[[var]]))
# } ALL NUMERIC OR INTEGER
iv1 <- function(x){
  sum(x)
}
temp_df <- dplyr::group_by(data, year, firmids) %>% 
  dplyr::summarise_at(vars(dpm, door3, door4, door5, at, ps, air, drv, wt, hp2wt, hp, euro, japan, size, wb), funs(iv1, sum)) 
data <- data %>% left_join(temp_df, by= c("year", "firmids"))
data <- data %>% mutate(dpm_iv1 = dpm_sum - dpm,
                           door3_iv1 = door3_sum - door3,
                           door4_iv1 = door4_sum - door4,
                           door5_iv1 = door5_sum - door5,
                           at_iv1 = at_sum - at, 
                           ps_iv1 = ps_sum - ps,
                           air_iv1 = air_sum - air,
                           drv_iv1 = drv_sum - drv,
                           wt_iv1 = wt_sum -wt,
                           hp2wt_iv1 = hp2wt_sum - hp2wt,
                           hp_iv1 = hp_sum - hp,
                           euro_iv1 = euro_sum - euro,
                           japan_iv1 = japan_sum - japan,
                           size_iv1 = size_sum - size,
                           wb_iv1 = wb_sum - wb)
rm(temp_df)


iv2 <- function(x){
  sum(x)
}
temp <- function(x){
  sum(x)
}
# sum of characteristics for products by the same firm
data <- data %>% left_join(group_by(data, year) %>% summarise_at(vars(dpm, door3, door4, door5, at, ps, air, drv, wt, hp2wt, hp, euro, japan, size, wb), funs(iv2, temp)), by= c("year"))

data <- data %>% mutate(dpm_iv2 = dpm_iv2 - dpm_sum,
                        door3_iv2 = door3_iv2 - door3_sum,
                        door4_iv2 = door4_iv2 - door4_sum,
                        door5_iv2 = door5_iv2 - door5_sum,
                        at_iv2 = at_iv2 - at_sum, 
                        ps_iv2 = ps_iv2 - ps_sum,
                        air_iv2 = air_iv2 - air_sum,
                        drv_iv2 = drv_iv2 - drv_sum,
                        wt_iv2 = wt_iv2 -wt_sum,
                        hp2wt_iv2 = hp2wt_iv2 - hp2wt_sum,
                        hp_iv2 = hp_iv2 - hp_sum,
                        euro_iv2 = euro_iv2 - euro_sum,
                        japan_iv2 = japan_iv2 - japan_sum,
                        size_iv2 = size_iv2 - size_sum,
                        wb_iv2 = wb_iv2 - wb_sum)

# average of characteristics for products in the same year
temp_df <- group_by(data, year) %>% count(year)
data <- data %>% left_join(temp_df, by= c("year"))

data <- data %>% mutate(dpm_iv3 = (dpm_temp - dpm)/(n-1),
                        door3_iv3 = (door3_temp - door3)/(n-1),
                        door4_iv3 = (door4_temp - door4)/(n-1),
                        door5_iv3 = (door5_temp - door5)/(n-1),
                        at_iv3 = (at_temp - at)/(n-1), 
                        ps_iv3 = (ps_temp - ps)/(n-1),
                        air_iv3 = (air_temp - air)/(n-1),
                        drv_iv3 = (drv_temp - drv)/(n-1),
                        wt_iv3 = (wt_temp -wt)/(n-1),
                        hp2wt_iv3 = (hp2wt_temp - hp2wt)/(n-1),
                        hp_iv3 = (hp_temp - hp)/(n-1),
                        euro_iv3 = (euro_temp - euro)/(n-1),
                        japan_iv3 = (japan_temp - japan)/(n-1),
                        size_iv3 = (size_temp - size)/(n-1),
                        wb_iv3 = (wb_temp - wb)/(n-1))
