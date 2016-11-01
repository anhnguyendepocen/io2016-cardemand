# 1st set of instruments : product jt

# 2nd set of instruments : for each product jt, sum of characteristics of OTHER products by the same firm r in the same market t
#   technique : take sum of characteristics of ALL products by the same firm r in the same market t, and substract characteristics of product jt
temp_df <- group_by(cardata, year, firmids) %>% 
  summarise_at(vars(CONSTANT, dpm, mpd, door3, door4, door5, at, ps, air, drv, wt, hp2wt, hp, euro, japan, size, wb), funs(sum))
temp_df <- (select(cardata, year, firmids) %>% left_join(temp_df, by= c("year", "firmids"))) - (select(cardata, year, firmids, CONSTANT, dpm, mpd, door3, door4, door5, at, ps, air, drv, wt, hp2wt, hp, euro, japan, size, wb))
temp_df <- select(temp_df, -year, -firmids)
colnames(temp_df) <- paste0(colnames(temp_df), "_iv1")
temp_df$id <- cardata$id
cardata <- cardata %>% left_join(temp_df, by = c("id"))
rm(temp_df)

# 3rd set of instruments : sum of characteristics for products by all the other firms in the same market
#   technique : sum of characteristics of ALL products in the same market (temp1_df) - sum of characteristics of ALL products by the same firm in the same market (temp2_df)
temp1_df <- group_by(cardata, year) %>% 
  summarise_at(vars(CONSTANT, dpm, mpd, door3, door4, door5, at, ps, air, drv, wt, hp2wt, hp, euro, japan, size, wb), funs(sum))
temp1_df <- select(cardata, year, firmids) %>% left_join(temp1_df, by= c("year"))
temp2_df <- group_by(cardata, year, firmids) %>% 
  summarise_at(vars(CONSTANT, dpm, mpd, door3, door4, door5, at, ps, air, drv, wt, hp2wt, hp, euro, japan, size, wb), funs(sum))
temp2_df <- select(cardata, year, firmids) %>% left_join(temp2_df, by= c("year", "firmids"))
temp1_df <- select(temp1_df - temp2_df, -year, -firmids)
colnames(temp1_df) <- paste0(colnames(temp1_df), "_iv2")
temp1_df$id <- cardata$id
cardata <- cardata %>% left_join(temp1_df, by= c("id"))
rm(temp1_df, temp2_df)

# 4th set of instruments - nested logit - sum of characteristics for products by all the other firms in the same category, in the same market
#   technique : take sum of characteristics of ALL products in the same category g in the same market t, and substract characteristics of product jt
temp_df <- group_by(cardata, year, cat) %>% 
  summarise_at(vars(CONSTANT, dpm, mpd, door3, door4, door5, at, ps, air, drv, wt, hp2wt, hp, euro, japan, size, wb), funs(sum))
temp_df <- (select(cardata, year, cat) %>% left_join(temp_df, by= c("year", "cat"))) 
temp_df <- select(temp_df, -year, -cat)
temp_df <- temp_df - (select(cardata, CONSTANT, dpm, mpd, door3, door4, door5, at, ps, air, drv, wt, hp2wt, hp, euro, japan, size, wb))
colnames(temp_df) <- paste0(colnames(temp_df), "_iv4")
temp_df$id <- cardata$id
cardata <- cardata %>% left_join(temp_df, by = c("id"))
rm(temp_df)

