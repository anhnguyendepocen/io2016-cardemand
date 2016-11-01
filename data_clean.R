cardata <- read.csv('data/carpanel.csv', sep= ";") %>%
  select(-yr) %>%
  arrange(year,firmids, id)


### Car models of BLP
temp <- arrange(cardata, firmids, name, id) %>% select(firmids, name, id, year, cat)
carmodels <- filter(temp, firmids == 1, name == "TYCORO") %>%  
  bind_rows(filter(temp, firmids == 18, name == "FDESCO")) %>%
  bind_rows(filter(temp, firmids == 19, name == "CVCHEV")) %>%
  bind_rows(filter(temp, firmids == 3, name == "HDACCO")) %>% 
  bind_rows(filter(temp, firmids == 19, name == "BKCENT")) %>%
  bind_rows(filter(temp, firmids == 18, name == "LCTWNC")) %>%
  bind_rows(filter(temp, firmids == 19, name == "CDSEVI")) %>%  
  bind_rows(filter(temp, firmids == 8, name == "BW733I")) %>%
  filter(year == 1981)
rm(temp)


### Adjusted gas price
cardata_gasprice <-  read.csv('data/gasprice.csv', sep = ";")
temp_yrs <- distinct(cardata, year)$year
cardata_gasprice <- filter(cardata_gasprice, year %in% temp_yrs)
rm(temp_yrs)
cardata_cpi <- read.csv('data/USCPI.csv', sep = ";")
cardata_gasprice <-  left_join(cardata_gasprice, cardata_cpi, by = "year")  %>%
  mutate(gasprice_adj = gasprice*100/cpi)
cardata <- cardata %>% left_join(cardata_gasprice, by = "year") 
rm(cardata_gasprice, cardata_cpi)

# miles per dollar = miles per gallon * dollar price of gallon
# dollars per mile = gallons per mile * dollar price of gallon = (1/miles per gallon)* adj gas price
# Adjusted car price
# CONSTANT

cardata <- cardata %>% mutate(dpm  =  (1/mpg)*gasprice_adj, 
                              mpd = 1/dpm,
                              p_adj = p*100/cpi,
                              CONSTANT = 1)
# Scaling to match BLP data
cardata <- cardata %>% mutate(p_adj = p_adj/1000, #price in 1000 cpi adjusted dollars
                              q = q/1000,
                              mpg = mpg/10,
                              mpd = mpd/10,
                              dpm = dpm*10,
                              wt = wt/1000, 
                              wb = wb/100,
                              hp = hp/100)


# Market shares 
# number of household now by unit (previously in thousands of households in original dataset)
cardata_market <- read.csv('data/UShouseholds.csv', sep = ";")
cardata <- cardata %>% left_join(cardata_market, by = "year")
cardata$s = cardata$q/cardata$nb_hh
rm(cardata_market)

# Market share of the outside option
cardata <- cardata %>% 
  left_join(summarize(group_by(cardata, year), st = sum(s)), by = "year") %>%
  mutate(s0 = 1- st) %>%
  mutate(ls_ls0 = log(s) - log(s0))


# Market share within nest (category) - for nested logit
cardata <- cardata %>% left_join(group_by(cardata, year, cat) %>% summarise(q_cat = sum(q)), by= c("year", "cat")) %>% mutate(s_cat = q/q_cat)

# Re-order columns
cardata <- cardata %>% select(year, firmids, name, id, cat, ls_ls0, s, s0, st, s_cat, q, q_cat, p, p_adj, CONSTANT, cy, dr, at, ps, air, drv, wt, dpm, mpd, disp, hp, lng, wdt, wb, mpg, euro, japan, dom, reli, dfi, hp2wt, size, nb_hh)

# door dummies
cardata <- cardata %>% bind_cols(as_data_frame(setNames(lapply(unique(cardata$dr), 
                                                         function(x){as.integer(cardata$dr == x)}), 
                                                  paste0('door', unique(cardata$dr)))))

# brand dummies
cardata <- cardata %>% bind_cols(as_data_frame(setNames(lapply(unique(cardata$firmids), 
                                               function(x){as.integer(cardata$firmids == x)}), 
                                        paste0('firm', unique(cardata$firmids)))))
