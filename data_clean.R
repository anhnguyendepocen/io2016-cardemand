data_car <- read.csv('data/carpanel.csv', sep= ";")
years <- dplyr::distinct(data_car, year) #years is a dataframe with years

### Adjusted gas price
data_gasprice <-  read.csv('data/gasprice.csv', sep = ";")
data_gasprice <- dplyr::filter(data_gasprice, year %in% years$year)
data_cpi <- read.csv('data/USCPI.csv', sep = ";")
data_gasprice <-  dplyr::left_join(data_gasprice, data_cpi, by = "year")  %>%
  dplyr::mutate(gasprice_adj = gasprice*100/cpi)
#data_gasprice$gasprice_adj <- data_gasprice$gasprice*100/data_gasprice$cpi

data <- data_car %>% left_join(data_gasprice, by = "year") 

# dollars per mile = gallons per mile * dollar price of gallon = (1/miles per gallon)* adj gas price
data$dpm = (1/data$mpg)*data$gasprice_adj

# Adjusted car price
data <- data %>%
  mutate(p_adj = p*100/cpi)

# door dummies
data <- data %>% bind_cols(as_data_frame(setNames(lapply(unique(data$dr), 
                                                         function(x){as.integer(data$dr == x)}), 
                                                  paste0('door', unique(data$dr)))))
# year dummies

data <- data %>% bind_cols(as_data_frame(setNames(lapply(unique(data$year), 
                                                         function(x){as.integer(data$year == x)}), 
                                                  paste0('year', unique(data$year)))))

# brand dummies
data <- data %>% bind_cols(as_data_frame(setNames(lapply(unique(data$firmids), 
                                               function(x){as.integer(data$firmids == x)}), 
                                        paste0('firm', unique(data$firmids)))))

# Market shares
data_market <- read.csv('data/UShouseholds.csv', sep = ";")
data <- data %>% left_join(data_market, by = "year")
data$nb_hh <- data$nb_hh*1000 #number of household now by unit (previously in thousands of households)

data$s = data$q/data$nb_hh

# Market share of the outside option
data <- data %>% 
  select(-yr) %>%
  arrange(year,firmids, id) %>%
  left_join(summarize(group_by(data, year), st = sum(s)), by = "year") %>%
  mutate(s0 = 1- st) %>%
  mutate(ls_ls0 = log(s) - log(s0))


# Market share within group
data <- data %>% left_join(group_by(data, year, cat) %>% summarise(q_cat = sum(q)), by= c("year", "cat")) %>% mutate(s_cat = q/q_cat)

head(data)

