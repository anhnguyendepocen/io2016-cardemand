### BLP instruments are collinear (near or perfectly). This script is to detect the near collinearity and choose which instruments to keep
## Choice rule (ad hoc) : keep only 1 instrument from any set that has a correlation coefficient higher than 0.9
P <- as.matrix(select(cardata, p_adj)) # endogeneous variable
options(digits=2)

Z1 <- as.matrix(select(cardata, CONSTANT_iv1, mpd_iv1, door3_iv1, door4_iv1, door5_iv1, at_iv1, ps_iv1, air_iv1, drv_iv1, wt_iv1, hp2wt_iv1, hp_iv1, euro_iv1, japan_iv1, wb_iv1, size_iv1))
cor(Z1) > 0.9
# Highly correlated : CONSTANT_iv1, mpd_iv1, door4_iv1, at_iv1, ps_iv1, wt_iv1, hp2wt_iv1, hp_iv1, wb_iv1, size_iv1
# keep CONSTANT_iv1
Z1 <- as.matrix(select(cardata, CONSTANT_iv1, door3_iv1, door5_iv1, air_iv1, drv_iv1, euro_iv1, japan_iv1))
cor(Z1)
cor(P, Z1) 

Z2 <- as.matrix(select(cardata, CONSTANT_iv2, mpd_iv2, door3_iv2, door4_iv2, door5_iv2, at_iv2, ps_iv2, air_iv2, drv_iv2, wt_iv2, hp2wt_iv2, hp_iv2, euro_iv2, japan_iv2, wb_iv2, size_iv2))
cor(Z2) > 0.9
# Highly correlated : CONSTANT_iv2,  wt_iv2, hp_iv2, wb_iv2, hp2wt_iv2, size_iv2
#keep CONSTANT_iv2
Z2 <- as.matrix(select(cardata, CONSTANT_iv2, mpd_iv2, door3_iv2, door4_iv2, door5_iv2, at_iv2, ps_iv2, air_iv2, drv_iv2,  euro_iv2, japan_iv2))
cor(Z2) > 0.9
# Highly correlated : at_iv2, ps_iv2
# keep at_iv2
Z2 <- as.matrix(select(cardata, CONSTANT_iv2, mpd_iv2,door3_iv2, door4_iv2, door5_iv2, at_iv2, air_iv2, drv_iv2,  euro_iv2, japan_iv2))
# Highly correlated : door5_iv2, drv_iv2
# keep drv_iv2 
Z2 <- as.matrix(select(cardata, CONSTANT_iv2, mpd_iv2,door3_iv2, door4_iv2, at_iv2, air_iv2, drv_iv2,  euro_iv2, japan_iv2))
cor(Z2)
cor(P, Z2)

Z <- cbind(Z1, Z2)
cor(Z) > 0.9
cor(Z) < -0.9

# Highly correlated : CONSTANT_iv1, door4_iv2 -0.968
#  keep CONSTANT_iv1
Z2 <- as.matrix(select(cardata, CONSTANT_iv2, mpd_iv2, door3_iv2, at_iv2, air_iv2, drv_iv2,  euro_iv2, japan_iv2))
Z <- cbind(Z1, Z2)
ZZ <- t(Z) %*% Z
solve(ZZ)
rm(Z, ZZ)

Z0 <- as.matrix(select(cardata, mpd, door3, door4, door5, at, ps, air, drv, wt, hp2wt, hp, euro, japan,  wb, size))
cor(Z0) > 0.9
cor(Z0) < -0.9
# highly correlated : wt, wb, size
# Keep size, Drop wb and wt : wb is wheelbase, and wt is weight. Most mass production cars are pretty standard along these dimensions, and consumer choice is mostly based on the size of the car, not the weight. The wheelbase however may be relevant, especially for city drivers concerned with parking space, but size is still a good overall indication for this concern, and it is the US of A, ain't no space problem over there.
Z0 <- as.matrix(select(cardata, mpd, door3, door4, door5, at, ps, air, drv, hp, hp2wt, euro, japan, size))
ZZ <- t(Z0) %*% Z0
solve(ZZ) #invertible

Z <- cbind(Z0, Z1)
ZZ <- t(Z) %*% Z
solve(ZZ) #invertible

Z <- cbind(Z0, Z2)
ZZ <- t(Z) %*% Z
solve(ZZ) #invertible

Z <- cbind(Z1, Z2)
ZZ <- t(Z) %*% Z
solve(ZZ)

# ONE BY ONE check in Z2
coll <- lm(data = cardata, japan_iv2 ~ CONSTANT_iv2 + euro_iv2 + CONSTANT_iv1 + euro_iv1 + japan_iv1)
summary(coll)

coll <- lm(data = cardata, euro_iv2 ~  CONSTANT_iv2 + japan_iv2 + CONSTANT_iv1 + euro_iv1 + japan_iv1)
summary(coll)

coll <- lm(data = cardata, euro_iv2*japan_iv2 ~  CONSTANT_iv2 + japan_iv2 + CONSTANT_iv1 + euro_iv1 + japan_iv1)
summary(coll)
# conclusion : japan_iv2 and euro_iv2 are both very highly collinear with CONSTANT_iv2 + CONSTANT_iv1 + euro_iv1 + japan_iv1

# excluding euro_iv2/japan_iv2
Z2 <- as.matrix(select(cardata, CONSTANT_iv2, mpd_iv2, door3_iv2, at_iv2, air_iv2, drv_iv2))
Z <- cbind(Z0, Z1, Z2)
ZZ <- t(Z) %*% Z
solve(ZZ) # INVERTIBLE
Matrix::rankMatrix(ZZ)
dim(ZZ)
# ZZ is of full rank.



# Nested Logit instruments
Z4 <- as.matrix(select(cardata, CONSTANT_iv4, mpd_iv4, door3_iv4, door4_iv4, door5_iv4, at_iv4, ps_iv4, air_iv4, drv_iv4, wt_iv4, hp2wt_iv4, hp_iv4, euro_iv4, japan_iv4, wb_iv4, size_iv4))
cor(Z4) > 0.9
# remove hp2wt_iv4 wb_iv4 door4_iv4 door5_iv4 door3_iv4 at_iv4 ps_iv4 wt_iv4 hp_iv4
Z4 <- as.matrix(select(cardata, CONSTANT_iv4, mpd_iv4, air_iv4, euro_iv4,  japan_iv4, size_iv4))
cor(Z4) > 0.9
cor(Z4) < -0.9

solve(t(Z4) %*% Z4) #invertible

# 1 2 3 and 4
Z <- cbind(Z0, Z1, Z2, Z4)
ZZ <- t(Z) %*% Z
solve(ZZ) # INVERTIBLE
Matrix::rankMatrix(ZZ)
dim(ZZ)


### TAKE AWAY :  Instrumental variables to keep from set 2 and 3 and 4 : CONSTANT_iv1, air_iv1, door3_iv1, door5_iv1, drv_iv1, euro_iv1, japan_iv1, CONSTANT_iv2, air_iv2, mpd_iv2, door3_iv2, at_iv2, drv_iv2, CONSTANT_iv4, air_iv4, mpd_iv4, size_iv4, euro_iv4, japan_iv4
