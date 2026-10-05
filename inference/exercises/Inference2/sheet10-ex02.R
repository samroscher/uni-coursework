# statistical inference 2
# sheet 10 exercise 10.2
# Topic: Spatial Models


library(sp)
library(gstat)
library(ggplot2)
library(corrplot)



# ---- (a) ----

data(meuse)
data(meuse.grid)

corrplot(
  cor(meuse[, 1:6]),
  method = "color",
  type = "upper",
  tl.cex = 0.8
)

ggplot(meuse, aes(x = dist.m, y = lead)) +
  geom_point() +
  geom_smooth(method = "lm") +
  geom_smooth(method = "loess") +
  labs(
    x = "Distance to river bank (m)",
    y = "Lead concentration (mg/kg)"
  )

ggplot(meuse, aes(x = x, y = y, color = log(lead))) +
  geom_point() +
  scale_color_viridis_c() +
  labs(
    x = "X coordinate",
    y = "Y coordinate",
    color = "Log lead concentration"
  )

meuse_df <- meuse
meuse_grid_df <- meuse.grid

coordinates(meuse) <- ~ x + y
coordinates(meuse.grid) <- ~ x + y
gridded(meuse.grid) <- TRUE



# ---- (b) ----

meuse_df$log_lead <- log(meuse_df$lead)
meuse$log_lead <- meuse_df$log_lead

lm_dist <- lm(
  log_lead ~ dist,
  data = meuse_df
)

summary(lm_dist)

lm_xy <- lm(
  log_lead ~ x + y,
  data = meuse_df
)

summary(lm_xy)

meuse_df$resid_dist <- residuals(lm_dist)
meuse_df$resid_xy <- residuals(lm_xy)

meuse$resid_dist <- meuse_df$resid_dist
meuse$resid_xy <- meuse_df$resid_xy

plot(
  meuse_df$log_lead,
  meuse_df$resid_dist,
  xlab = "Log lead concentration",
  ylab = "Residuals"
)

plot(
  meuse_df$log_lead,
  meuse_df$resid_xy,
  xlab = "Log lead concentration",
  ylab = "Residuals"
)

ggplot(meuse_df, aes(x = x, y = y, color = resid_dist)) +
  geom_point() +
  scale_color_viridis_c()

ggplot(meuse_df, aes(x = x, y = y, color = resid_xy)) +
  geom_point() +
  scale_color_viridis_c()



# ---- (d) ----

variogram_lead <- variogram(
  resid_dist ~ 1,
  meuse
)

variogram_fit <- fit.variogram(
  variogram_lead,
  model = vgm(
    psill = NA,
    model = "Sph",
    range = NA,
    nugget = NA
  )
)

plot(
  variogram_lead,
  model = variogram_fit
)

kriging_fit <- krige(
  log_lead ~ 1,
  meuse,
  newdata = meuse.grid,
  model = variogram_fit
)

spplot(
  kriging_fit["var1.pred"],
  main = "Kriged values"
)

meuse.grid$pred_dist <- predict(
  lm_dist,
  newdata = meuse_grid_df
)

meuse.grid$pred_xy <- predict(
  lm_xy,
  newdata = meuse_grid_df
)

meuse.grid$pred_kriging <- kriging_fit$var1.pred

grid_df <- as.data.frame(meuse.grid)

ggplot(grid_df, aes(x = x, y = y, color = pred_dist)) +
  geom_point() +
  scale_color_viridis_c()

ggplot(grid_df, aes(x = x, y = y, color = pred_xy)) +
  geom_point() +
  scale_color_viridis_c()

ggplot(grid_df, aes(x = x, y = y, color = pred_kriging)) +
  geom_point() +
  scale_color_viridis_c()

ggplot(
  grid_df,
  aes(x = x, y = y, color = abs(pred_dist - pred_kriging))
) +
  geom_point() +
  scale_color_viridis_c()

ggplot(
  grid_df,
  aes(x = x, y = y, color = abs(pred_xy - pred_kriging))
) +
  geom_point() +
  scale_color_viridis_c()



# ---- (e) ----

meuse_df$pred_dist <- predict(
  lm_dist,
  newdata = meuse_df
)

meuse_df$pred_xy <- predict(
  lm_xy,
  newdata = meuse_df
)

cv_kriging <- krige.cv(
  log_lead ~ 1,
  meuse,
  model = variogram_fit
)

mse <- c(
  distance = mean((meuse_df$log_lead - meuse_df$pred_dist)^2),
  coordinates = mean((meuse_df$log_lead - meuse_df$pred_xy)^2),
  kriging = mean(cv_kriging$residual^2)
)

mae <- c(
  distance = mean(abs(meuse_df$log_lead - meuse_df$pred_dist)),
  coordinates = mean(abs(meuse_df$log_lead - meuse_df$pred_xy)),
  kriging = mean(abs(cv_kriging$residual))
)

performance <- data.frame(
  MSE = mse,
  MAE = mae
)

performance
