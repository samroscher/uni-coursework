# statistical inference 2
# sheet 7 exercise 7.2
# Topic: Extreme Value Analysis


library(dplyr)
library(ggplot2)
library(extRemes)

data <- read.csv("Ex_7_2_precip_germany21.csv", header = TRUE)



# ---- (a) ----
# Plot daily precipitation and compute descriptive statistics.

hist(
  data$R,
  breaks = 1000,
  xlim = c(0, 15),
  freq = FALSE,
  xlab = "Recorded precipitation"
)

hist(
  log(data$R[data$R > 0]),
  breaks = 1000,
  xlim = c(-3, 6),
  freq = FALSE,
  xlab = "Log of recorded precipitation"
)

summary(data$R)
mean(data$R, na.rm = TRUE)
sd(data$R, na.rm = TRUE)



# ---- (b) ----
# Fit a GEV distribution to monthly precipitation maxima.

monthly_max <- data %>%
  filter(R >= 0) %>%
  group_by(YEAR, MONTH) %>%
  summarize(
    max_rain = max(R),
    .groups = "drop"
  )

hist(
  monthly_max$max_rain,
  breaks = 50,
  freq = FALSE,
  xlab = "Monthly maximum precipitation"
)

gev_fit <- fevd(
  monthly_max$max_rain,
  period.basis = "month",
  type = "GEV"
)

gev_fit$results$par

ggplot(monthly_max, aes(x = max_rain)) +
  geom_histogram(
    aes(y = after_stat(density)),
    color = "red"
  ) +
  stat_function(
    fun = devd,
    args = list(
      loc = gev_fit$results$par[1],
      scale = gev_fit$results$par[2],
      shape = gev_fit$results$par[3]
    ),
    color = "blue"
  )



# ---- (c) ----
# Plot yearly rainfall maxima and estimate a linear time trend.

yearly_max <- data %>%
  filter(R >= 0) %>%
  group_by(YEAR) %>%
  summarize(
    max_rain = max(R),
    .groups = "drop"
  )

plot(
  yearly_max$YEAR,
  yearly_max$max_rain,
  type = "p",
  xlab = "Year",
  ylab = "Yearly maximum precipitation"
)

intensity_model <- lm(
  max_rain ~ YEAR,
  data = yearly_max
)

abline(intensity_model)

summary(intensity_model)



# ---- (d) ----
# Define extreme rainfall using the global 95% quantile and count
# threshold exceedances per year.

quantile95 <- quantile(
  data$R[data$R != 0],
  0.95
)

extreme_days <- data %>%
  filter(R > quantile95) %>%
  group_by(YEAR) %>%
  summarize(
    n = n(),
    .groups = "drop"
  )

plot(
  extreme_days$YEAR,
  extreme_days$n,
  type = "p",
  xlab = "Year",
  ylab = "Number of extreme rainfall events"
)

frequency_model <- lm(
  n ~ YEAR,
  data = extreme_days
)

abline(frequency_model)

summary(frequency_model)


# Compare with yearly maximum intensity.
plot(
  yearly_max$YEAR,
  yearly_max$max_rain,
  type = "l",
  xlab = "Year",
  ylab = "Most extreme precipitation that year"
)

abline(intensity_model)

summary(intensity_model)
