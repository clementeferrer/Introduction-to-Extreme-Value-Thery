library(ggplot2)

### Figura 1

# Function to calculate the CDF of the maximum of n exponential random variables
F_Mn <- function(z, n, lambda) {
  (1 - exp(-lambda * z))^n
}

# Values
lambda <- 1  # You can change the lambda value
z_values <- seq(0, 10, by = 0.01)
n_values <- c(2, 5, 10, 20,50, 100,200,500,1000,5000,10000,50000)  # You can add more values of n

# Data frame to store results
df <- expand.grid(z = z_values, n = n_values)

# Calculate CDF values for each combination of n and z
df$F_Mn <- mapply(F_Mn, df$z, df$n, lambda)

# Plotting
ggplot(df, aes(x = z, y = F_Mn, color = factor(n))) +
  geom_line() +
  labs(title = "CDF of Maximum of Exponential RVs",
       x = "z",
       y = expression(F[italic(M[n])](z))) +
  scale_color_discrete(name = "n") +
  theme_bw()


### Figura 2

# Function to calculate the PDF of the maximum of n exponential random variables
f_Mn <- function(y, n, lambda) {
  n * (1 - exp(-lambda * y))^(n - 1) * lambda * exp(-lambda * y)
}

# Data frame to store results
df <- expand.grid(y = z_values, n = n_values)

# Calculate PDF values for each combination of n and y
df$f_Mn <- mapply(f_Mn, df$y, df$n, lambda)

# Plotting
ggplot(df, aes(x = y, y = f_Mn, color = factor(n))) +
  geom_line() +
  labs(title = "PDF of Maximum of Exponential RVs",
       x = "y",
       y = expression(f[italic(M[n])](y))) +
  scale_color_discrete(name = "n") +
  theme_bw()


### Figura 3

# Function to calculate the CDF of M*_n
CDF_M_star_n <- function(z, n, lambda, b_n) {
  (1 - exp(-lambda * z)/n)^n
}

# Values
b_n <- 2  # Arbitrary fixed constant, you can change it

# Data frame to store results
df <- expand.grid(z = z_values, n = n_values)

# Calculate CDF values for each combination of n and z
df$CDF_M_star_n <- mapply(CDF_M_star_n, df$z, df$n, lambda, b_n)

# Plotting
ggplot(df, aes(x = z, y = CDF_M_star_n, color = factor(n))) +
  geom_line() +
  labs(title = "CDF of normalized Maximum of Exponential RVs",
       x = "z",
       y = expression(F[italic(M[n])](z))) +
  scale_color_discrete(name = "n") +
  theme_bw()

### Figura Aux. Heavy tails example

# Definir las funciones de densidad
pdf_X <- function(x) {
  ifelse(x > exp(1), 1 / (x * log(x)), 0)
}

pdf_normal <- function(x) {
  dnorm(x)
}

pdf_pareto <- function(x, alpha = 2) {
  ifelse(x > exp(1), alpha * exp(1)^alpha / x^(alpha + 1), 0)
}

# Crear un dataframe con los valores de x y las densidades
df <- data.frame(x = x_values, 
                 Densidad_X = pdf_X(x_values),
                 Densidad_Normal = pdf_normal(x_values),
                 Densidad_Pareto = pdf_pareto(x_values, alpha = 2))

# Crear el gráfico usando ggplot2
ggplot(df, aes(x)) +
  geom_line(aes(y = Densidad_X, color = "Densidad Dada"), size = 1.5) +
  geom_line(aes(y = Densidad_Normal, color = "Normal"), linetype = "dashed", size = 1.5) +
  geom_line(aes(y = Densidad_Pareto, color = "Pareto"), linetype = "dotted", size = 1.5) +
  labs(title = "Comparación de Densidades",
       y = "Densidad",
       x = "x") +
  scale_color_manual(values = c("blue", "red", "green"),
                     labels = c("Densidad Dada", "Normal", "Pareto")) +
  theme_bw() +
  theme(legend.position = "top")


### Figura 4,5,6

# Function to calculate Gumbel distribution function
Gumbel <- function(z, a, b) {
  exp(-exp(-(z - a)/b))
}

# Function to calculate Frechet distribution function
Frechet <- function(z, a, b, gamma) {
  ifelse(z <= a, 0, exp(-((z - a)/b)^(-gamma)))
}

# Function to calculate Weibull distribution function
Weibull <- function(z, a, b, gamma) {
  ifelse(z < a, exp(-(-((z - a)/b))^gamma), 1)
}

# Values
z_values <- seq(-5, 5, by = 0.1)
a_values <- c(0, -2, 2)
b_values <- c(1, 0.5)
gamma_values <- c(1, 2)

# Create data frame for Gumbel
df_gumbel <- expand.grid(z = z_values, a = a_values, b = b_values)

# Calculate Gumbel values
df_gumbel$G <- mapply(Gumbel, df_gumbel$z, df_gumbel$a, df_gumbel$b)

# Create data frame for Frechet
df_frechet <- expand.grid(z = z_values, a = a_values, b = b_values, gamma = gamma_values)

# Calculate Frechet values
df_frechet$G <- mapply(Frechet, df_frechet$z, df_frechet$a, df_frechet$b, df_frechet$gamma)

# Create data frame for Weibull
df_weibull <- expand.grid(z = z_values, a = a_values, b = b_values, gamma = gamma_values)

# Calculate Weibull values
df_weibull$G <- mapply(Weibull, df_weibull$z, df_weibull$a, df_weibull$b, df_weibull$gamma)

# Gumbel Plot
ggplot(df_gumbel, aes(x = z, y = G, color = factor(a), linetype = factor(b))) +
  geom_line() +
  labs(title = "Gumbel Distribution",
       x = "z",
       y = "G(z)",
       color = "Parameter 'a'",
       linetype = "Parameter 'b'") +
  theme_bw()

# Frechet Plot
ggplot(df_frechet, aes(x = z, y = G, color = factor(a), linetype = interaction(factor(b), factor(gamma)), shape = factor(gamma))) +
  geom_line() +
  labs(title = "Frechet Distribution",
       x = "z",
       y = "G(z)",
       color = "Parameter 'a'",
       linetype = "Parameters 'b' and 'gamma'",
       shape = "Parameter 'gamma'") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")) +
  scale_color_manual(values = c("blue", "red", "green", "purple")) +
  theme_bw()

# Weibull Plot
ggplot(df_weibull, aes(x = z, y = G, color = factor(a), linetype = interaction(factor(b), factor(gamma)), shape = factor(gamma))) +
  geom_line() +
  labs(title = "Weibull Distribution",
       x = "z",
       y = "G(z)",
       color = "Parameter 'a'",
       linetype = "Parameters 'b' and 'gamma'",
       shape = "Parameter 'gamma'") +
  scale_linetype_manual(values = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")) +
  scale_color_manual(values = c("blue", "red", "green", "purple")) +
  theme_bw()

### Figura 7

# Function to calculate generalized extreme value distribution function with domain restriction
GEV <- function(z, mu, sigma, xi) {
  if (1 + xi * (z - mu) / sigma > 0) {
    if (xi == 0) {
      exp(-exp(-(z - mu) / sigma))
    } else {
      exp(- (1 + xi * ((z - mu) / sigma))^(-1 / xi))
    }
  } else {
    NaN  # Return NaN for values outside the domain
  }
}

# Values for mu and sigma
mu_values <- c(-5, 5)
sigma_values <- c(0.5, 1.5)

# Values for xi
xi_values <- c(-0.5, 0, 0.5)

# Create data frame for combinations of parameters
df_gev <- expand.grid(z = z_values, mu = mu_values, sigma = sigma_values, xi = xi_values)

# Calculate GEV values for each combination with domain restriction
df_gev$G <- mapply(GEV, df_gev$z, df_gev$mu, df_gev$sigma, df_gev$xi)

# Remove rows with NaN values (outside the domain)
df_gev <- df_gev[!is.nan(df_gev$G), ]

# Plotting with different linetypes and colors
ggplot(df_gev, aes(x = z, y = G, linetype = interaction(factor(mu), factor(sigma)), color = factor(xi))) +
  geom_line() +
  labs(title = "Generalized Extreme Value Distribution",
       x = "z",
       y = "G(z)",
       linetype = "Parameter 'mu' and 'sigma'",
       color = "Parameter 'xi'") +
  theme_bw()


### Figura 8
mu <- 0
sigma <- 1
p <- ppoints(1000)
yp <- -log(1 - p)
zp_1 <- mu - (sigma / 0.2) * (1 - yp^(-0.2))   # xi =  0.2
zp_2 <- mu - sigma * log(yp)                   # xi =   0
zp_3 <- mu - (sigma / -0.2) * (1 - yp^(0.2))   # xi = -0.2

df <- data.frame(p = 1/p, zp_1 = zp_1, zp_2 = zp_2, zp_3 = zp_3)

ggplot(df, aes(x = p)) +
  geom_line(aes(y = zp_1, color = "xi > 0"), size = 1) +
  geom_line(aes(y = zp_2, color = "xi == 0"), size = 1) +
  geom_line(aes(y = zp_3, color = "xi < 0"), size = 1) +
  scale_x_log10() +
  labs(title = "Return Level versus Time Period (1/p)",
       x = "Time Period (1/p)",
       y = "Return Level") +
  scale_color_manual(values = c("xi > 0" = "red", "xi == 0" = "blue", "xi < 0" = "green"),
                     labels = c(expression(xi < 0), expression(xi == 0), expression(xi > 0))) +
  theme_bw() 
  theme(legend.position = "top")
  
### Aplicación I
  
library(extRemes)  
data("PORTw", package = "extRemes")  

# Create a line plot using ggplot2
line_plot_PORTW <- ggplot(PORTw, aes(x = Year, y = TMX1)) +
  geom_line(color = "black", size = 1) +
  labs(x = "Year", y = "Maximum winter temperature") +
  theme_bw()

# Create a histogram using ggplot2
hist_plot_PORTW <- ggplot(data = PORTw, aes(x = TMX1)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(x = "Maximum Winter Temperature", y = "Frequency", title = "") +
  theme_bw()

library(patchwork)
line_plot_PORTW / hist_plot_PORTW

fit1 <- fevd(TMX1, PORTw, units = "deg C")
summary(fi1)
ci(fit1, type = "parameter")
ci(fit1, return.period = c(2, 20, 100))

par(mfrow = c(2, 2))
plot(fit1, type = c("probprob"), main = "PP")
plot(fit1, type = c("qq"), main = "QQ")
plot(fit1, type = c("density"), main = "Density")
plot(fit1, type = c("rl"), main = "Return level")

### Aplicación II

setwd("C:/Users/ccfer/OneDrive/Escritorio/Simulación")
temperature_data = read.csv("cp_temperature.csv",header=T)

# Annual Maximum Ave Temperature #
yr1 = 1951
yr2 = 2017
n = yr2 - yr1 + 1

annmax = matrix(NA,nrow=n,ncol=1)
for (i in 1:n)
{
  yr = 1950 + i
  index = which((temperature_data[,1] == yr))
  temperature_yr = temperature_data[index,4]
  annmax[i,1] = max(temperature_yr,na.rm=T)
}

annmax_df <- data.frame(Year = seq(yr1, yr2), Annual_Max_Temperature = annmax[, 1])

# Create a line plot using ggplot2
line_plot <- ggplot(data = annmax_df, aes(x = Year, y = Annual_Max_Temperature)) +
  geom_line(color = "black", size = 1) +
  labs(x = "Year", y = "Annual Maximum Temperature") +
  theme_bw()

# Create a histogram using ggplot2
hist_plot <- ggplot(data = data.frame(Annual_Max_Temperature = annmax), aes(x = Annual_Max_Temperature)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(x = "Annual Maximum Temperature", y = "Frequency", title = "") +
  theme_bw()

line_plot / hist_plot

fit_temperature = fevd(annmax,type="GEV")

summary(fit_temperature)
ci(fit_temperature, type = "parameter")
ci(fit_temperature, return.period = c(2, 20, 100))

par(mfrow = c(2, 2))
plot(fit_temperature, type = c("probprob"), main = "PP")
plot(fit_temperature, type = c("qq"), main = "QQ")
plot(fit_temperature, type = c("density"), main = "Density")
plot(fit_temperature, type = c("rl"), main = "Return level")

### Aplicación III

library(dplyr)

stgo_data <- read.csv("3525704.csv",header=TRUE)
stgo_data <- stgo_data %>% mutate(TMAX = (TMAX - 32) * 5/9)

# Create an empty vector to store max TMAX values for each year
max_tmax_values <- numeric()

# Iterate over each year from 1973 to 2023
for (year in 1973:2023) {
  # Extract the first four characters from DATE and convert to numeric
  stgo_data$year_extracted <- as.numeric(substr(stgo_data$DATE, 1, 4))
  
  # Filter the data for the current year
  year_data <- stgo_data %>% filter(year_extracted == year)
  
  # If there are rows for the current year and TMAX is not all NA, find and store the max TMAX value
  if (nrow(year_data) > 0 && !all(is.na(year_data$TMAX))) {
    max_tmax_values <- c(max_tmax_values, max(year_data$TMAX, na.rm = TRUE))
  } else {
    max_tmax_values <- c(max_tmax_values, NA)
  }
}

# Create Dataset
max_stgo <- data.frame(AÑO = seq(1973, 2023, by = 1), MAX = max_tmax_values)

# Create a line plot using ggplot2
line_plot_stgo <- ggplot(max_stgo, aes(x = AÑO, y = MAX)) +
  geom_line(color = "black", size = 1) +
  labs(x = "Year", y = "Annual Maximum Temperature") +
  theme_bw()

# Create a histogram using ggplot2
hist_plot_stgo <- ggplot(data = max_stgo, aes(x = MAX)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(x = "Annual Maximum Temperature", y = "Frequency", title = "") +
  theme_bw()

line_plot_stgo / hist_plot_stgo

fit_temperature_stgo = fevd(max_tmax_values,type="GEV")
summary(fit_temperature_stgo)
ci(fit_temperature_stgo, type = "parameter")
ci(fit_temperature_stgo, return.period = c(2, 20, 100))

par(mfrow = c(2, 2))
plot(fit_temperature_stgo, type = c("probprob"), main = "PP")
plot(fit_temperature_stgo, type = c("qq"), main = "QQ")
plot(fit_temperature_stgo, type = c("density"), main = "Density")
plot(fit_temperature_stgo, type = c("rl"), main = "Return level")


### Fig16 - Bayesian traces

fB <- fevd(TMX1, PORTw, method = "Bayesian")
plot(fB, "trace")
postmode(fB)

