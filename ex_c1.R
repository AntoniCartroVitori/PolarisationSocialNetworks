# Codi Exercici c

# Per replicar els mateixos resultats establim una seed
set.seed(123)


# Paràmetres
N <- 10000
T <- 15

# Definim les funcions de canvi d'opinió
left_shift <- function(x) x - (1 + x) / 4
right_shift <- function(x) x + (1 - x) / 4

# Definim les funcions de probabilitat de cada regla
# Les opinions per contingut de dreta es fan restant a 1
# les probabilitats per contingut d'esquerra, respectivament
# Probability functions for each rule
r1_left <- function(x) (1 - x)^2 / ((1 + x)^2 + (1 - x)^2)
r2_left <- function(x) (1 - x) / 2
r3_left <- function(x) (1 + x) / 2


# Funcions per simular cada escenari
simulate_r1 <- function(x0) {
  x <- matrix(NA, nrow = N, ncol = T + 1)
  x[, 1] <- x0
  for (t in 1:T) {
    p_left <- r1_left(x[, t])
    choices <- runif(N) < p_left
    x[, t + 1] <- ifelse(choices, left_shift(x[,t]), right_shift(x[,t]))
  }
  return(x[, T + 1])
}

simulate_r2 <- function(x0) {
  x <- matrix(NA, nrow = N, ncol = T + 1)
  x[, 1] <- x0
  for (t in 1:T) {
    p_left <- r2_left(x[, t])
    choices <- runif(N) < p_left
    x[, t + 1] <- ifelse(choices, left_shift(x[,t]), right_shift(x[,t]))
  }
  return(x[, T + 1])
}

simulate_r3 <- function(x0) {
  x <- matrix(NA, nrow = N, ncol = T + 1)
  x[, 1] <- x0
  for (t in 1:T) {
    p_left <- r3_left(x[, t])
    choices <- runif(N) < p_left
    x[, t + 1] <- ifelse(choices, left_shift(x[,t]), right_shift(x[,t]))
  }
  return(x[, T + 1])
}


# Distribucions que segueixen inicialment les opinions
# S1: Uniforme [-1, 1]
s1_x0 <- runif(N, min = -1, max = 1)
# S2: Normal Truncada N(0, 1/13) entre [-1, 1]
s2_x0 <- rnorm(N, mean = 0, sd = sqrt(1/13))

# Simulem cada regla i escenari
simulation_s1_r1 <- simulate_r1(s1_x0)
simulation_s1_r2 <- simulate_r2(s1_x0)
simulation_s1_r3 <- simulate_r3(s1_x0)
simulation_s2_r1 <- simulate_r1(s2_x0)
simulation_s2_r2 <- simulate_r2(s2_x0)
simulation_s2_r3 <- simulate_r3(s2_x0)

library(gridExtra)
# Creem un tipus de visualització per les taula
theme <- ttheme_minimal(
  core = list(
    fg_params = list(
      fontface = "plain", 
      cex = 1  # Text size
    ),
    bg_params = list(
      fill = c("white", "lightgray", "white", "lightgray", "white"),  # Different colors for rows
      alpha = 0.5  # Transparency of background colors
    ),
    linecolor = "black",  # Border color
    lwd = 5  # Border line width (thin)
  ),
  header = list(
    fg_params = list(
      fontface = "bold",  # Make header text bold
      cex = 1  
    ),
    bg_params = list(
      fill = "gray",  # Background color for header
      alpha = 0.5  # Transparency for header
    ),
    linecolor = "black",  # Border color for header
    lwd = 5  # Border width for header
  )
)

# Computem les mitjanes i variàncies de les distribucions computades
mean_s1_r1 <- round(mean(simulation_s1_r1), digits=4)
mean_s1_r2 <- round(mean(simulation_s1_r2), digits=4)
mean_s1_r3 <- round(mean(simulation_s1_r3), digits=4)
mean_s2_r1 <- round(mean(simulation_s2_r1), digits=4)
mean_s2_r2 <- round(mean(simulation_s2_r2), digits=4)
mean_s2_r3 <- round(mean(simulation_s2_r3), digits=4)

means <- data.frame(
  "Mean S1:R1" = mean_s1_r1,
  "Mean S1:R2" = mean_s1_r2,
  "Mean S1:R3" = mean_s1_r3,
  "Mean S2:R1" = mean_s2_r1,
  "Mean S2:R2" = mean_s2_r2,
  "Mean S2:R3" = mean_s2_r3
)
colnames(means) <- gsub("\\.", " ", colnames(means))
pdf("Means.pdf", width = 12.5, height = 1)
grid.table(means, theme = theme)
dev.off()


var_s1_r1 <- round(var(simulation_s1_r1), digits=4)
var_s1_r2 <- round(var(simulation_s1_r2), digits=4)
var_s1_r3 <- round(var(simulation_s1_r3), digits=4)
var_s2_r1 <- round(var(simulation_s2_r1), digits=4)
var_s2_r2 <- round(var(simulation_s2_r2), digits=4)
var_s2_r3 <- round(var(simulation_s2_r3), digits=4)

variances <- data.frame(
  "Var S1:R1" = var_s1_r1,
  "Var S1:R2" = var_s1_r2,
  "Var S1:R3" = var_s1_r3,
  "Var S2:R1" = var_s2_r1,
  "Var S2:R2" = var_s2_r2,
  "Var S2:R3" = var_s2_r3
)
colnames(variances) <- gsub("\\.", " ", colnames(variances))
pdf("Variances.pdf", width = 12.5, height = 1)
grid.table(variances, theme = theme)
dev.off()

# Creem els histogrames i els guardem en PDF
pdf("Opinion_Histograms.pdf", width = 10, height = 6)  # Open PDF device, set size
par(mfrow = c(2, 3))  # 2 rows (S1, S2), 3 columns (R1, R2, R3)
hist(simulation_s1_r1,
     main = "S1 : R1 (Reinforcement)",
     xlab = "Opinions",
     ylab = "Frequency",
     breaks = 50,
     col = "red",
     border = "black",
     xlim = c(-1, 1),
     ylim = c(0, N/2)
)

hist(simulation_s1_r2,
     main = "S1 : R2 (Fair)",
     xlab = "Opinions",
     ylab = "Frequency",
     breaks = 50,
     col = "green",
     border = "black",
     xlim = c(-1, 1),
     ylim = c(0, N/3)
)

hist(simulation_s1_r3,
     main = "S1 : R3 (Opposite)",
     xlab = "Opinions",
     ylab = "Frequency",
     breaks = 50,
     col = "blue",
     border = "black",
     xlim = c(-1, 1),
     ylim = c(0, N/10)
)

hist(simulation_s2_r1,
     main = "S2 : R1 (Reinforcement)",
     xlab = "Opinions",
     ylab = "Frequency",
     breaks = 50,
     col = "red",
     border = "black",
     xlim = c(-1, 1),
     ylim = c(0, N/2)
)

hist(simulation_s2_r2,
     main = "S2 : R2 (Fair)",
     xlab = "Opinions",
     ylab = "Frequency",
     breaks = 50,
     col = "green",
     border = "black",
     xlim = c(-1, 1),
     ylim = c(0, N/4)
)

hist(simulation_s2_r3,
     main = "S2 : R3 (Opposite)",
     xlab = "Opinions",
     ylab = "Frequency",
     breaks = 50,
     col = "blue",
     border = "black",
     xlim = c(-1, 1),
     ylim = c(0, N/10)
)

dev.off()