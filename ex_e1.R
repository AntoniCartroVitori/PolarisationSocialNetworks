# Codi Exercici e

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


# Funcions per simular cada escenari amb abandonament
simulate_with_abandonment_r1 <- function(x0) {
  x <- matrix(NA, nrow = N, ncol = T + 1)
  x[, 1] <- x0 
  active <- rep(TRUE, N)
  steps_survived <- rep(T, N)
  
  for (t in 1:T) {
    p_left <- r1_left(x[active, t])
    choices <- runif(sum(active)) < p_left
    
    x[, t + 1] <- x[, t]
    x[active, t + 1][choices] <- left_shift(x[active, t][choices])
    x[active, t + 1][!choices] <- right_shift(x[active, t][!choices])
    
    is_left <- choices
    aligns <- (is_left & x[active, t] < 0) | (!is_left & x[active, t] > 0) | (x[active, t] == 0)
    abandon_prob <- ifelse(aligns, 0.05, 0.25)
    abandons <- runif(sum(active)) < abandon_prob
    
    if (any(abandons)) {
      who_abandons <- which(active)[abandons]
      steps_survived[who_abandons] <- t
      active[who_abandons] <- FALSE
    }
  }
  return(steps_survived)
}


simulate_with_abandonment_r2 <- function(x0) {
  x <- matrix(NA, nrow = N, ncol = T + 1)
  x[, 1] <- x0 
  active <- rep(TRUE, N)
  steps_survived <- rep(T, N)
  
  for (t in 1:T) {
    p_left <- r2_left(x[active, t])
    choices <- runif(sum(active)) < p_left
    
    x[, t + 1] <- x[, t]
    x[active, t + 1][choices] <- left_shift(x[active, t][choices])
    x[active, t + 1][!choices] <- right_shift(x[active, t][!choices])
    
    is_left <- choices
    aligns <- (is_left & x[active, t] < 0) | (!is_left & x[active, t] > 0) | (x[active, t] == 0)
    abandon_prob <- ifelse(aligns, 0.05, 0.25)
    abandons <- runif(sum(active)) < abandon_prob
    
    if (any(abandons)) {
      who_abandons <- which(active)[abandons]
      steps_survived[who_abandons] <- t
      active[who_abandons] <- FALSE
    }
  }
  return(steps_survived)
}

simulate_with_abandonment_r3 <- function(x0) {
  x <- matrix(NA, nrow = N, ncol = T + 1)
  x[, 1] <- x0 
  active <- rep(TRUE, N)
  steps_survived <- rep(T, N)
  
  for (t in 1:T) {
    p_left <- r3_left(x[active, t])
    choices <- runif(sum(active)) < p_left
    
    x[, t + 1] <- x[, t]
    x[active, t + 1][choices] <- left_shift(x[active, t][choices])
    x[active, t + 1][!choices] <- right_shift(x[active, t][!choices])
    
    is_left <- choices
    aligns <- (is_left & x[active, t] < 0) | (!is_left & x[active, t] > 0) | (x[active, t] == 0)
    abandon_prob <- ifelse(aligns, 0.05, 0.25)
    abandons <- runif(sum(active)) < abandon_prob
    
    if (any(abandons)) {
      who_abandons <- which(active)[abandons]
      steps_survived[who_abandons] <- t
      active[who_abandons] <- FALSE
    }
  }
  return(steps_survived)
}


# Distribucions que segueixen inicialment les opinions
# S1: Uniforme [-1, 1]
s1_x0 <- runif(N, min = -1, max = 1)
# S2: Normal Truncada N(0, 1/13) entre [-1, 1]
s2_x0 <- rnorm(N, mean = 0, sd = sqrt(1/13))

# Simulem cada regla i escenari
simulation_steps_s1_r1 <- simulate_with_abandonment_r1(s1_x0)
simulation_steps_s1_r2 <- simulate_with_abandonment_r2(s1_x0)
simulation_steps_s1_r3 <- simulate_with_abandonment_r3(s1_x0)
simulation_steps_s2_r1 <- simulate_with_abandonment_r1(s2_x0)
simulation_steps_s2_r2 <- simulate_with_abandonment_r2(s2_x0)
simulation_steps_s2_r3 <- simulate_with_abandonment_r3(s2_x0)

# Creem els histogrames
pdf("Opinion_Histograms_2.pdf", width = 10, height = 6)  # Open PDF device, set size
par(mfrow = c(2, 3))  # 2 rows (S1, S2), 3 columns (R1, R2, R3)
hist(simulation_steps_s1_r1,
     main = "S1 : R1 (Reinforcement)",
     xlab = "Contents Consumed",
     ylab = "Frequency",
     breaks = 0:T,
     col = "red",
     border = "black",
     xlim = c(0, T),
     ylim = c(0, N/2)
)

hist(simulation_steps_s1_r2,
     main = "S1 : R2 (Fair)",
     xlab = "Contents Consumed",
     ylab = "Frequency",
     breaks = 0:T,
     col = "green",
     border = "black",
     xlim = c(0, T),
     ylim = c(0, N/2)
)
hist(simulation_steps_s1_r3,
     main = "S1 : R3 (Opposite)",
     xlab = "Contents Consumed",
     ylab = "Frequency",
     breaks = 0:T,
     col = "blue",
     border = "black",
     xlim = c(0, T),
     ylim = c(0, N/4)
)

hist(simulation_steps_s2_r1,
     main = "S2 : R1 (Reinforcement)",
     xlab = "Contents Consumed",
     ylab = "Frequency",
     breaks = 0:T,
     col = "red",
     border = "black",
     xlim = c(0, T),
     ylim = c(0, N/2)
)

hist(simulation_steps_s2_r2,
     main = "S2 : R2 (Fair)",
     xlab = "Contents Consumed",
     ylab = "Frequency",
     breaks = 0:T,
     col = "green",
     border = "black",
     xlim = c(0, T),
     ylim = c(0, N/3)
)

hist(simulation_steps_s2_r3,
     main = "S2 : R3 (Opposite)",
     xlab = "Contents Consumed",
     ylab = "Frequency",
     breaks = 0:T,
     col = "blue",
     border = "black",
     xlim = c(0, T),
     ylim = c(0, N/4)
)

dev.off()