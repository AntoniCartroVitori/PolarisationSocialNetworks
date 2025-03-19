# Codi Exercici b

# Per replicar els mateixos resultats establim una seed
set.seed(123)


# Definim les opinions inicials
x0 <- c(-0.5, -0.2, 0, 0.2, 0.5)

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


# Agrupem valors i resultats
shifts <- list(
  l = sapply(x0, left_shift),
  r = sapply(x0, right_shift)
)

probabilities_rules <- list(
  r1l = sapply(x0, r1_left),
  r2l = sapply(x0, r2_left),
  r3l = sapply(x0, r3_left)
)

# Visualitzem els resultats en una taula
results_df <- data.frame(
  "Initial Opinions X0" = x0,
  "Left Change" = shifts$l,
  "Right Change" = shifts$r,
  "R1 Left Probability" = round(probabilities_rules$r1l, digits = 4),
  "R2 Left Probability" = round(probabilities_rules$r2l, digits = 4),
  "R3 Left Probability" = round(probabilities_rules$r3l, digits = 4)
)
colnames(results_df) <- gsub("\\.", " ", colnames(results_df))

library(gridExtra)
# Creem un tipus de visualització per la taula
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

pdf("One_Step_Simulation.pdf", width = 10, height = 3)
grid.table(results_df, theme = theme)
dev.off()
