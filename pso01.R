# Load necessary libraries
libs <- c("pso", "dplyr", "ggplot2")
invisible(lapply(libs, function(pkg) if (!require(pkg, character.only = TRUE)) install.packages(pkg)))
lapply(libs, library, character.only = TRUE)

# Load dataset
df <- read.csv("smart_grid_energy_dataset.csv", row.names = 1)
df <- na.omit(df)

# Compute energy cost and demand deviation
df$energy_cost <- df$voltage * df$current * 0.01  # Approximate cost factor
df$demand_deviation <- abs(df$demand - mean(df$demand))  # Demand deviation

# Objective function based on f(x) = Σ (CiEi + PiDi)
objective_function <- function(x) {
  index <- round(x)  # Ensure it's an integer
  
  if (index < 1 || index > nrow(df)) return(Inf)  # Ensure index is within bounds
  
  C <- df$energy_cost[index]
  E <- df$power_co[index]
  P <- 0.05  # Example penalty coefficient
  D <- df$demand_deviation[index]
  
  return(-(C * E + P * D))  # Minimize total cost function
}

pso_result <- psoptim(
  par = c(1), 
  fn = objective_function,
  lower = 1, upper = nrow(df),
  control = list(maxit = 100, s = 50, w = 0.9)  # Explicit swarm size & inertia weight
)

# Extract best solution
best_index <- round(pso_result$par)[1]
best_value <- objective_function(best_index)

# Display result
cat("Best Index:", best_index, "\n")
cat("Optimized Energy Cost:", best_value, "\n")

