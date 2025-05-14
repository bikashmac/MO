# Load necessary libraries
libs <- c("GA", "dplyr", "ggplot2")
invisible(lapply(libs, function(pkg) if (!require(pkg, character.only = TRUE)) install.packages(pkg)))
lapply(libs, library, character.only = TRUE)

# Load dataset
df <- read.csv("smart_grid_energy_dataset.csv", row.names = 1)
df <- na.omit(df)

# Compute energy cost and demand deviation
df$energy_cost <- df$voltage * df$current * 0.01  # Approximate cost factor
df$demand_deviation <- abs(df$demand - mean(df$demand))  # Demand deviation

# Objective function based on the given equation
objective_function <- function(index) {
  index <- round(index)  # Ensure it's an integer
  
  if (length(index) > 1) {
    index <- index[1]  # Select the first index if multiple values exist
  }
  
  if (index < 1 || index > nrow(df)) return(Inf)  # Ensure index is within bounds
  
  C <- df$energy_cost[index]
  E <- df$power_co[index]
  P <- 0.05  # Example penalty coefficient
  D <- df$demand_deviation[index]
  
  return(-(C * E + P * D))  # Minimize total cost function
}

result <- ga(
  type = "real-valued",
  fitness = function(x) objective_function(x),
  lower = 1,
  upper = nrow(df),
  popSize = 200,
  pcrossover = 0.8,
  pmutation = 0.2,
  maxiter = 100,
  run = 50
)

# Extract best solution safely
best_index <- round(result@solution)[1]  # Select the first value if multiple exist
best_value <- objective_function(best_index)

# Display result
cat("Best Index:", best_index, "\n")
cat("Optimized Energy Cost:", best_value, "\n")
