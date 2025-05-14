# Load required libraries
library(dplyr)
library(ggplot2)

# Load dataset with error handling
df <- tryCatch(
  read.csv("smart_grid_energy_dataset.csv"),
  error = function(e) stop("Error loading dataset: Ensure 'smart_grid_energy_dataset.csv' exists and has correct column names.")
)

# Verify required columns
required_cols <- c("time", "voltage", "current", "demand")
missing_cols <- setdiff(required_cols, colnames(df))
if (length(missing_cols) > 0) {
  stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
}

# Normalize relevant features
df <- df %>%
  mutate(norm_voltage = scale(voltage),
         norm_current = scale(current),
         norm_demand = scale(demand),
         loss = runif(n(), 0, 0.2))  # Simulated network loss

# Fitness function for optimization
evaluate_fitness <- function(chromosome) {
  chromosome$congestion_score <- (0.5 / (chromosome$norm_voltage + 1e-5)) +
    (0.3 * chromosome$loss) +
    (0.2 * chromosome$norm_demand)
  return(sum(chromosome$congestion_score))
}

# Simulated Annealing Optimization (Single Run)
run_sa <- function(data, max_iter = 50) {
  fitness_history <- numeric()
  
  objective <- function(x) {
    chromosome <- data.frame(
      edge = paste0("Link_", 1:6),
      norm_voltage = x[1:6],
      norm_current = x[7:12],
      loss = x[13:18],
      norm_demand = x[19:24]
    )
    fitness <- evaluate_fitness(chromosome)
    fitness_history <<- c(fitness_history, fitness)
    return(fitness)
  }
  
  result <- optim(par = runif(24, 0, 1),
                  fn = objective,
                  method = "SANN",
                  control = list(maxit = max_iter, trace = TRUE))
  
  optimized <- data.frame(
    edge = paste0("Link_", 1:6),
    norm_voltage = result$par[1:6],
    norm_current = result$par[7:12],
    loss = result$par[13:18],
    norm_demand = result$par[19:24],
    fitness = tail(fitness_history, 1)
  )
  
  return(list(solution = optimized, fitness = fitness_history))
}

set.seed(123)
sa_result <- run_sa(df, max_iter = 50)

# Display optimized solution
print(sa_result$solution)

