# Load required library
if (!requireNamespace("stats", quietly = TRUE)) {
  install.packages("stats")
}

# Define the PDF of the generalized beta distribution
genbeta_pdf <- function(y, alpha, beta, lb, lb_offset) {
  x <- (y - lb) / lb_offset
  pdf_value <- (x^(alpha - 1)) * ((1 - x)^(beta - 1)) / (lb_offset * beta(alpha, beta))
  return(pdf_value)
}

# Function to generate random samples from the generalized beta distribution
genbeta_sample <- function(n, alpha, beta, lb, lb_offset) {
  # Initialize a vector to store samples
  samples <- numeric(n)
  count <- 0
  
  # Set up the rejection sampling loop
  while (count < n) {
    # Propose values between lb and lb + lb_offset
    y_proposal <- runif(1, lb, lb + lb_offset)
    u <- runif(1)  # Uniformly distributed random number for acceptance
    
    # Calculate acceptance probability
    pdf_val <- genbeta_pdf(y_proposal, alpha, beta, lb, lb_offset)
    max_pdf <- genbeta_pdf(lb + lb_offset / 2, alpha, beta, lb, lb_offset) # peak near center
    
    if (u < pdf_val / max_pdf) {
      # Accept the proposal
      count <- count + 1
      samples[count] <- y_proposal
    }
  }
  
  return(samples)
}

# Example usage
set.seed(42)
n <- 1000       # Number of samples
alpha <- 2      # Alpha parameter
beta <- 3       # Beta parameter
lb <- -.999        # Lower bound
lb_offset <- .999 - lb  # Offset

samples <- genbeta_sample(n, alpha, beta, lb, lb_offset)

# Plot the histogram of the samples to visualize the distribution
hist(samples, breaks = 30, probability = TRUE, main = "Simulated Generalized Beta Distribution", xlab = "y")
curve(genbeta_pdf(x, alpha, beta, lb, lb_offset), add = TRUE, col = "blue", lwd = 2)
