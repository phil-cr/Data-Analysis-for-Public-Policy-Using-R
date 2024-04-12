# Install and load the readr package
install.packages("readr")
library(readr)

# Specify the path to your large CSV file
input_file <- "path/to/your/large_file.csv"

# Get the size of the CSV file
file_size <- file_size(input_file)

# Define the chunk size (in bytes) - 5MB
chunk_size <- 5 * 1024 * 1024

# Calculate the number of chunks
num_chunks <- ceiling(file_size / chunk_size)

# Read and write the CSV file in chunks
for (i in 1:num_chunks) {
  # Calculate the starting row for this chunk
  start_row <- (i - 1) * chunk_size + 1
  
  # Read the chunk of data
  chunk <- read_csv(input_file, skip = start_row - 1, n_max = chunk_size)
  
  # Write the chunk to a separate CSV file
  output_file <- paste0("chunk_", i, ".csv")
  write_csv(chunk, output_file)
  
  # Print progress
  cat("Chunk", i, "written to", output_file, "\n")
}
