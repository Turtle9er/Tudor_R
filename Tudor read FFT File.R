# R script to read and plot binary FFT data with timestamps from the Teensy SD card logger.

# --- Configuration ---
# This line will open a file browser dialog for you to select the binary file.
file_path <- file.choose()

# This MUST match the #define FFT_LENGTH in your C++ code
fft_length <- 2048
# The number of data points (frequency bins) in each saved PSD record
points_per_record <- fft_length / 2

# --- Read Binary File ---
# Open the file in binary read mode ("rb")
con <- file(file_path, "rb")

# Create empty lists to store timestamps and FFT records
all_timestamps <- list()
all_records <- list()

# Loop until the end of the file
tryCatch({
  while (TRUE) {
    # Read the 4-byte timestamp (unsigned integer)
    timestamp <- readBin(con, what = "integer", size = 4, n = 1, signed = FALSE)
    
    # If readBin returns an empty vector, we've likely reached the end of the file
    if (length(timestamp) == 0) {
      break
    }
    
    # Read one complete FFT record (1024 single-precision floats)
    record <- readBin(con, what = "numeric", size = 4, n = points_per_record)
    
    # Check if we successfully read a full record after the timestamp
    if (length(record) < points_per_record) {
      cat("Incomplete record found at the end of the file. Stopping.\n")
      break
    }
    
    # Add the data to our lists
    all_timestamps[[length(all_timestamps) + 1]] <- timestamp
    all_records[[length(all_records) + 1]] <- record
  }
}, finally = {
  # Make sure to close the connection, even if an error occurs
  close(con)
})


# --- Process Data ---
# Combine the lists into a final data structure
if (length(all_records) > 0) {
  # Create a matrix for the PSD data
  fft_data_matrix <- do.call(cbind, all_records)
  # Create a vector for the timestamps
  timestamps_vector <- unlist(all_timestamps)
  
  cat("Successfully read", ncol(fft_data_matrix), "FFT records.\n")
  cat("First timestamp (ms):", timestamps_vector[1], "\n")
  cat("Last timestamp (ms):", tail(timestamps_vector, 1), "\n")
  
  # --- Example: Plot the first FFT spectrum ---
  # The ODR of the IIS3DWB is 26.667 kHz. The max frequency is half of that.
  nyquist_frequency <- 26667 / 2
  # Calculate the frequency for each bin
  frequency_bins <- seq(0, nyquist_frequency, length.out = points_per_record)
  
  # Plot the first spectrum
  plot(frequency_bins[-1], fft_data_matrix[-c(1), 4], type = 'l', xlim = c(0, 2000),
       xlab = "Frequency (Hz)", ylab = "Magnitude", 
       main = paste("First FFT Spectrum (Timestamp:", timestamps_vector[1], "ms)"))
  
} else {
  cat("No data was read from the file. The file may be empty or the path may be incorrect.\n")
}

diff(unlist(all_timestamps))
  
