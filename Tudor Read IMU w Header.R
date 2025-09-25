# R script to read and parse binary IMU data from the logger.
#
# The binary file is structured in blocks, where each block contains:
# 1. A 4-byte unsigned integer for the timestamp (milliseconds).
# 2. 256 consecutive IMU samples. Each sample consists of three 2-byte 
#    signed integers (X, Y, Z axes).
# Total block size = 4 + (256 * 3 * 2) = 1540 bytes.

filepath <- file.choose()
fifo_size <- 340

  # Check if the file exists
  if (!file.exists(filepath)) {
    stop("Error: File not found at the specified path.")
  }
  
  # Open the file for binary reading
  con <- file(filepath, "rb")
  
  # Lists to store the parsed data from each block
  all_timestamps <- list()
  all_samples <- list()
  imu_data <- data.frame()
  header <- list()
  
  block_count <- 0
  
  tryCatch({
   
    
    # 1. Read file_type: char[4]
    # We read 4 raw bytes and convert them back to a character string.
    # We use trimws() to remove any trailing null characters for cleaner display.
    header$file_type <- trimws(rawToChar(readBin(con, "raw", n = 4)))
    
    # 2. Read the four uint16_t values (version, sample_rate_hz, etc.)
    # These are 2-byte, unsigned integers. We can read all four at once.
    # Endianness should match how the file was written ("little" is common).
    int_values <- readBin(con, "integer", n = 4, size = 2, signed = FALSE, endian = "little")
    
    header$version <- int_values[1]
    header$sample_rate_hz <- int_values[2]
    header$fifo_size_samples <- int_values[3]
    header$unit_number <- int_values[4]
    
    # 3. Read the reserved padding: char[20]
    # We read this as raw data, as its content is not meant to be interpreted.
    header$reserved <- readBin(con, "raw", n = 20)
    while(TRUE) {
      
      # 1. Read the 4-byte timestamp
      # readBin returns a vector; we take the first element.
      # It will return an empty vector at the end of the file.
      timestamp <- readBin(con, "integer", n = 1, size = 4)
      tmp <- readBin(con, "integer", n = 1, size = 4)
      
      # If timestamp is empty, we've reached the end of the file
      if (length(timestamp) == 0) {
        break
      }
      
      # 2. Read the 256 * 3 = 768 accelerometer values (int16)
      num_readings <- fifo_size * 3
      samples_raw <- readBin(con, "integer", n = num_readings, size = 2, signed = TRUE)
      
      # Check if we read a full block of sample data
      if (length(samples_raw) < num_readings) {
        warning("Incomplete block found at the end of the file. Discarding.")
        break
      }
      
      # Reshape the raw vector into a 256x3 matrix (samples x axes)
      samples_matrix <- matrix(samples_raw, nrow = fifo_size, ncol = 3, byrow = TRUE)
      colnames(samples_matrix) <- c("X", "Y", "Z")
      
      imu_data <-  bind_rows(imu_data , samples_matrix %>% as_tibble())
      
      # Store the results for this block
      all_timestamps[[length(all_timestamps) + 1]] <- timestamp

      
      block_count <- block_count + 1
    }
  }, finally = {
    # Ensure the file connection is closed
    close(con)
  })
  
  diff(unlist(all_timestamps))
  
  write.table(log_data, file="clipboard-16384", sep="\t", row.names=FALSE, col.names=T)

