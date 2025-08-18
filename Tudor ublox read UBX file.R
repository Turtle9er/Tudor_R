#
# This R script demonstrates how to read and parse a binary u-blox (.ubx) file.
# It searches for UBX messages, extracts their components (Class, ID, Payload),
# and prints the information for each message found.
#

# --- Configuration ---
ubx_filename <- file.choose()
#
# This R script demonstrates how to read and parse a binary u-blox (.ubx) file.
# It searches for UBX messages, extracts their components (Class, ID, Payload),
# and prints the information for each message found.
#

# --- Configuration ---
ubx_filename <- "assistnow.ubx" # The name of your .ubx file
UBX_SYNC1 <- as.raw(0xB5)
UBX_SYNC2 <- as.raw(0x62)

# --- Main Parsing Logic ---

# Check if the file exists before proceeding
if (!file.exists(ubx_filename)) {
  stop(paste("Error: File not found at", ubx_filename))
}

# Open a connection to the file in binary read mode ("rb")
con <- file(ubx_filename, "rb")

message_count <- 0

tryCatch({
  # Loop through the file byte by byte until the end
  while (TRUE) {
    # Read one byte (as raw data)
    byte1 <- readBin(con, "raw", n = 1)
    
    # readBin returns a zero-length vector at the end of the file
    if (length(byte1) == 0) {
      break # End of file
    }
    
    # Check for the first sync character
    if (byte1 == UBX_SYNC1) {
      # Read the next byte to check for the second sync character
      byte2 <- readBin(con, "raw", n = 1)
      
      if (length(byte2) == 0) {
        break # End of file
      }
      
      if (byte2 == UBX_SYNC2) {
        # --- Found a UBX Message Header ---
        message_count <- message_count + 1
        
        # Read the Class and ID (1 byte each)
        msg_class <- readBin(con, "integer", n = 1, size = 1, signed = FALSE)
        msg_id <- readBin(con, "integer", n = 1, size = 1, signed = FALSE)
        
        # Read the 2-byte little-endian length
        len_bytes <- readBin(con, "integer", n = 2, size = 1, signed = FALSE)
        payload_length <- len_bytes[1] + len_bytes[2] * 256
        
        # Read the payload based on the calculated length
        payload <- readBin(con, "raw", n = payload_length)
        
        # Read the 2-byte checksum
        checksum <- readBin(con, "raw", n = 2)
        
        # Print the details of the found message
        cat(sprintf(
          "--- Message %d ---\n",
          message_count
        ))
        cat(sprintf(
          "Class: 0x%02X, ID: 0x%02X\n",
          msg_class,
          msg_id
        ))
        cat(sprintf("Payload Length: %d bytes\n", payload_length))
        
        # --- Payload Parsing Logic ---
        # Check if this is an MGA-ANO (AssistNow Offline) message
        if (msg_class == 0x13 && msg_id == 0x20) {
          # The payload for MGA-ANO contains time information.
          # R is 1-based, so we access bytes by their position.
          # Year is 2 bytes, little-endian, at positions 5 and 6.
          payload_int <- as.integer(payload)
          msg_year <- payload_int[5] + 2000
          # Month is at position 6
          msg_month <- payload_int[6]
          # Day is at position 7
          msg_day <- payload_int[7]
          
          cat(sprintf(
            "MGA-ANO Date: %d-%02d-%02d\n",
            msg_year,
            msg_month,
            msg_day
          ))
          payload_preview <- paste(sprintf("0x%02X", as.integer(head(payload, 16))), collapse = " ")
          cat(sprintf("Payload Preview: %s ...\n\n", payload_preview))
          break;
        }
        
        # Optionally print the first few bytes of the payload
        payload_preview <- paste(sprintf("0x%02X", as.integer(head(payload, 16))), collapse = " ")
        cat(sprintf("Payload Preview: %s ...\n\n", payload_preview))
        
      }
    }
  }
  
}, finally = {
  # Ensure the file connection is closed, even if an error occurs
  close(con)
  cat(sprintf("Parsing complete. Found %d messages.\n", message_count))
})

