library(tidyverse)

# --- Define Constants ---
WHEEL_CIRCUMFERENCE_M <- 2.2
SAMPLES_PER_REV <- 18
CONVERSION_FACTOR_KMH <- (WHEEL_CIRCUMFERENCE_M / SAMPLES_PER_REV) * 1000000 * 3.6
CONVERSION_FACTOR_REVOLUTION <- WHEEL_CIRCUMFERENCE_M * 1000000 * 3.6

# --- Main Parsing Function (Updated for 4 Sensors) ---
# Prompt user to select the binary file
file_path <- file.choose()

{
  if (!file.exists(file_path)) {
    stop("File not found.")
  }

  con <- file(file_path, "rb")
  records_list <- list()

  while (TRUE) {
    # Sensor 1: Optical (Deg)
    DegSamples <- readBin(con, integer(), n = 1, size = 4)
    if (length(DegSamples) == 0) {
      break # End of file
    }
    DegTimeArray <- list(readBin(con, integer(), n = max(0, DegSamples), size = 4))
    DegSumTime <- sum(unlist(DegTimeArray))

    # Sensor 2: Optical 2 (Deg2)
    DegSamples2 <- readBin(con, integer(), n = 1, size = 4)
    DegTimeArray2 <- list(readBin(con, integer(), n = max(0, DegSamples2), size = 4))
    DegSumTime2 <- sum(unlist(DegTimeArray2))
    
    # Sensor 3: Hall Effect (Hall)
    HallSamples <- readBin(con, integer(), n = 1, size = 4)
    HallTimeArray <- list(readBin(con, integer(), n = max(0, HallSamples), size = 4))
    HallSumTime <- sum(unlist(HallTimeArray))

    # Sensor 4: Hall Effect 2 (Hall2)
    HallSamples2 <- readBin(con, integer(), n = 1, size = 4)
    HallTimeArray2 <- list(readBin(con, integer(), n = max(0, HallSamples2), size = 4))
    HallSumTime2 <- sum(unlist(HallTimeArray2))

    # Assemble the full record for all 4 sensors
    record <- list(
      DegSumTime = DegSumTime, DegSamples = DegSamples, DegTimeArray = DegTimeArray,
      DegSumTime2 = DegSumTime2, DegSamples2 = DegSamples2, DegTimeArray2 = DegTimeArray2,
      HallSumTime = HallSumTime, HallSamples = HallSamples, HallTimeArray = HallTimeArray,
      HallSumTime2 = HallSumTime2, HallSamples2 = HallSamples2, HallTimeArray2 = HallTimeArray2
    )
    
    records_list[[length(records_list) + 1]] <- record
  }

  close(con)

  if(length(records_list) > 0) {
    log_data <- bind_rows(records_list)
  }
}

# --- Data Correction: Fix Split Revolutions ---

# Create an empty list to store the corrected records
{
corrected_records <- list()
i <- 1 # Initialize a counter for the while loop

cat("--- Starting data correction process... ---\n")
original_rows <- nrow(log_data)

while (i <= original_rows) {
  # Get the current row
  current_row <- log_data[i, ]
  
  # Check if the revolution might be split AND that we are not at the last row
  if (current_row$DegSamples < SAMPLES_PER_REV && i < original_rows) {
    next_row <- log_data[i + 1, ]
    
    # Check if this row and the next one form a complete 18-sample revolution
    is_valid_split <- (current_row$DegSamples + next_row$DegSamples == SAMPLES_PER_REV) &&
                      (current_row$DegSamples2 + next_row$DegSamples2 == SAMPLES_PER_REV) &&
                      (current_row$HallSamples + next_row$HallSamples == SAMPLES_PER_REV) &&
                      (current_row$HallSamples2 + next_row$HallSamples2 == SAMPLES_PER_REV)
    
    if (is_valid_split) {
      # --- Merge the two rows ---
      merged_row <- current_row # Start with the first row as a template
      
      # Combine the time arrays from both rows
      merged_row$DegTimeArray <- list(c(unlist(current_row$DegTimeArray), unlist(next_row$DegTimeArray)))
      merged_row$DegTimeArray2 <- list(c(unlist(current_row$DegTimeArray2), unlist(next_row$DegTimeArray2)))
      merged_row$HallTimeArray <- list(c(unlist(current_row$HallTimeArray), unlist(next_row$HallTimeArray)))
      merged_row$HallTimeArray2 <- list(c(unlist(current_row$HallTimeArray2), unlist(next_row$HallTimeArray2)))
      
      # Update the sample counts to 18
      merged_row$DegSamples <- SAMPLES_PER_REV
      merged_row$DegSamples2 <- SAMPLES_PER_REV
      merged_row$HallSamples <- SAMPLES_PER_REV
      merged_row$HallSamples2 <- SAMPLES_PER_REV
      
      # Recalculate the SumTime for the newly combined arrays
      merged_row$DegSumTime <- sum(unlist(merged_row$DegTimeArray))
      merged_row$DegSumTime2 <- sum(unlist(merged_row$DegTimeArray2))
      merged_row$HallSumTime <- sum(unlist(merged_row$HallTimeArray))
      merged_row$HallSumTime2 <- sum(unlist(merged_row$HallTimeArray2))
      
      # Add the single merged row to our corrected list
      corrected_records[[length(corrected_records) + 1]] <- merged_row
      
      # IMPORTANT: Skip the next row since we've already processed it
      i <- i + 2 
      next # Continue to the next iteration of the loop
    }
  }
  
  # If the row is complete (or is a short row we can't fix), add it as is
  # We can choose to keep or discard incomplete rows that couldn't be merged
  if (current_row$DegSamples == SAMPLES_PER_REV) {
    corrected_records[[length(corrected_records) + 1]] <- current_row
  }
  
  # Move to the next row
  i <- i + 1
}

# Combine the list of corrected records into a final, clean tibble
corrected_log_data <- bind_rows(corrected_records)

cat("Data correction complete.\n")
cat("Original number of rows:", original_rows, "\n")
cat("Corrected number of rows:", nrow(corrected_log_data), "\n\n")


# --- Verification ---
cat("--- Sample Counts Before Correction ---\n")
print(table(log_data$DegSamples))

cat("\n--- Sample Counts After Correction ---\n")
# This table should now ideally only show '18'
print(table(corrected_log_data$DegSamples))
}

# --- IMPORTANT ---
# Now, use 'corrected_log_data' for all subsequent analysis.
# For example:
# rev_speed_data <- corrected_log_data %>% ...
# per_sample_data <- corrected_log_data %>% ...

#log_data <- log_data_raw %>% filter(DegSamples==18 | DegSamples2==18 | HallSamples==18 | HallSamples2==18)

# --- 1. Calculate Per-Revolution Speed and Comparison Metrics ---
rev_speed_data <- corrected_log_data %>%
  filter(DegSumTime > 0, DegSumTime2 > 0, HallSumTime > 0, HallSumTime2 > 0) %>%
  mutate(
    # Calculate average speed for the revolution from each of the 4 sensors
    DegSpeed_kmh = CONVERSION_FACTOR_REVOLUTION / DegSumTime,
    DegSpeed2_kmh = CONVERSION_FACTOR_REVOLUTION / DegSumTime2,
    HallSpeed_kmh = CONVERSION_FACTOR_REVOLUTION / HallSumTime,
    HallSpeed2_kmh = CONVERSION_FACTOR_REVOLUTION / HallSumTime2,
    
    # Calculate the RANGE (max - min) of the four speed calculations
    RevSpeedRange_kmh = pmax(DegSpeed_kmh, DegSpeed2_kmh, HallSpeed_kmh, HallSpeed2_kmh) - 
                        pmin(DegSpeed_kmh, DegSpeed2_kmh, HallSpeed_kmh, HallSpeed2_kmh),
                        
    revolution_id = row_number()
  )

# --- 2. Visualize Per-Revolution Consistency ---
# This plot now shows 4 lines, making it easy to spot a deviant sensor.
rev_speed_data %>%
  select(revolution_id, DegSpeed_kmh, DegSpeed2_kmh, HallSpeed_kmh, HallSpeed2_kmh) %>%
  pivot_longer(cols = !revolution_id, names_to = "sensor", values_to = "speed_kmh") %>%
  ggplot(aes(x = revolution_id, y = speed_kmh, color = sensor)) +
    geom_line(alpha = 0.8, linewidth = 0.7) +
    labs(
      title = "4-Sensor Speeds Per Revolution Over Time",
      x = "Revolution Number", y = "Average Speed (km/h)", color = "Sensor"
    ) +
    theme_minimal() +
    scale_color_viridis_d()


# --- 1. Calculate Per-Sample Speed and Comparison Metrics ---
per_sample_data <- corrected_log_data %>%
  mutate(record_id = row_number()) %>%
  # Convert time arrays to speed arrays for all four sensors
  mutate(
    DegSpeed_kmh = map(DegTimeArray, ~ CONVERSION_FACTOR_KMH / unlist(.x)),
    DegSpeed2_kmh = map(DegTimeArray2, ~ CONVERSION_FACTOR_KMH / unlist(.x)),
    HallSpeed_kmh = map(HallTimeArray, ~ CONVERSION_FACTOR_KMH / unlist(.x)),
    HallSpeed2_kmh = map(HallTimeArray2, ~ CONVERSION_FACTOR_KMH / unlist(.x))
  ) %>%
  # Calculate the range for each sample across the four sensors
  mutate(
    # pmap now takes a list of 4 to work on them simultaneously
    speed_range_array = pmap(list(DegSpeed_kmh, DegSpeed2_kmh, HallSpeed_kmh, HallSpeed2_kmh), ~ {
      temp_df <- tibble(s1=..1, s2=..2, s3=..3, s4=..4)
      pmax(temp_df$s1, temp_df$s2, temp_df$s3, temp_df$s4) - 
      pmin(temp_df$s1, temp_df$s2, temp_df$s3, temp_df$s4)
    }),
    max_speed_range = map_dbl(speed_range_array, ~max(.x, na.rm = TRUE))
  )

# --- 2. Find and Isolate Problematic Revolutions ---
RANGE_THRESHOLD_KMH <- 2.0
problem_revolutions <- per_sample_data %>%
  filter(max_speed_range > RANGE_THRESHOLD_KMH) %>%
  arrange(desc(max_speed_range))

cat(paste("\n--- Found", nrow(problem_revolutions), "revolutions with a sample speed range >", 
          RANGE_THRESHOLD_KMH, "km/h ---\n"))

# --- 3. Visualize a Specific Faulty Signal ---
# Pick the ID of the worst offending revolution to visualize
if (nrow(problem_revolutions) > 0) {
  record_to_plot <- problem_revolutions$record_id[1]

  per_sample_data %>%
    filter(record_id == record_to_plot) %>%
    select(record_id, DegSpeed_kmh, DegSpeed2_kmh, HallSpeed_kmh, HallSpeed2_kmh) %>%
    # Unnest the four list columns into a long format
    unnest(cols = c(DegSpeed_kmh, DegSpeed2_kmh, HallSpeed_kmh, HallSpeed2_kmh)) %>%
    mutate(sample_number = row_number()) %>%
    pivot_longer(
      cols = !c(record_id, sample_number),
      names_to = "sensor", values_to = "speed_kmh"
    ) %>%
    ggplot(aes(x = sample_number, y = speed_kmh, color = sensor)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2.5) +
      labs(
        title = paste("Detailed 4-Sensor Speed Profile for Revolution #", record_to_plot),
        subtitle = "Identifying a false trigger within a single revolution",
        x = "Sample Number", y = "Instantaneous Speed (km/h)"
      ) +
      theme_linedraw() +
      scale_x_continuous(breaks = 1:max(SAMPLES_PER_REV, na.rm=TRUE)) +
      scale_color_viridis_d()
}


# Install and load the corrplot package for better visualization
# install.packages("corrplot")
library(corrplot)

# --- 1. Prepare Data in a "Wide" Format ---
# We need a column for each sensor's speed reading.
wide_speed_data <- per_sample_data %>%
  select(record_id, DegSpeed_kmh, DegSpeed2_kmh, HallSpeed_kmh, HallSpeed2_kmh) %>%
  unnest(cols = c(DegSpeed_kmh, DegSpeed2_kmh, HallSpeed_kmh, HallSpeed2_kmh))

# --- 2. Calculate the Correlation Matrix ---
# We select only the speed columns for the calculation
sensor_speeds <- wide_speed_data %>%
  select(DegSpeed_kmh, DegSpeed2_kmh, HallSpeed_kmh, HallSpeed2_kmh)

# Calculate the correlation matrix, handling any potential NA values
cor_matrix <- cor(sensor_speeds, use = "pairwise.complete.obs")

# Print the matrix
cat("--- Sensor Speed Correlation Matrix ---\n")
print(round(cor_matrix, 4))

# --- 3. Visualize the Correlation Matrix ---
# A visual plot is much easier to interpret than a table.
corrplot(cor_matrix,
         method = "color",       # Use color to represent correlation
         type = "upper",         # Show the upper triangle of the matrix
         order = "hclust",       # Reorder for better visualization
         addCoef.col = "white",  # Add the correlation coefficient
         tl.col = "black",       # Text label color
         tl.srt = 45,            # Rotate text labels
         title = "\n\nCorrelation Plot of Sensor Speeds",
         mar = c(0,0,2,0)         # Adjust margins
)


# --- 1. Prepare Data for a Continuous Plot ---
# This involves unnesting all sensor data and creating a global sample ID.
continuous_plot_data <- per_sample_data %>%
  # Select the speed arrays
  select(DegSpeed_kmh, DegSpeed2_kmh, HallSpeed_kmh, HallSpeed2_kmh) %>%
  
  # Unnest all list columns to create a row for every single sample
  unnest(cols = c(DegSpeed_kmh, DegSpeed2_kmh, HallSpeed_kmh, HallSpeed2_kmh)) %>%
  
  # Create a continuous x-axis variable
  mutate(global_sample_id = row_number()) %>%
  
  # Pivot to a long format for plotting with ggplot
  pivot_longer(
    cols = !global_sample_id,
    names_to = "sensor",
    values_to = "speed_kmh"
  )

# --- 2. Generate the Continuous Line Graph ---
ggplot(continuous_plot_data %>% filter(sensor=="DegSpeed_kmh"), aes(x = global_sample_id, y = speed_kmh, color = sensor)) +
  # Use a thin line with some transparency for clarity
  geom_line(linewidth = 0.6, alpha = 0.8) +
  labs(
    title = "Continuous Speed Timeline for All Sensors",
    subtitle = "Plotting every sample from the entire data log",
    x = "Global Sample ID (Continuous Timeline)",
    y = "Instantaneous Speed (km/h)",
    color = "Sensor"
  ) +
  theme_minimal() +
  # Use a colorblind-friendly palette
  scale_color_viridis_d() +
  # Optional: Improve performance for very large datasets
  theme(plot.subtitle = element_text(size = 8))


# --- 1. Calculate Pairwise Speed Differences ---
# We start with the corrected data to ensure we're working with complete revolutions.
paired_diff_data <- corrected_log_data %>%
  # Add a record_id for tracking
  mutate(record_id = row_number()) %>%
  
  # First, convert all time arrays to speed arrays
  mutate(
    DegSpeed_kmh = map(DegTimeArray, ~ CONVERSION_FACTOR_KMH / unlist(.x)),
    DegSpeed2_kmh = map(DegTimeArray2, ~ CONVERSION_FACTOR_KMH / unlist(.x)),
    HallSpeed_kmh = map(HallTimeArray, ~ CONVERSION_FACTOR_KMH / unlist(.x)),
    HallSpeed2_kmh = map(HallTimeArray2, ~ CONVERSION_FACTOR_KMH / unlist(.x))
  ) %>%
  
  # Now, calculate the difference within each sensor pair using map2
  mutate(
    Optical_Difference = map2(DegSpeed_kmh, DegSpeed2_kmh, ~ .x - .y),
    Hall_Difference = map2(HallSpeed_kmh, HallSpeed2_kmh, ~ .x - .y)
  )

# --- 2. Prepare Data for Plotting ---
paired_plot_data <- paired_diff_data %>%
  # Keep only the columns we need
  select(record_id, Optical_Difference, Hall_Difference) %>%
  
  # Unnest both difference arrays
  unnest(cols = c(Optical_Difference, Hall_Difference)) %>%
  
  # Add the sample number for the x-axis
  group_by(record_id) %>%
  mutate(sample_number = row_number()) %>%
  ungroup() %>%
  
  # Pivot to a long format suitable for faceting in ggplot
  pivot_longer(
    cols = c(Optical_Difference, Hall_Difference),
    names_to = "sensor_pair",
    values_to = "difference_kmh"
  )

# --- 3. Generate the Paired Difference Plots ---
ggplot(paired_plot_data, aes(x = factor(sample_number), y = difference_kmh, fill = sensor_pair)) +
  geom_boxplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 1) +
  
  # This function creates the two separate plots
  facet_wrap(~ sensor_pair, ncol = 1) +
  
  labs(
    title = "Speed Difference Within Sensor Pairs for Each Segment",
    subtitle = "Comparing Optical vs. Optical and Hall vs. Hall",
    x = "Sample Number (Segment)",
    y = "Speed Difference (km/h)"
  ) +
  theme_bw() +
  guides(fill = "none") # Hide the legend as the titles are clear
