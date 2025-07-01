


# Step 1: Create a 20x20 raster
rGrazed <- rPlotID

# Step 2: Create 10x10 cluster mask (each cluster represents a 2x2 block)
cluster_map <- mCons

# Step 3: Initialize raster matrix to fill
r_mat <- matrix(NA, nrow=nrow(rGrazed), ncol=ncol(rGrazed))

# Step 4: Loop through 10x10 clusters
for (i in 1:10) {
  for (j in 1:10) {
    # Get the 2x2 pixel indices in the 20x20 matrix
    row_idx <- ((i-1)*2 + 1):((i-1)*2 + 2)
    col_idx <- ((j-1)*2 + 1):((j-1)*2 + 2)
    
    if (cluster_map[i, j] == 1) {
      # Sample 3x 1's and 1x 0 randomly
      sampled_vals <- sample(c(1, 1, 1, 0)) # one of the plots in the cons areas is not grazed
    } else {
      # All values are 1
      sampled_vals <- rep(1, 4)
    }
    
    # Fill the 2x2 block into the matrix
    r_mat[row_idx, col_idx] <- matrix(sampled_vals, nrow=2, ncol=2)
  }
}

# Step 5: Assign values to raster
rGrazed[] <- as.vector(t(r_mat))  # transpose to match raster cell order


