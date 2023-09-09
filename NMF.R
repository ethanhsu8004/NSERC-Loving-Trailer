
# Load NMF package
library(NMF)
library(dplyr)

# Loading Data
Data <- readRDS("TrailerData/Merged_Data_daily_hourly_exclude_five.rds")
Data_non_na <- na.omit(Data) #filtering out any non NA values


first_column <- Data_non_na[,1, drop = FALSE] #drop the first column for now since we don't need to adjust the background for time
background_levels <- sapply(Data_non_na[,-1], min, na.rm = TRUE) #finding the minimum for all columns except the first
adjusted_Data_non_na <- as.data.frame(sapply(names(Data_non_na[,-1]), function(tracer){ #adjusting by subtracting the minimum value
  Data_non_na[[tracer]] - background_levels[[tracer]]
}))

Data_non_na_adjusted_for_background_levels<- data.frame(first_column, adjusted_Data_non_na) #merging the time and the adjusted background levels

Data_non_na_adjusted_non_VOC <- Data_non_na_adjusted_for_background_levels[,1:11] #THE NON VOC ADJUSTED DATA
Data_non_na_adjusted_VOC <- Data_non_na_adjusted_for_background_levels[,12:length(Data_non_na_adjusted_for_background_levels)] #THE VOC FOR ADJUSTED DATA


Benzene_VOC <- Data_non_na_adjusted_VOC[,which(names(Data_non_na_adjusted_VOC) == "benzene"), drop = FALSE]#creating new dataframe  for Benzene
Ethane_VOC <- Data_non_na_adjusted_VOC[,which(names(Data_non_na_adjusted_VOC) == "ethane"), drop = FALSE]
Propane_VOC <- Data_non_na_adjusted_VOC[,which(names(Data_non_na_adjusted_VOC) == "propane"), drop = FALSE] 
Data_non_na_adjusted_VOC <- Data_non_na_adjusted_VOC %>% select(-benzene, -ethane, -propane) 

adjusting_negligible_background_from_LOD <- function(data_frame, LOD){ #adjustments that were made according to paper
  adjusted <- data_frame
  for (x in names(data_frame)){
    
    min_value <- min(data_frame[x], na.rm = TRUE)
    max_value <- max(data_frame[x], na.rm = TRUE)
    if (min_value < 2 * LOD || max_value > 100 * LOD ){
      adjusted[[x]] <- -100
    }
  }
  return (adjusted)
}

ethane <- adjusting_negligible_background_from_LOD(Ethane_VOC, 0.1) #LOD FOR ETHANE is 0.1
benzene <- adjusting_negligible_background_from_LOD(Benzene_VOC, 0.005) #LOD for BENZENE is 0.005
propane<-  adjusting_negligible_background_from_LOD(Propane_VOC, 0.05) #LOD FOR PROPANE is 0.05
Adjusted_others_VOC <- adjusting_negligible_background_from_LOD(Data_non_na_adjusted_VOC, 0.01) #LOD FOR REST OF VOC is 0.01


replace_negatives_with_random <- function(data_frame, LOD){
  adjusted <- data_frame
  for (x in names(data_frame)){
    negatives_exist <- any(data_frame[[x]] < 0, na.rm = TRUE)
    if (negatives_exist){
      adjusted[[x]] <- runif(nrow(data_frame), 0, 0.5 * LOD)
    }
  }
  return (adjusted)
}

ethane <- replace_negatives_with_random (ethane, 0.1)
benzene <- replace_negatives_with_random(benzene, 0.005)
propane <- replace_negatives_with_random(propane, 0.05)
Adjusted_others_VOC <- replace_negatives_with_random(Adjusted_others_VOC, 0.01)
Merged_VOC <- cbind(ethane, benzene, propane, Adjusted_others_VOC)

#normalizing function
normalize_column <- function(column){
  background <- quantile(column, 0)
  max <- quantile(column, 0.99)
  return ((column - background)/(max - background))
}


#Getting the Transpose
Normalized_Data <- Data_non_na_adjusted_non_VOC 
Normalized_Data[,-1] <- sapply(Data_non_na_adjusted_non_VOC[,-1], normalize_column) #normalize the NON_VOC
Normalized_Data <- Normalized_Data %>% select(-nox, -h2o_sync) #filtered these out because I don't have their LOD values
Transpose <- cbind(Normalized_Data, Merged_VOC) #combine the non-VOC and VOC
rownames(Transpose) <- as.character(Transpose[,1])
Transpose_Matrix <- t(as.matrix(Transpose[,-1])) 

number_row<- dim(Transpose_Matrix)[1] #store number of rows (used for checking)
number_column<- dim(Transpose_Matrix)[2] #store number of columns


#---------------NMF SECTION--------------------#
n_rows <- nrow(Transpose_Matrix)
n_cols <- ncol(Transpose_Matrix)
weight_matrix <- matrix(0, nrow(Transpose_Matrix), ncol(Transpose_Matrix))
LOD_vector = c(0.05, 0.05, 1, 0.05, 0.05, 3, 0.1, 0.1, 0.1, 0.05, 0.005) #hardcoded values for LOD_vector
Rest = rep(0.01, 18) #since VOC is 0.01 just created 17 of them 
LOD_vector_merged = c(LOD_vector, Rest) #merged the two results above


#creating uncertainty Matrix ???
for (i in 1:n_rows) { 
  for (j in 1:n_cols) {
    xij <- Transpose_Matrix[i, j]
    LOD <- LOD_vector_merged[i]  # Get LOD value for this row 
    if (i == 5){ #ch4
      weight_matrix[i, j] <- sqrt(xij)
    }
    else if(i == 4){
      weight_matrix[i, j] <- 0.25 * sqrt(xij)
    }
    
    else if (i == 7){
      weight_matrix[i, j] <- 0.5 * sqrt(xij)
    }
    else if (i == 2){
      weight_matrix[i,j <- 0.1 * sqrt(xij)]
    }
    else if (xij <= LOD) {
      weight_matrix[i, j] <- 2 * LOD #equation 5a) in reference paper
    } else {
      weight_matrix[i, j] <- sqrt(((0.1 * xij)**2 + LOD**2))  #equation 5c) in reference paper
    }
  }
}

#function below used to estimate the optimal rank and will be used in the nmf() function. 
estimate_rank <- nmfEstimateRank(Transpose_Matrix, 4:20, method = "ls-nmf", weight = weight_matrix, 5)
measures <- estimate_rank$measures
fit <- estimate_rank$fit
consensus <- estimate_rank$consensus

measures
fit
consensus

output <- nmf(Transpose_Matrix, rank = 6, weight = weight_matrix, method = "ls-nmf", seed = 123)
W <- basis(fit)
H <- coef(fit)





