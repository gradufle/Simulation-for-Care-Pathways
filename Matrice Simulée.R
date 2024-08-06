main <- read_dta("main.dta")

af <- data.frame(main,c("ghm1","ghm2","finessgeo1","finessgeo2","annee_sortie1"))
af <- main %>% select(ghm1,ghm2,finessgeo1,finessgeo2,annee_sortie1)
View(af)

#The source of our problem:
#alltodo <- expand.grid(all1, allf)

#Lets simulate a random dataset of this format, with a way lower number of rows:
set.seed(1)

h1 <- 6
h2 <- 6
original_matrix <- matrix(round(rexp(h1*h2, rate = 0.7)*10), nrow = h2, ncol = h1)
new_diag <- apply(original_matrix, 2, sum) * round(rexp(1, rate = 0.9)*3)
diag(original_matrix) <- new_diag
print(original_matrix)

n <- 5
m <- 5
# Function to create a new matrix with the value distributed
distribute_value <- function(value, n, m) {
  new_matrix <- matrix(0, nrow = n, ncol = m)
  remaining_value <- value
  
  while (remaining_value > 0) {
    i <- sample(1:n, 1)
    j <- sample(1:m, 1)
    # Determine a random value to place, ensuring it doesn't exceed the remaining value
    assign_value <- sample(1:remaining_value, 1)
    # Retravailler ? possibilité avec rmultinom:
    #x <- rmultinom(1, m1[5,1], rep.int(1 / 25, 25))
    new_matrix[i, j] <- new_matrix[i, j] + assign_value
    remaining_value <- remaining_value - assign_value
  }
  return(new_matrix)
}
# List to store all new matrices
all_new_matrices <- list()
# Loop through each element in the original matrix
for (i in 1:nrow(original_matrix)) {
  for (j in 1:ncol(original_matrix)) {
    value <- original_matrix[i, j]
    new_matrix <- distribute_value(value, n, m)
    new_matrix <- reshape2::melt(new_matrix)
    IJ <- matrix(c(i,j) , byrow = TRUE, nrow = n*m , ncol=2) 
    new_matrix2 <- cbind(new_matrix, IJ)
    all_new_matrices[[paste0("Matrix_", i, "_", j)]] <- new_matrix2
    Lmat <- cbind(all_new_matrices)
  }
}
# Display the list of new matrices
all_new_matrices
print(new_matrix2)
MDS <- do.call(rbind.data.frame, all_new_matrices)
print(MDS)
