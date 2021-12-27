# set up vignettes

file <- data.frame(matrix(NA, 1, 6))
colnames(file) <- c("File", "Title", "PDF", "R", "Depends", "Keywords")
file[1,] <- c("Package_Introduction.Rmd", "Package Introduction", "Package_Introduction.html", "Package_Introduction.R", "", "")
file[2,] <- c("Time_Series.Rmd", "Dynamic Ideal Point Models", "Times_Series.html", "Time_Series.R", "", "")
saveRDS(file, "build/vignette.rds")