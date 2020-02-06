# startup.R
#
# emphasize the importance of using RStudio project in creating a project
# workflow. This script will create two directories and populate them with
# files: 
#   - data/mtcars.csv
#   - plots/plots.png
#
# Usage:
# from interactive R session: source('R/startup.R')
# from the terminal:          Rscript startup.R

# Create Data Directory
message('Looking for data directory ... ')
if (!dir.exists('data')) {
  message('Creating `data` directory ... ')
  dir.create('data')
} else {
  message('data directory found!')
}

# Create .csv of mtcars
message("Looking for `mtcars.csv` data set ... ")
if (!file.exists('data/mtcars.csv')) {
  message('Creating `mtcars.csv ...')
  df <- data.frame(car = row.names(mtcars))
  df <- cbind(df, mtcars)
  row.names(df) <- NULL
  write.csv(df, 'data/mtcars.csv', row.names = FALSE)
} else {
  message("`mtcars.csv` data set located!")
}

# Create Plot Directories
message('Looking for plots directory ... ')
if (!dir.exists('plots')) {
  message('Creating `plots` directory ...')
  dir.create('plots')
} else {
  message('plots directory found!')
}

# Save Plots
if (length(list.files('plots')) == 0) {
  message('Creating histograms ... ')
  df <- read.csv('data/mtcars.csv')
  col_types <- vapply(df, class, 'character')
  numeric_cols <- col_types[grepl('numeric', col_types)]
  lapply(names(numeric_cols), function(nm) {
    png(file=sprintf("plots/%s.png", nm), width=600, height=600)
    hist(df[[nm]], main=sprintf('Histogram of: %s', nm), xlab = nm)
    dev.off()
  })
} 