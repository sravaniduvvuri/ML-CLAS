##INPUT_RASTER=file
##TIMELINE=file
##FIELD_SAMPLES=file
##SAMPLE_PROJECTION=file
##SPLIT=number
##OUTPUT_FOLDER=folder


# Load required libraries
library(raster)
library(zoo)
library(dtwSat)
library(sp)
library(ggplot2)
library(caret)

setwd(OUTPUT_FOLDER)

# Load data
rvi <- brick(INPUT_RASTER)
rvi

timeline <- scan(TIMELINE, what = "dates")
timeline

head(timeline)

rts <- twdtwRaster(rvi, timeline = timeline)
rts

field_samples <- read.csv(FIELD_SAMPLES)
field_samples
names(field_samples)
head(field_samples)

plot(rvi)

points(field_samples)
proj_str <- scan(SAMPLE_PROJECTION, what = "character")
proj_str
library(caret)
set.seed(1) # set for reproducibility 
I <- unlist(createDataPartition(field_samples$label, p = SPLIT))
training_samples <- field_samples[I,]
validation_samples <- field_samples[-I,]

#Get timeseries from raster 
training_ts <- getTimeSeries(rts, 
                             y = training_samples, 
                             proj4string = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


#Create Temporal patterns
profiles_library <- createPatterns(training_ts, 
                                   freq = 10, 
                                   formula = y ~ poly(x,degree = 3))
plot (profiles_library, type = "patterns") + theme (legend.position = c(0.8, 0.21))
# Set TWDTW weight function 
#log_fun <- logisticWeight(-0.1, 50)

# Run TWDTW analysis 
system.time(
  twdtw_lucc <- twdtwApply(x = rts, 
                           y = profiles_library, 
                           alpha = 0.1,
                           beta = 80,
                           progress = 'text', 
                           minrows = 3,
                           legacy = FALSE,
                           time.window = TRUE)
) 
  #library(parallel)
#  library(foreach)
  #  
  # cl <- makeCluster(detectCores(), type = "FORK")
  # registerDoParallel(cl)
  #  
  
  
  #registerDoSEQ()
  
plot(twdtw_lucc, type = "distance", time.levels = 1)

# Plot TWDTW classification results 
plot(twdtw_lucc, type = "map")

# Plot mapped area time series 
plot(twdtw_lucc, type = "area")

# Plot land-cover changes
# plot(twdtw_lucc, type = "changes")

# ---- echo = TRUE, eval = TRUE, warning = FALSE, message = FALSE--------------
# Assess classification 
twdtw_assess <- 
  twdtwAssess(twdtw_lucc, 
              y = validation_samples, 
              proj4string = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
              conf.int = .95)

# Plot map accuracy 
plot(twdtw_assess, type = "accuracy")

# Plot area uncertainty 
plot(twdtw_assess, type = "area")

# Plot misclassified samples  
plot(twdtw_assess, type = "map", samples = "incorrect")

# Get latex table with error matrix 
twdtwXtable(twdtw_assess, table.type = "matrix")

# Get latex table with error accuracy 
twdtwXtable(twdtw_assess, table.type = "accuracy")

# Get latex table with area uncertainty 
twdtwXtable(twdtw_assess, table.type = "area")

writeRaster(twdtw_lucc, format="GTIFF", overwrite=TRUE)

# overall report
text_to_save <- capture.output(print(twdtw_assess))
file_path <- "output-report.txt"
writeLines(text_to_save, file_path)

# accuracySummary
text_to_save <- capture.output(print(twdtw_assess@accuracySummary))
file_path <- "output-accuracy-summary.txt"
writeLines(text_to_save, file_path)

# Error Matrix
text_to_save <- capture.output(print(twdtw_assess@accuracySummary$ErrorMatrix))
file_path <- "output-error-matrix.txt"
writeLines(text_to_save, file_path)

print("Output file generated")