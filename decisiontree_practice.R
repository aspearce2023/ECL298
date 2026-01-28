#Step A: Identify what field we want to use for this. H2+E2_Left_Clip, so thats probably H2 field. 
#Step B: Clip the raster, I chose the bottom of the field, making sure to include the left and right sides. The left side has more weeds and the right side is more clean. The result is 3.3gb. 
#Step C: In QGIS make the training vectors of weed, crop, dirt, and mechanic. 
#Step D: Load the data and perform decision tree 
# Step 0: Packages
library(terra)
library(rpart)    
# Step 1: Load data
H2_field  <- rast("H2_Clipped.tif")
training  <- vect("training_data.gpkg")
clip_mask <- vect("clip_mask.gpkg")
if (!all(crs(H2_field) == crs(training))) {
  training <- project(training, H2_field)}
if (!all(crs(H2_field) == crs(clip_mask))) {
  clip_mask <- project(clip_mask, H2_field)}
# Step 2: Clean raster and apply field mask
H2_field[H2_field == 0] <- NA
H2_field_fieldonly <- mask(H2_field, clip_mask)
# Step 3: Build training dataframe
training_df <- extract(
  H2_field_fieldonly,
  training,
  df    = TRUE,
  na.rm = TRUE)
training_df$class <- factor(training$class[training_df$ID])
training_df$ID <- NULL
training_df <- na.omit(training_df)
# Step 4: Fit decision tree
set.seed(6)
fit_dt <- rpart(
  class ~ .,
  data   = training_df,
  method = "class")
# Step 5: Predict over the field raster
H2_classified <- predict(
  H2_field_fieldonly,   # already masked to field only
  fit_dt,
  type     = "class",
  filename = "H2_classified_dt_fieldonly.tif",
  overwrite = TRUE)
H2_classified <- mask(H2_classified, clip_mask)
# Step 6: Plot result
plot(H2_classified, main = "Decision-tree classification")
#Setp E: accuracy assessments 
#Step 1: Extract predicted classes at the training polygons
acc_df <- extract(
  H2_classified,
  training,
  df    = TRUE,
  na.rm = TRUE)
#Step 2: Observed classes from polygon attribute table
acc_df$observed <- factor(training$class[acc_df$ID])
#Step 3: Identify the prediction column (it is the extracted raster layer name)
pred_col <- setdiff(names(acc_df), c("ID", "observed"))
if (length(pred_col) != 1) {
  stop("Expected exactly 1 prediction column, found: ", paste(pred_col, collapse = ", "))}
acc_df$predicted <- factor(acc_df[[pred_col]])
#Step 4: Keep only what we need and drop NAs
acc_df <- acc_df[, c("observed", "predicted")]
acc_df <- na.omit(acc_df)
#Step 5: Confusion matrix
conf_mat <- table(
  Observed  = acc_df$observed,
  Predicted = acc_df$predicted)
conf_mat
#Step 6: Overall accuracy
overall_accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
overall_accuracy
#Step 7: Producer's and User's accuracy
producers_accuracy <- diag(conf_mat) / rowSums(conf_mat)
users_accuracy     <- diag(conf_mat) / colSums(conf_mat)
accuracy_table <- data.frame(
  Class = rownames(conf_mat),
  Producers_Accuracy = round(producers_accuracy, 3),
  Users_Accuracy     = round(users_accuracy, 3))
accuracy_table
#Step 8: Kappa (and full confusion matrix stats)
library(caret)
all_levels <- sort(unique(c(levels(acc_df$observed), levels(acc_df$predicted))))
acc_df$observed  <- factor(acc_df$observed,  levels = all_levels)
acc_df$predicted <- factor(acc_df$predicted, levels = all_levels)
cm <- confusionMatrix(
  data      = acc_df$predicted,
  reference = acc_df$observed)
cm$overall["Kappa"]
cm
#Step F: Showing things graphically
library(viridisLite)
conf_mat <- table(acc_df$observed, acc_df$predicted)
par(
  mar = c(7, 9.5, 4, 2))
image(
  t(conf_mat[nrow(conf_mat):1, ]),
  axes = FALSE,
  col = viridisLite::viridis(100),
  main = "Confusion matrix")
x_pos <- seq(0, 1, length.out = ncol(conf_mat))
y_pos <- seq(0, 1, length.out = nrow(conf_mat))
axis(1, at = x_pos, labels = colnames(conf_mat), las = 2)
axis(2, at = y_pos, labels = rev(rownames(conf_mat)), las = 2)
mtext("Predicted class", side = 1, line = 5)
mtext("Observed class",  side = 2, line = 6.5)
nr <- nrow(conf_mat)
nc <- ncol(conf_mat)
for (i in seq_len(nr)) {
  for (j in seq_len(nc)) {
    text(
      x = x_pos[j],
      y = y_pos[nr - i + 1],
      labels = conf_mat[i, j],
      cex = 0.85,
      col = "black")}}
