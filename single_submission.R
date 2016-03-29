#Making single submission
#preparing dataframe for submission
agg_pred<-read.csv(file="h2rf_prob.csv",header=TRUE)

#number of entries to be choosen
k<-5
ids <- NULL
for (i in 1:nrow(agg_pred)) {
  idx <- as.character(agg_pred[i,1])
  ids <- append(ids, rep(idx,k))
}
predictions_top <- as.vector(apply(agg_pred[,-1], 1, function(x) names(sort(x)[12:(12-k+1)])))

submission <- NULL
submission$id <- ids
submission$country <- predictions_top

# generate submission file
submission <- as.data.frame(submission)
write.csv(submission, "su_output.csv", quote=FALSE, row.names = FALSE)