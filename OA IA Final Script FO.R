required_pkgs <- c(
  "readxl","dplyr","stringr","janitor","caret","pROC","rpart","rpart.plot",
  "ggplot2","tibble","tidyr","cluster"
)

installed <- rownames(installed.packages())
for (p in required_pkgs) {
  if (!p %in% installed) install.packages(p, dependencies = TRUE)
}
invisible(lapply(required_pkgs, library, character.only = TRUE))

set.seed(123)

file_candidates <- c(
  "Original Dataset of Employee Attrition.xlsx",
  "Original_Dataset_of_Employee_Attrition.xlsx"
)

file_path <- file_candidates[file.exists(file_candidates)][1]

if (is.na(file_path)) {
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    ctx <- tryCatch(rstudioapi::getActiveDocumentContext(), error = function(e) NULL)
    if (!is.null(ctx) && nzchar(ctx$path)) {
      script_dir <- dirname(ctx$path)
      candidate2 <- file.path(script_dir, file_candidates)
      file_path2 <- candidate2[file.exists(candidate2)][1]
      if (!is.na(file_path2)) file_path <- file_path2
    }
  }
}

if (is.na(file_path)) {
  message("Excel file not found automatically. Please select it now...")
  file_path <- file.choose()
}

df_raw <- readxl::read_excel(file_path) %>%
  janitor::clean_names()

df_raw <- df_raw %>%
  mutate(across(where(is.character), ~ stringr::str_replace_all(.x, "\u00A0", " "))) %>%
  mutate(across(where(is.character), ~ stringr::str_squish(.x)))

predictors <- c(
  "gender","age","maritalstatus","years_experience","years_experience_lastorganization",
  "monthly_salary","over_time","business_travel","distance_to_work","work_live_balance",
  "job_support","recognition","job_satisfaction","psychological_exhaustion","job_opportunities"
)

model_df <- df_raw %>%
  select(attrition, all_of(predictors))

model_df$attrition <- factor(model_df$attrition, levels = c("No","Yes"))

factor_cols <- c(
  "gender","maritalstatus","monthly_salary","over_time","business_travel",
  "distance_to_work","work_live_balance","job_support","recognition",
  "job_satisfaction","psychological_exhaustion","job_opportunities"
)
factor_cols <- intersect(factor_cols, names(model_df))
model_df <- model_df %>% mutate(across(all_of(factor_cols), as.factor))

missing_by_col <- colSums(is.na(model_df))
missing_tbl <- tibble::tibble(variable = names(missing_by_col), missing_n = as.integer(missing_by_col)) %>%
  arrange(desc(missing_n))
write.csv(missing_tbl, "missingness_audit.csv", row.names = FALSE)

mode_value <- function(x) {
  ux <- na.omit(x)
  if (length(ux) == 0) return(NA)
  tab <- sort(table(ux), decreasing = TRUE)
  names(tab)[1]
}

if (any(missing_by_col > 0)) {
  for (nm in names(model_df)) {
    if (any(is.na(model_df[[nm]]))) {
      if (is.numeric(model_df[[nm]])) {
        model_df[[nm]][is.na(model_df[[nm]])] <- median(model_df[[nm]], na.rm = TRUE)
      } else {
        mv <- mode_value(model_df[[nm]])
        model_df[[nm]][is.na(model_df[[nm]])] <- mv
        model_df[[nm]] <- droplevels(model_df[[nm]])
      }
    }
  }
}

num_cols <- names(model_df)[sapply(model_df, is.numeric)]
num_cols <- setdiff(num_cols, "attrition")

outlier_summary <- lapply(num_cols, function(v) {
  x <- model_df[[v]]
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  flagged <- sum(x < lower | x > upper, na.rm = TRUE)
  tibble::tibble(variable = v, outliers_flagged = flagged, lower = lower, upper = upper)
}) %>% dplyr::bind_rows()

write.csv(outlier_summary, "outlier_audit_iqr.csv", row.names = FALSE)

idx <- caret::createDataPartition(model_df$attrition, p = 0.70, list = FALSE)
train_df <- model_df[idx, ]
test_df  <- model_df[-idx, ]

train_df <- droplevels(train_df)
test_df  <- droplevels(test_df)

majority_class <- names(sort(table(test_df$attrition), decreasing = TRUE))[1]
baseline_pred <- factor(rep(majority_class, nrow(test_df)), levels = levels(test_df$attrition))
baseline_cm <- caret::confusionMatrix(baseline_pred, test_df$attrition, positive = "Yes")

dv <- caret::dummyVars(attrition ~ ., data = train_df, fullRank = TRUE)

x_train <- predict(dv, newdata = train_df) %>% as.data.frame()
x_test  <- predict(dv, newdata = test_df)  %>% as.data.frame()

y_train <- train_df$attrition
y_test  <- test_df$attrition

logit <- glm(y_train ~ ., data = x_train, family = binomial)

p_logit <- predict(logit, newdata = x_test, type = "response")

pred_logit_05 <- factor(ifelse(p_logit >= 0.5, "Yes", "No"), levels = c("No","Yes"))
pred_logit_04 <- factor(ifelse(p_logit >= 0.4, "Yes", "No"), levels = c("No","Yes"))

cm_logit_05 <- caret::confusionMatrix(pred_logit_05, y_test, positive = "Yes")
cm_logit_04 <- caret::confusionMatrix(pred_logit_04, y_test, positive = "Yes")

roc_logit <- pROC::roc(response = y_test, predictor = p_logit, levels = c("No","Yes"), direction = "<")
auc_logit <- as.numeric(pROC::auc(roc_logit))

aliased <- tryCatch({
  a <- alias(logit)$Complete
  if (is.null(a)) "None detected" else capture.output(print(a))
}, error = function(e) "Alias() check failed")

writeLines(aliased, "aliased_terms_logit.txt")

tree_fit <- rpart::rpart(attrition ~ ., data = train_df, method = "class")

cp_tbl <- tree_fit$cptable
best_cp <- cp_tbl[which.min(cp_tbl[,"xerror"]), "CP"]
tree_pruned <- rpart::prune(tree_fit, cp = best_cp)

p_tree <- predict(tree_pruned, newdata = test_df, type = "prob")[,"Yes"]
pred_tree_05 <- factor(ifelse(p_tree >= 0.5, "Yes", "No"), levels = c("No","Yes"))

cm_tree_05 <- caret::confusionMatrix(pred_tree_05, y_test, positive = "Yes")

roc_tree <- pROC::roc(response = y_test, predictor = p_tree, levels = c("No","Yes"), direction = "<")
auc_tree <- as.numeric(pROC::auc(roc_tree))

png("plot_decision_tree.png", width = 1400, height = 700)
rpart.plot::rpart.plot(tree_pruned, type = 2, extra = 104, fallen.leaves = TRUE)
dev.off()

tree_imp <- tibble::tibble(
  variable = names(tree_pruned$variable.importance),
  importance = as.numeric(tree_pruned$variable.importance)
) %>% arrange(desc(importance))

write.csv(tree_imp, "tree_variable_importance.csv", row.names = FALSE)

top10_imp <- tree_imp %>% head(10)

p_imp <- ggplot(top10_imp, aes(x = reorder(variable, importance), y = importance)) +
  geom_col() + coord_flip() +
  labs(title = "Top 10 variable importance (Decision Tree)", x = NULL, y = "Importance")

ggsave("plot_tree_variable_importance.png", plot = p_imp, width = 10, height = 6)

get_metrics <- function(cm, auc_val = NA_real_) {
  acc <- as.numeric(cm$overall["Accuracy"])
  rec <- as.numeric(cm$byClass["Sensitivity"])
  prec <- as.numeric(cm$byClass["Precision"])
  f1 <- as.numeric(cm$byClass["F1"])
  tibble::tibble(Accuracy = acc, Recall = rec, Precision = prec, F1 = f1, AUC = auc_val)
}

results_tbl <- dplyr::bind_rows(
  tibble::tibble(Model = "Baseline (Majority class)") %>% bind_cols(get_metrics(baseline_cm, NA_real_)),
  tibble::tibble(Model = "Logistic (thr=0.5)")       %>% bind_cols(get_metrics(cm_logit_05, auc_logit)),
  tibble::tibble(Model = "Logistic (thr=0.4)")       %>% bind_cols(get_metrics(cm_logit_04, auc_logit)),
  tibble::tibble(Model = "Decision Tree (thr=0.5)")  %>% bind_cols(get_metrics(cm_tree_05, auc_tree))
)

write.csv(results_tbl, "model_results_testset.csv", row.names = FALSE)

roc_df <- bind_rows(
  tibble::tibble(
    fpr = 1 - roc_logit$specificities,
    tpr = roc_logit$sensitivities,
    model = "Logit"
  ),
  tibble::tibble(
    fpr = 1 - roc_tree$specificities,
    tpr = roc_tree$sensitivities,
    model = "Tree"
  )
)

p_roc <- ggplot(roc_df, aes(x = fpr, y = tpr, linetype = model)) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0) +
  labs(
    title = paste0("ROC Curves (AUC: Logit=", round(auc_logit, 3), ", Tree=", round(auc_tree, 3), ")"),
    x = "False Positive Rate",
    y = "True Positive Rate"
  )

ggsave("plot_roc_curves.png", plot = p_roc, width = 10, height = 6)

top_risk_capture <- function(prob, y, top_pct) {
  n <- length(prob)
  k <- ceiling(n * top_pct)
  ord <- order(prob, decreasing = TRUE)
  top_idx <- ord[1:k]
  leavers_total <- sum(y == "Yes")
  leavers_in_top <- sum(y[top_idx] == "Yes")
  tibble::tibble(
    top_pct = top_pct,
    top_n = k,
    leavers_in_top = leavers_in_top,
    leavers_total = leavers_total,
    capture_rate = leavers_in_top / leavers_total
  )
}

capture_tbl <- bind_rows(
  top_risk_capture(p_logit, y_test, 0.10),
  top_risk_capture(p_logit, y_test, 0.20),
  top_risk_capture(p_logit, y_test, 0.30)
)

write.csv(capture_tbl, "top_risk_capture_logit.csv", row.names = FALSE)

attr_rate_plot <- function(df, xvar, filename, title) {
  rate_tbl <- df %>%
    group_by(.data[[xvar]]) %>%
    summarise(attr_rate_yes = mean(attrition == "Yes"), .groups = "drop")
  
  p2 <- ggplot(rate_tbl, aes(x = .data[[xvar]], y = attr_rate_yes)) +
    geom_col() +
    labs(title = title, x = xvar, y = "Attrition rate (Yes)") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(filename, plot = p2, width = 10, height = 6)
}

attr_rate_plot(model_df, "job_satisfaction", "plot_attrition_job_satisfaction.png", "Attrition rate by Job Satisfaction")
attr_rate_plot(model_df, "monthly_salary", "plot_attrition_monthly_salary.png", "Attrition rate by Monthly Salary")
attr_rate_plot(model_df, "over_time", "plot_attrition_overtime.png", "Attrition rate by OverTime")

pp <- caret::preProcess(x_train, method = c("center","scale"))
x_train_sc <- predict(pp, x_train)
x_test_sc  <- predict(pp, x_test)

knn_ctrl <- caret::trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = caret::twoClassSummary,
  savePredictions = "final"
)

knn_grid <- expand.grid(k = seq(3, 25, 2))

knn_fit <- caret::train(
  x = x_train_sc,
  y = y_train,
  method = "knn",
  metric = "ROC",
  trControl = knn_ctrl,
  tuneGrid = knn_grid
)

p_knn <- predict(knn_fit, newdata = x_test_sc, type = "prob")[,"Yes"]
pred_knn_05 <- factor(ifelse(p_knn >= 0.5, "Yes", "No"), levels = c("No","Yes"))
cm_knn_05 <- caret::confusionMatrix(pred_knn_05, y_test, positive = "Yes")

roc_knn <- pROC::roc(response = y_test, predictor = p_knn, levels = c("No","Yes"), direction = "<")
auc_knn <- as.numeric(pROC::auc(roc_knn))

results_tbl2 <- bind_rows(
  results_tbl,
  tibble::tibble(Model = paste0("KNN (thr=0.5, best k=", knn_fit$bestTune$k, ")")) %>%
    bind_cols(get_metrics(cm_knn_05, auc_knn))
)

write.csv(results_tbl2, "model_results_testset_with_knn.csv", row.names = FALSE)

x_all <- predict(dv, newdata = model_df) %>% as.data.frame()
x_all_sc <- predict(pp, x_all)

wss <- sapply(2:8, function(k) {
  km <- kmeans(x_all_sc, centers = k, nstart = 20)
  km$tot.withinss
})

elbow_df <- tibble::tibble(k = 2:8, tot_withinss = wss)

p_elbow <- ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() + geom_point() +
  labs(title = "Elbow plot for k-means", x = "Number of clusters (k)", y = "Total within-cluster SS")

ggsave("plot_kmeans_elbow.png", plot = p_elbow, width = 8, height = 5)

k_chosen <- 3
km_final <- kmeans(x_all_sc, centers = k_chosen, nstart = 30)

model_df$cluster <- factor(km_final$cluster)

invisible(lapply(split(model_df, model_df$cluster), function(d) write.csv(d, paste0("cluster_", as.character(d$cluster[1]), "_profiles.csv"), row.names = FALSE)))

cluster_summary <- model_df %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    attrition_rate_yes = mean(attrition == "Yes"),
    .groups = "drop"
  ) %>%
  arrange(desc(attrition_rate_yes))

write.csv(cluster_summary, "cluster_summary.csv", row.names = FALSE)

p_cluster <- ggplot(cluster_summary, aes(x = cluster, y = attrition_rate_yes)) +
  geom_col() +
  labs(title = "Attrition rate by k-means cluster", x = "Cluster", y = "Attrition rate (Yes)")

ggsave("plot_attrition_by_cluster.png", plot = p_cluster, width = 7, height = 5)

print("Script finished.")