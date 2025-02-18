# ------------------------------------------------------------------- #
# Predictive Modeling & Machine Learning Syntax - Regression Methods
# Example Syntax Only - Real data and variables are not public
# ------------------------------------------------------------------- #

# Install and Load Required Packages ----
## Specify packages needed to process code in this file
Req_Pkgs <- c("psych", 
              "reshape2",
              "janitor",
              "glmnet",
              "caret",
              "tidyverse",
              "tidymodels")

## Function to install and load Req_Pkgs
install_load <- function(x){
  
for(i in x){
  if((i %in% c(installed.packages()[,"Package"])) == F){install.packages(i)}
  library(i,character.only = T,quietly = T)}
  
  }

## Run function on Req_Pkgs
install_load(Req_Pkgs)

# Set Preference for Tidymodels ----
tidymodels_prefer()

# Import Data ----
## Read in csv file
y_train <- read_csv('path.csv')

# Remove non-model data ----
## These variables were used for data selection decisions
y_train <- y_train %>% 
  select(-c(unique_id,'...')) %>% 
  filter(y != 0) # 0 is a data error

# Descriptive Data ----
## Multicollinearity Between Proxies ---
### Squared correlation closer to 1 = higher degree of collinearity (correlation)
cors <- round(
  cor(y_train), 2
  ) ^ 2

cors_df <- melt(cors)

cors_df <- cors_df[lower.tri(cors, diag = F), ]

cors_df <- cors_df[lower.tri(cors), ] %>% 
  arrange(desc(value)) %>% 
  rename('r2' = 'value')

## Counts for each proxy
desc <- y_train %>% 
  select(-c(y)) %>% 
  pivot_longer(everything()) %>%
  group_by(across(everything())) %>%
  summarise(N=n()) %>%
  pivot_wider(names_from = name,values_from=N,values_fill=0)

## Summary for y
desc_y <- as_tibble(describe(y_train$y))

openxlsx::write.xlsx(list(multicollinearity = cors_df, counts = desc, y = desc_y), 'descriptives.xlsx')

rm(cors,cors_df,desc, desc_y)

# Outliers ----
## There are some y that we can't predict  (unusual circumstances)
## Test models with and without outliers
## Median Absolute Deviation 
### Removes 10% of sample and reduces max(y)
y_train %>% filter(y > median(y)+(mad(y)*3)) %>% nrow()/nrow(y_train)*100
y_train %>% filter(y <= median(y)+(mad(y)*3)) %>% summarise(max(y))
### Trimming the top 0.5% (keeping 99.5%)
filter(y_train, y < min(sort(y_train$y, decreasing = T)[1:(.005*NROW(y_train))])) %>% summarise(max(y))
y_train_trimmed <- filter(y_train, y < min(sort(y_train$y, decreasing = T)[1:(.005*NROW(y_train))]))

# Model Step: Drop variables from models ----
## Only tested using Elastic Net (But used for next modeling step)
### Elastic was determined to be best for testing drops
y_train_adj <- y_train %>% 
  select(-c(reference_category_1, #Reference value
            reference_category_2, #Reference value
            reference_category_3, #Reference value
            x_drop_1, #Low coefficient
            x_drop_2) #Low/inconsistent coefficient
            )

y_train_trimmed_adj <- y_train_trimmed %>% 
  select(-c(reference_category_1, #Reference value
            reference_category_2, #Reference value
            reference_category_3, #Reference value
            x_drop_1, #Low coefficient
            x_drop_2) #Low/inconsistent coefficient
  )

# Early predictors ----
## The predictors in our model are collected at a number of time points 
## Some predictors are collected earlier than others
## Test a model's predictive power based on early predictors alone
### Creates a dataset with only early predictors by removing middle and late predictors
y_train_early <- y_train %>% select(-c(
  late_x_1,
  late_x_2,
  late_x_3,
  late_x_4,
  late_x_5,
  late_x_6,
  late_x_7,
  late_x_8,
  late_x_9,
  late_x_10,
  late_x_11,
  x_drop_2))


## Model Step: Drop reference values and test including late_x_3 & late_x_4 as early predictors ----
### Only tested using Elastic Net (But used for next modeling step)
### Elastic was determined to be best for testing drops
y_train_early_adj <- y_train %>% select(-c(
  late_x_1,
  late_x_2,
  late_x_5,
  late_x_6,
  late_x_7,
  late_x_8,
  late_x_9,
  late_x_10,
  late_x_11,
  x_drop_2,
  reference_category_1,
  reference_category_2,
  reference_category_3,
  x_drop_1))

# Model Step: Remove additional variables ----
y_train_adj2 <- y_train_adj %>% 
  select(-c(x_drop_3))

y_train_trimmed_adj2 <- y_train_trimmed_adj %>% 
  select(-c(x_drop_3))

y_train_early_adj2 <- y_train_early_adj %>% 
  select(-c(x_drop_3,
            late_x_3,
            late_x_4))

# Regression Models ----
## Setting up range of lambda values (hyper-parameterization) ----
### The larger the lambda, the greater the penalty; greater reduction in coefficient magnitude
####lambda <- 10^seq(-3, 3, length = 100) # estimate 100 lambda values 
### Extending lambda because Lasso best fit was the minimum of initial set above
lambda <- 10^seq(-4, 2, length = 100)

## Function to get parameters from best result ----
get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

## Ridge ----
### Key Feature: Retains all variables (regardless of multicollinearity) and shrinks coefficients
### Limitation: Keeps all variables
set.seed(677)
y_ridge <- train(
  log10(y) ~ .,
  data = y_train, 
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tuneGrid = expand.grid(alpha = 0, lambda = lambda) #alpha = 0 applies 0% weight to L1 penalty (absolute of coefficients) thus 100% weight to L2 penalty (squared coefficients)
)

y_ridge_fit <- get_best_result(y_ridge)

set.seed(677)
y_trimmed_ridge <- train(
  log10(y) ~ .,
  data = y_train_trimmed, 
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tuneGrid = expand.grid(alpha = 0, lambda = lambda) #alpha = 0 applies 0% weight to L1 penalty (absolute of coefficients) thus 100% weight to L2 penalty (squared coefficients)
)

y_trimmed_ridge_fit <- get_best_result(y_trimmed_ridge)

set.seed(677)
y_early_ridge <- train(
  log10(y) ~ .,
  data = y_train_early, 
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tuneGrid = expand.grid(alpha = 0, lambda = lambda) #alpha = 0 applies 0% weight to L1 penalty (absolute of coefficients) thus 100% weight to L2 penalty (squared coefficients)
)

y_early_ridge_fit <- get_best_result(y_early_ridge)

set.seed(677)
y_ridge_adj2 <- train(
  log10(y) ~ .,
  data = y_train_adj2, 
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tuneGrid = expand.grid(alpha = 0, lambda = lambda) #alpha = 0 applies 0% weight to L1 penalty (absolute of coefficients) thus 100% weight to L2 penalty (squared coefficients)
)

y_ridge_adj2_fit <- get_best_result(y_ridge_adj2)

set.seed(677)
y_trimmed_ridge_adj2 <- train(
  log10(y) ~ .,
  data = y_train_trimmed_adj2, 
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tuneGrid = expand.grid(alpha = 0, lambda = lambda) #alpha = 0 applies 0% weight to L1 penalty (absolute of coefficients) thus 100% weight to L2 penalty (squared coefficients)
)

y_trimmed_ridge_adj2_fit <- get_best_result(y_trimmed_ridge_adj2)

set.seed(677)
y_early_ridge_adj2 <- train(
  log10(y) ~ .,
  data = y_train_early_adj2, 
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tuneGrid = expand.grid(alpha = 0, lambda = lambda) #alpha = 0 applies 0% weight to L1 penalty (absolute of coefficients) thus 100% weight to L2 penalty (squared coefficients)
)

y_early_ridge_adj2_fit <- get_best_result(y_early_ridge_adj2)

### Coefficients
y_ridge_coefs <- as.data.frame.matrix(
  coef(
    y_ridge$finalModel,
    y_ridge$bestTune$lambda
    )
  ) %>% 
  mutate(s1 = round(s1, 3))

y_trimmed_ridge_coefs <- as.data.frame.matrix(
  coef(
    y_trimmed_ridge$finalModel,
    y_trimmed_ridge$bestTune$lambda
    )
  ) %>% 
  mutate(s1 = round(s1, 3))

y_early_ridge_coefs <- as.data.frame.matrix(
  coef(
    y_early_ridge$finalModel,
    y_early_ridge$bestTune$lambda
  )
) %>% 
  mutate(ridge_early_coefs = round(s1, 3)) %>%
  select(-s1)

y_ridge_adj2_coefs <- as.data.frame.matrix(
  coef(
    y_ridge_adj2$finalModel,
    y_ridge_adj2$bestTune$lambda
  )
) %>% 
  mutate(ridge_coefs = round(s1, 3)) %>%
  select(-s1)

y_trimmed_ridge_adj2_coefs <- as.data.frame.matrix(
  coef(
    y_trimmed_ridge_adj2$finalModel,
    y_trimmed_ridge_adj2$bestTune$lambda
  )
) %>% 
  mutate(ridge_trimmed_adj2_coefs = round(s1, 3)) %>%
  select(-s1)

y_early_ridge_adj2_coefs <- as.data.frame.matrix(
  coef(
    y_early_ridge_adj2$finalModel,
    y_early_ridge_adj2$bestTune$lambda
  )
) %>% 
  mutate(ridge_early_adj2_coefs = round(s1, 3)) %>%
  select(-s1)

## LASSO ----
### Key Feature: Reduces overfitting, shrinks coefficients to zero (eliminates variables)
### Limitation: Multicollinearity as LASSO may inaccurately pick one correlated variable over the other
set.seed(677)
y_lasso <- train(
  log10(y) ~ .,
  data = y_train,
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tuneGrid = expand.grid(alpha = 1, lambda = lambda) #alpha = 1 applies 100% weight to L1 penalty (absolute of coefficients)
)

y_lasso_fit <- get_best_result(y_lasso)

set.seed(677)
y_trimmed_lasso <- train(
  log10(y) ~ .,
  data = y_train_trimmed,
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tuneGrid = expand.grid(alpha = 1, lambda = lambda) #alpha = 1 applies 100% weight to L1 penalty (absolute of coefficients)
)

y_trimmed_lasso_fit <- get_best_result(y_trimmed_lasso)

set.seed(677)
y_early_lasso <- train(
  log10(y) ~ .,
  data = y_train_early,
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tuneGrid = expand.grid(alpha = 1, lambda = lambda) #alpha = 1 applies 100% weight to L1 penalty (absolute of coefficients)
)

y_early_lasso_fit <- get_best_result(y_early_lasso)

y_lasso_adj2 <- train(
  log10(y) ~ .,
  data = y_train_adj2,
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tuneGrid = expand.grid(alpha = 1, lambda = lambda) #alpha = 1 applies 100% weight to L1 penalty (absolute of coefficients)
)

y_lasso_adj2_fit <- get_best_result(y_lasso_adj2)

set.seed(677)
y_trimmed_lasso_adj2 <- train(
  log10(y) ~ .,
  data = y_train_trimmed_adj2,
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tuneGrid = expand.grid(alpha = 1, lambda = lambda) #alpha = 1 applies 100% weight to L1 penalty (absolute of coefficients)
)

y_trimmed_lasso_adj2_fit <- get_best_result(y_trimmed_lasso_adj2)

set.seed(677)
y_early_lasso_adj2 <- train(
  log10(y) ~ .,
  data = y_train_early_adj2,
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tuneGrid = expand.grid(alpha = 1, lambda = lambda) #alpha = 1 applies 100% weight to L1 penalty (absolute of coefficients)
)

y_early_lasso_adj2_fit <- get_best_result(y_early_lasso_adj2)

### Coefficients
y_lasso_coefs <- as.data.frame.matrix(
  coef(
    y_lasso$finalModel,
    y_lasso$bestTune$lambda
    )
  ) %>% 
  mutate(s1 = round(s1, 3))

y_trimeed_lasso_coefs <- as.data.frame.matrix(
  coef(
    y_trimmed_lasso$finalModel,
    y_trimmed_lasso$bestTune$lambda
    )
  ) %>% 
  mutate(s1 = round(s1, 3))

y_early_lasso_coefs <- as.data.frame.matrix(
  coef(
    y_early_lasso$finalModel,
    y_early_lasso$bestTune$lambda
  )
) %>% 
  mutate(lasso_early_coefs = round(s1, 3)) %>%
  select(-s1)

y_lasso_adj2_coefs <- as.data.frame.matrix(
  coef(
    y_lasso_adj2$finalModel,
    y_lasso_adj2$bestTune$lambda
  )
) %>% 
  mutate(lasso_adj2_coefs = round(s1, 3)) %>%
  select(-s1)

y_trimmed_lasso_adj2_coefs <- as.data.frame.matrix(
  coef(
    y_trimmed_lasso_adj2$finalModel,
    y_trimmed_lasso_adj2$bestTune$lambda
  )
) %>% 
  mutate(lasso_trimmed_adj2_coefs = round(s1, 3)) %>%
  select(-s1)

y_early_lasso_adj2_coefs <- as.data.frame.matrix(
  coef(
    y_early_lasso_adj2$finalModel,
    y_early_lasso_adj2$bestTune$lambda
  )
) %>% 
  mutate(lasso_early_adj2_coefs = round(s1, 3)) %>%
  select(-s1)

## Elastic Net ----
### Key Feature: Combines Ridge & LASSO, handles multicollinearity and variable selection
### Limitation: Tuning challenges (lambda & alpha); can have more small coefficients or few large coefficients
set.seed(677)
y_elastic <- train(
  log10(y) ~ .,
  data = y_train,
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tunelength = 10 # uses 10 alpha and 10 lambda parameter values
)

y_elastic_fit <- get_best_result(y_elastic)

set.seed(677)
y_trimmed_elastic <- train(
  log10(y) ~ .,
  data = y_train_trimmed,
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tunelength = 10 # uses 10 alpha and 10 lambda parameter values
)

y_trimmed_elastic_fit <- get_best_result(y_trimmed_elastic)

set.seed(677)
y_early_elastic <- train(
  log10(y) ~ .,
  data = y_train_early,
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tunelength = 10 # uses 10 alpha and 10 lambda parameter values
)

y_early_elastic_fit <- get_best_result(y_early_elastic)

set.seed(677)
y_elastic_adj <- train(
  log10(y) ~ .,
  data = y_train_adj,
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tunelength = 10 # uses 10 alpha and 10 lambda parameter values
)

y_elastic_adj_fit <- get_best_result(y_elastic_adj)

set.seed(677)
y_trimmed_elastic_adj <- train(
  log10(y) ~ .,
  data = y_train_trimmed_adj,
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tunelength = 10 # uses 10 alpha and 10 lambda parameter values
)

y_trimmed_elastic_adj_fit <- get_best_result(y_trimmed_elastic_adj)

set.seed(677)
y_early_elastic_adj <- train(
  log10(y) ~ .,
  data = y_train_early_adj,
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tunelength = 10 # uses 10 alpha and 10 lambda parameter values
)

y_early_elastic_adj_fit <- get_best_result(y_early_elastic_adj)

set.seed(677)
y_elastic_adj2 <- train(
  log10(y) ~ .,
  data = y_train_adj2,
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tunelength = 10 # uses 10 alpha and 10 lambda parameter values
)

y_elastic_adj2_fit <- get_best_result(y_elastic_adj2)

set.seed(677)
y_trimmed_elastic_adj2 <- train(
  log10(y) ~ .,
  data = y_train_trimmed_adj2,
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tunelength = 10 # uses 10 alpha and 10 lambda parameter values
)

y_trimmed_elastic_adj2_fit <- get_best_result(y_trimmed_elastic_adj2)

set.seed(677)
y_early_elastic_adj2 <- train(
  log10(y) ~ .,
  data = y_train_early_adj2,
  method = "glmnet",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tunelength = 10 # uses 10 alpha and 10 lambda parameter values
)

y_early_elastic_adj2_fit <- get_best_result(y_early_elastic_adj2)

### Coefficients
y_elastic_coefs <- as.data.frame.matrix(
  coef(
    y_elastic$finalModel,
    y_elastic$bestTune$lambda
    )
  ) %>% 
  mutate(s1 = round(s1, 3))

y_trimmed_elastic_coefs <- as.data.frame.matrix(
  coef(
    y_trimmed_elastic$finalModel,
    y_trimmed_elastic$bestTune$lambda
    )
  ) %>% 
  mutate(s1 = round(s1, 3))

y_early_elastic_coefs <- as.data.frame.matrix(
  coef(
    y_early_elastic$finalModel,
    y_early_elastic$bestTune$lambda
  )
) %>% 
  mutate(elnet_early_coefs = round(s1, 3)) %>%
  select(-s1)

y_elastic_adj_coefs <- as.data.frame.matrix(
  coef(
    y_elastic_adj$finalModel,
    y_elastic_adj$bestTune$lambda
  )
) %>% 
  mutate(elnet_adj_coefs = round(s1, 3)) %>%
  select(-s1)

y_trimmed_elastic_adj_coefs <- as.data.frame.matrix(
  coef(
    y_trimmed_elastic_adj$finalModel,
    y_trimmed_elastic_adj$bestTune$lambda
  )
) %>% 
  mutate(elnet_trimmed_adj_coefs = round(s1, 3)) %>%
  select(-s1)

y_early_elastic_adj_coefs <- as.data.frame.matrix(
  coef(
    y_early_elastic_adj$finalModel,
    y_early_elastic_adj$bestTune$lambda
  )
) %>% 
  mutate(elnet_early_adj_coefs = round(s1, 3)) %>%
  select(-s1)

y_elastic_adj2_coefs <- as.data.frame.matrix(
  coef(
    y_elastic_adj2$finalModel,
    y_elastic_adj2$bestTune$lambda
  )
) %>% 
  mutate(elnet_adj2_coefs = round(s1, 3)) %>%
  select(-s1)

y_trimmed_elastic_adj2_coefs <- as.data.frame.matrix(
  coef(
    y_trimmed_elastic_adj2$finalModel,
    y_trimmed_elastic_adj2$bestTune$lambda
  )
) %>% 
  mutate(elnet_trimmed_adj2_coefs = round(s1, 3)) %>%
  select(-s1)

y_early_elastic_adj2_coefs <- as.data.frame.matrix(
  coef(
    y_early_elastic_adj2$finalModel,
    y_early_elastic_adj2$bestTune$lambda
  )
) %>% 
  mutate(elnet_early_adj2_coefs = round(s1, 3)) %>%
  select(-s1)

## Negative Binomial Model ----
### Key Feature: Models count data that is over-dispersed (i.e., variance > mean)
### Limitation: No variable selection or adjustment for multicollinearity
set.seed(677)
y_nb <- train(
  y ~ .,
  data = y_train,
  method = "glm.nb",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tuneGrid = expand.grid(link = "log")
)

y_nb_fit <- get_best_result(y_nb)

set.seed(677)
y_trimmed_nb <- train(
  y ~ .,
  data = y_train_trimmed,
  method = "glm.nb",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tuneGrid = expand.grid(link = "log")
)

y_trimmed_nb_fit <- get_best_result(y_trimmed_nb)

set.seed(677)
y_early_nb <- train(
  y ~ .,
  data = y_train_early,
  method = "glm.nb",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tuneGrid = expand.grid(link = "log")
)

y_early_nb_fit <- get_best_result(y_early_nb)

set.seed(677)
y_nb_adj2 <- train(
  y ~ .,
  data = y_train_adj2,
  method = "glm.nb",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tuneGrid = expand.grid(link = "log")
)

y_nb_adj2_fit <- get_best_result(y_nb_adj2)

set.seed(677)
y_trimmed_nb_adj2 <- train(
  y ~ .,
  data = y_train_trimmed_adj2,
  method = "glm.nb",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tuneGrid = expand.grid(link = "log")
)

y_trimmed_nb_adj2_fit <- get_best_result(y_trimmed_nb_adj2)

set.seed(677)
y_early_nb_adj2 <- train(
  y ~ .,
  data = y_train_early_adj2,
  method = "glm.nb",
  trControl = trainControl("repeatedcv", repeats = 10, number = 10), # 10 separate 10-fold cross validations (100 models)
  tuneGrid = expand.grid(link = "log")
)

y_early_nb_adj2_fit <- get_best_result(y_early_nb_adj2)

### Coefficients 
y_nb_coefs <- tibble(
  var_name = names(coef(y_nb$finalModel)),
  coef = coef(y_nb$finalModel))

y_trimmed_nb_coefs <- tibble(
  var_name = names(coef(y_trimmed_nb$finalModel)),
  coef = coef(y_trimmed_nb$finalModel))

y_early_nb_coefs <- tibble(
  var_name = names(coef(y_early_nb$finalModel)),
  nb_early_coefs = round(coef(y_early_nb$finalModel),3))

y_nb_adj2_coefs <- tibble(
  var_name = names(coef(y_nb_adj2$finalModel)),
  nb_coefs = round(coef(y_nb_adj2$finalModel),3))

y_trimmed_nb_adj2_coefs <- tibble(
  var_name = names(coef(y_trimmed_nb_adj2$finalModel)),
  nb_trimmed_coef = round(coef(y_trimmed_nb_adj2$finalModel),3))

y_early_nb_adj2_coefs <- tibble(
  var_name = names(coef(y_early_nb_adj2$finalModel)),
  nb_early_coefs = round(coef(y_early_nb_adj2$finalModel),3))

# Save Model Fits ----
save(y_ridge_adj2,
     y_lasso_adj2,
     y_elastic_adj2,
     y_nb_adj2,
     y_trimmed_ridge_adj2,
     y_trimmed_lasso_adj2,
     y_trimmed_elastic_adj2,
     y_trimmed_nb_adj2,
     y_early_ridge_adj2,
     y_early_lasso_adj2,
     y_early_elastic_adj2,
     y_early_nb_adj2,
     file = "regression_trained_models.RData")

# Performance Metrics Using Test Data ----
## Load R Data from model fits ----
load('y_regression_trained_models.RData')
## Read in test data ----
y_test <- read_csv('path.csv')

## Ridge ----
### Calculate ridge predictions 
y_ridge_predictions <- y_ridge_adj2 %>% predict(y_test)
y_trimmed_ridge_predictions <- y_trimmed_ridge_adj2 %>% predict(y_test)
y_early_ridge_predictions <- y_early_ridge_adj2 %>% predict(y_test)

### Evaluation ridge metrics
y_ridge_metrics <- tibble(
  model = c(
    'full',
    'trimmed',
    'early'),
  mae = c(
    MAE(ceiling(10^y_ridge_predictions), y_test$y),
    MAE(ceiling(10^y_trimmed_ridge_predictions), y_test$y),
    MAE(ceiling(10^y_early_ridge_predictions), y_test$y)),
  rmse = c(
    RMSE(ceiling(10^y_ridge_predictions), y_test$y),
    RMSE(ceiling(10^y_trimmed_ridge_predictions), y_test$y),
    RMSE(ceiling(10^y_early_ridge_predictions), y_test$y)),
  rsquare = c(
    R2(ceiling(10^y_ridge_predictions), y_test$y),
    R2(ceiling(10^y_trimmed_ridge_predictions), y_test$y),
    R2(ceiling(10^y_early_ridge_predictions), y_test$y))
)

## LASSO ----
### Calculate LASSO predictions
y_lasso_predictions <- y_lasso_adj2 %>% predict(y_test)
y_trimmed_lasso_predictions <- y_trimmed_lasso_adj2 %>% predict(y_test)
y_early_lasso_predictions <- y_early_lasso_adj2 %>% predict(y_test)

### Evaluation LASSO metrics
y_lasso_metrics <- tibble(
  model = c(
    'full',
    'trimmed',
    'early'),
  mae = c(
    MAE(ceiling(10^y_lasso_predictions), y_test$y),
    MAE(ceiling(10^y_trimmed_lasso_predictions), y_test$y),
    MAE(ceiling(10^y_early_lasso_predictions), y_test$y)),
  rmse = c(
    RMSE(ceiling(10^y_lasso_predictions), y_test$y),
    RMSE(ceiling(10^y_trimmed_lasso_predictions), y_test$y),
    RMSE(ceiling(10^y_early_lasso_predictions), y_test$y)),
  rsquare = c(
    R2(ceiling(10^y_lasso_predictions), y_test$y),
    R2(ceiling(10^y_trimmed_lasso_predictions), y_test$y),
    R2(ceiling(10^y_early_lasso_predictions), y_test$y))
)

## Elastic Net ----
### Calculate elastic net predictions
y_elastic_predictions <- y_elastic_adj2 %>% predict(y_test)
y_trimmed_elastic_predictions <- y_trimmed_elastic_adj2 %>% predict(y_test)
y_early_elastic_predictions <- y_early_elastic_adj2 %>% predict(y_test)

### Evaluation elastic net metrics
y_elastic_metrics <- tibble(
  model = c(
    'full',
    'trimmed',
    'early'),
  mae = c(
    MAE(ceiling(10^y_elastic_predictions), y_test$y),
    MAE(ceiling(10^y_trimmed_elastic_predictions), y_test$y),
    MAE(ceiling(10^y_early_elastic_predictions), y_test$y)),
  rmse = c(
    RMSE(ceiling(10^y_elastic_predictions), y_test$y),
    RMSE(ceiling(10^y_trimmed_elastic_predictions), y_test$y),
    RMSE(ceiling(10^y_early_elastic_predictions), y_test$y)),
  rsquare = c(
    R2(ceiling(10^y_elastic_predictions), y_test$y),
    R2(ceiling(10^y_trimmed_elastic_predictions), y_test$y),
    R2(ceiling(10^y_early_elastic_predictions), y_test$y))
)

## Negative Binomial ----
### Calculate negative binomial predictions
y_nb_predictions <- y_nb_adj2 %>% predict(y_test)
y_trimmed_nb_predictions <- y_trimmed_nb_adj2 %>% predict(y_test)
y_early_nb_predictions <- y_early_nb_adj2 %>% predict(y_test)

### Evaluation negative binomial metrics
y_nb_metrics <- tibble(
  model = c(
    'full',
    'trimmed',
    'early'),
  mae = c(
    MAE(ceiling(y_nb_predictions), y_test$y),
    MAE(ceiling(y_trimmed_nb_predictions), y_test$y),
    MAE(ceiling(y_early_nb_predictions), y_test$y)),
  rmse = c(
    RMSE(ceiling(y_nb_predictions), y_test$y),
    RMSE(ceiling(y_trimmed_nb_predictions), y_test$y),
    RMSE(ceiling(y_early_nb_predictions), y_test$y)),
  rsquare = c(
    R2(ceiling(y_nb_predictions), y_test$y),
    R2(ceiling(y_trimmed_nb_predictions), y_test$y),
    R2(ceiling(y_early_nb_predictions), y_test$y))
)

## Plot Metrics ----
mae_plot <- y_metrics %>% 
  ggplot(aes(x = mae, y = model, fill = method)) +
  geom_col(position = position_dodge2()) +
  scale_fill_brewer(palette="BrBG") +
  geom_text(aes(label = round(mae, 2)), position = position_dodge(0.9), hjust = 1.5, color = 'white', fontface = 'bold') +
  labs(x = 'MAE', y = NULL) +
  jtools::theme_apa() ; mae_plot

rmse_plot <- y_metrics %>% 
  ggplot(aes(x = rmse, y = model, fill = method)) +
  geom_col(position = position_dodge2()) +
  scale_fill_brewer(palette="BrBG") +
  geom_text(aes(label = round(rmse, 2)), position = position_dodge(.9), hjust = 1.5, color = 'white', fontface = 'bold') +
  labs(x = 'RMSE', y = NULL) +
  jtools::theme_apa() ; rmse_plot

r2_plot <- y_metrics %>% 
  ggplot(aes(x = rsquare, y = model, fill = method)) +
  geom_col(position = position_dodge2()) +
  scale_fill_brewer(palette="BrBG") +
  geom_text(aes(label = round(rsquare, 3)), position = position_dodge(.9), hjust = 1.5, color = 'white', fontface = 'bold') +
  labs(x = 'R-Squared', y = NULL) +
  jtools::theme_apa() ; r2_plot

## Coefficient Plot
y_coefs <- left_join(
  rownames_to_column(y_ridge_adj2_coefs, var = 'var_name'),
  rownames_to_column(y_trimmed_ridge_adj2_coefs, var = 'var_name'), by = 'var_name') %>% 
  left_join(
  rownames_to_column(y_early_ridge_adj2_coefs, var = 'var_name'), by = 'var_name') %>% 
  left_join(
  rownames_to_column(y_lasso_adj2_coefs, var = 'var_name'), by = 'var_name') %>% 
  left_join(
  rownames_to_column(y_trimmed_lasso_adj2_coefs, var = 'var_name'), by = 'var_name') %>% 
  left_join(
  rownames_to_column(y_early_lasso_adj2_coefs, var = 'var_name'), by = 'var_name') %>% 
  left_join(
  rownames_to_column(y_elastic_adj2_coefs, var = 'var_name'), by = 'var_name') %>% 
  left_join(
  rownames_to_column(y_trimmed_elastic_adj2_coefs, var = 'var_name'), by = 'var_name') %>% 
  left_join(
  rownames_to_column(y_early_elastic_adj2_coefs, var = 'var_name'), by = 'var_name') %>% 
  left_join(
  y_nb_adj2_coefs, by = 'var_name') %>% 
  left_join(
  y_trimmed_nb_adj2_coefs, by = 'var_name') %>% 
  left_join(
  y_early_nb_adj2_coefs,
  by = 'var_name') %>% 
  pivot_longer(-var_name, values_to = 'coefs', names_to = 'method') %>% 
  mutate(model = str_extract(method, 'trimmed|early'),
         across(method, ~str_extract(., 'ridge|lasso|elnet|nb'))) %>% 
  mutate(across(method, ~case_when(. == 'elnet' ~ 'elastic', . == 'nb' ~ 'neg_binomial', T ~ .)),
         across(model, ~replace_na(., 'full')))

coef_plot <- y_coefs %>%
  filter(var_name != '(Intercept)') %>% 
  mutate(across(var_name, ~fct_reorder(., coefs, .na_rm = T)),
         across(model, ~fct_relevel(., 'full', 'trimmed', 'early')),
         across(method, ~fct_relevel(., 'ridge', 'lasso', 'elastic', 'neg_binomial'))) %>%
  ggplot(aes(x = coefs, y = var_name, color = method, shape = model)) +
  geom_vline(xintercept = 0, linetype = 'dotted', color = 'black', size = .5)+
  geom_point() +
  scale_color_brewer(palette="BrBG") +
  labs(x = 'Coefficient Value', y = NULL) +
  jtools::theme_apa() ; coef_plot


t<-ggpubr::ggarrange(mae_plot, rmse_plot, r2_plot, nrow = 1, common.legend = T, legend = T) 

ggpubr::ggarrange(t,coef_plot, ncol = 2 , common.legend = T, legend = 'top', widths = c(1,2))

# Composite Score Calculation ----
## Weight coefficients so that the are normalized across models
### Scores will range from 0-1
y_nb_adj2_coefs <- y_nb_adj2_coefs %>% 
  mutate(nb_coefs_weighted = nb_coefs/sum(abs(nb_coefs)))

y_early_nb_adj2_coefs <- y_early_nb_adj2_coefs %>% 
  mutate(nb_early_coefs_weighted = nb_early_coefs/sum(abs(nb_early_coefs)))

y_test_long <- y_test %>% 
  pivot_longer(-c(unique_id, y), names_to = 'var_name', values_to = 'value')

y_cs <- y_test_long %>% 
  left_join(y_nb_adj2_coefs, by = 'var_name') %>% 
  left_join(y_early_nb_adj2_coefs, by = 'var_name') %>% 
  mutate(base_cs = y_nb_adj2_coefs$nb_coefs_weighted[which(y_nb_adj2_coefs$var_name == '(Intercept)')],
         early_base_cs = y_early_nb_adj2_coefs$nb_early_coefs_weighted[which(y_early_nb_adj2_coefs$var_name == '(Intercept)')]) %>% 
  filter(var_name != '(Intercept)') %>% 
  mutate(weighted_value = (value*nb_coefs_weighted),
         early_weighted_value = (value*nb_early_coefs_weighted)) %>% 
  group_by(unique_id) %>% 
  summarise(y = mean(y), 
            cs = sum(weighted_value, na.rm = T)+ min(base_cs),
            early_cs = sum(early_weighted_value, na.rm = T)+ min(early_base_cs))

cors <- data.frame(
  'performance-y' = cor(x = y_cs$cs, y = y_cs$y),
  'planning-y'= cor(x = y_cs$early_cs, y = y_cs$y))

openxlsx::write.xlsx(list(
  'cor' = cors,
  'planning_cs_desc' = psych::describe(y_cs$early_cs),
  'performance_cs_desc' = psych::describe(y_cs$cs)
), 'cs_desc.xlsx')

openxlsx::write.xlsx(list(
  'planning_weights' = y_early_nb_adj2_coefs,
  'performance_weights' = y_nb_adj2_coefs,
  'calculated_cs_test_data' = y_cs %>% rename(planning_cs = early_cs, performance_cs = cs)),
  file = 'Regression Based Composite Score Weights and Test Calculations.xlsx'
)
