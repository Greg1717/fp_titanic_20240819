library(data.table)
library(caret)
library(corrplot)
tr <- read.csv(file = "./data/train.csv")
te <- read.csv(file = "./data/test.csv")
ge <- read.csv(file = "./data/gender_submission.csv")
tr <- as.data.table(tr)
te <- as.data.table(te)
ge <- as.data.table(ge)
head(tr)
tr
head(te)
head(ge)


# Preprocessing ===============================================================
custom_preprocessing <- function(dt) {
  # for testing: dt <- copy(tr)
  # for testing: dt <- copy(te)
  ## Remove NAs =================================================================
  dt <- dt[, colMeans(is.na(dt)) < 0.9, with = FALSE]
  ## Impute Age =================================================================
  age_child_median <- dt[!is.na(Age) & Age <= 18, median(Age)]
  age_adult_median <- dt[!is.na(Age) & Age >  18, median(Age)]
  ### Identify if adult or child based on 'Name'
  dt[, child := fcase(
    Name %like% "(Master|Miss)",
    TRUE,
    Name %like% "(Mr\\.|Mrs\\.|Don\\.|Rev\\.|Dr\\.|Mme\\.|Major\\.|Lady\\.|Sir\\.|Ms\\.|Mlle\\.|Capt\\.|Countess\\.|Col|Jonkheer|Dona\\.)",
    FALSE,
    default = NA
  )]
  dt[is.na(child)]
  # impute Age where missing
  dt[is.na(Age), Age := ifelse(child == TRUE, age_child_median, age_adult_median)]
  remove(age_child_median)
  remove(age_adult_median)
  # impute Fare ===============================================================
  fare_median_Pclass3 <- dt[!is.na(Fare) & Pclass == 3, median(Fare)]
  dt[is.na(Fare) & Pclass == 3, Fare := fare_median_Pclass3]

  # remove irrelevant variables
  dt[, Name := NULL]
  dt[, Cabin := NULL]
  dt[, Embarked := NULL]
  dt[, Ticket := NULL]
  # create family_size
  dt[, family_size := SibSp + Parch + 1]
  dt[, SibSp := NULL]
  dt[, Parch := NULL]
  # 1 character variable remains; convert to factor
  ## Convert All Character Vectors to Factors =================================
  char_cols <- sapply(dt, is.character)
  # review unique values
  lapply(dt[, ..char_cols], unique)
  # review length of unique values
  sapply(lapply(dt[, ..char_cols], unique), length)
  # convert character to factor (in df as it does not work in dt)
  df <- as.data.frame(dt)
  df[char_cols] <- lapply(df[char_cols], factor)
  dt <- as.data.table(df)
  remove(df)
  ## dummyVars ==================================================================
  if ("Survived" %in% names(dt)) {
    dummyAlg <- dummyVars(Survived ~ ., data = dt)
  } else {
    dummyAlg <- dummyVars(~ ., data = dt)
  }
  dt <- predict(dummyAlg, dt)
  ## Remove Correlated Predictors ===============================================
  cors <- cor(dt)
  correlatedPredictors <- findCorrelation(x = cors, cutoff = 0.9)
  corrplot::corrplot(cors)
  dt <- dt[, -correlatedPredictors]
  cors <- cor(dt)
  corrplot::corrplot(cors)
  return(dt)
}

# dt <- custom_preprocessing(tr)

dt <- custom_preprocessing(te)

dt
str(dt)
summary(dt)

# ML
# Compare Performance