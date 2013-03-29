normalize.by.denominator <- function(m)
{
  data.dict <- read.csv("data/data_dictionary.csv")
  names(data.dict)[1] <- "Var"
  # Tot 2010 Census Pop
  for (i in data.dict$Var[data.dict$Denominator.note == "Tot 2010 Census Pop"])
    m[, i] <- m[, i] / m$Tot_Population_CEN_2010
  
  # Tot ACS Pop
  for (i in data.dict$Var[data.dict$Denominator.note == "Tot ACS Pop"])
    m[, i] <- m[, i] / m$Tot_Population_ACS_06_10

  # ACS Pop 5+ Years
  for (i in data.dict$Var[data.dict$Denominator.note == "ACS Pop 5+ Years"])
    m[, i] <- m[, i] / (m$Tot_Population_ACS_06_10 - m$Pop_under_5_ACS_06_10)

  # ACS Pop 25+
  for (i in data.dict$Var[data.dict$Denominator.note == "ACS Pop 25+"])
    m[, i] <- m[, i] / (m$Tot_Population_ACS_06_10 - m$Pop_under_5_ACS_06_10 - 
                          m$Pop_5_17_ACS_06_10 - m$Pop_18_24_ACS_06_10)

  # ACS Poverty Universe
  for (i in data.dict$Var[data.dict$Denominator.note == "ACS Poverty Universe"])
    m[, i] <- m[, i] / m$Pov_Univ_ACS_06_10

  # ACS Pop 1 year or over
  for (i in data.dict$Var[data.dict$Denominator.note == "ACS Pop 1 year or over"])
    m[, i] <- m[, i] / m$Pop_1yr_Over_ACS_06_10

  # 2010 Census Related Family Households
  for (i in data.dict$Var[data.dict$Denominator.note == "2010 Census Related Family Households"])
    m[, i] <- m[, i] / m$Rel_Family_HHDS_CEN_2010
  
  # ACS Related Family Households
  for (i in data.dict$Var[data.dict$Denominator.note == "ACS Related Family Households"])
    m[, i] <- m[, i] / m$Rel_Family_HHD_ACS_06_10
  
  # Tot ACS occupied HUs
  for (i in data.dict$Var[data.dict$Denominator.note == "Tot ACS occupied HUs"])
    m[, i] <- m[, i] / m$Tot_Occp_Units_ACS_06_10
  
  # Tot 2010 Census occupied HUs
  for (i in data.dict$Var[data.dict$Denominator.note == "Tot 2010 Census occupied HUs"])
    m[, i] <- m[, i] / m$Tot_Occp_Units_CEN_2010

  # 2010 Census Tot HUs
  for (i in data.dict$Var[data.dict$Denominator.note == "2010 Census Tot HUs" | 
                            data.dict$Denominator.note == "Tot_Housing_Units_CEN_2010"])
    m[, i] <- m[, i] / m$Tot_Housing_Units_CEN_2010
  
  # ACS Tot HUs
  for (i in data.dict$Var[data.dict$Denominator.note == "ACS Tot HUs"])
    m[, i] <- m[, i] / m$Tot_Housing_Units_ACS_06_10
  
  # Total ACS OCC HU
  for (i in data.dict$Var[data.dict$Denominator.note == "Total ACS OCC HU"])
    m[, i] <- m[, i] / m$Owner_Occp_HU_ACS_06_10
  
  # MailBack_Area_Count_CEN_2010
  for (i in data.dict$Var[data.dict$Denominator.note == "MailBack_Area_Count_CEN_2010"])
    m[, i] <- m[, i] / m$MailBack_Area_Count_CEN_2010
  
  return(m)
}

exclude.columns <- function(m)
{
  # Remove columns that contain duplicated info (e.g. state names and state codes)
  # Also remove columns that are margin of errors
    excl <- !(1:ncol(m) %in% c(1, 2, 4, 8, grep(".*MOE.*", names(m))))
  m <- m[, excl]
  return(m)
}

impute.averages <- function(m) {
  # Note that county and tract codes are not unique.  The only way to uniquely
  # identify a tract appears to be the state-county-tract combo
  col.name <- names(m)
  col.name <- col.name[!(col.name %in% c("GIDBG", "State", "State_name", "County", "County_name",
                                         "Tract", "Block_Group", "Flag", "AIAN_LAND"))]

  tract.code <- paste(m$State_name, m$County_name, m$Tract)
  county.code <- paste(m$State_name, m$County_name)
  for (col in col.name) {
    ind <- (1:nrow(m))[is.na(m[, col])]
    if (length(ind) == 0)
      next
    message(sprintf("Imputing column %s", col))
    avg <- list()
    n <- 1
    for (i in ind) {
      if ((n %% 100) == 0) {
        cat(sprintf("... %d/%d", n, length(ind)), fill=T)
      }
      if (!is.null(avg[[tract.code[i]]])) {
        m[i, col] <- avg[[tract.code[i]]]
      }
      else if (!is.null(avg[[county.code[i]]])) {
        m[i, col] <- avg[[county.code[i]]]
      }
      else {
        state <- m$State_name[i]
        county <- m$County_name[i]
        tract <- m$Tract[i]
        sub.tract <- subset(m, State_name == state & County_name == county & Tract == tract,
                            select=col)
        if (sum(is.na(sub.tract[, col])) < nrow(sub.tract)/2) {
          avg[[tract.code[i]]] <- mean(sub.tract[, col], na.rm=T)
          m[i, col] <- avg[[tract.code[i]]]
#           message(sprintf("%s - %f - %d", tract.code[i], m[i, col], i))
        }
        else {
          sub.county <- subset(m, State_name == state & County_name == county, select=col)
          avg[[county.code[i]]] <- mean(sub.county[, col], na.rm=T)
          m[i, col] <- avg[[county.code[i]]]
#           message(sprintf("%s - %f - %d", county.code[i], m[i, col], i))
        }
      }
      n <- n + 1
    }
  }
  return(m)
}

impute.averages.county.level <- function(m) {
  # Note that county and tract codes are not unique.  The only way to uniquely
  # identify a tract appears to be the state-county-tract combo
  col.name <- names(m)
  col.name <- col.name[!(col.name %in% c("GIDBG", "State", "State_name", "County", "County_name",
                                         "Tract", "Block_Group", "Flag", "AIAN_LAND"))]

  county.code <- paste(m$State_name, m$County_name)
  for (col in col.name) {
    ind <- (1:nrow(m))[is.na(m[, col])]
    if (length(ind) == 0)
      next
    message(sprintf("Imputing column %s", col))
    avg <- list()
    for (i in ind) {
      if (!is.null(avg[[county.code[i]]])) {
        m[i, col] <- avg[[county.code[i]]]
      }
      else {
        state <- m$State_name[i]
        county <- m$County_name[i]
        sub.county <- subset(m, State_name == state & County_name == county, select=col)
        avg[[county.code[i]]] <- mean(sub.county[, col], na.rm=T)
        m[i, col] <- avg[[county.code[i]]]
#           message(sprintf("%s - %f - %d", county.code[i], m[i, col], i))
      }
    }
  }
  return(m)
}

preprocess <- function(m)
{
  m <- impute.averages(m)
  m <- normalize.by.denominator(m)
  m <- exclude.columns(m)
  ## m <- impute.averages.county.level(m)
  return(m)
}

load("data/train.RData")
processed.data <- preprocess(train)
original.data <- train
save(processed.data, original.data, file="data/processed_data.RData")
