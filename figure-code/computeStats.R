# Author: Jeff Jackson
# Last Updated: 29-JUN-2020
#
# Description: function to compute the summary statistics (mean, standard error,
# confidence interval, etc.) for all values taken at a certain pressure. Return
# a data frame containing the summary statistics at each pressure.

computeStats <- function(ctds, binParam, varToGroup) {

  library(tidyr)
  library(dplyr)

  # source("lower_ci.R")
  # source("upper_ci.R")

  # Extract the required columns from each ctd object in the list.
  df1 <- lapply(ctds, function(x) as.data.frame(x[["data"]][c(binParam, varToGroup)]))

  # Join all of the new data frames into one data frame such that for each
  # value of pressure all of the other parameter's (e.g. temperature) values
  # are listed in the row; one parameter column for each data frame joined. The
  # code is using a full join because even if there is only one value for a
  # certain pressure it is important to capture it.
  df2 <- df1 %>% purrr::reduce(full_join, by = "pressure")

  # Sort the rows by pressure in ascending order.
  sortedDF <- sort(df2$pressure, index.return = TRUE)
  df3 <- df2[sortedDF$ix,]

  # Get first three characters from parameter string.
  s3 <- substr(varToGroup, 1,3)

  # Change the data frame so that the parameter columns associated with a
  # particular pressure are now each a row.
  dfAll <- df3 %>%
    pivot_longer(cols = starts_with(s3), names_to = "parameter", values_to = varToGroup)

  # Compute the mean and 95% confidence intervals on the parameter values for
  # each pressure.
  dfStats <- dfAll %>%
    group_by(pressure) %>%
    summarise(smean = mean(eval(parse(text=paste(varToGroup))), na.rm = TRUE),
              ssd = sd(eval(parse(text=paste(varToGroup))), na.rm = TRUE),
              count = n()) %>%
    mutate(se = ssd / sqrt(count),
           lower_ci = lower_ci(smean, se, count),
           upper_ci = upper_ci(smean, se, count))


  # Return the data frame with the summary statistics at each pressure.
  return(dfStats)

}
