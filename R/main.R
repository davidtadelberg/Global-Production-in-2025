# main.R
#
# David Adelberg
# david.adelberg@yale.edu
#
# Outputs predicted GDP data
#

# Main function
# 1) Loads data into memory
#   a) Data comes from Quandl
#     i)  UGID  = United Nations Global Indicators
#     ii) ODA   = IMF Cross Country Macroeconomic Statistics
#   b) Data may be stored on disk or may be fetched from Quandl
# 2) Processes Data, calculating country statistics
#   a) Median Population Growth
#   b) Median Investment
#   c) Convergence Indicator (based off Recent Per Capita GDP)
#   d) Median Inflation
#   e) Mean Historic GDP Growth Rate
# 3) Fits a linear model to this data
# 4) Outputs predicted GDP for the world's countries.
#
main <- function() {
  options = get_options()
  init(options)
  my_data = get_data(options)
  processed_data = process_data(options, my_data)
  predictions = make_predictions(options, processed_data)
  predictions = format_predictions(options, predictions)
  export(predictions$test, options$files$test_predictions)
  export(predictions$predictions, options$files$predictions)
}

# Creates an object containing the program options
# One can easily change program behavior by modifying this function
get_options <- function() {
  dbs = c("UGID", "ODA") # These are the databases used
  selectors = list(UGID = un_select, ODA = imf_select) # These are the functions used to select wanted codes
  reload = list(r_init = TRUE, # Reload R libraries?
                quandl_init = TRUE, # Reload quandl?
                data_init = FALSE, # Redownload country code data from server?
                get_data = list(UGID = FALSE, ODA = FALSE), # Redownload data from these databases?
                get_codes = list(UGID = FALSE, ODA = FALSE)) # Redownload codes for these databases?
  make_predictions = list(print = FALSE, m = 20, seed = 50) # Multiple Imputation by Chained Equations (mice) library options. m is number of imputations.
  files = list(wanted_data = setNames(as.list(paste(dbs, "wanted_data.csv", sep="")), dbs), # UGID => UGIDwanted_data.csv, ODA => ODAwanted_data.csv
               wanted_codes = setNames(as.list(paste(dbs, "wanted_codes.csv", sep="")), dbs), # UGID => UGIDwanted_codes.csv, ODA => ODAwanted_codes.csv
               test_predictions = "gdp_test_predictions.csv", # Output test predictions to this file
               predictions = "gdp_predictions.csv", # Output predictions to this file
               iso_codes = "iso_codes.csv" # ISO codes in this file
  )
  dates = list(max_testing_calibration = as.Date("2003-12-31", format="%Y-%m-%d"), # When calibrating model, stop with 2003 data (avoid lookahead bias)
               most_recent_data = as.Date("2014-12-31", format="%Y-%m-%d"), # Ignore IMF's predictions for the future
               prediction_date = as.Date("2025-12-31", format="%Y-%m-%d")) # Predict for this date
  quandl = list(api_key = '4M_9pg6yjNyhuczTH46L') # Quandl API key
  
  options = list(dbs = dbs,
                 selectors = selectors,
                 reload = reload,
                 make_predictions = make_predictions,
                 files = files,
                 dates = dates,
                 quandl = quandl
  )
}

# Format predictions
# Create a table with columns country, predicted % growth, actual % growth
format_predictions <- function(options, predictions) {
  colnames(predictions$test) <- c("Country", 
                                  paste("Predicted (% growth)", options$dates$most_recent_data, sep=", "),
                                  paste("Actual (% growth, estimated missing data)", options$dates$most_recent_data, sep=", "))
  predictions$test[,2:3] = (predictions$test[,2:3]-1)*100
  
  colnames(predictions$predictions) <- c("Country", 
                                         paste("Predicted (% growth)", options$dates$prediction_date, sep=", "))
  predictions$predictions[,2] = (predictions$predictions[,2]-1)*100
  
  predictions
}

# Outputs a data frame with predictions
# Data has missing values for some predictors for some countries. Some countries don't even have any GDP data.
# I use multiple imputation to estimate predictors and predicted values
# This allows me to estimate GDP for all countries, even those with missing data
make_predictions <- function(options, processed_data) {
  mi_data = mice(processed_data,
                     print = options$make_predictions$print, # print imputations?
                     m = options$make_predictions$m, # number of imputations
                     seed = options$make_predictions$seed) # seed (for repeatability)
  
  test = with(mi_data, exp=data.frame(country = country, 
                                         predicted = exp(predict(lm(log(eleven_year_gdp_growth.calibration) ~ 
                                                                      log(median_population_growth.calibration) +
                                                                      log(median_investment.calibration) +
                                                                      convergence_indicator.calibration +
                                                                      log(inflation.calibration) +
                                                                      log(mean_historic_gdp_growth_rate.calibration)))),
                                         actual = eleven_year_gdp_growth.calibration))

  predictions = with(mi_data, exp=data.frame(country = country, 
                                         predicted = exp(predict(lm(log(eleven_year_gdp_growth.calibration) ~ 
                                                                      log(median_population_growth.calibration) +
                                                                      log(median_investment.calibration) +
                                                                      convergence_indicator.calibration +
                                                                      log(inflation.calibration) +
                                                                      log(mean_historic_gdp_growth_rate.calibration)),
                                                                 newdata = data.frame( # predict expects the data to be .calibration, so that's what I do here.
                                                                   country=country,
                                                                   median_population_growth.calibration=median_population_growth.prediction,
                                                                   median_investment.calibration=median_investment.prediction,
                                                                   convergence_indicator.calibration=convergence_indicator.prediction,
                                                                   inflation.calibration=inflation.prediction,
                                                                   mean_historic_gdp_growth_rate.calibration=mean_historic_gdp_growth_rate.prediction)
                                                                 ))))
  # I will average over several data frames in the following lines of code.
  
  lapply(list(test=test, predictions=predictions), function(el) {
    country = el$analyses[[1]]$country
    results = lapply(el$analyses, function(el) {el[-1]})
    results = Reduce('+', results) / length(results)
    cbind(country, results)
  })
}

# Gets the country names from an imf data frame
# The dataframe should be in the format Country - Indicator
get_imf_countries <- function(df) {
  names = str_split(colnames(df), " - ", 2)
  sapply(names, function(el) {el[1]})
}

# Gets a subset of a data frame where the colname matches a pattern.
match_df_colnames <- function(df, pattern) {
  res = df[, grep(pattern, colnames(df))]
}

# Returns data processed for calibration and data processed for prediction
# Calculates country statistics from Quandl data: Median Population Growth, Median Investment, Convergence Indicator (based off Log Per Capita GDP), Median Inflation, and Historic GDP growth
# Calculates Recent GDP growth for calibration.
process_data <- function(options, data) {
  un_data = data[[1]]
  imf_data = data[[2]]
  
  gdp_growth_rate = function(dta) {
    gdps = match_df_colnames(dta, "*Country GDP based on PPP Valuation, USD Billions")
    countries = get_imf_countries(gdps)
    growth_rate = sapply(gdps, function(col) {
      exp(mean(log((c(col[-1], NA) / col)), na.rm=TRUE))
    })
    names(growth_rate) = countries
    growth_rate
  }
  
  processed_data = lapply(c(options$dates$max_testing_calibration, options$dates$most_recent_data), function(date) {
    # Ignore dates larger than date
    modified_data = imf_data[as.Date(imf_data$Date, format="%Y-%m-%d") <= date, ]
    
    # Process Population Data
    popcols = match_df_colnames(modified_data, "*Population, Millions")
    countries = get_imf_countries(popcols)
    median_pop_growth = sapply(popcols, function(col) {
      median(c(col[-1], NA) / col, na.rm=TRUE)
    })
    population_frame = data.frame(country=countries, median_population_growth = median_pop_growth) # This frame contains median population growth
    
    # Process investment Data
    invcols = match_df_colnames(modified_data, "*Total Investment, % of GDP")
    countries = get_imf_countries(invcols)
    median_investment = sapply(invcols, function(col) {
      median(col, na.rm=TRUE)
    })
    investment_frame = data.frame(country=countries, median_investment = median_investment) # This frame contains median investment
    
    # Process per capita gdp data
    gdppc = match_df_colnames(modified_data, "*GDP per Capita at Current Prices")
    gdppc = tail(gdppc, 1)
    countries = get_imf_countries(gdppc)
    log_gdppc = log(as.numeric(gdppc))
    convergence_frame = data.frame(country=countries, convergence_indicator = (log_gdppc - median(log_gdppc, na.rm=TRUE)), row.names = NULL) # This frame contains log per capita GDP data
    
    # Process inflation (GDP Deflator) data
    deflator = match_df_colnames(modified_data, "*GDP Deflator")
    countries = get_imf_countries(deflator)
    inflation_rate = sapply(deflator, function(col) {
      median(c(col[-1], NA) / col, na.rm=TRUE)
    })
    inflation_frame = data.frame(country=countries, inflation=inflation_rate) # This frame contains median inflation rates
    
    # Process Historic GDP Growth Rates
    historic_gdp_frame = data.frame(country=countries, mean_historic_gdp_growth_rate=gdp_growth_rate(modified_data)) # This frame contains historic GDP growth rates
    
    # Merge all of these calculated frames
    frames = list(population_frame, investment_frame, convergence_frame, inflation_frame, historic_gdp_frame)
    merged = Reduce(function(x,y) merge(x,y, all=TRUE), frames)
  })
  
  dates = as.Date(imf_data$Date, format="%Y-%m-%d")
  # Get recent GDP growth
  sub_data = imf_data[dates <= options$dates$most_recent_data & dates >= options$dates$max_testing_calibration, ]
  gdp_growth = gdp_growth_rate(sub_data)^(nrow(sub_data)-1)
  
  current_gdp_frame = data.frame(country=names(gdp_growth), eleven_year_gdp_growth = gdp_growth) # This frame contains GDP growth over 11 years
  
  processed_data[[1]] = merge(processed_data[[1]], current_gdp_frame)
  
  colnames(processed_data[[1]])[-1] = paste(colnames(processed_data[[1]])[-1], "calibration", sep=".")
  colnames(processed_data[[2]])[-1] = paste(colnames(processed_data[[2]])[-1], "prediction", sep=".")
  
  merge(processed_data[[1]], processed_data[[2]])
}
  

# If Codes are stored on disk, load them; else, download from server
get_codes <- function(options, db) {
  codes_path = options$files$wanted_codes[[db]]
  if(file.exists(codes_path) & !options$reload$get_codes[[db]]) {
    import(codes_path)
  }
  else {
    wanted_codes = as.data.frame(get_wanted_api_codes(options, db))
    export(wanted_codes, codes_path)
    wanted_codes[[1]]
  }
}

# Transform the Quandl Name
# Into the format Country - Data Field
process_name <- function(name) {
  db_rest = str_split(name, "\\.", 2)[[1]]
  if (db_rest[1] == "UGID") {
    country_rest = str_split(str_split(db_rest[2], "NATAC_10_")[[1]][2], " - ", 2)[[1]] # Use the version 10 system for national accounts collection
    nn = paste(
      subset(country_codes.df, Code==country_rest[1])$Name,
      country_rest[2],
      sep=" - ")
    nn
  }
  else if (db_rest[1] == "ODA") {
    country_rest = str_split(db_rest[2], "_", 2)[[1]]
    paste(
      subset(country_codes.df, Code==country_rest[1])$Name,
      subset(imf_codes.df, Code==str_split(country_rest[2], " - ", 2)[[1]][1])$Name,
      sep=" - ")
  }
}

# Get Wanted Data From Server or Disk
get_data <- function(options) {
  sapply(options$dbs, function(db) {
    data_path = options$files$wanted_data[[db]]
    if(file.exists(data_path) & !options$reload$get_data[[db]]) { # If the data is on Disk and we don't want to reload it, import the data
      import(data_path)
    }
    else {
      codes = get_codes(options, db)
      
      dta = Quandl(codes, type="raw", collapse="annual")
      colnames(dta)[-1] = sapply(colnames(dta)[-1], process_name) # Convert API names to English
      export(dta, data_path)
      dta
    }
  })
}

# Initialize R libraries, Quandl, and Country code data
init <- function(options) { 
  r_init(options)
  quandl_init(options)
  data_init(options)
}

# initialize r libraries
r_init <- function(options) {
  if (options$reload$r_init) {
    if (FALSE %in% sapply(c("downloader", "rio", "bit64", "stringr", "mice"),
                          function(l) {require(l, character.only = TRUE)})) {
      print("error loading libraries")
    }
  }
}

# initialize Quandl API
quandl_init <- function (options) {
  if (options$reload$quandl_init) {
    if (!require(Quandl)) {
      print("Couldn't use quandl library")
    }
    else {
      Quandl.api_key(options$quandl$api_key)
    }
  }
}

# Creates a global variable country_codes.df that stores country codes and their translations to English.
# Also creates a global variable imf_codes.df that stores Quandl IMF API codes and their translations to English. 
data_init <- function(options) {
  file_path = options$files$iso_codes
  if (file.exists(file_path) & !options$reload$data_init) {
    country_codes.df <<- import(file_path)
  }
  else {
    url = "https://s3.amazonaws.com/quandl-static-content/API+Descriptions/WHO/ccodes.txt"
    download(url, dest=file_path, mode="wb")
    country_codes.df <<- read.csv(file_path, sep="|")
    
    colnames(country_codes.df) = c("Code", "Name")
    
    # Additional regional codes used by the UN dataset
    country_codes.df <<- rbind(country_codes.df, data.frame(
      Code=c("UVK", "TWN", 5200, 5310, 5100, 5206, 5204, 5815, 5817, 5803, 5207, 5304, 5001), # IMF and WHO disagree on status of Kosovo and Taiwan
      Name=c("Kosovo", "Taiwan", "Americas+","Asia+","Africa+","Caribbean+","Central America+", 
             "Low Income Food Deficit Countries+", "Net Food Importing Developing Countries+",
             "Small Island Developing States+", "South America+", "South-Eastern Asia+", "World+")
    ))
    
    export(country_codes.df, file_path)
  }
  
  imf_codes.df <<- data.frame(
    Code = c("PPPGDP", "NGDP_D", "NID_NGDP", "NGDPD", "LP", 
             "BCA_NGDPD", "NGDPDPC"),
    Name = c("Country GDP based on PPP Valuation, USD Billions",
              "GDP Deflator", "Total Investment, % of GDP",
              "GDP at Current Prices, USD Billions", "Population, Millions",
              "Current Account Balance, % of GDP", "GDP per Capita at Current Prices"
             )
  )
  
  country_codes.df
}

# Get API codes for a Quandl database
quandl_all_db_codes <- function(options, db) {
   url = "https://www.quandl.com/api/v3/databases/"
   url = paste(url, db, "/codes", "?api_key=", options$quandl$api_key, sep="")
   path = paste(db, "_codes.zip", sep="")
   download(url, dest=path, mode="wb")
   data = read.csv(unzip(path))
}

# Select from codes where code begins with prefix
prefix_select <- function(codes, prefix) {
  pattern = paste(prefix, "*", sep="")
  grep(pattern, codes)
}

# Get wanted UN National Accounts data
un_select <- function(codes) {
  prefix_select(codes, "UGID/NATAC_10_") # Use Version 10 of National Accounts Data
}

# Select wanted API codes in order to get wanted data
imf_select <- function(codes) {
  postfixes = c("PPPGDP", "NGDP_D", "NID_NGDP", "NGDPD", "LP") # Use these indicators
  pattern = paste("*", paste(postfixes, collapse="|"), sep="")
  grep(pattern, codes)
}

# Gets wanted api codes
get_wanted_api_codes <- function(options, db) {
  codes.df = quandl_all_db_codes(options, db)
  if (is.null(codes.df)) {
    print("Error loading data")
  }
  else {
    colnames(codes.df) = c("Code", "Explanation")
    codes.df$Code[options$selectors[[db]](codes.df$Code)]
  }
}

main()