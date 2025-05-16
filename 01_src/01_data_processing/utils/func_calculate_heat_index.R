#' @title Calculate Heat Index from Air Temperature and Dew Point
#' @description 
#' Calculates the heat index based on air temperature and dew point temperature using the 
#' National Weather Service (NWS) modified Rothfusz regression formula. The function first calculates
#' relative humidity from dew point using vapor pressure equations, then applies the heat index 
#' calculation with appropriate adjustments based on temperature and humidity ranges.
#'
#' @param df A data frame containing air temperature and dew point columns
#' @param air_temp_col Character string specifying the column name for air temperature
#' @param dew_point_col Character string specifying the column name for dew point temperature
#' @param air_temp_celsius Logical; TRUE if air temperature is provided in Celsius (default = TRUE)
#' @param dew_point_celsius Logical; TRUE if dew point temperature is provided in Celsius (default = TRUE)
#' @param heat_index_celsius Logical; TRUE if heat index output should be in Celsius (default = FALSE)
#' @param heat_index_col Character string specifying the column name for the output heat index (default = "heat_index")
#'
#' @details
#' The heat index calculation follows the NWS implementation of the Rothfusz regression formula:
#' 
#' For heat index values ≥ 80°F, the full Rothfusz regression is used:
#' HI = -42.379 + 2.04901523*T + 10.14333127*RH - 0.22475541*T*RH - 0.00683783*T^2 - 
#'      0.05481717*RH^2 + 0.00122874*T^2*RH + 0.00085282*T*RH^2 - 0.00000199*T^2*RH^2
#' 
#' With adjustments:
#' - When RH < 13% and 80°F < T < 112°F: 
#'   Adjustment = -((13-RH)/4) * sqrt((17-abs(T-95))/17)
#' - When RH > 85% and 80°F < T < 87°F:
#'   Adjustment = +((RH-85)/10) * ((87-T)/5)
#' 
#' For heat index values < 80°F, a simpler formula is used:
#' HI = 0.5 * (T + 61.0 + ((T-68.0)*1.2) + (RH*0.094))
#' 
#' For vapor pressure and relative humidity calculations:
#' - Saturation vapor pressure is calculated using an exponential relationship
#' - Actual vapor pressure is derived from dew point temperature
#' - Relative humidity is calculated as the ratio of actual to saturation vapor pressure
#'
#' @note 
#' The heat index formula is most applicable for temperatures ≥ 80°F (26.7°C) and
#' relative humidity ≥ 40%. The function handles lower values using the NWS simplified equation.
#'
#' @references
#' Rothfusz, L.P. (1990). The Heat Index Equation. National Weather Service Technical Attachment SR 90-23.
#' 
#' National Weather Service. Heat Index Equation. 
#' https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml
#' 
#' National Physical Laboratory. Dew Point and Relative Humidity.
#' https://www.npl.co.uk/resources/q-a/dew-point-and-relative-humidity
#'
#' @return A data frame with all original columns plus an additional column containing 
#' the calculated heat index values
#'
#' @examples
#' # Create sample data frame with air temperature and dew point in Celsius
#' weather_data <- data.frame(
#'   air_temp = c(30, 32, 35, 37, 28), 
#'   dew_point = c(22, 24, 28, 25, 20)
#' )
#' 
#' # Calculate heat index (output in Fahrenheit)
#' result_f <- calculate_heat_index(
#'   weather_data, 
#'   "air_temp", 
#'   "dew_point", 
#'   air_temp_celsius = TRUE,
#'   dew_point_celsius = TRUE,
#'   heat_index_celsius = FALSE
#' )
#' 
#' # Calculate heat index (output in Celsius)
#' result_c <- calculate_heat_index(
#'   weather_data, 
#'   "air_temp", 
#'   "dew_point", 
#'   air_temp_celsius = TRUE,
#'   dew_point_celsius = TRUE,
#'   heat_index_celsius = TRUE
#' )
#'
#' @export

calculate_heat_index <- function(df, 
                                air_temp_col, 
                                dew_point_col, 
                                air_temp_celsius = TRUE, 
                                dew_point_celsius = TRUE,
                                heat_index_celsius = FALSE,
                                heat_index_col = "heat_index") {
  
  # Make a copy of the dataframe to avoid modifying the original
  result_df <- df
  
  # Convert air temperature to Fahrenheit if needed for heat index calculation
  if (air_temp_celsius) {
    air_temp_f <- df[[air_temp_col]] * 9/5 + 32
  } else {
    air_temp_f <- df[[air_temp_col]]
  }
  
  # Convert dew point to Fahrenheit if needed for vapor pressure calculations
  if (dew_point_celsius) {
    dew_point_f <- df[[dew_point_col]] * 9/5 + 32
  } else {
    dew_point_f <- df[[dew_point_col]]
  }
  
  # Convert temperatures to Kelvin for vapor pressure calculations
  air_temp_k <- (air_temp_f - 32) * 5/9 + 273.15
  dew_point_k <- (dew_point_f - 32) * 5/9 + 273.15
  
  # Calculate saturation vapor pressure at air temperature (using Equation 2 from the reference)
  # The formula works on temperature in Celsius, so convert from Kelvin
  air_temp_c <- air_temp_k - 273.15
  ln_es <- 611.2 + (17.62 * air_temp_c)/(243.12 + air_temp_c)
  es <- exp(ln_es)  # Saturation vapor pressure in Pa
  
  # Calculate actual vapor pressure from dew point (using Equation 2 from the reference)
  # The formula works on temperature in Celsius, so convert from Kelvin
  dew_point_c <- dew_point_k - 273.15
  ln_e <- 611.2 + (17.62 * dew_point_c)/(243.12 + dew_point_c)
  e <- exp(ln_e)  # Actual vapor pressure in Pa
  
  # Calculate relative humidity (using Equation 1 from the reference)
  rh <- 100 * e / es
  
  # Ensure relative humidity is between 0 and 100 percent
  rh <- pmax(0, pmin(100, rh))
  
  # Calculate heat index using the Rothfusz regression (from the second image)
  hi <- -42.379 + 
        2.04901523 * air_temp_f + 
        10.14333127 * rh - 
        0.22475541 * air_temp_f * rh - 
        0.00683783 * air_temp_f^2 - 
        0.05481717 * rh^2 + 
        0.00122874 * air_temp_f^2 * rh + 
        0.00085282 * air_temp_f * rh^2 - 
        0.00000199 * air_temp_f^2 * rh^2
  
  # Apply adjustments as specified in the reference
  # Adjustment when RH < 13% and 80°F < T < 112°F
  adjustment1 <- ifelse(rh < 13 & air_temp_f > 80 & air_temp_f < 112,
                       ((13 - rh) / 4) * sqrt((17 - abs(air_temp_f - 95)) / 17),
                       0)
  hi <- hi - adjustment1
  
  # Adjustment when RH > 85% and 80°F < T < 87°F
  adjustment2 <- ifelse(rh > 85 & air_temp_f > 80 & air_temp_f < 87,
                       ((rh - 85) / 10) * ((87 - air_temp_f) / 5),
                       0)
  hi <- hi + adjustment2
  
  # Use simple formula for low heat index values
  simple_hi <- 0.5 * (air_temp_f + 61.0 + ((air_temp_f - 68.0) * 1.2) + (rh * 0.094))
  hi <- ifelse(simple_hi < 80, simple_hi, hi)
  
  # Add heat index to the result dataframe
  # Convert to Celsius if requested
  if (heat_index_celsius) {
    hi_output <- (hi - 32) * 5/9
  } else {
    hi_output <- hi
  }
  
  result_df[[heat_index_col]] <- hi_output
  
  return(result_df)
}

# Example usage:
# df <- data.frame(air_temp = c(30, 32, 35), dew_point = c(22, 24, 28))
# result <- calculate_heat_index(df, "air_temp", "dew_point", 
#                               air_temp_celsius = TRUE, 
#                               dew_point_celsius = TRUE,
#                               heat_index_celsius = TRUE,
#                               heat_index_col = "heat_index")
