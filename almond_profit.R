#' Almond PrYield function
#' 
#' A function to determine almond yield in response to climate and almond profit in relation to the found almond anomalies
#'
#' @param data A dataset including minimum temperature in Celsisus and precipation for the months of January and February
#' 
#' @param baseline_yield A value indicating normal almond yield production in tons/acre. Given value of 1.
#'
#' @param yearly_cost A value indicating cost of almond production in dollars/year. Given value of 3500. 
#'
#' @param baseline_price A value indicating normal price of almonds without any anomalies in tons/acre. Given value of 4000.
#'
#' @returns Dataframe consisting of year column and revenue
#' 
#' @export
#'
#' @examples
#' 
#' 
almond_yield_profit <- function(data, baseline_yield = 1,
                                yearly_cost = 3500,
                                baseline_price = 4000){
  
  data_filter <- data |> 
    filter(month == 2 | month == 1) |> 
    group_by(month, year) |> 
    summarize(mean_min_temp = mean(tmin_c),
              total_precip = sum(precip)) |> 
    ungroup() |> 
    pivot_wider(
      names_from = month,
      values_from = c(mean_min_temp, total_precip)
    ) |> 
    select(-mean_min_temp_1, - total_precip_2) |> 
    mutate(yield_anomaly = (-0.015 * mean_min_temp_2 - 0.0046 * mean_min_temp_2^2 - 0.07 * total_precip_1 + 0.0043 * total_precip_1^2 + 0.28),
           total_yield = baseline_yield + yield_anomaly,
           price_per_ton = baseline_price + -1.04 * yield_anomaly,
           revenue = price_per_ton * total_yield - yearly_cost
    ) 
  results <- data_filter %>% 
    select(year, revenue)
  
  return(results)
}