# Simulation for product lauching

#make variables
library(decisionSupport)

make_variables<-function(est,n=1)
  
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i, as.numeric(x[1,i]),envir=.GlobalEnv)}

make_variables(estimate_read_csv(paste("Input_perfume.csv")))

Product_launch_function <- function(x, varnames) {
  
# Calculate fixed cost
  
  Fixed_cost <- sum(Rent_yearly, 
                    Utility_bill,
                    Delivery_cost,
                    Salary_cost,
                    Insurance_cost,
                    Renovation_cost,
                    Advertisement_cost)

# Calculate fixed cost for each new product
  
  Fixed_cost_perfume <- vv (Fixed_cost + Perfume_production_loss,
                            var_CV, n_years)
  
  Fixed_cost_candle <- vv (Fixed_cost + Candle_production_loss,
                           var_CV, n_years)
  
  Fixed_cost_pouch <- vv (Fixed_cost + Candle_production_loss,
                          var_CV, n_years)
  
# Calculate unit cost for each product
  
  Unit_cost_perfume <- vv ((Perfume_cost_raw + 
                            Perfume_cost_production)/ 30,
                           var_CV, n_years)
  
  Unit_cost_candle <- vv ((Candle_cost_raw +
                           Candle_cost_production)/ 30,
                          var_CV, n_years)
  
  Unit_cost_pouch <- vv ((Pouch_cost_raw +
                          Pouch_cost_production)/ 30,
                         var_CV, n_years)

# Calculate sale volumes
  
# Total sale volumes for perfume
  
# Perfume sale voumes if market is good
  
  Chance_perfume_market_yes_no <- chance_event(Chance_perfume_market,
                                               value_if = 1,
                                               value_if_not = 0,
                                               n = 1)
  
  Perfume_sale_volume <- if (Chance_perfume_market_yes_no == 1) {
    vv (Perfume_sale_volume,
    var_CV, n_years)
  } else {
    vv (Perfume_sale_volume - (Perfume_sale_volume * Perfume_sale_decrease),
        var_CV, n_years)
  }
  
# Increase in perfume sale volumes during festive seasons
  
  Chance_perfume_sale_festive_yes_no <- chance_event(Chance_perfume_sale_festive,
                                                     value_if = 1,
                                                     value_if_not = 0,
                                                     n = 1)
  
  Total_perfume_sale_volume <- if (Chance_perfume_sale_festive_yes_no == 1) {
    vv (Perfume_sale_volume + Perfume_festive_sale_volume,
        var_CV, n_years)
  } else {
    vv (Perfume_sale_volume,
        var_CV, n_years)
  }

# Total sale volumes for candle

# Candle sale volumes if market is good
  
  Chance_candle_market_yes_no <- chance_event(Chance_candle_market,
                                              value_if = 1,
                                              value_if_not = 0,
                                              n = 1)
  
  Candle_sale_volume <- if (Chance_candle_market_yes_no == 1) {
    vv (Candle_sale_volume,
        var_CV, n_years)
  } else {
    vv (Candle_sale_volume - (Candle_sale_volume * Candle_sale_decrease),
        var_CV, n_years)
  }
  
# Increase in candle sales during festive seasons
  
  Chance_candle_sale_festive_yes_no <- chance_event(Chance_candle_sale_festive,
                                                    value_if = 1,
                                                    value_if_not = 0,
                                                    n = 1)
  
  Total_candle_sale_volume <- if (Chance_candle_sale_festive_yes_no == 1) {
    vv (Candle_sale_volume + Candle_festive_sale_volume,
        var_CV, n_years)
  }
  
# Total sale for perfume pouch

# Perfume pouch sale if market is good
  
  Chance_pouch_market_yes_no <- chance_event(Chance_pouch_market,
                                             value_if = 1,
                                             value_if_not = 0,
                                             n = 1)
  
  Pouch_sale_volume <- if (Chance_pouch_market_yes_no == 1) {
    vv (Pouch_sale_volume,
        var_CV, n_years)
  } else {
    vv (Pouch_sale_volume - (Pouch_sale_volume * Pouch_sale_decrease),
        var_CV, n_years)
  }
  
# Increase in pouch sale during festive season
  
  
  
  
  
  
  
  
  
  
  
  
}
