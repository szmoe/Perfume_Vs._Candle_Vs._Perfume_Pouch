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
  Fixed_cost

# Calculate fixed cost for each new product
  
  Fixed_cost_perfume <- vv (Fixed_cost + Perfume_production_loss,
                            var_CV, n_years)
  
  Fixed_cost_candle <- vv (Fixed_cost + Candle_production_loss,
                           var_CV, n_years)
  
  Fixed_cost_pouch <- vv (Fixed_cost + Pouch_production_loss,
                          var_CV, n_years)
  
# Calculate unit cost for each product
  
  Unit_cost_perfume <- vv ((Perfume_cost_raw + 
                            Perfume_cost_production)/ 200,
                           var_CV, n_years)
  Unit_cost_perfume
  
  Unit_cost_candle <- vv ((Candle_cost_raw +
                           Candle_cost_production)/ 300,
                          var_CV, n_years)
  Unit_cost_candle
  
  Unit_cost_pouch <- vv ((Pouch_cost_raw +
                          Pouch_cost_production)/ 350,
                         var_CV, n_years)
  Unit_cost_pouch

# Calculate sale volumes and selling price
  
# Total sale volumes for perfume
  
# Perfume sale voumes if market is good
  
  Chance_perfume_market_yes_no <- chance_event(Chance_perfume_market,
                                               value_if = 1,
                                               value_if_not = 0,
                                               n = 1)
  
  Perfume_sale_volume <- if (Chance_perfume_market_yes_no == 1) {
    vv (Perfume_sale_volume + (Perfume_sale_volume *Perfume_sale_increase),
    var_CV, n_years)
  } else {
    vv (Perfume_sale_volume,
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
  
# Reduction in selling price due to market competition if good market is good
  
  Chance_perfume_competitor <- if (Chance_perfume_market_yes_no == 1) {
    vv (Chance_perfume_competitor,
        var_CV, n_years)
  } else {
    Chance_perfume_competitor <- 0
  }
  
  Chance_perfume_competitor_yes_no <- chance_event(Chance_perfume_competitor,
                                                   value_if = 1,
                                                   value_if_not = 0,
                                                   n = 1)
  
  Perfume_selling_price_adjusted <- if (Chance_perfume_competitor_yes_no == 1) {
    vv (Perfume_selling_price - (Perfume_selling_price *
                                 Reduced_perfume_selling_price),
        var_CV, n_years)
  } else {
    vv (Perfume_selling_price,
        var_CV, n_years)
  }
  

# Total sale volumes for candle

# Candle sale volumes if market is good
  
  Chance_candle_market_yes_no <- chance_event(Chance_candle_market,
                                              value_if = 1,
                                              value_if_not = 0,
                                              n = 1)
  
  Candle_sale_volume <- if (Chance_candle_market_yes_no == 1) {
    vv (Candle_sale_volume + (Candle_sale_volume * Candle_sale_increase),
        var_CV, n_years)
  } else {
    vv (Candle_sale_volume,
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
  } else {
    vv (Candle_sale_volume,
        var_CV, n_years)
  }
  
# Reduction in selling price due to market competition if good market is good
  
  Chance_candle_competitor <- if (Chance_candle_market_yes_no == 1) {
    vv (Chance_candle_competitor,
        var_CV, n_years)
  } else {
    Chance_candle_competitor <- 0
  }
  
  Chance_candle_competitor_yes_no <- chance_event(Chance_candle_competitor,
                                                  value_if = 1,
                                                  value_if_not = 0,
                                                  n = 1)
  
  Candle_selling_price_adjusted <- if (Chance_candle_competitor_yes_no == 1) {
    vv (Candle_selling_price - (Candle_selling_price *
                                Reduced_candle_selling_price),
        var_CV, n_years)
  } else {
    vv (Candle_selling_price,
        var_CV, n_years)
  }
    
  
# Total sale for perfume pouch

# Perfume pouch sale if market is good
  
  Chance_pouch_market_yes_no <- chance_event(Chance_pouch_market,
                                             value_if = 1,
                                             value_if_not = 0,
                                             n = 1)
  
  Pouch_sale_volume <- if (Chance_pouch_market_yes_no == 1) {
    vv (Pouch_sale_volume + (Pouch_sale_volume * Pouch_sale_increase),
        var_CV, n_years)
  } else {
    vv (Pouch_sale_volume ,
        var_CV, n_years)
  }
  
# Increase in pouch sale during festive season
  
  Chance_pouch_sale_festive_yes_no <- chance_event(Chance_pouch_sale_festive,
                                                   value_if = 1,
                                                   value_if_not = 0,
                                                   n = 1)
  
  Total_pouch_sale_volume <- if (Chance_pouch_sale_festive_yes_no == 1) {
    vv (Pouch_sale_volume + Pouch_festive_sale_volume,
        var_CV, n_years)
  } else {
    vv (Pouch_sale_volume,
        var_CV, n_years)
  }
  
# Reduction in selling price due to market competition if good market is good
  
  Chance_pouch_competitor <- if (Chance_pouch_market_yes_no == 1) {
    vv (Chance_pouch_competitor,
        var_CV, n_years)
  } else {
    Chance_pouch_competitor <- 0
  }
  
  Chance_pouch_competitor_yes_no <- chance_event(Chance_pouch_competitor,
                                                 value_if = 1,
                                                 value_if_not = 0,
                                                 n = 1)
  
  Pouch_selling_price_adjusted <- if (Chance_pouch_competitor_yes_no == 1) {
    vv (Pouch_selling_price - (Pouch_selling_price *
                               Reduced_pouch_selling_price),
        var_CV, n_years)
  } else {
    vv (Pouch_selling_price,
        var_CV, n_years)
  }
  
# Calculate gross profit
  
  Perfume_gross_profit <- Total_perfume_sale_volume * 
                         (Perfume_selling_price_adjusted - Unit_cost_perfume) -
                          Fixed_cost_perfume
  
  Candle_gross_profit <- Total_candle_sale_volume *
                         (Candle_selling_price_adjusted - Unit_cost_candle) -
                          Fixed_cost_candle
  
  Pouch_gross_profit <- Total_pouch_sale_volume *
                        (Pouch_selling_price_adjusted - Unit_cost_pouch) -
                         Fixed_cost_pouch
  
# Calculate taxable income
  
  Perfume_taxable_income <- Perfume_gross_profit + Current_income
  
  Candle_taxable_income <- Candle_gross_profit + Current_income
  
  Pouch_taxable_income <- Pouch_gross_profit + Current_income
  
  
# Calculate Net profit 
  
  Perfume_net_profit <- Perfume_gross_profit + Current_income  
                            - (Perfume_taxable_income * income_tax)
  
  Candle_net_profit <- Candle_gross_profit + Current_income 
                           - (Candle_taxable_income * income_tax)
  
  Pouch_net_profit <- Pouch_gross_profit + Current_income 
                          - (Pouch_taxable_income * income_tax)
  
# Calculate NPV with discount rate
  
  NPV_perfume <- discount(x = Perfume_net_profit,
                          discount_rate = discount_rate,
                          calculate_NPV = TRUE)
  
  NPV_candle <- discount(x = Candle_net_profit,
                         discount_rate = discount_rate,
                         calculate_NPV = TRUE)
  
  NPV_pouch <- discount(x = Pouch_net_profit,
                        discount_rate = discount_rate,
                        calculate_NPV = TRUE)
  
  return(list(Profit_Perfume = NPV_perfume,
              Profit_Candle = NPV_candle,
              Profit_Pouch = NPV_pouch,
              Cashflow_Perfume = Perfume_net_profit,
              Cashflow_Candle = Candle_net_profit,
              Cashflow_Pouch = Pouch_net_profit))
  
}

input_table <- read.csv("Input_perfume.csv")

# Run the Monte Carlo Simulation

product_mc_simulation <- mcSimulation(estimate = 
                                      estimate_read_csv("Input_perfume.csv"),
                                      model_function = Product_launch_function,
                                      numberOfModelRuns = 2000,
                                      functionSyntax = "plainNames")

# Plot distributions histogram

plot_distributions(mcSimulation_object = product_mc_simulation,
                   vars = c ("Profit_Perfume", "Profit_Candle",
                             "Profit_Pouch"),
                   method = 'hist_simple_overlay',
                   base_size = 7)

# Plot distributions boxplot

plot_distributions(mcSimulation_object = product_mc_simulation,
                   vars = c ("Profit_Perfume", "Profit_Candle",
                             "Profit_Pouch"),
                   method = 'boxplot')

# Plot distributions smooth overlay

plot_distributions(mcSimulation_object = product_mc_simulation,
                   vars = c ("Profit_Perfume", "Profit_Candle",
                             "Profit_Pouch"),
                   method = 'smooth_simple_overlay')

# Plot cashflows

plot_cashflow(mcSimulation_object = product_mc_simulation,
              cashflow_var_name = c ("Cashflow_Perfume","Cashflow_Candle",
                                     "Cashflow_Pouch")) 

plot_cashflow(mcSimulation_object = product_mc_simulation,
              cashflow_var_name = "Cashflow_Perfume")
              

#Find EVPI 

mcSimulation_table <- data.frame(product_mc_simulation$x, 
                                 product_mc_simulation$y[1:3])

evpi_perfume <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Profit_Perfume")
evpi_candle <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Profit_Candle")
evpi_pouch <- multi_EVPI(mc = mcSimulation_table, first_out_var = "Profit_Pouch")


plot_evpi(evpi_perfume, decision_vars = "Profit_Perfume")
plot_evpi(evpi_candle, decision_vars = "Profit_Candle")
plot_evpi(evpi_pouch, decision_vars = "Profit_Pouch")


#Find PLS result
pls_result <- plsr.mcSimulation(object = product_mc_simulation,
                                resultName = names
                                (product_mc_simulation$y)[1], 
                                ncomp = 1)

plot_pls(pls_result, input_table = input_table, threshold = 0)

# Check breakeven point in unit

# Revenue per unit

Unit_revenue_perfume <- Perfume_selling_price - Unit_cost_perfume
Unit_revenue_perfume

Unit_variable_cost <- (Perfume_cost_raw + Perfume_cost_production)/200

Unit_breakeven_point <- Fixed_cost_perfume/ (Unit_revenue_perfume - Unit_variable_cost)
Unit_breakeven_point
#Without considering income tax, breakeven point will reach in a year for perfume
#but cashflow looks stunted. Low profit?


          

