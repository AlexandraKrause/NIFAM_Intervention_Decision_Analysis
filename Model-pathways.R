library(readr)
library(decisionSupport)
library (DiagrammeR)
library(tidyverse)
library(readxl)
library(msm)
#problems: cashflow running costs (payout and investment time) and more than 24 months: 
#somehing odd there

# "SQ" stands for "Status quo" within this code
#find the model here: https://app.diagrams.net/#G1rKY23sZwEmLgBIFNd0zmtS8JVzBUd-ON


#make_variables <- function(est,n=1)
#{ x<-random(rho=est, n=n)
#for(i in colnames(x)) assign(i,
#                             as.numeric(x[1,i]),envir=.GlobalEnv)
#}

#tt<-make_variables(as.estimate(input_estimates))


####Input data####

input_estimates <- data.frame(variable = c(
  "Knowledge_generation",
  "Activities_on_market",
  "Knowledge_spreading",
  "Reaching_customers",
  "Running_costs",
  "Market_income",
  "Health_investment_reduction",
  "SQ_Health_investment_reduction",
  "SQ_Cheap_Food",
  "Workforce",
  "SQ_Workforce",
  "var_slight", "discount_rate",
  "payout_months", "investment_months",
  "failure_risk","SQ_failure_risk"),
  lower = c(1000,5000,1000,
            5000,4000,9000,500,1,
            1000,1000,1,
            1,1,24,3,0.06,0.04),
  median = NA,
  upper = c(5000,6000,4000,
            6000,8000,15000,800,40,
            2000,2000,20,
            1,1,24,3,0.06,0.04),
  distribution = c("posnorm", "posnorm",
                   "posnorm","posnorm","posnorm",
                   "posnorm",
                   "posnorm","posnorm",
                   "posnorm","posnorm","posnorm",
                   "const","const","const","const",
                   "const","const"),
  label = c("Knowledge generation investment (Dollar/Month)",
            "Knowledge payoff (Dollar/Month)",
            "Marketing investment (Dollar/Month)",
            "Marketing payoff (Dollar/Month)",
            "Running costs (Dollar/Month)",
            "Market income (Dollar/Month)",
            "Health after intervention (Dollar/Month)",
            "Health without intervention (Dollar/Month)",
            "Low priced food (Dollar/Month)",
            "Workforce after intervention(Dollar/Month)",
            "Workforce without intervention (Dollar/Month)",
            "Coefficient of variation",
            "Discout rate",
            "Months of receiving money (Dollar/Month)",
            "Months of paying into empowerment efforts (Dollar/Month)",
            "Risk failure", "Risk failure SQ"),
  Description = c("Knowledge_generation",
                  "Activities_on_market",
                  "Knowledge_spreading",
                  "Reaching_customers",
                  "Running_costs",
                  "Market_income",
                  "Health_investment_reduction",
                  "SQ_Health_investment_reduction",
                  "SQ_Cheap_Food",
                  "Workforce",
                  "SQ_Workforce",
                  "var_slight", "discount_rate",
                  "payout_months", "investment_months",
                  "failure_risk","SQ_failure_risk"))

#lnorm

input_estimates <- input_estimates %>%
  mutate(variable = as.character(variable),
         distribution = as.character(distribution),
         label = as.character(label),
         Description = as.character(Description))

####Explanation of the input estimates:####

#The discount rate is essential to determine the better decision option. 
#It represents the likability of a decision-maker to wait for benefits 
#that occur after a shorter or longer time period.
#Here, a discount rate of 1 is used, which is comparably small.
#The higher the discount rate, the less
#likely a person is to wait for an outcome.
#Depending on local cases, different discount rates can be used. 
#One case study could also compare several discount rates to each other,
#reflecting the heterogeneous willingness 
#of several groups to invest.
#A higher discount rate would make the status quo intervention more attractive.
#The duration of investments (see the section showing the cashflow down below)
#also leads to the different willingness to invest since smaller
#time periods demand less waiting for income.

################Reminder about the make_variables function######################

# make_variables <- function(est,n=1)
# { x<-random(rho=est, n=n)
# for(i in colnames(x)) assign(i,
#                               as.numeric(x[1,i]),envir=.GlobalEnv)
# }#Then call:
#   make_variables(as.estimate(input_table_gender))



####Start of decision function####

decision_function <- function(x, varnames){
  
  #Risk
  # later one failure risk per step,specifically tailored. Right now only one
  #general one is implemented.
  #also,running costs should be differently implemented in cashflow
  
  failure <- chance_event((1-failure_risk), 1, 0,
                         n = (payout_months + investment_months))
  
  SQ_failure <- chance_event((1-SQ_failure_risk), 1, 0,
                            n = (payout_months + investment_months))
  
  #Education investment and outcome
  
  Knowledge_generation <- c(vv(var_mean = Knowledge_generation, 
                               var_CV = var_slight, 
                               n = investment_months), rep(0,payout_months))
  
  Knowledge_generation <- Knowledge_generation * failure
  
  
  
  Activities_on_market <- c(rep(0,investment_months), 
                            vv(var_mean = Activities_on_market, 
                               var_CV = var_slight, 
                               n = payout_months))
  
  Activities_on_market<- Knowledge_spreading * failure
  
  
  #Marketing investment and outcome
  
  Reaching_customers <- c(rep(0,investment_months), 
                      vv(var_mean = Reaching_customers, 
                         var_CV = var_slight, 
                         n = payout_months))
  
  
  Reaching_customers <- Reaching_customers * failure
  
  
  Knowledge_spreading <- c(vv(var_mean = Knowledge_spreading, 
                             var_CV = var_slight, 
                             n = investment_months), rep(0, payout_months))
  
  Knowledge_spreading <- Knowledge_spreading * failure
  
  #Running costs of market
'  Running_costs <- c(vv(var_mean = Running_costs, 
                                  var_CV = var_slight, 
                                  n = investment_months), rep(0,payout_months))'
  #cashlow not adaptable?
  Running_costs1 <- c(vv(var_mean = Running_costs, 
                         var_CV = var_slight, 
                         n = investment_months))
  
  Running_costs2 <- c(vv(var_mean = Running_costs, 
                        var_CV = var_slight, 
                        n = payout_months))
                        
  Running_costs = Running_costs1 + Running_costs2
  
  Running_costs <- Running_costs * failure
  
  # Market income
  
  Market_income <- c(rep (0,investment_months),
                           vv(var_mean = Market_income, 
                              var_CV = var_slight, 
                              n = payout_months))
  
  Market_income <- Market_income * failure
  
  #Health outcome
  
  Health_investment_reduction <- c(rep (0,investment_months),
                                   vv(var_mean = Health_investment_reduction, 
                                      var_CV = var_slight, 
                                      n = payout_months))
  
  Health_investment_reduction <- Health_investment_reduction * SQ_failure
  

  
  #Status quo health outcome
  
  
  SQ_Health_investment_reduction <- c(rep (0,investment_months),
                                      vv(var_mean = SQ_Health_investment_reduction, 
                                         var_CV = var_slight, 
                                         n = payout_months))
  
  SQ_Health_investment_reduction <- SQ_Health_investment_reduction * SQ_failure
  
  #SQ cheap food outcome
  
  SQ_Cheap_Food <- c(rep (0,investment_months),
                           vv(var_mean = SQ_Cheap_Food, 
                              var_CV = var_slight, 
                              n = payout_months))
  
  SQ_Cheap_Food <- SQ_Cheap_Food * SQ_failure
  
  #Workforce outcome
  
  
  Workforce <- c(rep (0,investment_months),
                 vv(var_mean = Workforce, 
                    var_CV = var_slight, 
                    n = payout_months))
  
  Workforce <- Workforce * failure

  # SQ Workforce outcome
  
  SQ_Workforce <-  c(rep (0,investment_months),
                     vv(var_mean = SQ_Workforce, 
                        var_CV = var_slight, 
                        n = payout_months))
  
  SQ_Workforce <- SQ_Workforce * SQ_failure
  

  
  ### Explanation of the value varier function (vv())###
  
  #The value varier function (vv()) is used to vary the variables depending
  #on the years of investment and payouts, leading to a variable time series.
  #var_mean is set as the initial variable that differs by var_CV, the coefficient
  #of variation, which is set within the input parameters.
  #n is the number of produced values, meaning the length of time for the
  #initial variable var_mean, for example, "SQ_Workforce."

  
  
  ####Decision Option calculations####  
  ###Computing the empowerment and status quo decision options###
  #Status quo decision option compared to empowerment decision option#
  
  ##Status quo decision option##
  
  Profit_SQ <- (SQ_Cheap_Food + SQ_Workforce + SQ_Health_investment_reduction)
  
  #status quo: probably there will not be workforce benefits or
  #health investment reductions. maybe for some people, for whom the market is
  #no alternative to their healthy diets?
  #how to find benefit of cheap food? -> ask people how much less per month 
  #they pay per meal when not going to the market. 
  #maybe something like healthier feeling? (what is it worth to people?)

  
  #Computing the Status Quo NPV (Net present value)#
  
  NPV_no_intervention <- discount(Profit_SQ,
                           discount_rate = discount_rate, calculate_NPV = TRUE) 
  
  ##Market decision option##
  
  PartA <- Reaching_customers + Market_income + Health_investment_reduction
           + Workforce
  
  PartB <-  Knowledge_generation  + Knowledge_spreading + Running_costs
  
  
  Market_profit <-  (PartA - PartB)
  
  # recahing customers: expected added amout of customers reached per marketing,
  # equals the additional monthly income. 
  #workforce measurement: e.g. wai index? is health investment reduction measurable
  #within the short time? 
  
  #Computing the Empowerment NPV (Net present value)#
  
  
  NPV_Market_profit <- discount(Market_profit,
                           discount_rate = discount_rate, calculate_NPV = TRUE)
  NPV_decision_profit_with_Market <- NPV_Market_profit - NPV_no_intervention
  
  
  ####Return list####
  
  return(list(NPV_no_intervention = NPV_no_intervention,
              NPV_decision_profit_with_Market = NPV_decision_profit_with_Market,
              NPV_Market_profit = NPV_Market_profit,
              Cashflow_decision_market =  Market_profit
              
  )) 
  
}

####Monte Carlo Simulation####
mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::as.estimate(input_estimates),
  model_function = decision_function,
  numberOfModelRuns = 10000,
  functionSyntax = "plainNames"
)

#The Monte-Carlo simulation is run with 10,000 repetitions.

####Plot NPV distributions####

#Plot Net Present Value (NPV) distributions
#NPVs show the overall economic output value of a decision option.
#By using the plot_distributions() function, both decisions or
#each separately can be plotted.
#The expected NPV for one decision option
#represents an overlay of the full results of the Monte Carlo simulation.
#The x-axis shows the monetary range farm women can expect for either option.

#Plot empowerment decision option
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_profit_with_Market"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 7)
#plot both
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_profit_with_Market",
                                             "NPV_no_intervention"),
                                    method = 'smooth_simple_overlay', 
                                    base_size = 14)

#Plot distributions one by one
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_no_intervention",
                                    method = 'boxplot_density')

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_decision_profit_with_Market",
                                    method = 'boxplot_density')

####Boxplots####

#By using the plot_distributions() function,
#also the decision's boxplots can be plotted.
#Boxplots show the median (central line), 
#the 25th and 75th percentiles (sides of boxes) and any outliers 
#(light circles outside of boxes).

#'boxplot' empowerment decision option

decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("NPV_decision_profit_with_Market",
                                             "NPV_no_intervention"
                                    ),
                                    method = 'boxplot', 
                                    base_size = 7)


####Cashflow analysis####

#Here, the plot_cashflow() function is used with the outputs from the
#mcSimulation() function.
#The cash flow represents the history of the simulated intervention period. 
#The cashflow plot visualizes the time structure of investments and payouts
#During x months of investments.

Cashflow <- plot_cashflow(mcSimulation_object = mcSimulation_results,
                          cashflow_var_name = "Cashflow_decision_market",
                          x_axis_name = "Month",
                          y_axis_name = "Cashflow in Dollar",
                          color_25_75 = "green4",
                          color_5_95 = "green1",
                          color_median = "red")

Cashflow

mcSimulation_results
####PLS####

#In this section, a post-hoc analysis is applied to the mcSimulation() outputs,
#using a partial least square regression (PLS)
#with the plsr.mcSimulation() function. 
#This function uses the outputs of the mcSimulation() selecting
#all the input variables from the decision analysis function 
#in the parameter object.
#The PLS is run by defining the outcome variable in the parameter resultName,
#which in this case is "NPV_decision_profit_with_Market".
#If a programmer is unsure how to find this input, 
#names(mcSimulation_results$x) and names(mcSimulation_results$y)
#are useful code chunks to get an overview over the parameters, providing
#a legend of the objects.
#Then,
#names(mcSimulation_results$y)[n] can be used to select the correct resultName.


#The output is the determined Variable 
#Importance in the Projection (VIP) score, shown in a bar graph, 
#and coefficients of a Projection to Latent Structures (PLS) regression model.
#The plots show the variables to which the model is more sensitive. These
#variables have the most exaggerated impact on the decision outcome. 
#Furthermore, they are the
#most correlated with the outcome.
#The VIP shows the importance of the variables with a correlation to the output.
#In the plot, positive and negative values
#are visualized compared to the baseline option.
#Red colors indicate negative values and green colors positive ones.
#A positive value is not 
#only positive compared to the baseline, but also 
#is positively related to the outcome. Same is valid for a negative value:
#It is negative compared to the baseline and negatively related to the outcome.

names(mcSimulation_results$x)
names(mcSimulation_results$y)

#By using the existing input estimates,
#Empowerment workforce payout (Dollar/Month),
#the monetary value achieved by gathering money from the last step
#of the empowerment decision option, the investments into health care and food
#which then lead to more working hours per month compared to doable
#working hours with having less food and health care.
#This is the most important variable in this decision scenario,
#leading to higher payouts than investments. This variable has
#positive importance to the outcome.
#Concluding, 
#the empowerment decision option in this scenario would be very beneficial.
#No negative value is shown in this plot.


#	Pls of	"NPV_decision_profit_with_Own_business_branch_1"

#pls_result_1 <- plsr.mcSimulation(object = mcSimulation_results,
#                                resultName = names(mcSimulation_results$y)[3],
#                                ncomp = 1)

#or use the variable name directly:

pls_result_1 <- plsr.mcSimulation(object = mcSimulation_results,
                                 resultName = "NPV_decision_profit_with_Market",
                                 ncomp = 1)

#Plot PLS
#The colors of the bars represent the positive or negative coefficient 
#of the given input variable with the output variable.

plot_pls(pls_result_1, threshold = 0.8, input_table = input_estimates)


####EVPI####

#This last part of this code is meant to guide further research.
#The Expected Value of Perfect Information analysis (EVPI) visualizes variables.
#Further research could help make better-informed decisions in the
#future. Farm women might benefit from reduced uncertainties of the plotted
#variables since more perfect information benefits
#more informed decision-making.
#Without perfect information of the plotted variables, the farm women
#might suffer from opportunity losses.

#The EVPI is calculated as the amount of money a decision-maker should pay to
#receive better information about highly uncertain variables and 
#still profit.
#If upper and lower input estimate ranges are not broad,
#it is unlikely that an EVPI is
#plotted since enough information is given. With broader ranges,
#the higher the insecurity of the forecast, leading to the plotting of
#a positive EVPI.

mcSimulation_table <- data.frame(mcSimulation_results$x,
                                 mcSimulation_results$y[2:3])
evpi <- multi_EVPI(mc = mcSimulation_table, 
                   first_out_var = "NPV_Market_profit")
plot_evpi<-plot_evpi(evpi,
                     decision_vars = "NPV_Market_profit")
plot_evpi

#[1] "NPV_no_intervention", NPV_decision_profit_with_Market"
#and [3] "NPV_Market_profit". 

#Check
names(mcSimulation_results$x)
names(mcSimulation_results$y[1:3])

colnames(mcSimulation_results$y)

#################Get more information###########################################
##Information about different parts of the Decision Analysis coding##
?chance_event
?decisionSupport
?multi_EVPI
?plsr.mcSimulation
?stat_density
?plot_cashflow
?multi_EVPI
?vv()

