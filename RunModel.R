
# Script set-up ----------------------------------------------------------------
# Packages
library(tidyverse)

library(cowplot)
library(data.table)
library(doParallel)
library(doRNG)
library(dplyr)
library(foreach)
library(iterators)
library(rngtools)

library(fs)
library(glue)
library(vroom)

library(ggh4x)
library(ggthemes)

library(gstat)
library(ineq)
library(parallel)
library(raster)
library(sf)
library(sp)
library(spData)
library(spdep)



# Config via environment variables  -----------------------------------------
# Enables easy-to-run toy example for ABM with one repetition and 
# experiment one visualization via environment variables

# Root folder for outputs (default: "Ts_Data"; toy run will set "Ts_Data_toy")
OUTDIR_SUFFIX <- Sys.getenv("OUTDIR_SUFFIX", "")
out_root <- paste0(
  "Ts_Data", if (nzchar(OUTDIR_SUFFIX)) paste0("_", OUTDIR_SUFFIX) else "")

# Path to conditions RDS (default: ExperimentConditions.rds)
CONDITIONS_RDS <- Sys.getenv("CONDITIONS_RDS", "ExperimentConditions.rds")

# Number of repetitions (default: 50)
n_reps_env <- Sys.getenv("N_REPS", NA_character_)
if (!is.na(n_reps_env)) n_reps <- as.integer(n_reps_env) else n_reps <- 50

# Timesteps (default: 100; toy keeps 100 but this allows overriding)
TimeSteps_env <- Sys.getenv("TIME_STEPS", NA_character_)
if (!is.na(TimeSteps_env)) TimeSteps <- 
  as.integer(TimeSteps_env) else TimeSteps <- 100

# Single-core for toy (TOY_RUN=1 disables parallellism)
TOY_RUN <- Sys.getenv("TOY_RUN", "0") == "1"

# dir_create helper (keep if not already defined)
dir_create <- function(path) if (
  !dir.exists(path)) dir.create(path, recursive = TRUE)

# Ensure base output folders exist under out_root
dir_create(out_root)
dir_create(file.path(out_root, "Ranchers"))
# dir_create(file.path(out_root, "Precip", "tmp")) # uncomment for supplemental analysis
# dir_create(file.path(out_root, "Precip", "final")) # uncomment for supplemental analysis

# saving specific
compress_chunks <- TRUE

# Parallel setup ----------------------------------------------------------
cl <- NULL

if (!TOY_RUN) {
  cl <- parallel::makeCluster(parallel::detectCores() - 1)
  doParallel::registerDoParallel(cl)
  on.exit(try(stopCluster(cl), silent = TRUE), add = TRUE)
} else {
  foreach::registerDoSEQ()
}

# Model parameters --------------------------------------------------------
N_Communities <- 100 #Must be perfect square number and a multiple of the number of plots for the code below to work.
N_Plots <- 4 #Must be perfect square number for the code below to work.
Starting_Prop_Conservation <- .25
StartingGrassMin <- 0.2#Lower bound of grass/rain. i.e., the worst areas have 20% of the best (before grazing)
Spatial_Autocorrelation_Rain <- 0.98 # This affects the spatial autocorelation of grass growth (0 is fully random, 1 is completely determined by space)
Animal_cost <- 5 #Price of animals. Used for buying/selling and also financial gains from having animals
GrassPerCow <- 0.15
Min_cows <- 2 #Min cows per person
Cow_Inequality <- 1 #Exponential rate for within-community cattle distribution. higher numbers = more EQUAL
Econ_Inequality <- 0.1 #Scalar, lower number = less within-community inequality (centered at community mean)
beta <- 0.1 # Social learning bias
Fodder_Price_Per_Cow <- 1 #Set high to essentially eliminate supplemental food buying from model
CapitalGainRate <- 0.02 #Financial gains from money in the bank
AnimalGainRate <- 0.02 #Financial gains from having animals (multiplied by the value of the animals and paid in cash)
AnimalObservedValueRate <- 0.1 #Multiplied by the economic value of animals and added to the observed economic wellbeing for social learning
MaxGeneralSale <- 0.25 #Maximum proportion of animals people can sell when NOT in conservation
MaxConservationSale <- 0.50 #Maximum proportion of animals people in conservation can sell
AnimalBaseline <- 2 #People never sell below this number

Supplemental_fodder <- TRUE #Does Cons grazing give supp food
Set_Adoption <- TRUE #Is adoption of Conservation set at the starting number
Forecasts <- "No_one" #"No_one", "Everyone", "Rich","Poor", "Conservation"
ForecastError <- 0 #How wrong are forecasts; basically ranges 0:100

# Scenarios ---------------------------------------------------------------
scenarios <- c("bad" , "ok", "good") # scenarios with individual sd of change
scn_sd <- setNames(c(1, 0.4, 0.3), scenarios)

# Derive sd with:
# source("StylizedLandscape/PredictedRainfall.R")
# scn_sd <- setNames(
#   round(extreme_pixels$sd_change_percent, 1),
#   extreme_pixels$Scenario
# )

# Experimental conditions -------------------------------------------------

# Full set of conditions for experiments 1-3 + supplemental (per default):
conditions <- readRDS(CONDITIONS_RDS)

# Main loop over conditions -----------------------------------------------
for (cond_id in seq_len(nrow(conditions))) {
  
  # Set params for this condition
  Forecasts <- conditions$Forecasts[cond_id]
  Set_Adoption <- conditions$Set_Adoption[cond_id]
  Supplemental_fodder <- conditions$Supplemental_fodder[cond_id]
  Starting_Prop_Conservation <- conditions$Starting_Prop_Conservation[cond_id]
  ForecastError<-conditions$ForecastError[cond_id]
  if ("beta" %in% colnames(conditions)) {
    beta <- conditions$beta[cond_id]
  }
  
  message("Running condition ", cond_id, " of ", nrow(conditions),
          ": Forecasts=", Forecasts,
          ": Set_Adoption=", Set_Adoption,
          ", Supplemental_fodder=", Supplemental_fodder,
          ", Starting_Prop_Conservation=", Starting_Prop_Conservation
          ,", ForecastError=", ForecastError,
          ", beta=", beta
  )
  
  # Create an experiment label for output filenames
  exp_setting <- paste0(
    "FOREC_", Forecasts,
    "_ADOP_", Set_Adoption,
    "_FODD_", Supplemental_fodder,
    "_PROPCONS_", Starting_Prop_Conservation,
    "_FORECERR_", ForecastError,
    "_beta_", beta
  )
  
  AllSimResults <- vector("list", n_reps)
  
  
  # Downloads non-summarized data, comment to save space and run time
  Ranchers_all_condition <- data.table::data.table()
  
  for (rep in seq_len(n_reps)) {
    cat("  Rep:", rep, "\n")
    set.seed(123 + rep)
    
    ## T0 set-up landscape (a) -----------------------------------------------------
    
    source("StylizedLandscape/MakeStylizedLandscape.R")
    rstack0   <- rstack
    Ranchers0 <- Ranchers
    
    # Parallel setup
    ResultsList <- foreach(
      target_scenario = scenarios,
      .packages = c("data.table", "dplyr", "raster", "ineq", "sf"),
      .combine  = 'list',
      .multicombine = TRUE,
      .init = list(),
      .errorhandling = "stop",
      .export = c("exp_setting", "compress_chunks", "rep", "beta")
    ) %dorng% {
      
      # Generate precipitation timeseries based on stylized landscape of this 
      # repetition and scenario-specific sd
      source("StylizedLandscape/PrecipTimeseries.R", local = TRUE)
      
      rstack   <- rstack0
      Ranchers <- Ranchers0
      
      PrecipStack <- PrecipTimeseries(
        StandardDev = scn_sd[target_scenario]
      )
      
      # Downloads non-summarized data, comment to save space and run time
      # Save precip map (per rep and scenario)
      Precip_sf <- PrecipStack %>%
        dplyr::mutate(
          Scenario  = target_scenario,
          Rep       = rep,
          Condition = exp_setting
        )

      # Uncomment to collect full data
      Ranchers_history_scnrep <- data.table::data.table()

      # One file per rep & scenario (safe to write in parallel)
      # precip_tmp_file <- file.path(
      #   out_root, "Precip", "tmp",
      #   sprintf("%s_rep%03d_%s.gpkg", exp_setting, rep, target_scenario)
      # )
      
      
      # T0 summary
      SummaryDat<-data.frame(Time=0,AvgMoney=mean(Ranchers$Money),
                             Gini = ineq::ineq(Ranchers$TotalEconomicWellbeing,type="Gini"),
                             AvgCows = mean(Ranchers$stock_count),
                             TotalCows = sum(Ranchers$stock_count),
                             AvgGrass = mean(rstack[["T0Grass"]][]),
                             TotalGrass = sum(rstack[["T0Grass"]][]),
                             PropCons = length(which(rstack[["Cons"]][]==T))/
                               (N_Communities*N_Plots),
                             Scenario = target_scenario)
      
      
      # Clean T0 layers we won't keep
      rstack$Grass<-rstack$T0Grass
      T0GrassIndex<-which(names(rstack) == "T0Grass")
      rstack <- rstack[[ -T0GrassIndex ]]
      T0RainIndex<-which(names(rstack) == "T0Rain")
      rstack <- rstack[[ -T0RainIndex ]]
      DeprivIndex<-which(names(rstack) == "Depriv")
      rstack <- rstack[[ -DeprivIndex ]]
      PopIndex<-which(names(rstack) == "Pop")
      rstack <- rstack[[ -PopIndex ]]
      
      
      for(ts in 1:TimeSteps){
        
        cat("TimeSteps:", ts, "\n")
        
        RanchersPast<-Ranchers
        rstackPast<-rstack
        
        
        # Module 1 - grazing (b) -------------------------------------------------------
        
        #vectorized version of code used to graze in the starting landscape
        df<-raster::as.data.frame(rstackPast,xy=T)# Easier as a dataframe
        
        # Summarize grazed plots by community
        df2 <- df %>% 
          filter(Grazed == 1) %>% # for just the grazed plots
          group_by(CommID) %>%
          summarize(
            Grass = sum(Grass), #calculate the amount of grass and total plots/community
            n = n(),
            .groups = 'drop'
          ) 
        
        CommAnimals<-df%>%dplyr::group_by(CommID)%>% #group by community
          dplyr::summarize(Animal=sum(Animal)) #Total animals (in both grazed and ungrazed plots)
        
        df2<-merge(df2,CommAnimals,by="CommID")%>% #Add these to one df
          mutate(
            Grass2 = ceiling(Grass - (GrassPerCow * Animal)), #animals eat evenly across grazed plots
            DeficitGrass = ifelse(Grass2 < 0, Grass2, 0), #save the deficit
            Grass = ifelse(Grass2 <= 0, 1, Grass2) #don't let the final number go all the way to 0 (for multiplicative growth)
          ) %>%
          dplyr::select(-Grass2)
        
        # Join the group-level summary back to the main df
        df<- df %>%
          left_join(df2, by = "CommID", suffix = c("", ".comm")) %>%
          mutate(
            Grass = case_when(
              Grazed == 1 ~ ceiling(Grass.comm / n), #for the grazed plots, distribute the grass/community
              TRUE ~ Grass
            )
          ) %>%
          dplyr::select(-Grass.comm, -Animal.comm, -n, -DeficitGrass) #remove cols no longer using
        
        rGrass <- rasterFromXYZ(df[order(df$y, df$x), c("x", "y", "Grass")])
        GrassIndex<-which(names(rstack) == "Grass")
        rstack <- rstack[[ -GrassIndex ]] #Remove old grass
        rstack<-stack(rstack,rGrass) #Add new grass
        
        
        # Module 2 - supplemental fodder (c, d) ---------------------------------------------
        #we identify which communities did not have enough grass to feed all animals. 
        #Ranchers buy supplemental fodder to feed animals as necessary and as they are 
        #able (given economic wellbeing). 
        
        #If no one is in deficit. They all get 0
        if(length(which(df2$DeficitGrass<0))==0){
          Ranchers$StockDeficit<-0
          
        }
        
        #If there are deficits, do the following:
        if(length(which(df2$DeficitGrass<0))>0){ #This continues to wrap the next module
          
          #substitute to be just communities with a deficit
          df2<-df2%>%filter(DeficitGrass<0)
          
          RanchersDef<-Ranchers%>%filter(CommID %in% df2$CommID) #only the ranchers from communities with deficit
          RanchersNotDef<-Ranchers%>%filter(!CommID %in% df2$CommID) #Ranchers that were not deficient 
          
          df2$AnimalsDeficit<-df2$DeficitGrass/GrassPerCow #How many animals short was the community grass
          df2$ProportionDeficit<-abs(df2$AnimalsDeficit)/df2$Animal #what proportion of animals were they unable to feed
          
          #This handles NAs produced when there are no animals in a community
          df2$ProportionDeficit<-ifelse(is.na(df2$ProportionDeficit)==T,1,df2$ProportionDeficit) 
          
          RanchersDef<-RanchersDef%>%
            left_join(df2[,c("CommID","ProportionDeficit")], by = "CommID") %>% #get how many animals need food per person
            mutate(StockDeficit  = ceiling(stock_count  * ProportionDeficit))%>% #ceiling since partially feeding an animal counts as not feeding it!
            dplyr::select(-ProportionDeficit)
          
          #In conditions where no supplemental fodder is available from the conservation 
          #program, animals die. In conditions where supplemental fodder is given by the 
          #conservation grazing program, it is distributed as necessary.
          if(Supplemental_fodder ==TRUE){ #give free food to people in conservation. Limit to 10 or fewer.
            
            RanchersDef$StockDeficit<-ifelse(RanchersDef$Conservation==TRUE & RanchersDef$stock_count <=10 ,0,RanchersDef$StockDeficit)
            
          }
          
          #Now reduce the stock deficit and the money of each person as they buy fodder
          # Calculate how many cows each person can afford fodder for
          RanchersDef$CowsAffordable <- floor(RanchersDef$Money / Fodder_Price_Per_Cow) 
          
          # Determine how many cows they can actually feed (can't exceed StockDeficit)
          RanchersDef$CowsToFeed <- pmin(RanchersDef$CowsAffordable, RanchersDef$StockDeficit)
          
          # Calculate the actual money spent
          RanchersDef$MoneySpent <- RanchersDef$CowsToFeed * Fodder_Price_Per_Cow
          
          # Update Money and StockDeficit accordingly
          RanchersDef$Money <- RanchersDef$Money - RanchersDef$MoneySpent
          RanchersDef$StockDeficit <- RanchersDef$StockDeficit - RanchersDef$CowsToFeed
          
          #remove intermediate columns
          RanchersDef<-RanchersDef%>%dplyr::select(-c(CowsAffordable,CowsToFeed,MoneySpent))
          
          
          # Module 3 - animals die (e) --------------------------------------------------
          
          RanchersDef$stock_count<-RanchersDef$stock_count-RanchersDef$StockDeficit #reduce stock by deficit number. 
          RanchersDef$stock_count[RanchersDef$stock_count<0] <- 0 #No negative numbers
          
          #Merge the deficit and not deficit ranchers
          RanchersNotDef$StockDeficit<-0
          Ranchers<-rbind(RanchersDef,RanchersNotDef)%>%arrange(CommID,person_id)
          
          #change number of animals in the raster
          ranimals<-Ranchers%>%group_by(CommID)%>%summarise(Animal=sum(stock_count)/N_Plots) #summarise as table, divide by N_Plots as this is the total number per community
          df<-df%>%dplyr::select(-Animal)#remove old number from raster df
          ranimals<-merge(df,ranimals,by="CommID") #add new animal column
          ranimals <- rasterFromXYZ(ranimals[order(ranimals$y, ranimals$x), c("x", "y", "Animal")])#make a raster
          
          AnIndex<-which(names(rstack) == "Animal") #remove old raster layer
          rstack <- rstack[[ -AnIndex ]] 
          rstack<-stack(rstack,ranimals)#add new animal raster layer
          
        }
        
        ## Module 4 - individuals receive payoff via labor and animals (f)--------------
        #We want to increase people's money by some percentage.
        #We also want to give them money for each animal beyond what that animal is worth 
        #(as animals have more value than just sale value)
        
        #Make money from money
        Ranchers$Money<-Ranchers$Money*CapitalGainRate
        
        #money from animals
        Ranchers$Money<-Ranchers$Money+(Ranchers$stock_count*Animal_cost*AnimalGainRate)
        
        #Recalculate total economic wellbeing
        Ranchers<-Ranchers%>%mutate(TotalEconomicWellbeing=Money+(stock_count *Animal_cost))
        
        
        df2<-Ranchers%>%group_by(CommID)%>%summarise(Mean_economic_wellbeing=mean(TotalEconomicWellbeing),
                                                     Conservation=as.logical(min(Conservation)))
        
        #Update raster with mean economic wellbeing by community
        df<-df%>%dplyr::select(-Mean_economic_wellbeing)
        rWell<-merge(df,df2,by="CommID")
        rWell <- rasterFromXYZ(rWell[order(rWell$y, rWell$x), c("x", "y", "Mean_economic_wellbeing")])
        
        WellIndex<-which(names(rstack) == "Mean_economic_wellbeing")
        rstack <- rstack[[ -WellIndex ]]
        rstack<-stack(rstack,rWell)
        
        ## Module 5 - conservation adoption (g)-----------------------------------------
        #Communities then observe all surrounding communities (queen’s case) and use 
        #success biased imitation. This is handled in the make stylized landscape script, 
        # where we make the 'adj_list' object to disproportionately copy the communities 
        # with the highest average rancher economic wellbeing. 
        
        #Only do this if conservation proportion is not fixed
        if(Set_Adoption==FALSE){
          
          #remake df2 with stock count  
          df2<-Ranchers%>%group_by(CommID)%>%summarise(Mean_economic_wellbeing=mean(TotalEconomicWellbeing),
                                                       stock_count =mean(stock_count ),
                                                       Conservation=as.logical(min(Conservation)))
          
          # Convert df to data.table for speed
          df2 <- as.data.table(df2)
          
          # Initialize vector to store new behaviors
          new_behavior <- character(nrow(df2))
          
          # Vectorized helper: pre-extract payoff and behavior
          behaviors <- df2$Conservation
          
          #Make payoffs for cows more observable
          payoffs <- df2$Mean_economic_wellbeing+(df2$stock_count*Animal_cost*
                                                    AnimalObservedValueRate)
          
          
          
          # Loop over each community (can be parallelized if needed)
          for (i in seq_len(nrow(df2))) {
            neighbors <- adj_list[[i]]
            
            
            neighbor_behaviors <- behaviors[neighbors]
            neighbor_payoffs   <- payoffs[neighbors]
            
            # Mean payoffs by behavior
            payoff_A <- mean(neighbor_payoffs[neighbor_behaviors == TRUE], na.rm = TRUE)
            payoff_B <- mean(neighbor_payoffs[neighbor_behaviors == FALSE], na.rm = TRUE)
            
            # Handle case where all neighbors are A or all B
            if (is.nan(payoff_A)) payoff_A <- 0
            if (is.nan(payoff_B)) payoff_B <- 0
            
            # Payoff-biased probability of adopting A
            #pA <- exp(.1 * payoff_A) / (exp(.1 * payoff_A) + exp(.1 * payoff_B))
            pA <- 1 / (1 + exp(-beta * (payoff_A - payoff_B)))
            
            # Draw new behavior
            new_behavior[i] <- sample(c(TRUE, FALSE), size = 1, prob = c(pA, 1 - pA))
          }
          
          # Update the dataframe
          df2[, Conservation := new_behavior]
          
          df2<-as.data.frame(df2)
          
          # Need to assign conservation in the raster
          df2$Conservation<-as.numeric(as.logical(df2$Conservation)) #make into 1s and 0s
          
          df<-df%>%dplyr::select(-Cons)
          rCons<-merge(df,df2,by="CommID")
          rCons <- rasterFromXYZ(rCons[order(rCons$y, rCons$x), c("x", "y", "Conservation")])
          names(rCons)<-"Cons" #to match above
          
          ConsIndex<-which(names(rstack) == "Cons")
          rstack <- rstack[[ -ConsIndex ]]
          rstack<-stack(rstack,rCons)
          
        }
        
        
        ## Module 6 - reassign conservation plots (j) ----------------------------------
        #For communities that have adopted the conservation grazing program, the plot 
        # with the lowest grass cover is allocated for protection. 
        
        df<-raster::as.data.frame(rstack,xy=T)
        dfCons<-df%>%filter(Cons==1)#for just the conservation communities
        
        if(nrow(dfCons)>0){ #if there are still conservation communities
          dfCons<-dfCons%>%group_by(CommID)%>%
            mutate(
              min_grass = min(Grass, na.rm = TRUE),
              is_min = Grass  == min_grass
            ) %>%
            group_modify(~ {
              # Randomly select one of the tied plots
              chosen <- sample(which(.x$is_min), 1)
              .x$Grazed <- 1
              .x$Grazed[chosen] <- 0
              .x
            }) %>%
            ungroup() %>%
            dplyr::select(-min_grass, -is_min)
          
          dfCons <- dfCons %>%
            relocate(CommID , .after = y)
        }
        
        dfNotCons<-df%>%filter(Cons==0)#for just the conservation communities
        
        if(nrow(dfNotCons)>0){ #only if there are some not cons communities
          dfNotCons$Grazed<-1 #All grazed in non-conservation communities
        }
        df<-rbind(dfCons,dfNotCons)
        
        rGrazed  <- rasterFromXYZ(df[order(df$y, df$x), c("x", "y", "Grazed")]) #make this into a raster
        
        GrazedIndex<-which(names(rstack) == "Grazed") #get the index
        rstack <- rstack[[ -GrazedIndex ]] #remove the old grazing plan
        rstack<-stack(rstack,rGrazed)
        
        
        ## Module 7 - weather forecast & buying / selling animals (h, i)---------------
        #In anticipation of the next season rain, ranchers decide whether to buy or sell 
        #animals and how many. In baseline conditions, ranchers randomly select five 
        #other ranchers and disproportionately adopt the buying/selling strategy of the 
        #most successful (i.e., success biased behavioral transmission).
        
        # Payoff-biased imitation function
        PayoffUpdate<-function(dff){
          
          #Make the observed economic wellbeing where cows are more observable.
          dff$ObservedEconomicWellbeing<-dff$TotalEconomicWellbeing+(dff$stock_count*Animal_cost*AnimalObservedValueRate)
          
          dt <- as.data.table(dff) #Make into a data table object, much faster than df
          
          # Preallocate new column
          dt[, new_strategy := PastStockChangeProp]  # initialize with default
          
          # Process each community
          dt[, new_strategy := {
            n <- .N
            result <- PastStockChangeProp  # initialize
            #get their own wellbeing and past strategy
            for (i in seq_len(n)) {
              self_id <- person_id[i]
              self_val <- ObservedEconomicWellbeing[i]
              self_strategy <- PastStockChangeProp[i]
              
              # Exclude self from peer pool
              peer_idx <- setdiff(seq_len(n), i)
              sampled_idx <- sample(peer_idx, size = min(10, length(peer_idx)), replace = FALSE) #sample up to 10 people
              
              peer_payoffs <- ObservedEconomicWellbeing[sampled_idx]
              peer_strategies <- PastStockChangeProp[sampled_idx]
              
              if (sum(peer_payoffs) == 0) {
                probs <- rep(1 / length(sampled_idx), length(sampled_idx))
              } else {
                probs <- peer_payoffs / sum(peer_payoffs)
              }
              
              chosen_peer <- sampled_idx[sample(seq_along(sampled_idx), size = 1, prob = probs)]
              
              if (ObservedEconomicWellbeing[chosen_peer] > self_val) {
                result[i] <- PastStockChangeProp[chosen_peer]
              }
              # else keep original strategy
            }
            
            result
          }, by = CommID]
          return(as.data.frame(dt)%>%dplyr::select(-ObservedEconomicWellbeing))
        }
        
        #### Now, apply to only the groups not using forecasts
        if(Forecasts=="No_one"){Ranchers<-PayoffUpdate(Ranchers) } #everyone learns socially
        
        if(Forecasts=="Everyone"){Ranchers<-Ranchers%>%mutate(new_strategy=NA)} #No one learns socially
        
        if(Forecasts=="Conservation"){
          Forecasters<-Ranchers%>%dplyr::filter(Conservation==TRUE)%>%mutate(new_strategy=NA) #Only non-conservation communities learn socially
          Learners<-Ranchers%>%dplyr::filter(Conservation==FALSE)
          Learners<-PayoffUpdate(Learners)
          Ranchers<-rbind(Forecasters,Learners)
        }
        
        if(Forecasts=="Rich"){
          Rich<-Ranchers%>%group_by(CommID)%>%summarise(wellbeing=mean(TotalEconomicWellbeing ))%>%
            mutate(Rich=wellbeing>=quantile(wellbeing,0.75))%>%dplyr::select(-wellbeing)
          Ranchers<-base::merge(Ranchers,Rich,by="CommID")
          
          Rich<-Ranchers%>%dplyr::filter(Rich==TRUE)%>%mutate(new_strategy=NA) #Rich don't learn socially
          Poor<-Ranchers%>%dplyr::filter(Rich==FALSE) 
          Poor<-PayoffUpdate(Poor) #everyone else learns socially
          Ranchers<-rbind(Rich,Poor)
          Ranchers<-Ranchers%>%dplyr::select(-Rich) #remove this column, we don't need it anymore
        }  
        
        if(Forecasts=="Poor"){
          Poor<-Ranchers%>%group_by(CommID)%>%summarise(wellbeing=mean(TotalEconomicWellbeing ))%>%
            mutate(Poor=wellbeing<=quantile(wellbeing,0.25))%>%dplyr::select(-wellbeing)
          Ranchers<-base::merge(Ranchers,Poor,by="CommID")
          
          Poor<-Ranchers%>%dplyr::filter(Poor==TRUE)%>%mutate(new_strategy=NA) #poor don't learn socially
          Rich<-Ranchers%>%dplyr::filter(Poor==FALSE) 
          
          Rich<-PayoffUpdate(Rich) #everyone else learns socially
          Ranchers<-rbind(Rich,Poor)
          Ranchers<-Ranchers%>%dplyr::select(-Poor)#remove this column, we don't need it anymore
        } 
        
        
        # Now need to assign new strategy to people/groups with forecasts
        
        df<-raster::as.data.frame(rstack,xy=T)
        
        NextGrass<-PrecipStack%>%filter(Time==ts)%>%as.data.frame(.)%>%dplyr::select(-geometry,Time)
        
        df<-base::merge(df,NextGrass,by=c("CommID","PlotID"))
        
        
        
        # linear effect of rain quality on grass growth
        # r_max controls response to rain. 
        # multiplier on current grass controls response to prexisting grass
        grass_growth <- function(current_grass, 
                                 rain_quality, 
                                 r_max = 300,
                                 K = 100) {
          
          # Logistic growth equation modified by rainfall
          growth <- (r_max * (rain_quality / 100)) * current_grass * 0.01 * (1 - current_grass / K)
          # Update
          new_grass <- current_grass + growth
          # Ensure grass does not exceed carrying capacity and is positive
          new_grass <- pmax(new_grass, 0)
          new_grass
        }
        
        # Update grass level. Note that this is NOT additive! but next grass is highly dependent on past 
        df$NextGrass<- grass_growth(current_grass = df$Grass, rain_quality = df$Precip)
        
        NextGrass<-df #Make an object so we can remake df without losing it
        
        #make the error in the forecast
        ForecastedGrass=rnorm(n=nrow(df), mean= df$NextGrass, sd= ForecastError)
        df$ForecastedGrass<-ForecastedGrass
        rm(ForecastedGrass)
        
        df2<-df%>%group_by(CommID)%>%
          summarise(
            Animals = sum(Animal, na.rm = TRUE),  # Sum all animals in the group
            ForecastedGrass = sum(ForecastedGrass[Grazed == 1], na.rm = TRUE))%>%   # Sum grass only where Grazed
          mutate(ForcastedAnimalCapacity=ForecastedGrass/GrassPerCow)%>% #how many animals can this sustain
          #what would be the ideal change. Multiply by 0.85 to keep from aiming to go all the way to 0
          mutate(ForecastedOptimalStrategy = dplyr::if_else(
            Animals > 0,
            (ForcastedAnimalCapacity*0.85 - Animals) / Animals, # avoid dividing by 0
            1
          )) %>%
          # mutate(ForecastedOptimalStrategy=(ForcastedAnimalCapacity*0.85 -Animals) / Animals)%>% 
          dplyr::select(CommID,ForecastedOptimalStrategy)
        
        # df2$ForecastedOptimalStrategy <- ifelse(is.na(df2$ForecastedOptimalStrategy), 1, df2$ForecastedOptimalStrategy)
        
        Forecasters<-Ranchers%>%filter(is.na(new_strategy)==TRUE) #just people using forecasts
        Learners<-Ranchers%>%filter(is.na(new_strategy)==FALSE) #just people not using forecasts
        Forecasters<-base::merge(Forecasters,df2,by="CommID") #apply to whole community
        Forecasters$new_strategy<-Forecasters$ForecastedOptimalStrategy #match column name of learners
        Forecasters<-Forecasters%>%dplyr::select(-ForecastedOptimalStrategy) #get rid of extra column
        
        Ranchers<-rbind(Forecasters,Learners)
        
        #There are two constraints 1) opportunity to sell and 2) money to buy.
        #Buying animals is dependent on rancher economic wellbeing (do they have enough money). 
        #Selling cows is easier if you are involved in conservation. People keep a baseline number of cows
        
        # Copy original animals for reference
        Ranchers$animals_before <- Ranchers$stock_count
        
        # Apply buying/selling logic
        Ranchers<- within(Ranchers, {
          
          # Proposed change: positive (buy), negative (sell)
          
          proposed_change <- round(new_strategy  * stock_count)  # round to ensure whole animals
          
          #for people that have 0, but want to buy, make 1.
          #could use a different number than 1 dependent on their money
          proposed_change[stock_count == 0 & new_strategy >= 0.5] <- 1
          
          # --- Buying Logic ---
          buying <- proposed_change > 0
          max_affordable <- floor(Money / Animal_cost)  # maximum animals they can buy
          actual_change <- ifelse(buying, pmin(proposed_change, max_affordable), 0) #if buying, buy the smaller of what they want or what they can afford
          
          # --- Selling Logic ---
          selling <- proposed_change < 0
          # Max proportion of herd allowed to sell
          max_sell_prop <- ifelse(Conservation == 1, MaxConservationSale, MaxGeneralSale) #The max they can sell
          max_sell <- floor(stock_count * max_sell_prop) #how many animals could they possibly sell
          allowed_sell <- pmax(stock_count - AnimalBaseline, 0) #If they have two or fewer animals, they wont sell
          max_allowed_sell <- pmin(max_sell, allowed_sell) #What could they sell, smaller # of allowed and able to
          
          # Final selling change: negative integer, bounded
          sell_change <- pmax(proposed_change, -max_allowed_sell) #if they want to sell more than their max, don't allow
          
          # Combine both
          actual_change[selling] <- sell_change[selling]
          
          # Update animals and wealth
          stock_count <- stock_count + actual_change
          Money <- Money  - Animal_cost * actual_change  # Note: selling gives positive change to wealth
          
          actual_animals_change <- actual_change
        })
        
        
        Ranchers$PastStockChangeAnimal<-Ranchers$actual_animals_change 
        Ranchers$PastStockChangeProp <-Ranchers$actual_animals_change /Ranchers$animals_before
        #If infinite (if they had 0 before and bought 1)
        Ranchers$PastStockChangeProp<-ifelse(is.finite(Ranchers$PastStockChangeProp),Ranchers$PastStockChangeProp,1.0)
        #If NA, make 0
        Ranchers$PastStockChangeProp<-ifelse(is.na(Ranchers$PastStockChangeProp),0,Ranchers$PastStockChangeProp)
        Ranchers$PastStock <-Ranchers$animals_before
        
        #Make sure this is updated for the next round.
        Ranchers<-Ranchers%>%
          mutate(TotalEconomicWellbeing=Money+(stock_count *Animal_cost))
        
        
        # Uncomment to save full data
        Ranchers_history_scnrep <- data.table::rbindlist(
          list(
            Ranchers_history_scnrep,
            data.table::as.data.table(Ranchers)[
              , `:=`(ts = ts, Rep = rep, Scenario = target_scenario)]
          ),
          use.names = TRUE, fill = TRUE
        )
        
        
        Ranchers <- Ranchers %>% dplyr::select(
          -c(
            StockDeficit,
            new_strategy,
            animals_before,
            actual_animals_change,
            sell_change,
            max_allowed_sell,
            allowed_sell,
            max_sell,
            max_sell_prop,
            selling,
            actual_change,
            max_affordable,
            buying,
            proposed_change
          )
        )
        
        
        #Remake the Cows raster
        #change number of animals in the raster
        df<-raster::as.data.frame(rstack,xy=T)
        ranimals<-Ranchers%>%group_by(CommID)%>%summarise(Animal=sum(stock_count)/N_Plots) #summarise as table, divide by N_Plots as this is the total number per community
        df<-df%>%dplyr::select(-Animal)
        ranimals<-merge(df,ranimals,by="CommID")
        ranimals <- rasterFromXYZ(ranimals[order(ranimals$y, ranimals$x), c("x", "y", "Animal")])
        
        AnIndex<-which(names(rstack) == "Animal")
        rstack <- rstack[[ -AnIndex ]]
        rstack<-stack(rstack,ranimals)
        
        
        ## Update rain
        #Rains then replenish the grass cover of the landscape following the climatic 
        # conditions of the specific simulation and following a logistic growth curve. 
        
        df<-raster::as.data.frame(rstack,xy=T)
        df<-df%>%dplyr::select(-Grass)
        NextGrass<-NextGrass[c("x","y","NextGrass")]
        names(NextGrass)[3]<-"Grass"
        df<-base::merge(df,NextGrass,by=c("x","y"))
        rstack<-rasterFromXYZ(df[order(df$y, df$x),])
        
        # Downloads non-summarized data, comment to save space and run time
        # Build output path: one file per (condition, scenario, rep, ts), postprocess later
        tif_dir <- file.path(out_root, "Landscapes", exp_setting, target_scenario, sprintf("rep%03d", rep))
        dir.create(tif_dir, recursive = TRUE, showWarnings = FALSE)

        tif_path <- file.path(tif_dir, sprintf("ts%03d.tif", ts))

        raster::writeRaster(
          rstack,
          filename = tif_path,
          format   = "GTiff",
          options  = c("COMPRESS=LZW"),
          overwrite = TRUE
        )
        
        raster::plot(rstack[[c("Cons", "Mean_economic_wellbeing", "Grazed", "Animal", "Grass")]])
        
        #### Add summary stats to table
        
        SummaryDat2<-data.frame(Time=ts,AvgMoney=mean(Ranchers$Money),
                                Gini = ineq::ineq(Ranchers$TotalEconomicWellbeing,type="Gini"),
                                AvgCows = mean(Ranchers$stock_count),
                                TotalCows = sum(Ranchers$stock_count),
                                AvgGrass = mean(rstack[["Grass"]][]),
                                TotalGrass = sum(rstack[["Grass"]][]),
                                PropCons = length(which(rstack[["Cons"]][]==T))/
                                  (N_Communities*N_Plots))
        
        
        SummaryDat2$Scenario <- target_scenario
        SummaryDat <- rbind(SummaryDat, SummaryDat2)
        
      }
      
      
      # Downloads non-summarized data, comment to save space and run time
      if (!"Condition" %in% names(Ranchers_history_scnrep)) {
        Ranchers_history_scnrep[, Condition := exp_setting]
      }
      data.table::setcolorder(
        Ranchers_history_scnrep,
        c("Condition","Scenario","Rep","ts",
          setdiff(names(Ranchers_history_scnrep), c("Condition","Scenario","Rep","ts")))
      )

      chunk_dir <- file.path(out_root,"Ranchers","tmp", exp_setting)
      dir.create(chunk_dir, recursive = TRUE, showWarnings = FALSE)

      chunk_ext <- if (compress_chunks) "csv.gz" else "csv"
      chunk_file <- file.path(
        chunk_dir,
        sprintf("%s_rep%03d_%s.%s", exp_setting, rep, target_scenario, chunk_ext)
      )

      data.table::fwrite(
        Ranchers_history_scnrep,
        chunk_file,
        compress = if (compress_chunks) "gzip" else "none"
      )
      
      SummaryDat
      
    }
    
    names(ResultsList) <- scenarios
    
    # Summary stats
    RepResults <- data.table::rbindlist(ResultsList, use.names = TRUE, fill = TRUE)
    RepResults$Rep <- rep
    AllSimResults[[rep]] <- RepResults
    
  }
  
  # Combine all repetitions for this condition
  AllSimResultsDF <- data.table::rbindlist(AllSimResults, use.names = TRUE, fill = TRUE)
  
  
  ## Save summary output -----------------------------------------------------
  write.csv(AllSimResultsDF,
            file.path(out_root, paste0(exp_setting, ".csv")),
            row.names = FALSE)
  
  # Combine precip files, comment to save file size and space
  
  # Gather the temp files for this condition
  # cond_pattern <- paste0("^", gsub("\\.", "\\\\.", exp_setting), "_rep\\d+_.*\\.gpkg$")
  # tmp_dir   <- file.path(out_root, "Precip", "tmp")
  # tmp_files <- list.files(path = tmp_dir, pattern = cond_pattern, full.names = TRUE)
  # 
  # 
  # if (length(tmp_files) > 0) {
  #   # Read & row-bind while preserving geometry
  #   precip_list <- lapply(tmp_files, function(f) sf::read_sf(f, quiet = TRUE))
  #   Precip_condition <- do.call(dplyr::bind_rows, precip_list)
  # 
  #   # Write ONE GeoPackage per condition (single layer with all reps & scenarios)
  #   final_gpkg <- file.path(out_root, "Precip", "final", paste0(exp_setting, "_Precip.gpkg"))
  #   sf::write_sf(Precip_condition, final_gpkg, delete_dsn = TRUE)
  # 
  #   # clean up the small temp files now that we have the final file
  #   file.remove(tmp_files)
  # 
  # }

  # Helpful if non-summarized data is downloaded
  ## Combine ranchers (memory safe version)
  chunk_dir <- file.path(out_root,"Ranchers","tmp", exp_setting)
  final_dir <- file.path(out_root,"Ranchers")
  dir.create(final_dir, recursive = TRUE, showWarnings = FALSE)

  final_csv <- file.path(final_dir, paste0(exp_setting, "_Ranchers.csv"))
  final_gz  <- paste0(final_csv, ".gz")

  pattern <- if (compress_chunks) "\\.csv\\.gz$" else "\\.csv$"

  if (dir.exists(chunk_dir)) {
    chunk_files <- list.files(chunk_dir, pattern = pattern, full.names = TRUE)
    if (length(chunk_files) > 0) {
      message("Combining ", length(chunk_files), " rancher chunk files for ", exp_setting, " ...")

      # Pass 1: discover the union of columns across all chunks (headers only)
      all_cols <- character(0)
      for (f in chunk_files) {
        hdr <- data.table::fread(f, nrows = 0, showProgress = FALSE)
        all_cols <- union(all_cols, names(hdr))
      }
      # Put important keys first for readability
      key_first <- c("Condition","Scenario","Rep","ts","CommID","PlotID","person_id")
      all_cols  <- c(intersect(key_first, all_cols), setdiff(all_cols, key_first))

      if (file.exists(final_csv)) file.remove(final_csv)

      # Pass 2: stream-append each chunk
      for (i in seq_along(chunk_files)) {
        dt <- data.table::fread(chunk_files[i], showProgress = FALSE)

        # Add any missing columns as NA, enforce column order
        miss <- setdiff(all_cols, names(dt))
        if (length(miss)) for (m in miss) dt[, (m) := NA]
        data.table::setcolorder(dt, all_cols)

        data.table::fwrite(dt, final_csv, append = i > 1)
        rm(dt); gc()
      }

      # Optionally gzip the final CSV
      if (compress_chunks) {
        R.utils::gzip(final_csv, destname = final_gz, overwrite = TRUE)
        message("  -> wrote ", final_gz)
      } else {
        message("  -> wrote ", final_csv)
      }

      # Clean up chunk files and the temp folder
      ok <- file.remove(chunk_files)
      if (all(ok)) {
        unlink(chunk_dir, recursive = TRUE, force = TRUE)
        message("  -> deleted chunks in ", chunk_dir)
      } else {
        warning("Some chunk files could not be deleted: ",
                paste(basename(chunk_files[!ok]), collapse = ", "))
      }
    } else {
      message("No rancher chunk files found for ", exp_setting)
    }
  }
  
  
}

if (!is.null(cl)) stopCluster(cl)

