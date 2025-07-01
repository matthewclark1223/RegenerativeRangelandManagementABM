library(data.table)
set.seed(123) # comment out for running many times
Supplemental_fodder<-FALSE #does Cons grazing give supp food
Set_Adoption<-TRUE #Is adoption of Conservation set at the starting number
Forecasts<- "No_one" #Can take values of "No_one", "Everyone", "Rich","Poor", "Conservation"
ForecastError<-0 #How wrong are forecasts.#basically ranges 0:100
Fodder_Price_Per_Cow<-100000 #play with this
Animal_cost<-5 #Price of animals. Used for buying/selling and also financial gains from having animals
CapitalGainRate<-1.02 #Financial gains from money in the bank
AnimalGainRate<-1.05 # Financial gains from having animals (multiplied by the value of the animals and paid in cash)
MaxGeneralSale<-0.2 #Maximum proportion of animals people can sell when NOT in conservation
MaxConservationSale<-0.6 #Maximum proportion of animals people in conservation can sell
AnimalBaseline<-2 #people never sell below this number
TimeSteps<-50
#Make rain data
source("../RCode/MakeStylizedLandscape.R")
source("../RCode/PrecipTimeseries.R")
PrecipStack<-PrecipTimeseries(TimeSteps=TimeSteps,StandardDev=0.2)

SummaryDat<-data.frame(Time=0,AvgMoney=mean(Ranchers$economic_wellbeing),
                       AvgCows = mean(Ranchers$stock_count),
                       TotalCows = sum(Ranchers$stock_count),
                       AverageGrass = mean(rstack[["T0Grass"]][]),
                       TotalGrass = sum(rstack[["T0Grass"]][]),
                       PropCons = length(which(rstack[["Cons"]][]==T))/
                         (N_Communities*N_Plots))


#Get rid of some t0 variables
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


RanchersPast<-Ranchers
rstackPast<-rstack

#


#####Model scheduling ##########################
#####Module 1 - Grazing#####
#vectorized code of was used to make the starting landscape
df<-raster::as.data.frame(rstackPast,xy=T)# Easier as a dataframe

########
# Step 1: Summarize grazed plots by community
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
  select(-Grass2)

# Step 2: Join the group-level summary back to the main df
df<- df %>%
  left_join(df2, by = "CommID", suffix = c("", ".comm")) %>%
  mutate(
    Grass = case_when(
      Grazed == 1 ~ ceiling(Grass.comm / n), #for the grazed plots, distribute the grass/community
      TRUE ~ Grass
    )
  ) %>%
  select(-Grass.comm, -Animal.comm, -n, -DeficitGrass) #remove cols no longer using
############
rGrass <- rasterFromXYZ(df[order(df$y, df$x), c("x", "y", "Grass")])

#Remove old grass
GrassIndex<-which(names(rstack) == "Grass")
rstack <- rstack[[ -GrassIndex ]]

#Add new grass
rstack<-stack(rstack,rGrass)




######Module 2 - Supplemental fodder#####
#we identify which communities did not have enough grass to feed all animals. 
#Ranchers buy supplemental fodder to feed animals as necessary and as they are able (given economic wellbeing). 

#If no one is in defecit. They all get 0
if(length(which(df2$DeficitGrass<0))==0){
  Ranchers$StockDeficit<-0
  Ranchers$MoneyDeficit<-0
}


#If there are decefits, do the following:
if(length(which(df2$DeficitGrass<0))>0){ #This continues to wrap the next module

  Ranchers<-RanchersPast%>%filter(CommID %in% df2$CommID) #only the ranchers from communities with deficit

df2$AnimalsDeficit<-df2$DeficitGrass/GrassPerCow #How many animals short was the community grass
df2$ProportionDeficit<-abs(df2$AnimalsDeficit)/df2$Animal

#This handles NAs produced when there are no animals in a community
df2$ProportionDeficit<-ifelse(is.na(df2$ProportionDeficit)==T,0,df2$ProportionDeficit) 

Ranchers<-Ranchers%>%
left_join(df2[,c("CommID","ProportionDeficit")], by = "CommID") %>% #get how many animals need food per person
  mutate(StockDeficit  = ceiling(stock_count  * ProportionDeficit))%>% #ceiling since partially feeding an animal counts as not feeding it!
  select(-ProportionDeficit)

#In conditions where no supplemental fodder is available from the conservation program, animals die. 
#In conditions where supplemental fodder is given by the conservation grazing program, it is distributed as necessary.
if(Supplemental_fodder ==TRUE){ #give free food to people in conservation maybe limit this...
  Ranchers$StockDeficit<-ifelse(Ranchers$Conservation==TRUE,0,Ranchers$StockDeficit)
}

#Now reduce the stock deficit and the money of each person as they buy fodder
Ranchers$MoneyDeficit<-Fodder_Price_Per_Cow*Ranchers$StockDeficit


Ranchers$economic_wellbeing <-Ranchers$economic_wellbeing-Ranchers$MoneyDeficit #reduce their money by the deficit

#If they had enough money, their deficit is gone. If not, it's the inverse of their money now.
Ranchers$MoneyDeficit<-ifelse(Ranchers$economic_wellbeing <0, (Ranchers$economic_wellbeing*-1),0 )

Ranchers$economic_wellbeing <-ifelse(Ranchers$economic_wellbeing <0, 0,Ranchers$economic_wellbeing ) #dont let it go below 0


######Module 3 - Animals die#####
Ranchers$StockDeficit<-ceiling(Ranchers$MoneyDeficit/Fodder_Price_Per_Cow) #ceiling as only half food still counts as not enough

Ranchers$stock_count<-Ranchers$stock_count-Ranchers$StockDeficit #reduce stock by deficit number. 
Ranchers$stock_count[Ranchers$stock_count<0] <- 0 #No negative numbers

#change number of animals in the raster
ranimals<-Ranchers%>%group_by(CommID)%>%summarise(Animal=sum(stock_count)/N_Plots) #summarise as table, divide by N_Plots as this is the total number per community
df<-df%>%dplyr::select(-Animal)
ranimals<-merge(df,ranimals,by="CommID")
ranimals <- rasterFromXYZ(ranimals[order(ranimals$y, ranimals$x), c("x", "y", "Animal")])

AnIndex<-which(names(rstack) == "Animal")
rstack <- rstack[[ -AnIndex ]]
rstack<-stack(rstack,ranimals)

}

#####Module 4 #PEOPLE GET MONEY FROM COWS AND MONEY ####

#We want to increase people's money by some percentage, so say 1.25* their current money.
#We also want to give them money for each animal beyond what that animal is worth (as animals have more value than just sale value)

#Make money from money
Ranchers$economic_wellbeing<-Ranchers$economic_wellbeing*CapitalGainRate

#money from animals
Ranchers$economic_wellbeing<-Ranchers$economic_wellbeing+(Ranchers$stock_count*Animal_cost*AnimalGainRate)

df2<-Ranchers%>%group_by(CommID)%>%summarise(Mean_economic_wellbeing=mean(economic_wellbeing),
                                             Conservation=as.logical(min(Conservation)))

#Update raster with mean economic wellbeing by community
df<-df%>%dplyr::select(-Mean_economic_wellbeing)
rWell<-merge(df,df2,by="CommID")
rWell <- rasterFromXYZ(rWell[order(rWell$y, rWell$x), c("x", "y", "Mean_economic_wellbeing")])

WellIndex<-which(names(rstack) == "Mean_economic_wellbeing")
rstack <- rstack[[ -WellIndex ]]
rstack<-stack(rstack,rWell)


#####Module 5 - Conservation adoption#####
#Communities then observe all surrounding communities (queen’s case) and use success biased imitation 
##This is handled in the make stylized landscape script, where we make the 'adj_list' object
#to disproportionately copy the communities with the highest average rancher economic wellbeing. 

#Only do this if conservation proportion is not fixed
if(Set_Adoption==FALSE){

# Convert df to data.table for speed
df2 <- as.data.table(df2)

# Initialize vector to store new behaviors
new_behavior <- character(nrow(df2))

# Vectorized helper: pre-extract payoff and behavior
behaviors <- df2$Conservation
payoffs <- df2$Mean_economic_wellbeing 

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
  pA <- exp(0.5 * payoff_A) / (exp(0.5 * payoff_A) + exp(0.5 * payoff_B))
  
  # Draw new behavior
  new_behavior[i] <- sample(c(TRUE, FALSE), size = 1, prob = c(pA, 1 - pA))
}

# Update the dataframe
df2[, Conservation := new_behavior]

df2<-as.data.frame(df2)

#####Need to assign conservation in the raster
df2$Conservation<-as.numeric(as.logical(df2$Conservation)) #make into 1s and 0s

df<-df%>%dplyr::select(-Cons)
rCons<-merge(df,df2,by="CommID")
rCons <- rasterFromXYZ(rCons[order(rCons$y, rCons$x), c("x", "y", "Conservation")])
names(rCons)<-"Cons" #to match above

ConsIndex<-which(names(rstack) == "Cons")
rstack <- rstack[[ -ConsIndex ]]
rstack<-stack(rstack,rCons)

}

######Module 5b - Reassign conservation plots#####
#For communities that have adopted the conservation grazing program, the plot with the lowest 
#grass cover is allocated for protection. 

df<-raster::as.data.frame(rstack,xy=T)
dfCons<-df%>%filter(Cons==1)#for just the conservation communities
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
  select(-min_grass, -is_min)

dfCons <- dfCons %>%
  relocate(CommID , .after = y)

dfNotCons<-df%>%filter(Cons==0)#for just the conservation communities
dfNotCons$Grazed<-1 #All grazed in non-conservation communities

df<-rbind(dfCons,dfNotCons)

rGrazed  <- rasterFromXYZ(df[order(df$y, df$x), c("x", "y", "Grazed")]) #make this into a raster

GrazedIndex<-which(names(rstack) == "Grazed") #get the index
rstack <- rstack[[ -GrazedIndex ]] #remove the old grazing plan
rstack<-stack(rstack,rGrazed)
#######






######Module 6 - buying / selling animals#####
#In anticipation of the next season rain, ranchers decide whether to buy or sell animals and how many. 
#In baseline conditions, ranchers randomly select five other ranchers and disproportionately adopt 
#the buying/selling strategy of the most successful (i.e., success biased behavioral transmission).

# Payoff-biased imitation function
PayoffUpdate<-function(dff){
  
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
      self_val <- economic_wellbeing[i]
      self_strategy <- PastStockChangeProp[i]
      
      # Exclude self from peer pool
      peer_idx <- setdiff(seq_len(n), i)
      sampled_idx <- sample(peer_idx, size = min(5, length(peer_idx)), replace = FALSE) #sample up to 5 people
      
      peer_payoffs <- economic_wellbeing[sampled_idx]
      peer_strategies <- PastStockChangeProp[sampled_idx]
      
      if (sum(peer_payoffs) == 0) {
        probs <- rep(1 / length(sampled_idx), length(sampled_idx))
      } else {
        probs <- peer_payoffs / sum(peer_payoffs)
      }
      
      chosen_peer <- sampled_idx[sample(seq_along(sampled_idx), size = 1, prob = probs)]
      
      if (economic_wellbeing[chosen_peer] > self_val) {
        result[i] <- PastStockChangeProp[chosen_peer]
      }
      # else keep original strategy
    }
    
    result
  }, by = CommID]
  return(as.data.frame(dt))
}

#### Now, apply to only the groups not using forecasts
if(Forecasts=="No_one"){Ranchers<-PayoffUpdate(Ranchers) } #everyone learns socially

if(Forecasts=="Everyone"){Ranchers<-Ranchers%>%mutate(new_strategy=NA) } #No one learns socially

if(Forecasts=="Conservation"){
  Forecasters<-Ranchers%>%dplyr::filter(Conservation==TRUE)%>%mutate(new_strategy=NA) #Only non-conservation communities learn socially
  Learners<-Ranchers%>%dplyr::filter(Conservation==FALSE)
  Learners<-PayoffUpdate(Learners)
  Ranchers<-rbind(Forecasters,Learners)
}

if(Forecasts=="Rich"){
  Rich<-Ranchers%>%group_by(CommID)%>%summarise(wellbeing=mean(economic_wellbeing ))%>%
    mutate(Rich=wellbeing>=quantile(wellbeing,0.75))%>%select(-wellbeing)
  Ranchers<-base::merge(Ranchers,Rich,by="CommID")
  
  Rich<-Ranchers%>%dplyr::filter(Rich==TRUE)%>%mutate(new_strategy=NA) #Rich don't learn socially
  Poor<-Ranchers%>%dplyr::filter(Rich==FALSE) 
  Poor<-PayoffUpdate(Poor) #everyone else learns socially
  Ranchers<-rbind(Rich,Poor)
  }  
  
if(Forecasts=="Poor"){
  Poor<-Ranchers%>%group_by(CommID)%>%summarise(wellbeing=mean(economic_wellbeing ))%>%
    mutate(Poor=wellbeing<=quantile(wellbeing,0.25))%>%select(-wellbeing)
  Ranchers<-base::merge(Ranchers,Poor,by="CommID")
  
  Poor<-Ranchers%>%dplyr::filter(Poor==TRUE)%>%mutate(new_strategy=NA) #poor don't learn socially
  Rich<-Ranchers%>%dplyr::filter(Poor==FALSE) 
  Rich<-PayoffUpdate(Rich) #everyone else learns socially
  Ranchers<-rbind(Rich,Poor)
} 


# Now need to assign new strategy to people/groups with forecasts

df<-raster::as.data.frame(rstack,xy=T)

NextGrass<-PrecipStack%>%filter(Time==ts)%>%as.data.frame(.)%>%select(-geometry,Time)

df<-base::merge(df,NextGrass,by=c("CommID","PlotID"))


############START HERE. THIS NEEDS MORE THINKING

grass_growth <- function(current_grass, rain_quality) {
  # Parameters (can be calibrated)
  K <- 80  # Carrying capacity (e.g., max grass biomass)
  r_max <- 0.8  # Maximum intrinsic growth rate per year
  
  # Rainfall effect (nonlinear): scaled between 0 and 1
  rain_effect <- (rain_quality / 100)^5
  
  # Logistic growth equation modified by rainfall
  growth <- r_max * rain_effect * current_grass * (1 - current_grass / K)
  
  # Update grass
  new_grass <- current_grass + growth
  
  # Ensure grass doesn't exceed carrying capacity
  ##Not done
  
  return(new_grass)
}


# Update grass level. Note that this is NOT additive! but next grass is highly dependent on past 
df$NextGrass<- grass_growth(current_grass = df$Grass, rain_quality = df$Precip)
NextGrass<-df #Make an object so we can remake df without losing it

ForecastedGrass=rnorm(n=nrow(df), mean= df$NextGrass, sd= ForecastError )

df$ForecastedGrass<-ForecastedGrass
  
df2<-df%>%group_by(CommID)%>%
  filter(Grazed==1)%>%summarise(Animals=sum(Animal),ForecastedGrass=sum(ForecastedGrass))%>% #add grass in grazed plots
  mutate(ForcastedAnimalCapacity=ForecastedGrass/GrassPerCow)%>% #how many animals can this sustain
  mutate(ForecastedOptimalStrategy=(ForcastedAnimalCapacity -Animals) / Animals)%>% #what would be the ideal change
  select(CommID,ForecastedOptimalStrategy)


Forecasters<-Ranchers%>%filter(is.na(new_strategy)==TRUE) #just people using forecasts
Learners<-Ranchers%>%filter(is.na(new_strategy)==FALSE) #just people not using forecasts
Forecasters<-base::merge(Forecasters,df2,by="CommID") #apply to whole community
Forecasters$new_strategy<-Forecasters$ForecastedOptimalStrategy #match column name of learners
Forecasters<-Forecasters%>%select(-ForecastedOptimalStrategy) #get rid of extra column

Ranchers<-rbind(Forecasters,Learners)




#Now need to actually buy/sell

#There are two constraints 1) opportunity to sell and 2) money to buy

#Buying animals is dependent on rancher economic wellbeing (do they have enough money).
#selling cows is easier if you are involved in conservation 
#people keep a baseline number of cows

# Copy original animals for reference
Ranchers$animals_before <- Ranchers$stock_count

# Apply buying/selling logic
Ranchers<- within(Ranchers, {
  
  # Proposed change: positive (buy), negative (sell)
  proposed_change <- floor(new_strategy  * stock_count )  # floor to ensure whole animals
  
  # --- Buying Logic ---
  buying <- proposed_change > 0
  max_affordable <- floor(economic_wellbeing / Animal_cost)  # maximum animals they can buy
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
  economic_wellbeing <- economic_wellbeing  - Animal_cost * actual_change  # Note: selling gives positive change to wealth
  
  # Optional: log actual change
  actual_animals_change <- actual_change
})

Ranchers$PastStockChangeAnimal<-Ranchers$actual_animals_change 
Ranchers$PastStockChangeProp <-Ranchers$actual_animals_change /Ranchers$animals_before
#If 0, make NA
Ranchers$PastStockChangeProp<-ifelse(is.na(Ranchers$PastStockChangeProp),0,Ranchers$PastStockChangeProp)
Ranchers$PastStock <-Ranchers$animals_before

Ranchers<-Ranchers%>%select(-c(StockDeficit,MoneyDeficit,new_strategy,animals_before,actual_animals_change,sell_change,max_allowed_sell,
                     allowed_sell,max_sell,max_sell_prop,selling,actual_change,max_affordable,buying,proposed_change))




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

######Module 7 - Rain#####
#Rains then replenish the grass cover of the landscape following the climatic conditions 
#of the specific simulation and following a logistic growth curve. 



df<-raster::as.data.frame(rstack,xy=T)
df<-df%>%select(-Grass)
NextGrass<-NextGrass[c("x","y","NextGrass")]
names(NextGrass)[3]<-"Grass"
df<-base::merge(df,NextGrass,by=c("x","y"))
rstack<-rasterFromXYZ(df[order(df$y, df$x),])

plot(rstack)
#plot(rstack[["Grass"]])

#### Add summary stats to table

SummaryDat2<-data.frame(Time=ts,AvgMoney=mean(Ranchers$economic_wellbeing),
                       AvgCows = mean(Ranchers$stock_count),
                       TotalCows = sum(Ranchers$stock_count),
                       AverageGrass = mean(rstack[["Grass"]][]),
                       TotalGrass = sum(rstack[["Grass"]][]),
                       PropCons = length(which(rstack[["Cons"]][]==T))/
                         (N_Communities*N_Plots))


SummaryDat<-rbind(SummaryDat,SummaryDat2)}



p1<-ggplot(SummaryDat,aes(x=Time,y=AvgMoney))+geom_line()+ggtitle("Money")
p2<-ggplot(SummaryDat,aes(x=Time,y=AvgCows))+geom_line()+ggtitle("Cows")
p3<-ggplot(SummaryDat,aes(x=Time,y=AverageGrass))+geom_line()+ggtitle("Grass")
p4<-ggplot(SummaryDat,aes(x=Time,y=PropCons))+geom_line()+ggtitle("Cons")

cowplot::plot_grid(p1, p2,p3,p4)

