source("../RCode/DeprivationDistribution.R")
set.seed(123)
#This landscape will have 10 layers.
#Among these layers, there are two distinct levels: community and individual plots

#Communities have the following attributes:
#-ID
#-Conservation (y/n)
#-Community wellbeing
#-Number of people
#-Number of cows

#Plots have the following attributes:
#-ID
#-Grass growth (rain)
#-Resting (y/n)
#-Grass cover (result of rain and grazing pressure (number of cows and whether resting))


#There are also an associated data frames: Ranchers
#The 'Ranchers' data frame tracks the community, grazing behavior, cattle buying/selling behavior, and income of each person


#Set control parameters
N_Communities<-100 #Must be perfect square number and a multiple of the number of plots for the code below to work. 
N_Plots<-4 #Must be perfect square number for the code below to work. 
Starting_Prop_Conservation<-0.25
StartingGrassMin<-0.2#lower bound of grass/rain. i.e., the worst areas have 20% of the best (before grazing)
Spatial_Autocorrelation_Rain<-0.98 # this affects the spatial autocorelation of grass growth (0 is fully random, 1 is completely determined by space)
GrassPerCow<-0.05 # will need to play with this
Min_cows<-2 #min cows per person
Cow_Inequality<-1 #exponential rate for within-community cattle distribution. higher numbers = more EQUAL
Econ_Inequality<-0.1 # Scalar, lower number = less within-community inequality (centered at community mean)

#Make community IDs
mCommID<-matrix(1:(N_Communities),nrow=sqrt(N_Communities))
rCommID <- raster::raster(mCommID)

#Make adjacency matrix of which communities touch each other 

# Initialize list to store adjacent cell numbers
adj_list <- vector("list", N_Communities)

# Get adjacency using Queen's case (directions = 8)
adj <- raster::adjacent(rCommID, cells = 1:N_Communities, directions = 8, pairs = TRUE, include = TRUE)

# Get community ID for each cell
comm_id_vec <- terra::values(rCommID)

# For each community (1 to 100)
for (i in 1:N_Communities) {
  
  # Find all adjacent cells for those cells
  adj_cells <- adj[adj[,1] == i, 2]
  
  
  # Remove duplicates
  adj_list[[i]] <- unique(adj_cells)
}

# Now adj_list[[i]] contains adjacent cell numbers to community i (Queen's case)


#Now disaggregate as above
rCommID<-raster::disaggregate(rCommID, fact=2)
raster::plot(rCommID)

#Make conservation initiative 
ConsComs<-rep(FALSE,N_Communities) #List of all False
Comindex<-sample(size=N_Communities*Starting_Prop_Conservation, #Get indexes of Cons comms
                 1:N_Communities,replace=F)
ConsComs[Comindex]<-TRUE #Assign conservation

mCons<-matrix(ConsComs,nrow=sqrt(N_Communities)) #Make a matrix

rCons <- raster::raster(mCons) #Make a raster
rCons<-raster::disaggregate(rCons, fact=2) #Expand so 4 plots/community
raster::plot(rCons)

#Make Deprivation (community wellbeing). Approximation of NASA deprivation index
mDepriv<-matrix(sample(x=Dep$Dep,size=N_Communities,replace=F),nrow=sqrt(N_Communities)) #sample from actual deprivation distribution of AOI
rDepriv <- raster::raster(mDepriv)
rDepriv<-raster::disaggregate(rDepriv, fact=2)
raster::plot(rDepriv)


#Make Population. Approximation of CSA community data
mPop<-matrix(rpois(n=N_Communities,lambda=50),nrow=sqrt(N_Communities)) 
rPop <- raster::raster(mPop)
rPop<-raster::disaggregate(rPop, fact=2)
rPop[]<-ceiling(rPop[]/4) #the disaggregation above effectively multiplies the number by 4
raster::plot(rPop)


#Make ANIMAL Population. Approximation of cattle estimates from AOI
#This is not correlated with deprivation as these are not correlated across Southern Africa
mAnimal<-matrix(rnbinom(n=N_Communities,mu=500,size=20),nrow=sqrt(N_Communities)) 
rAnimal<- raster::raster(mAnimal)
rAnimal<-raster::disaggregate(rAnimal, fact=2)
rAnimal[]<-ceiling(rAnimal[]/4) #the disaggregation above effectively multiplies the number by 4
raster::plot(rAnimal)

####Now do plot level characteristics

#Make the plot IDs
mPlotID<-matrix(1:(N_Communities*N_Plots),nrow=sqrt(N_Communities)*sqrt(N_Plots))
rPlotID <- raster::raster(mPlotID)
raster::plot(rPlotID)

#Make the t0 rain (grazing free grass cover) at each plot
#First make grass growth without cows. This assumes that all land has same growing potential. Rain is the only determinant 
#Grass takes number between StartingGrassMin and 1 
source("../RCode/SpatialAutocorrelationRaster.R")
rT0Rain<-MakeAutoCorMat(dummyMatrix = rPlotID,target_I = Spatial_Autocorrelation_Rain) #This is normally distributed. We want to bound and make pois
rT0Rain[]<- (rT0Rain[] - min(rT0Rain[])) / (max(rT0Rain[]) - min(rT0Rain[])) * (1 - StartingGrassMin) + StartingGrassMin
rT0Rain[]<-floor(rT0Rain[]*100) #Make discrete
raster::plot(rT0Rain)

#Now we need to assign each plot as resting or not based on whether it's in a conservation area
#Choose random. Later this will be the one with least grass, but not at start bc cows haven't grazed yet
source("../RCode/GrazedBinarySimulation.R")
raster::plot(rGrazed) # Returns this raster with 1/4 of the conservation plots labeled as 0


#Now for each community, we need to distribute the grazing evenly across the grazed plots
#so, we need to multiply the grass by 1/0 (whether they're grazed or not), and
#add all the grass in each community up. then have animals reduce it and redistribute


rstack<-raster::stack(rCommID,rAnimal,rPlotID,rT0Rain,rGrazed,rCons) #this will be easier as a df
df<-raster::as.data.frame(rstack,xy=T)
names(df)<-c("x","y","CommID","Animal","PlotID","T0Rain","Grazed","Cons")


df2<-df%>%dplyr::filter(Grazed==1)%>% #Only consider plots that are grazed
  dplyr::group_by(CommID)%>% #group by community
  dplyr::summarize(t0Rain=sum(T0Rain),n=dplyr::n()) #gt total grass/rain and animals
  
CommAnimals<-df%>%dplyr::group_by(CommID)%>% #group by community
  dplyr::summarize(Animal=sum(Animal))

df2<-merge(df2,CommAnimals,by="CommID")%>%
  dplyr::mutate(t0Grass=ceiling(t0Rain-(GrassPerCow*Animal)))%>% #cows eat
  dplyr:: mutate(t0Grass=ifelse(t0Grass<=0,1,t0Grass)) #don't let grass go all the way to 0



df$t0Grass<-NA
for(i in 1:nrow(df)){
  if(df[i,]$Grazed==1){ #If it was grazed
    Community<-df[i,]$CommID #for each community
    Community2<-df2%>%dplyr::filter(CommID==Community)
    df[i,]$t0Grass<-ceiling((Community2$t0Grass)/Community2$n) #Split the total grass by the number of grazed pxels
  }
  if(df[i,]$Grazed==0){ 
    df[i,]$t0Grass<-df[i,]$T0Rain
  }}

rT0Grass <- rasterFromXYZ(df[, c("x", "y", "t0Grass")])
raster::plot(rT0Grass)


rstack<-raster::stack(rCommID,rAnimal,
                      rDepriv,rPop,
                      rPlotID,rT0Rain,
                      rGrazed,rCons,
                      rT0Grass) 

names(rstack)<-c("CommID","Animal",
                 "Depriv","Pop",
                 "PlotID","T0Rain",
                 "Grazed","Cons",
                 "T0Grass")

#Make the Ranchers dataframe 
Ranchers<-raster::as.data.frame(rstack,xy=T)%>%dplyr::group_by(CommID)%>%
  dplyr::summarise(Animal=sum(Animal),Pop=sum(Pop),Depriv=median(Depriv))



#Complicated function to distribute animals within communities
library(tidyverse)
assign_cows <- function(people, cows, min_cows = Min_cows) {
base_cows_total <- people * min_cows
  extra_cows <- cows - base_cows_total
  
  # Right-tailed weights for extra cows
    weights <- rexp(people, rate = Cow_Inequality)
  weights <- weights / sum(weights)
  extra_counts <- round(weights * extra_cows)
  
  # Adjust rounding error
  diff <- extra_cows - sum(extra_counts)
  if (diff != 0) {
    idx <- sample(1:people, abs(diff), replace = TRUE)
    extra_counts[idx] <- extra_counts[idx] + sign(diff)
  }
  
  tibble(
    person_id = 1:people,
    stock_count = extra_counts + min_cows
  )
}


Ranchers <- Ranchers %>%
  rowwise() %>%
  mutate(person_data = list(assign_cows(Pop, Animal, min_cows = Min_cows))) %>%
  unnest(person_data) %>%
  ungroup()%>%dplyr::select(-c(Animal,Pop))
#ggplot(Ranchers)+geom_histogram(aes(x=stock_count))+facet_wrap(~CommID)

#Now, use community depriv and cattle numbers to assign Rancher wellbeing
# We center the economic wellbeing of each community at it's deprivation value
#Then, we rank ranchers based on their number of cows and assign an individual
#deprivation score as a +/- deviation from that community mean
#The alpha value controls the spread

#Then we invert so that higher numbers are better. This number is now considered how much
#money they have

alpha<-Econ_Inequality

Ranchers <- Ranchers %>%
  group_by(CommID) %>%
  mutate(
    # Step 1: Rank people by stock_count (inverse relationship)
    rank = rank(-stock_count, ties.method = "average"),
    
    # Step 2: Scale ranks to 0–100
    wellbeing_raw = (rank - min(rank)) / (max(rank) - min(rank)) * 100,
    
    # Step 3: Apply spread factor (shrink toward mean)
    mean_raw = mean(wellbeing_raw),
    shrunk = (wellbeing_raw - mean_raw) * alpha + mean_raw,
    
    # Step 4: Shift to match Depriv
    economic_wellbeing = shrunk - mean(shrunk) + Depriv,
    
    # Step 5: Clamp to 0–100
    economic_wellbeing = pmin(pmax(economic_wellbeing, 0), 100)
  ) %>%
  ungroup()%>%
  mutate(economic_wellbeing=100-economic_wellbeing)%>% #Making higher "numbers better"
  dplyr::select(-c(rank,wellbeing_raw,mean_raw,shrunk,Depriv))

#Add this to the raster
#Make dataframe from raster
df<-raster::as.data.frame(rstack,xy=T)
df2<-Ranchers%>%group_by(CommID)%>%summarise(Mean_economic_wellbeing=mean(economic_wellbeing))
rWell<-merge(df,df2,by="CommID")
rWell <- rasterFromXYZ(rWell[, c("x", "y", "Mean_economic_wellbeing")])
rstack<-stack(rstack,rWell)

##Add whether each person's community is in conservation
ConsComs<-data.frame(CommID=1:length(ConsComs),Conservation=ConsComs)
Ranchers<-base::merge(Ranchers,ConsComs,by="CommID")

#Assign behavior at 0 time step. Random for now, ranked according to herd size
PastStockChange<-sort(rnorm(n=nrow(Ranchers),mean=0,sd=0.2))
Ranchers <- Ranchers%>%dplyr::arrange(stock_count, decreasing = TRUE)%>%
  mutate(PastStockChangeProp=PastStockChange,
         PastStockChangeAnimal=round(PastStockChangeProp*stock_count),
         PastStock=stock_count-PastStockChangeAnimal,
         PastStockChangeProp=PastStockChangeAnimal/PastStock)%>%
  arrange(CommID,person_id)



#ggplot(Ranchers)+geom_histogram(aes(x=economic_wellbeing))+facet_wrap(~CommID)

#See here that cows and weath are associated within community, but not across communities
#This again, approximates the actual data from southern africa
ggplot(Ranchers,aes(x=stock_count,y=economic_wellbeing))+geom_point()



