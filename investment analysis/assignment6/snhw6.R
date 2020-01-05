# social network homework 6
# author: Feifan Gu

# import packages
library(dplyr)
library(data.table)
library(lubridate)
library(igraph)
library(reshape2)
library(RSiena)
library(Matrix)
library(stringr)
library(geosphere)
library(nnet)

# read datasets
startupexec <- fread("execs.csv")
indinvestor <- fread("individual_investors.csv")
deal <- fread("deal_details.csv")
investor <- fread("investor_details.csv")
company <- fread("company_details.csv")
people <- fread("people.csv")
name <- fread("representative_names.csv")

############################################ The only big question######################################
# limit deals to Venture Capital and from the 2000s onward
deal <- deal[Deal_Class=="Venture Capital"]
deal$Deal_Date <- as.Date(deal$Deal_Date,"%d %b %y")
after <- as.Date("2000-01-01")
deal <- deal[Deal_Date>=after]
deal[,Deal_Year:=year(Deal_Date)] 

# excude individuals that appear as both investors and entrepreneurs
dualid <- intersect(startupexec$PersonId,indinvestor$PersonId)
startupexec <- startupexec[!PersonId%in%dualid]
indinvestor <- indinvestor[!PersonId%in%dualid]

# subset individuals to only three kinds
peopleind <- people[`Primary Position`%in%c("Founder","Chief Executive Officer","Managing Director")]

# create a table with ego: investor, alter: entrepreneur, wave: year, and industry group
dat <- company[,c("CompanyID","Primary_Industry_Group")]
dat <- left_join(dat,deal[,c("DealId","CompanyId","Deal_Year")],by=c("CompanyID"="CompanyId"))
dat <- left_join(dat,startupexec[,c("PersonId","CompanyId")],by=c("CompanyID"="CompanyId"))
dat <- left_join(dat,indinvestor[,c("DealId","PersonId")],by=c("DealId"="DealId"))
dat <- as.data.table(dat)
names(dat) <- c("Investor","Entrepreneur","Year","Industry_group")
dat <- left_join(dat,people[,c("PersonId","Primary Position")],by=c("Entrepreneur"="PersonId"))
names(dat) <- c("Investor","Entrepreneur","Year","Industry_group","p")
dat <- as.data.table(dat)
dat <- dat[!is.na(p),c(1:4)]
sort(table(dat$Industry_group),decreasing = TRUE) # check the number of records of each industry group

########################################### Rsiena part  #####################################
# first trial: Commercial Services
# second trial: Healthcare Devices and Supplies
# third trial: Commercial Products
# fourth trial: Pharmaceuticals and Biotechnology
# fifth trial: Retail
SW <- dat[Industry_group=="Pharmaceuticals and Biotechnology"]
SWlist <- list() # a list of adjacency matrix which will be put into Rsiena
for(y in 2010:2018){
  trick <- dcast(SW[,c(1,2,3)], formula = Investor~Entrepreneur,length,value.var = "Year")
  rownames(trick) <- trick[,1]
  trick <- trick[,-1]
  trick[trick!=0]=0
  yeardeal <- SW[Year<=y]
  yeardeal <- yeardeal[Year>y-5,c(1,2)]
  for (n in 1:nrow(yeardeal)){
    trick[yeardeal$Investor[n],yeardeal$Entrepreneur[n]] <- 1
  }
  trick <- as.matrix(trick)
  SWlist[[as.character(y)]] <- trick # each of the list records the relationship between investor and entrepreneur in each year
  print(y)
}

dim(SWlist[[1]]) # check the dimension of each matrix

# create dependent 'variable' here, which is a list of matrixs representing a network with wave
SWsiena = sienaDependent( array( unlist(SWlist),
                                 dim = c(80, 76, 9)), type="bipartite",nodeSet=c("senders", "receivers"),allowOnly=FALSE)
# create two list of nodes: senders and receivers
senders = sienaNodeSet(80, nodeSetName = c('senders'))
receivers = sienaNodeSet(76, nodeSetName = c('receivers'))


#--------------------------------------------------------------------------------------
# 1.dyadic effect: ethinic. here I am not using outer() but a more intuitive method, I will use outer() for the rest part

trick[trick!=0]=0 # still use the matrix with same rownames and colnames as in the list
# check whether investor and entrepreneur share common race which is not white
SW <- left_join(SW,people[,c(1,3)],by=c("Investor"="PersonId"))
SW <- left_join(SW,people[,c(1,3)],by=c("Entrepreneur"="PersonId"))
SW$`Last Name.x`<-str_to_upper(SW$`Last Name.x`)
SW$`Last Name.y`<-str_to_upper(SW$`Last Name.y`)
SW <- left_join(SW,name[,c(1,17)],by=c("Last Name.x"="Name"))
SW <- left_join(SW,name[,c(1,17)],by=c("Last Name.y"="Name"))
SW <- as.data.table(SW)
SW[race.x!="white",perfect:=race.x==race.y]
SW$perfect[is.na(SW$perfect)]<-0
for (i in rownames(trick)){
  for (j in colnames(trick)){
    if(nrow(SW[Investor==i&Entrepreneur==j])!=0){
      if (SW[Investor==i&Entrepreneur==j]$perfect[1]){
        trick[i,j] <- 1
      }
    }
  }
}

ethniceff <- coDyadCovar(trick,nodeSet=c("senders", "receivers"),type="bipartite")

#-----------------------------------------------------------------------------------------
# 2.gender
# check if they share the same gender
ego <- as.data.table(rownames(trick))
alter <- as.data.table(colnames(trick))
ego <- left_join(ego,people[,c("PersonId","Gender")],by=c("V1"="PersonId"))
alter <- left_join(alter,people[,c("PersonId","Gender")],by=c("V1"="PersonId"))
ego$Gender <- (ego$Gender=='Male')+0
alter$Gender <- (alter$Gender=='Male')+0
gendereff <- outer(ego$Gender,alter$Gender)
rownames(gendereff) <- rownames(trick)
colnames(gendereff) <- colnames(trick)

gendereff <- coDyadCovar(gendereff,nodeSet=c("senders", "receivers"),type="bipartite")

#-----------------------------------------------------------------------------------------
# 3.top school
# check if they both went to top school
egosch <- as.data.table(rownames(trick))
altersch <- as.data.table(colnames(trick))
egosch <- left_join(egosch,people[,c("PersonId","Education")],by=c("V1"="PersonId"))
altersch <- left_join(altersch,people[,c("PersonId","Education")],by=c("V1"="PersonId"))

# choose top schools according to Ivy League and USnews best global uni. ranking in 2019
whether_topschool <- function(edu){
  return(grepl("Harvard",edu)|grepl("Massachusetts Institute of Technology",edu)
  |grepl("MIT",edu)|grepl("Princeton",edu)|grepl("Oxford",edu)|grepl("Columbia University",edu)
  |grepl("Cambridge",edu)|grepl("Caltech",edu)|grepl("California Institute of Technology",edu)
  |grepl("Yale",edu)|grepl("University of Chicago",edu)|grepl("Stanford",edu))
}

egosch$Education <- whether_topschool(egosch$Education)+0
altersch$Education <- whether_topschool(altersch$Education)+0

schooleff <- outer(egosch$Education,altersch$Education)
rownames(schooleff) <- rownames(trick)
colnames(schooleff) <- colnames(trick)

schooleff <- coDyadCovar(schooleff,nodeSet=c("senders", "receivers"),type="bipartite")

#-----------------------------------------------------------------------------------------
# 4.geo

load("edges_dist.RData")
edges_dist <- edges_dist[!duplicated(edges_dist$CompanyId)]

# generate a distance table between the location of them using lat and lon from the edges_dist table
egogeo <- as.data.table(rownames(trick))
altergeo <- as.data.table(colnames(trick))

egogeo <- left_join(egogeo,indinvestor[,c("PersonId","CompanyId")],by=c("V1"="PersonId"))
egogeo <- as.data.table(egogeo)
egogeo <- egogeo[!duplicated(egogeo$V1)]
egogeo <- left_join(egogeo,edges_dist[,c("CompanyId","comp_lat","comp_lon")])
altergeo <- left_join(altergeo,startupexec[,c("PersonId","CompanyId")],by=c("V1"="PersonId"))
altergeo <- as.data.table(altergeo)
altergeo <- altergeo[!duplicated(altergeo$V1)]
altergeo <- left_join(altergeo,edges_dist[,c("CompanyId","comp_lat","comp_lon")])

geoeff <- distm(as.matrix(egogeo[,c(4,3)]),as.matrix(altergeo[,c(4,3)]))

rownames(geoeff) <- rownames(trick)
colnames(geoeff) <- colnames(trick)

geoeff <- coDyadCovar(geoeff,nodeSet=c("senders", "receivers"),type="bipartite")

#-----------------------------------------------------------------------------------------
# 5.experience
# compare their experience, which is the difference of their start year
egoexp <- as.data.table(rownames(trick))
alterexp <- as.data.table(colnames(trick))

egoexp[,first:=min(dat[Investor==V1,Year]),by="V1"]
alterexp[,first:=min(dat[Entrepreneur==V1,Year]),by="V1"]

experienceeff <- outer(egoexp$first,alterexp$first,FUN = "-")

rownames(experienceeff) <- rownames(trick)
colnames(experienceeff) <- colnames(trick)

experienceeff <- coDyadCovar(experienceeff,nodeSet=c("senders", "receivers"),type="bipartite")

#-----------------------------------------------------------------------------------------
# 6.complementary skills
# check their complementary skills, 1 if investor is MBA and entrepreneur has engineering or Ph.d background
egoskills <- as.data.table(rownames(trick))
alterskills <- as.data.table(colnames(trick))

egoskills <- left_join(egoskills,people[,c("PersonId","Education")],by=c("V1"="PersonId"))
egoskills$Education <- grepl('MBA',egoskills$Education)+0
alterskills <- left_join(alterskills,people[,c("PersonId","Education")],by=c("V1"="PersonId"))
alterskills$Education <- grepl('Engineering',alterskills$Education)|grepl('Ph.d',alterskills$Education)

skillseff <- outer(egoskills$Education,alterskills$Education)

rownames(skillseff) <- rownames(trick)
colnames(skillseff) <- colnames(trick)

skillseff <- coDyadCovar(skillseff,nodeSet=c("senders", "receivers"),type="bipartite")

#-----------------------------------------------------------------------------------------
# Individual predictors for entrepreneurs: we generate vectors to describe different attributes of entrepreneurs 

# 7.race
# non-white: 1, white: 0
alterrace <- as.data.table(colnames(trick))
alterrace <- left_join(alterrace,SW[,c(2,8)],by=c("V1"="Entrepreneur"))
alterrace <- as.data.table(alterrace)
alterrace <- alterrace[!duplicated(alterrace$V1)]
alterrace$race.y <- (alterrace$race.y!='white')+0
alterrace$race.y[is.na(alterrace$race.y)] <- 0
entrerace <- coCovar(alterrace$race.y+0,nodeSet = "receivers")

# 8.gender
# male: 1, female: 0
entregender <- coCovar(alter$Gender,nodeSet = "receivers")

# 9.top school
# top school: 1, else: 0
entreschool <- coCovar(altersch$Education,nodeSet = "receivers")

# 10.geo
# located in top10 cities (according to frequency in the company table): 1, else: 0
topcity <- as.data.table(sort(table(company$City),decreasing = TRUE))
topcity <- topcity[-1,]
top10 <- topcity$V1[1:10]

altergeo <- left_join(altergeo,company[,c("CompanyID","City")],by=c("CompanyId"="CompanyID"))
altergeo <- as.data.table(altergeo)
altergeo[,is_top10:=as.numeric(City%in%top10),by="V1"]

entregeo <- coCovar(altergeo$is_top10,nodeSet = "receivers")

# 11.exp
# first year represent experience
entreexperience <- coCovar(alterexp$first,nodeSet = "receivers")

# 12.business skills
# MBA: 1, else: 0
alterskills <- as.data.table(colnames(trick))
alterskills <- left_join(alterskills,people[,c("PersonId","Education")],by=c("V1"="PersonId"))
alterskills$tech <- grepl('Engineering',alterskills$Education)|grepl('Ph.d',alterskills$Education)
alterskills$bus <- grepl('MBA',alterskills$Education)+0
entrebus <- coCovar(alterskills$bus,nodeSet = "receivers")

# 13.technical skills
# engineering or Ph.d: 1, else: 0
entretech <- coCovar(as.numeric(alterskills$tech),nodeSet = "receivers")

# 14.venture round
# count of getting investments
SW[,count:=nrow(.SD),by="Entrepreneur"]
round <- unique(SW[,c("Entrepreneur","count")])
alterround <- as.data.table(colnames(trick))
alterround <- left_join(alterround,round,by=c("V1"="Entrepreneur"))
entreround <- coCovar(alterround$count,nodeSet = "receivers")

#-----------------------------------------------------------------------------------------
# take account of when people join and leave the network
compchange <- sienaCompositionChange(rep(list(c(1,9)),80),nodeSet = "senders")
compchange2 <- sienaCompositionChange(rep(list(c(1,9)),76),nodeSet = "receivers")

# start rsiena
# create siena Data not using all effects, after trials, find that likely to return overall convergence rate being NA
CoEvolutionDataSW = sienaDataCreate(SWsiena,ethniceff,gendereff,experienceeff,schooleff,skillseff,entreschool,entretech,
                                    entregender,entrerace,entreround,entrebus,entreexperience,entregeo,
                                    compchange,compchange2,
                                    nodeSets = list(senders, receivers))

# include all other effects
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,cycle4,  outActSqrt, inPopSqrt, outInAss)
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,altX,
                                      interaction1="entregender")
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,altX,
                                      interaction1="entrerace")
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,altX,
                                      interaction1="entreschool")
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,altX,
                                      interaction1="entreround")
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,altX,
                                      interaction1="entrebus")
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,altX,
                                      interaction1="entretech")
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,altX,
                                      interaction1="entreexperience")
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,altX,
                                      interaction1="entregeo")
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,X,
                                      interaction1="ethniceff")
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,X,
                                      interaction1="gendereff")
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,X,
                                      interaction1="schooleff")
#CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,X,
#interaction1="geoeff") 
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,X,
                                      interaction1="experienceeff")
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,X,
                                      interaction1="skillseff")

#---------------------------------------------------------------------
# choose some robust effects to involve
CoEvolutionDataSW = sienaDataCreate(SWsiena,ethniceff,gendereff,schooleff,skillseff,
                                    entregender,entrerace,entreschool,entrebus,entretech,entregeo,
                                    compchange,compchange2,
                                    nodeSets = list(senders, receivers))
# get effects
CoEvolutionEffectsSW = getEffects(CoEvolutionDataSW)

# choose some of effects
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,cycle4,  outActSqrt, inPopSqrt, outInAss)
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,altX,
                                      interaction1="entregender")
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,altX,
                                      interaction1="entrerace")
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,altX,
                                      interaction1="entreschool")
#CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,altX,
                                      #interaction1="entreround")
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,altX,
                                      interaction1="entrebus")
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,altX,
                                      interaction1="entretech")
#CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,altX,
                                      #interaction1="entreexperience")
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,altX,
                                      interaction1="entregeo")
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,X,
                                    interaction1="ethniceff")
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,X,
                                      interaction1="gendereff")
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,X,
                                      interaction1="schooleff")
#CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,X,
                                      #interaction1="geoeff") 
#CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,X,
                                      #interaction1="experienceeff")
CoEvolutionEffectsSW = includeEffects(CoEvolutionEffectsSW,X,
                                      interaction1="skillseff")

#---------------------------------------------------------------------

# check the effects
CoEvolutionEffectsSW
# create algorithm
CoEvolutionAlgoSW = sienaAlgorithmCreate( projname = 'cp_results', diagonalize = 0.2)
# run Rsiena, please run!
CoEvolutionResults = siena07(CoEvolutionAlgoSW, data=CoEvolutionDataSW,
                             effects=CoEvolutionEffectsSW,nbrNodes = 4, useCluster = TRUE, initC = TRUE)
# check the results
CoEvolutionResults

# if not convergent enough, continue to run!
CoEvolutionResults = siena07(CoEvolutionAlgoSW, data=CoEvolutionDataSW,
                              effects=CoEvolutionEffectsSW,nbrNodes = 4, useCluster = TRUE, initC = TRUE,prevAns=CoEvolutionResults)
#check the results again
CoEvolutionResults

# not convergent, not stop, I can do this all day!
while(CoEvolutionResults$tconv.max>0.25){
  CoEvolutionResults = siena07(CoEvolutionAlgoSW, data=CoEvolutionDataSW,
                               effects=CoEvolutionEffectsSW,nbrNodes = 4, useCluster = TRUE, initC = TRUE,prevAns=CoEvolutionResults)
  CoEvolutionResults # check the results again
  }


######################################### Extra Credit ################################################
#--------------------------------------------------------------------------
# Q.A
# import data
outcome <- fread('individual_investor_outcomes.csv')

# regression
summary(glm(out_of_business ~ l4c_scaled + gender + ethnicity + age_diff + geo_dist + ivyplus + complementarity + male_exec + nonwhite_exec + ivyplus_exec + inv_long + inv_lat + year,
            outcome, family = "binomial"))
# it turns out gender, age and top school homophily can help investors avoid going out of business

#---------------------------------------------------------------------------
# Q.B
# regression outcomes
summary(glm(successful_investments ~ l4c_scaled + gender + ethnicity + age_diff + geo_dist + ivyplus + complementarity + male_exec + nonwhite_exec + ivyplus_exec + inv_long + inv_lat + year + 
              l4c_avg + gender_avg + ethnicity_avg + age_diff_avg + geo_dist_avg + ivyplus_avg + complementarity_avg + male_exec_avg + nonwhite_exec_avg + ivyplus_exec_avg,outcome,family=poisson(link = "log")))
# it turns out investment based on age homophily leads to more successful investments; but basiccally homophily is not helpful for successful investments 

#-------------------------------------------------------------------------
# Q.C
# import data
state <- fread('startup_states.csv')

# use a "multinomial logit" to predict the likelihood of a startup being in any particular state operation
# we want to set "Startup" as the reference so I found this code that does that
model <- multinom(company_state ~ l4c_scaled+gender+ethnicity+age_diff+geo_dist+ivyplus+complementarity+male_exec + nonwhite_exec + ivyplus_exec + comp_lon + comp_lat + year,data=state)
summary(model)

#calculate p-vals 
z = summary(model)$coefficients/summary(model)$standard.errors
z
#significance test
(1 - pnorm(abs(z), 0, 1)) * 2
