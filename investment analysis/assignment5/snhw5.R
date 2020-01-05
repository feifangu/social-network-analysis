# social network homework 5
# Author: Feifan Gu

# import packages
library(dplyr)
library(data.table)
library(lubridate)
library(igraph)
library(hhi)
library(plm)
library(pglm)
library(Matrix)
library(ggplot2)
library(plot3D)
library(rgl)
library(plotly)

# read datasets
company <- fread("company_details.csv")
deal <- fread("deal_details.csv")
investor <- fread("investor_details.csv")
investor_deal <- fread("investors_and_deals.csv")

################################################### Q1.A ##################################################

#----------------------------------------------------------------------------------------------------------
# preparation

# very first data filtering according to the requirements 
# filter Venture Capital type inverstor
investor_vc <- investor[Investor_Type=="Venture Capital"]
investor_deal_vc <- investor_deal[Investor_Id%in%investor_vc$InvestorId]
# filter deals that is after 1990
investor_deal_vc <- left_join(investor_deal_vc,deal[,c(1,2,4)],by=c("Deal_Id"="DealId"))
investor_deal_vc$Deal_Date <- as.Date(investor_deal_vc$Deal_Date,"%d %b %y")
after <- as.Date("1990-01-01")
investor_deal_vc <- filter(investor_deal_vc,Deal_Date>=after)
# filter deals that is co-invested
count_deal <- as.data.table(table(investor_deal_vc$Deal_Id))
names(count_deal) <- c("Deal_Id","count")
count_deal <- count_deal[count>=2]
investor_deal_vc <- as.data.table(investor_deal_vc)
investor_deal_vc <- investor_deal_vc[Deal_Id%in%count_deal$Deal_Id]
investor_deal_vc <- investor_deal_vc[,c(1,2,4,8,9)]

#----------------------------------------------------------------------------------------------------------
# compute status

# create edgelist between investors according their leadership relationship
# the ties would decay after 5 years
investor_deal_vc$Deal_Date <- as.Date(investor_deal_vc$Deal_Date,"%d %b %y")
investor_deal_vc[,Deal_Year:=year(Deal_Date)] # add a column 'Deal_Year' to record the year of the deal
investor_deal_vc[,status:=0] # add a column 'status' to record the status of an investor in a year
for (y in 1990:2018){
  fiveyear <- filter(investor_deal_vc,Deal_Year>=y-years(5)&Deal_Year<=y) # filter interactions within past 5 years
  edgelist <- left_join(fiveyear[,c(1,2,3,7)],fiveyear[,c(1,2)],by="Deal_Id") # create edgelist
  edgelist <- filter(edgelist,Investor_Id.x<Investor_Id.y)
  edgelist <- as.data.table(edgelist)
  edgelist[,weight:=mean(Lead_Investor),by=c("Investor_Id.x","Investor_Id.y")] # add weight according to percentage of being a lead investor
  edgelist_uni <- unique(edgelist,by=c("Investor_Id.x","Investor_Id.y"))[,c(1,5,6)]
  g <- graph_from_edgelist(as.matrix(edgelist_uni[,c(1,2)]))
  E(g)$weight <- edgelist_uni$weight
  investor_deal_vc[Deal_Year==y,status:=eigen_centrality(g)$vector[Investor_Id]] # calculate eigen_centrality as status
  #print(y)
}

#----------------------------------------------------------------------------------------------------------
# compute concentration (diversification)
# join in Primary Industry Code and Deal Size for concentration computing
investor_deal_vc_con <- left_join(investor_deal_vc,company[,c("CompanyID","Primary_Industry_Code")],by=c("CompanyId"="CompanyID"))
investor_deal_vc_con <- left_join(investor_deal_vc_con,deal[,c("DealId","Deal_Size")],by=c("Deal_Id"="DealId"))
investor_deal_vc_con <- as.data.table(investor_deal_vc_con)

# function calculating deal size percentage (market share in the formula) for each primary industry code
mktshare2 <- function(dt,pic){
  a<-100*sum(dt[Primary_Industry_Code==pic]$Deal_Size,na.rm = TRUE)/sum(dt$Deal_Size,na.rm = TRUE)
  return(a)
}

# function modified from the built-in 'hhi' function to avoid unnecessary judgement of whether sum of proportions is 100
# since there could be computer precision problems resulting in computer treating 100 as not 100
# So I rewrote the function to avoid that kind of error or warning
hhiii <- function (x, s) 
{
  if (!is.data.frame(x)) {
    stop("\"x\" must be data frame\n", "You have provided an object of class: ", 
         class(x)[1])
  }
  shares <- try(sum(x[, s]))
  d <- x[, s]
  if (!is.numeric(d)) {
    stop("\"s\" must be numeric vector\n", "You have provided an object of class: ", 
         class(d)[1])
  }
  hhi <- sum(d^2,na.rm = TRUE)
  return(hhi)
}

# function calculate the cumulative diversification (sum of market share of involved categories)
hhii <- function(iid,y){
  cumyear <- filter(investor_deal_vc_con,Investor_Id==iid&Deal_Year<=y)
  cumyear <- as.data.table(cumyear)
  cumyear[,marketshare2:=mktshare2(cumyear,Primary_Industry_Code),by="Primary_Industry_Code"]
  h <-hhiii(as.data.frame(unique(cumyear[,c("Primary_Industry_Code","marketshare2")])),"marketshare2")
  return(h)
}
# 'hhi2': record the Herfindahl index of each investor in each deal year representing diversification
investor_deal_vc_con[,hhi2:=hhii(Investor_Id,Deal_Year),by=c("Investor_Id","Deal_Year")]

#----------------------------------------------------------------------------------------------------------
# compute whether tends to invest in first deal

# join in the sort of deal to help judge whether the deal is the first one of that company
regression1 <- left_join(investor_deal_vc_con,deal[,c(1,3)],by=c("Deal_Id"="DealId"))
regression1 <- as.data.table(regression1)
# function to return 1 if more than half of the investor's cumulatively investment are first deal, else return 0 
whetherfirst <- function(iid,y){
  w <- mean(filter(regression1,Investor_Id==iid&Deal_Year<=y)$Deal_Number==1)>0.5
  return(w)
}

# 'whetherfirst': record whether more than half of the investor's cumulatively investment are first deal
regression1[,whetherfirst:=whetherfirst(Investor_Id,Deal_Year),by=c("Investor_Id","Deal_Year")]

#----------------------------------------------------------------------------------------------------------
# compute whether tends to invest in the IT industry

# join in Primary Industry Sector to help judge whether tend to invest in the IT industry
regression2 <- left_join(regression1,company[,c("CompanyID","Primary_Industry_Sector")],by=c("CompanyId"="CompanyID"))
regression2 <- as.data.table(regression2)
# function to return 1 if more than half of the investor's cumulatively investment are in IT, else return 0 
whetherIT <- function(iid,y){
  w <- mean(filter(regression2,Investor_Id==iid&Deal_Year<=y)$Primary_Industry_Sector=="Information Technology")>0.5
  return(w)
}

# 'whetherIT': record whether more than half of the investor's cumulatively investment are in IT
regression2[,whetherIT:=whetherIT(Investor_Id,Deal_Year),by=c("Investor_Id","Deal_Year")]

#----------------------------------------------------------------------------------------------------------
# whether tends to invest in early-stage companies

# join in Deal Type to help judge whether tend to invest in early-stage companies
regression3 <- left_join(regression2,deal[,c("DealId","Deal_Type_1")],by=c("Deal_Id"="DealId"))
regression3 <- as.data.table(regression3)
# function to return 1 if more than half of the investor's cumulatively investment are to early-stage companies, else return 0 
whetherearly <- function(iid,y){
  e <- filter(regression3,Investor_Id==iid&Deal_Year<=y)$Deal_Type
  w <- mean(e=="Early Stage VC"|e=="Accelerator/Incubator"|e=="Seed Round")>0.5
  return(w)
}

# 'whetherearly': record whether more than half of the investor's cumulatively investment are to early-stage companies
regression3[,whetherearly:=whetherearly(Investor_Id,Deal_Year),by=c("Investor_Id","Deal_Year")]

#----------------------------------------------------------------------------------------------------------
# regression

# add control: age of invest firm (cuurent year - first year of investment)
regression3[,age:=Deal_Year-min(Deal_Year),by="Investor_Id"]

# order by date to correctly do lagging stuff
regression3 <- regression3[order(Investor_Id,Deal_Date)]
regression3[,lagstatus:=shift(status),by="Investor_Id"] # lag status
regression3$lagstatus <- as.numeric(regression3$lagstatus)
# lag three control variables
regression3[,lagwhetherfirst:=shift(whetherfirst),by="Investor_Id"]
regression3[,lagwhetherearly:=shift(whetherearly),by="Investor_Id"]
regression3[,lagwhetherIT:=shift(whetherIT),by="Investor_Id"]

# remove the duplicate records because we only only want one record for one investor in one year
regression_uni1 <- unique(regression3[,c("hhi2","Investor_Id","lagstatus","lagwhetherfirst","lagwhetherearly","lagwhetherIT","Deal_Year","age")],
                          by=c("Investor_Id","Deal_Year"),fromLast = TRUE) # remove the duplicate records for one investor in one year

# turn these boolen type variables to numeric type
regression_uni1$lagwhetherfirst <- as.numeric(regression_uni1$lagwhetherfirst)
regression_uni1$lagwhetherearly <- as.numeric(regression_uni1$lagwhetherearly)
regression_uni1$lagwhetherIT <- as.numeric(regression_uni1$lagwhetherIT)

# regression including all the required control variables using plm
reg1 <- plm(hhi2~lagstatus+I(lagstatus^2)+lagwhetherfirst+lagwhetherearly+lagwhetherIT+age+as.Date(as.character(Deal_Year),"%Y"),
            regression_uni1,model = "within",effect = "individual",index = "Investor_Id")
summary(reg1)

################################################### Q1.B ##################################################

# define jaccard similarity function
jaccard_sim <- function(m) {
  A <- tcrossprod(m) # cross-product of the transpose of a matrix
  im <- which(A > 0, arr.ind=TRUE, useNames = F) # overlap
  b <- rowSums(m)
  Aim <- A[im]
  sparseMatrix(
    i = im[,1],
    j = im[,2],
    x = Aim / (b[im[,1]] + b[im[,2]] - Aim), # indicate the position and values of all non-0 entries in the matrix
    dims = dim(A)
  ) # return the jaccard similarity matrix
}

# function to return cumulatively invested industry of an investor before a certain deal year
pic <- function(iid,y){
  return(unique(regression3[Deal_Year<=y&Investor_Id==iid,Primary_Industry_Code]))
}

# filter out those industry that is null string, which could result in bug in the recogizing rownames of jaccard matrix part
regression3 <- regression3[Primary_Industry_Code!=""]

# run a for loop to compute sum of jaccard distance and fill in the main datatable
for(y in(1990:2018)){
  edge_list <- regression3[Deal_Year<=y,c("Primary_Industry_Code","Investor_Id")] 
  # use the edgelist to create an affiliation matrix which is a sparseMatrix
  A <- sparseMatrix(as.integer(as.factor(edge_list$Primary_Industry_Code)), # this function only recognize integers, so do transformation
                    as.integer(as.factor(edge_list$Investor_Id)),
                    x=1,
                    dimnames=list(levels(as.factor(edge_list$Primary_Industry_Code)),
                                  levels(as.factor(edge_list$Investor_Id)) 
                    ) # generate an affiliation matrix between industry and investor
  )
  A <- A >= 1 + 0
  jac <- 1-jaccard_sim(A) # calculate the jaccard distance matrix from A
  rownames(jac)<- rownames(A)
  colnames(jac)<- rownames(A)
  regression3[Deal_Year==y,sumjac:=0.5*sum(jac[pic(Investor_Id,y),pic(Investor_Id,y)]),by="Investor_Id"] # fill in the sum of jaccard distance
  edge_list <- data.frame(matrix(NA,0,2))
  cat("\r",round((y-1990)*100/(2018-1990),2),"done") # check what "percentage" the for loop is running to
}

# function to return the number of industry that the investor has invested in until a deal year
totalnumber <- function(iid,y){
  return(length(unique(regression3[Investor_Id==iid&Deal_Year<=y,Primary_Industry_Code])))
}

# use niche width's formula to calculate it 
regression3[,nichewidth:=1-1/(1+sumjac/(totalnumber(Investor_Id,Deal_Year)-1)),by=c("Deal_Year","Investor_Id")]
regression3[is.na(nichewidth),nichewidth:=0] # the na of niche width should be 0, because only investing in one industry should relate to the smallest width

# check whether niche width is between 0 and 1
min(regression3$nichewidth) # min is 0
max(regression3$nichewidth) # max is about 0.98

# add Mundlak control
regression3[,mundlak:=mean((lagstatus+lagwhetherfirst+lagwhetherearly+lagwhetherIT+age)/5,na.rm=TRUE),by="Investor_Id"]

# regression
regression_uni2 <- unique(regression3[,c("Investor_Id","nichewidth","lagstatus","lagwhetherfirst","lagwhetherearly","lagwhetherIT","Deal_Year","age","mundlak")],
                          by=c("Investor_Id","Deal_Year"),fromLast = TRUE) # remove the duplicate records for one investor in one year

reg2 <- glm(nichewidth~lagstatus+I(lagstatus^2)+lagwhetherfirst+lagwhetherearly+lagwhetherIT+as.Date(as.character(Deal_Year),"%Y")+
              age+mundlak,regression_uni2,family = quasibinomial(link = "logit"))
summary(reg2)

################################################### Q1.C ##################################################

# regression only with lagstatus and lagstatus^2
reg3 <- glm(nichewidth~lagstatus+I(lagstatus^2),regression_uni2,family = quasibinomial(link = "logit"))
summary(reg3)

# check min and max of lagstatus
min(lag(regression_uni2$lagstatus),na.rm = TRUE) # min is 0
max(lag(regression_uni2$lagstatus),na.rm = TRUE) # max is 1

obj <- as.data.frame(seq(0,1,length.out = 100)) # generate 100 points between min and max
colnames(obj) <- "lagstatus"
pred <- predict.glm(reg3,obj, type="response",se.fit = TRUE) # predict on the 100 points with standard error recorded
obj$pred <- pred$fit
obj$se <- pred$se.fit
obj <- as.data.table(obj)
obj <- obj[,.(lagstatus,pred,min = pred - 1.96 * se, max = pred + 1.96 * se)] # calculate 95% confidence interval

# plot predicted nichewidth ~ lagstatus
g1C <- ggplot(obj,aes(x = lagstatus, y = pred)) + geom_point() +geom_line()+ geom_ribbon(aes(ymin = min, ymax = max),alpha = 0.3)+
  labs(x ="lagstatus", y = "predicted nichewidth (diversification)")
g1C

################################################### Q2.A ##################################################

# check whether an investment is successful 
regression3[,whethersuccess:=Deal_Type_1%in%c("IPO","Merger/Acquisition","Buyout/LBO"),by=c("Investor_Id","Deal_Date")]
# 'numofsuccess' cumulative sum of count of successful investment
regression3[,numofsuccess:=cumsum(whethersuccess),by="Investor_Id"]
regression3[,lagnichewidth:=shift(nichewidth),by="Investor_Id"] # lag nichewidth to use in regression

regression_uni3 <- unique(regression3[,c("Investor_Id","numofsuccess","lagnichewidth","lagstatus","lagwhetherfirst","lagwhetherearly","lagwhetherIT","Deal_Year","age","mundlak")],
                          by=c("Investor_Id","Deal_Year"),fromLast = TRUE) # remove the duplicate records for one investor in one year

# to include the same control as 1A and 1B, use pglm here
reg4 <- pglm(numofsuccess~lagstatus+lagnichewidth+lagstatus:lagnichewidth+lagwhetherfirst+lagwhetherearly+lagwhetherIT
             +age+as.Date(as.character(Deal_Year),"%Y")+mundlak,regression_uni3,effect = "individual",index = "Investor_Id",family = "poisson")
summary(reg4)
################################################### Q2.B ##################################################

# regression using glm and only include lagstatus and lagnichewidth here
reg5 <- glm(numofsuccess~lagstatus+lagnichewidth+lagstatus:lagnichewidth,regression_uni3,family ="poisson" )
summary(reg5)

# check the range of the x variables
max(regression_uni3$lagstatus,na.rm = TRUE) # 1
min(regression_uni3$lagstatus,na.rm = TRUE) # 0
max(regression_uni3$lagnichewidth,na.rm = TRUE) # 0.98
min(regression_uni3$lagnichewidth,na.rm = TRUE) # 0

# predict on 10000 points in the x-plane
lagstatus <- seq(0, 1, length.out = 100)
lagnichewidth <- seq(min(regression_uni3$lagnichewidth,na.rm = TRUE),1,length.out = 100)
values <- as.data.frame(expand.grid(lagstatus,lagnichewidth)) # 10000 evenly distributed points
names(values) <- c("lagstatus","lagnichewidth")
values$fit <- predict.glm(reg5,values, type="response")
names(values) <- c("status","diversification","successful_investments")

# regular 3d plot
scatter3D(values$diversification, values$status, values$successful_investments)

# interactive 3d plot
plot3d(values$diversification, values$status, values$successful_investments)

# the contour plot
p1 = plot_ly(
  values,
  x = ~status,
  y = ~diversification,
  z = ~successful_investments,
  type = "contour",
  autocontour = FALSE,
  contours = list(
    end = max(values$successful_investments, na.rm = TRUE),
    size = abs(max(values$successful_investments, na.rm = TRUE) - min(values$successful_investments, na.rm = TRUE))/20,
    start = min(values$successful_investments, na.rm = TRUE),
    showlines = FALSE),
  line = list(smoothing = 0.85),
  colorscale = "Greys"
  ) %>%
  layout(font = "cmodern") %>%
  colorbar(len = 1, nticks = 10, title = "Estimated successful \n investments") %>%
  layout(yaxis = list(title = "Niche width")) %>%
  layout(xaxis = list(title = "Status"))
p1

################################################### Q3.A ##################################################

# calculate jaccard distance using similar method as in Q1, and do mds
for(y in(1990:2018)){
  edge_list2 <- regression3[Deal_Year<=y,c("Investor_Id","Primary_Industry_Sector")]
  A <- sparseMatrix(as.integer(as.factor(edge_list2$Investor_Id)), 
                    as.integer(as.factor(edge_list2$Primary_Industry_Sector)),
                    x=1,
                    dimnames=list(levels(as.factor(edge_list2$Investor_Id)), 
                                  levels(as.factor(edge_list2$Primary_Industry_Sector))
                    ) # create an affiliation matrix between investor and industry cumulatively
  )
  A <- A >= 1 + 0 
  jac2 <- 1-jaccard_sim(A) # calculate the jaccard distance
  mds <- cmdscale(jac2) # calculate the two coordinates using mds
  for(k in 1:length(rownames(A))){ # fill in according to industry names
    regression3[Deal_Year==y&Investor_Id==rownames(A)[k],"coordinate1"]<-mds[k,1]
    regression3[Deal_Year==y&Investor_Id==rownames(A)[k],"coordinate2"]<-mds[k,2]
  }
  edge_list2 <- data.frame(matrix(NA,0,2))
  print(y) # check which year the for loop is running to
}

# calculate modoid coordinate 1 for each industry in each year
medoid1 <- function(pis,y){
  inv <- regression3[Deal_Year==y&Primary_Industry_Sector==pis,Investor_Id] # pick all the syndicate partners of an investor in a deal
  for (i in inv){
    if(!sum(regression3[Investor_Id==i&Deal_Year==y,Primary_Industry_Sector]!=pis)){ # if all that syndicate partner's investment are only in that industry
      return(regression3[Investor_Id==i&Deal_Year==y,coordinate1][1]) # return the coordinate1 of that syndicate partner
    }
  } # if not return anything, which means no syndicate partner's investment are only in that industry, sum the deal_size in that industry 
  most <- regression3[Investor_Id%in%inv&Deal_Year==y&Primary_Industry_Sector==pis,sum:=sum(Deal_Size),by="Investor_Id"]
  if (sum(!is.na(most$sum))>0){ # if at least one of the syndicate partner's deal size is not NA
    return(most[sum=max(most$Deal_Size),Coordinate1][1]) # return the coordinate1 of the one with the most investment in that industry
  }
  else { # if all deal size is NA, I cannot compare
    return (NA) 
  }
}

# calculate modoid coordinate 2 for each industry in each year, function is basically the same as medoid1 other than change coordinate1 to coordinate2
medoid2 <- function(pis,y){
  inv <- regression3[Deal_Year==y&Primary_Industry_Sector==pis,Investor_Id]
  for (i in inv){
    if(!sum(regression3[Investor_Id==i&Deal_Year==y,Primary_Industry_Sector]!=pis)){
      return(regression3[Investor_Id==i&Deal_Year==y,coordinate2][1])
    }
  }
  most <- regression3[Investor_Id%in%inv&Deal_Year==y&Primary_Industry_Sector==pis,sum:=sum(Deal_Size),by="Investor_Id"]
  if (sum(!is.na(most$sum))>0){
    return(most[sum=max(most$Deal_Size),Coordinate2][1])
  }
  else {
    return (NA)
  }
}

# 'medoid1'and 'modoid2' record the medoid coordinates of the industry, no NA exists luckily after checking
regression3[,medoid1:=medoid1(Primary_Industry_Sector,Deal_Year),by=c("Primary_Industry_Sector","Deal_Year")]
regression3[,medoid2:=medoid2(Primary_Industry_Sector,Deal_Year),by=c("Primary_Industry_Sector","Deal_Year")]

# function return Euclidean distance between (c1,c2) and (m1,m2)
ucldist <- function(c1,c2,m1,m2){
  return(((c1-m1)^2+(c2-m2)^2)^0.5)
}

# 'dist': Euclidean distance between that deal's coordinates and the medoid
regression3[,dist:=ucldist(coordinate1,coordinate2,medoid1,medoid2),by=c("Investor_Id","Deal_Id")]

# function to generate the average distance of one investor's syndicate partners' coordinates to the medoid in one year
synavedist <- function(iid,y){
  syndeal <- regression3[Investor_Id==iid&Deal_Year==y,Deal_Id]
  return(mean(regression3[Deal_Id%in%syndeal&Investor_Id!=iid,dist]))
}

# 'synavedist': average distance of one investor's syndicate partners' in one year
regression3[,synavedist:=synavedist(Investor_Id,Deal_Year),by=c("Investor_Id","Deal_Year")]
# 'ownavedist': one's own average distance
regression3[,ownavedist:=mean(dist),by=c("Deal_Year","Investor_Id")]


# regression only using lead-investor's data
regression_uni4 <- unique(regression3[Lead_Investor==1,c("Investor_Id","Deal_Year","synavedist","ownavedist","lagstatus","lagwhetherfirst","lagwhetherearly","lagwhetherIT","Deal_Year","age","mundlak")],
                          by=c("Investor_Id","Deal_Year"),fromLast = TRUE)

# pglm: including the controls in 1A and 1B, regression between lagstatus, own average distance and syndicate average distance
reg6 <- pglm(synavedist~lagstatus+ownavedist+lagstatus:ownavedist+lagwhetherfirst+lagwhetherearly+lagwhetherIT
            +age,data=regression_uni4,effect = "individual",model="within",family = gaussian,index = "Investor_Id")
summary(reg6)

################################################### Q3.B ##################################################

# regression using glm and omit all the control varibles
reg7 <- glm(synavedist~lagstatus+ownavedist+lagstatus:ownavedist,regression_uni4,family = gaussian)
summary(reg7)

# check the range of x variables
max(regression_uni4$lagstatus,na.rm = TRUE) # 1
min(regression_uni4$lagstatus,na.rm = TRUE) # 0
max(regression_uni4$ownavedist) # 0.79
min(regression_uni4$ownavedist) # 0

# basically repeat the process of predicting and plotting
lagstatus <- seq(0, 1, length.out = 100)
ownavedist <- seq(0,max(regression_uni4$ownavedist),length.out = 100)
values2 <- as.data.frame(expand.grid(lagstatus,ownavedist))
names(values2) <- c("lagstatus","ownavedist")
values2$fit <- predict.glm(reg7,values2, type="response")
names(values2) <- c("status","owndistance","syndicatedistance")

# regular 3d plot
scatter3D(values2$syndicatedistance, values2$status, values2$owndistance)

# interactive 3d plot
plot3d(values2$syndicatedistance, values2$status, values2$owndistance)

# the contour plot
p2 = plot_ly(
  values2,
  x = ~status,
  y = ~owndistance,
  z = ~syndicatedistance,
  type = "contour",
  autocontour = FALSE,
  contours = list(
    end = max(values2$syndicatedistance, na.rm = TRUE),
    size = abs(max(values2$syndicatedistance, na.rm = TRUE) - min(values2$syndicatedistance, na.rm = TRUE))/20,
    start = min(values2$syndicatedistance, na.rm = TRUE),
    showlines = FALSE),
  line = list(smoothing = 0.85),
  colorscale = "Greys"
) %>%
  layout(font = "cmodern") %>%
  colorbar(len = 1, nticks = 10, title = "syndicatedistance") %>%
  layout(yaxis = list(title = "Owndistance")) %>%
  layout(xaxis = list(title = "Status"))
p2
