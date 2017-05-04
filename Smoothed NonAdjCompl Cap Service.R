# Smoothed Template University Productivity

# Tornqvist Index Research Apportionment
# Output: Adjusted Credit Hours for All Student FTEs by Coursework, Research Completions, Publications, Research Funding
# Input: All Appropriate Labor, Capital, & Intermediate


#Set working directory to where function inputs and outputs are saved
setwd("C:\\Users\\Ken\\Dropbox\\IEDP\\PhD\\Productivity\\Productivity Projects\\Aussie HE Research Briefing\\R Working Directory\\Directory 2006-2015")

library(dplyr)

# Import data relevant to Massy Model
Prod.Data <- read.csv("Merged R Inputs.CSV")

str(Prod.Data)

Prod.Data$Year <- factor(Prod.Data$Year)

# Create separate "Year" Vector for caluculations
Year <- unique(Prod.Data$Year)  

# Create a vector of the unique institutions in Prod.Data

All_Inst <- unique(Prod.Data$Institution)

# Create Depreciation Rate Column

Prod.Data <- Prod.Data %>%
  mutate(Dep.Rate = (Depreciation.and.Amortisation + Rep.and.Maint)/(K.Buildings + K.Library + K.Other.Prop.Plant.Equip))


# Specify Real Rates of Return
RRR <- rep(1/20, 10)


#---------------------- Write function for finding productivity of a single institution ----------------

Prod.Calc <- function(inst){        #Remember!!! The way this function is defined, it must take an entry
                                    #from a vector composed of only 'factor' variables 
  Prod.Data <- Prod.Data %>%
    filter(Institution == inst)     #Select only one institution
   
  


#--------------------Isolate Labor Expenses------------------------------

# Create Academic and Non-Academic Salaries Variables
LE.Academic <- Prod.Data$Academic.Benefits
LE.NonAc <- Prod.Data$Non.Academic.Benefits



# Smooth the inputs: three year averages

LE.Academic <- sapply(3:10, function(i){
  (LE.Academic[i-2]+LE.Academic[i-1]+LE.Academic[i])/3
})

LE.NonAc <- sapply(3:10, function(i){
  (LE.NonAc[i-2]+LE.NonAc[i-1]+LE.NonAc[i])/3
})

# Total Smoothd Labor Expenses
LE.Total <- LE.Academic + LE.NonAc


# Shares of Labor Expenses
LE.Ac.Share <- LE.Academic / LE.Total
LE.NonAc.Share <- LE.NonAc / LE.Total

# Create Labor Expenses Index
LE.Index <- sapply(2:8, function(i){
  (LE.Academic[i] / LE.Academic[i-1])^(0.5*(LE.Ac.Share[i] + LE.Ac.Share[i-1]))*
    (LE.NonAc[i] / LE.NonAc[i-1])^(0.5*(LE.NonAc.Share[i] + LE.NonAc.Share[i-1]))})



#--------------------Isolate Capital Expenses----------------------------


# Create Capital Services Variables
KE.Land <- Prod.Data$K.Land * RRR 
KE.Build <- Prod.Data$K.Buildings*(RRR + Prod.Data$Dep.Rate) 
KE.Eq.Lib <- (Prod.Data$K.Library + Prod.Data$K.Other.Prop.Plant.Equip)*(RRR + Prod.Data$Dep.Rate) 

KE.Eq.Lib
KE.Build
KE.Land


# Smooth the inputs: three year averages

KE.Land <- sapply(3:10, function(i){
  (KE.Land[i-2]+KE.Land[i-1]+KE.Land[i])/3
})

KE.Build <- sapply(3:10, function(i){
  (KE.Build[i-2]+KE.Build[i-1]+KE.Build[i])/3
})

KE.Eq.Lib <- sapply(3:10, function(i){
  (KE.Eq.Lib[i-2]+KE.Eq.Lib[i-1]+KE.Eq.Lib[i])/3
})


# Total Capital Expenses
KE.Total <- KE.Eq.Lib + KE.Build + KE.Land

# Shares of Capital Expenses
KE.Land.Share <- KE.Land / KE.Total
KE.Build.Share <- KE.Build / KE.Total
KE.Eq.Lib.Share <- KE.Eq.Lib / KE.Total

# Create Capital Expenses Indexes
KE.Index <- sapply(2:8, function(i){
  (KE.Land[i] / KE.Land[i-1])^(0.5*(KE.Land.Share[i]+KE.Land.Share[i-1]))*
    (KE.Build[i] / KE.Build[i-1])^(0.5*(KE.Build.Share[i]+KE.Build.Share[i-1]))*
    (KE.Eq.Lib[i] / KE.Eq.Lib[i-1])^(0.5*(KE.Eq.Lib.Share[i]+KE.Eq.Lib.Share[i-1]))})


#--------------------Isolate Intermediate Expenses------------------------------


# Create Scholarships and Admin Variables
IE.Scholar <- Prod.Data$Schol.Grants.Prizes 
IE.Admin <- Prod.Data$Other.Exp - Prod.Data$Schol.Grants.Prizes 

# Smooth the inputs: three year averages

IE.Scholar <- sapply(3:10, function(i){
  (IE.Scholar[i-2]+IE.Scholar[i-1]+IE.Scholar[i])/3
})

IE.Admin <- sapply(3:10, function(i){
  (IE.Admin[i-2]+IE.Admin[i-1]+IE.Admin[i])/3
})

# Total Labor Expenses
IE.Total <- IE.Scholar + IE.Admin

# Shares of Labor Expenses
IE.Scholar.Share <- IE.Scholar / IE.Total
IE.Admin.Share <- IE.Admin / IE.Total

# Create Labor Expenses Index
IE.Index <- sapply(2:8, function(i){
  (IE.Scholar[i] / IE.Scholar[i-1])^(0.5*(IE.Scholar.Share[i] + IE.Scholar.Share[i-1]))*
    (IE.Admin[i] / IE.Admin[i-1])^(0.5*(IE.Admin.Share[i] + IE.Admin.Share[i-1]))})


#-------------------Create Total Input Expenses Index-----------------------------

# Total Expenses
XE.Total <- IE.Total + LE.Total + KE.Total

# Shares of Input Componetent Expenses
IE.Total.Share <- IE.Total / XE.Total
LE.Total.Share <- LE.Total / XE.Total
KE.Total.Share <- KE.Total / XE.Total

# Create Input Expenses Index        #####-----IS THIS CORRECT?????? USE COMPONENT INDEXES------#######
XE.Index <- sapply(2:8, function(i){
  (IE.Index[i-1])^(0.5*(IE.Total.Share[i]+IE.Total.Share[i-1]))*
    (LE.Index[i-1])^(0.5*(LE.Total.Share[i]+LE.Total.Share[i-1]))*
    (KE.Index[i-1])^(0.5*(KE.Total.Share[i]+KE.Total.Share[i-1]))})


#----------------------Create Research Output Change Index--------------------------------------


# Create Output Component Indexes

Res.Compl.Index <- sapply(4:10, function(i){Prod.Data$RHD.Completions[i] / 
    Prod.Data$RHD.Completions[i-1]})
Res.Pub.Index <- sapply(4:10, function(i){Prod.Data$SciVal.All.Pubs[i] / 
    Prod.Data$SciVal.All.Pubs[i-1]})
Res.Fund.Index <- sapply(4:10, function(i){Prod.Data$Research.Rev[i] / 
    Prod.Data$Research.Rev[i-1]})


# Specify Component Weights
Compl.Weight <- 1/3
Pub.Weight <- 1/3
Fund.Weight <- 1/3


# Create Research Index Using Research Completions, Publications, & Funding
Res.Index <- sapply(2:8, function(i){
  (Res.Compl.Index[i-1])^(Compl.Weight)*
    (Res.Pub.Index[i-1])^(Pub.Weight)*
    (Res.Fund.Index[i-1])^(Fund.Weight)})


#----------------------Create Instruction Output Change Index--------------------------------------


# Don't use total Adjusted Student Load -- Just Student Load for those doing coursework
Instr.Index <- sapply(4:10, function(i){Prod.Data$Adjusted.FT.Load[i] / 
    Prod.Data$Adjusted.FT.Load[i-1]})


#----------------------Create Total Output Change Index--------------------------------------


# Specify Research & Instruction Weights
Res.Weight <- 0.5
Instr.Weight <- 0.5

# Create Total Output Index 
Q.Index <- sapply(2:8, function(i){
  (Res.Index[i-1])^(Res.Weight)*
    (Instr.Index[i-1])^(Instr.Weight)})



#---------------------Create Productivity Change Index---------------------------------

# Input Component Indexes    (with first year as "1")
LE.Index <- c(1, LE.Index)
KE.Index <- c(1, KE.Index)
IE.Index <- c(1, IE.Index)

LE.Index
KE.Index
IE.Index

# Input Total Index       
X.Index <- c(1, XE.Index)

X.Index

# Output Component Indexes
Res.Index <- c(1, Res.Index)
Instr.Index <- c(1, Instr.Index)

Res.Index
Instr.Index

# Output Total Index
Q.Index <- c(1, Q.Index)

Q.Index

# Productivity Change Index 
Prod.Change <- Q.Index / X.Index

Prod.Change

# Absolute Productivity 


Abs.Prod <- cumprod(Prod.Change)

Abs.Prod


##------------Alternatively use cumprod() function (cumulative product)--------------


#------------------------------Print All Program Outputs--------------------------------


Program.Outputs <- data.frame(Institution = rep(inst, 8), Group = Prod.Data$Mission[3:10],
                              Year = Year[3:10], Labor.Index = LE.Index, Capital.Index = KE.Index,
                              Intermediate.Index = IE.Index, Input.Index = X.Index,
                              Research.Index = Res.Index, Instruction.Index = Instr.Index,
                              Output.Index = Q.Index, Productivity.Change.Index = Prod.Change,
                              Absolute.Productivity = Abs.Prod)

Program.Outputs
}




#----------------------------------END Prod.Calc FUNCTION------------------------------------------------





#------------------------------------Run Function for all Institutions------------------------------ 


#Create the initial output data frame to work with for a "for loop"


#Prod.Change.Out <- Prod.Calc(All_Inst[1])

#Write a 'for loop' to knock out all the institutions in one go. 
#for(i in 2:39){
 # Prod.Change.Out <- rbind(Prod.Change.Out, Prod.Calc(All_Inst[i]))
#}


#-------------------------------------Alternatively, use lapply---------------------------

Prod.Change.Out <- do.call(rbind,lapply(All_Inst, Prod.Calc)) #do.call() can call any function, and the 
                                                              #function quickly takes all arguments that 
                                                              #are stored in a list    
Prod.Change.Out

write.csv(Prod.Change.Out, "OUTPUTS All institutions Smoothed Cap Serv.csv")   # Create CSV for program outputs




Prod.Change.Sum <- Prod.Change.Out %>%
  filter(Year == "2015") %>%
  select(Institution, Group, Absolute.Productivity)

Prod.Change.Sum

write.csv(Prod.Change.Sum, "OUTPUTS All inst Smoothed Cap Serv Smmary.csv")




