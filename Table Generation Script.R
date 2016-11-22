# Survey Item-by-item Responses - Table Generation Script
# Nick Holt - 7-29-2016
#------------------------------------------------------------------------
library(dplyr)
library(reshape2)
library(car)

# set working directory folder and import data
setwd("C:/Users/nholt2/Desktop/Automation")
hs <- read.csv("SurveyData.csv")

#------------------------------------------------------------------------
# Prep data
#------------------------------------------------------------------------

# eliminate rows missing organization and gender data
hsm <- subset(hs, Gender == "Male")
hsf <- subset(hs, Gender == "Female")
hs <- rbind(hsm, hsf)

#recode organization names
hs$Organization <- recode(hs$Organization, "'CI Bicol'='Bicol'; 'CI Colombia'='Colombia';'Little Rock'='USA (Little Rock)'; 'Lusaka'='Zambia'; 'Santo Domingo'='Dominican Republic (Santo Domingo)'; 'San Pedro Sula'='Honduras (San Pedro Sula)'; 'Jalisco'='Mexico (Jalisco)'")

#--------------------------------------------------------------------------
# Create summary levels for long format dataset
#--------------------------------------------------------------------------

#generate data for organization: total
total <- hs
total$Organization = "Total"

#generate data for organization: Ecuador
ecuador <- subset(hs, Organization == "Guayaquil" | Organization == "Quito")
ecuador$Organization = "Ecuador"

#generate data for organization: India
india <- subset(hs, Organization == "Delhi" | Organization == "Sahay")
india$Organization = "India"

#generate data for organization: Philippines
phil <- subset(hs, Organization == "Bicol" | Organization == "Manila" | Organization == "Quezon City")
phil$Organization = "Philippines"

#generate data for organization: latin am./caribbean
lac <- subset(hs, Organization == "Colombia" | Organization == "Dominican Republic (Santo Domingo)" |  Organization == "Guayaquil" | Organization == "Quito" | Organization == "Guatemala" | Organization == "Honduras (San Pedro Sula)" | Organization == "Mexico (Jalisco)")
lac$Organization = "Latin America & Caribbean"

#generate data for organization: asia & africa
asia <- subset(hs, Organization == "Delhi" | Organization == "Sahay" |  Organization == "Bicol" | Organization == "Manila" | Organization == "Quezon City" | Organization == "Zambia")
asia$Organization = "Asia & Africa"

#combine all dataframes into single long format dataframe
hs <- rbind(hs, total, ecuador, india, phil, lac, asia)

# create table -> data.frame of survey totals (denominators) for calculating percentages of males, females, totals in function below
gendTable <- as.data.frame(table(hs$Organization, hs$Gender))
gendTable <- subset(gendTable, Var2 %in% c("Male", "Female"))
gendTable2 <- dcast(gendTable, Var1 ~ Var2)
gendTable2$total <- gendTable2$Female + gendTable2$Male
names(gendTable2)[names(gendTable2)=="Var1"] <- "Organization"
names(gendTable2)[names(gendTable2)=="Male"] <- "Male_Total"
names(gendTable2)[names(gendTable2)=="Female"] <- "Female_Total"
names(gendTable2)[names(gendTable2)=="total"] <- "Total"
write.csv(gendTable2, file = "denominatorTotals.csv")


#--------------------------------------------------------------------------
# Function to generate tables
#--------------------------------------------------------------------------
#
# Function assumes that each column is a survey question
#
# Function takes each column and finds the number of unique responses and
# then for each unique response a table is generated with counts of
# males and females from each organization who gave that specific response.
# Percentages are also calculated in the table using data in gendTable2.
#--------------------------------------------------------------------------

f = function(df) {
  a <- which(colnames(df) == "HS1")
  for(i in colnames(df[, a:length(df)])) {
    levelsList <- unique(df[,i])
    for(j in levelsList) {
      temp <- df[df[,i] == j, c('Organization', 'Gender', i)]
      temp <- group_by(temp, Organization, Gender)
      table <- summarize(temp, count = n())
      table <- dcast(table, Organization ~ Gender, value.var='count')
      #handle cases with only one gender represented
      if (dim(table)[2] > 2) {table$gend_total <- rowSums(table[,c('Male', 'Female')])}
      else {table$gend_total <- table[,2]}
      table <- merge(table, gendTable2, by = "Organization")
      if ( ("Male" %in% colnames(table)) & ("Female" %in% colnames(table)) ) {
              table <- mutate(table, female_pct = Female/Female_Total, male_pct = Male/Male_Total, total_pct = gend_total/Total)
              cat("Nice, it worked! \n");
          }
          else if ("Male" %in% colnames(table)) {
              table$Female <- 0
              table <- mutate(table, female_pct = Female/Female_Total, male_pct = Male/Male_Total, total_pct = gend_total/Total)
          }
          else if ("Female" %in% colnames(table)) {
              table$Male <- 0
              table <- mutate(table, female_pct = Female/Female_Total, male_pct = Male/Male_Total, total_pct = gend_total/Total)
          }
          else {
            cat("Nope, sorry mate :( \n");
          }
      write.csv(table, paste0(i, "_", j, ".csv"), row.names = F)
    }
  }
}

# set directory where tables should be saved
setwd("C:/Users/nholt2/Desktop/Automation/Tables from R")

# run function
f(hs)