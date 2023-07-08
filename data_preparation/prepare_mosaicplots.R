library(graphics)

# contingency table using table() --> mosaicplot()
# Read a .csv file
titanic_dt <- read.csv2("data/titanic.csv", sep = ",")

# Prepare the data.frame
titanic_dt$Survived[titanic_dt$Survived == 0] <- "nicht überlebt"
titanic_dt$Survived[titanic_dt$Survived == 1] <- "überlebt"
titanic_dt$Sex[titanic_dt$Sex == "male"] <- "männlich"
titanic_dt$Sex[titanic_dt$Sex == "female"] <- "weiblich"
titanic_dt$Age[titanic_dt$Age < 20] <- "Kind"
titanic_dt$Age[titanic_dt$Age >= 20] <- "Erwachsene/r"
titanic_dt$Embarked[titanic_dt$Embarked == "S"] <- "Southampton"
titanic_dt$Embarked[titanic_dt$Embarked == "C"] <- "Cherbourg"
titanic_dt$Embarked[titanic_dt$Embarked == "Q"] <- "Queenstown"
titanic_dt$Embarked[titanic_dt$Embarked == ""] <- "unbekannt"

# Create mosaic plots
tbl1 <- prop.table(table(titanic_dt$Sex, titanic_dt$Survived))

mosaicplot(tbl1, color = TRUE)