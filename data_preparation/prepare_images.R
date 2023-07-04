### Import libraries------------------------------------------------------------
library(data.table)
library(treemap)
#...

### Create frequency trees------------------------------------------------------
## Explanation------------------------------------------------------------------
class_dt <- data.table(Kategorie = c("1. Klasse", "2. Klasse", "3. Klasse", "Personal"),
                       Unterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben", 
                                          "Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                       Häufigkeit = c(20, 15, 10, 5, 20, 15, 10, 5))             # Zahlen anpassen!!!
class_treemap <- treemap(class_dt, index = c("Kategorie", "Unterkategorie"), 
                         vSize = "Häufigkeit", vColor = "Kategorie")
png("images/class_treemap.png", width = 800, height = 600, res = 120)            # Zahlen anpassen!!!
print(class_treemap)
dev.off()

## Reading training-------------------------------------------------------------
sex_dt <- data.table(Kategorie = c("Männlich", "Weiblich"),
                     Unterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                     Häufigkeit = c(20, 15, 10, 5))                              # Zahlen anpassen!!!
sex_treemap <- treemap(sex_dt, index = c("Kategorie", "Unterkategorie"), 
                       vSize = "Häufigkeit", vColor = "Kategorie")
png("images/sex_treemap.png", width = 800, height = 600, res = 120)            # Zahlen anpassen!!!
print(class_treemap)
dev.off()

## Creating training------------------------------------------------------------
# Empty nodes-------------------------------------------------------------------


# Empty branches----------------------------------------------------------------

## Quiz-------------------------------------------------------------------------
# Reading exercise--------------------------------------------------------------
sex_age_dt <- data.table(Kategorie = c("Männlich", "Weiblich"),
                           Unterkategorie = c("Kind", "Erwachsen", "Kind", "Erwachsen"),
                           Unterunterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben", 
                                                   "Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                           Häufigkeit = c(20, 15, 10, 5, 20, 15, 10, 5))         # Zahlen anpassen!!!
sex_age_treemap <- treemap(sex_age_dt, index = c("Kategorie", "Unterkategorie", "Unterunterkategorie"), 
                           vSize = "Häufigkeit", vColor = "Kategorie")
png("images/class_treemap.png", width = 800, height = 600, res = 120)            # Zahlen anpassen!!!
print(sex_age_treemap)
dev.off()

# Creating exercise-------------------------------------------------------------


### Create mosaic plots - ? on-site ?