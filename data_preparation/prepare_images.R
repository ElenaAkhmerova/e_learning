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
class_age_dt <- data.table(Kategorie = c("1. Klasse", "2. Klasse", "3. Klasse", "Personal"),
                           Unterkategorie = c("Kind", "Erwachsen", "Kind", "Erwachsen", 
                                              "Kind", "Erwachsen", "Kind", "Erwachsen"),
                           Unterunterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben", 
                                                   "Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                           Häufigkeit = c(20, 15, 10, 5, 20, 15, 10, 5))         # Zahlen anpassen!!!
class_age_treemap <- treemap(class_age_dt, index = c("Kategorie", "Unterkategorie", "Unterunterkategorie"), 
                             vSize = "Häufigkeit", vColor = "Kategorie")
png("images/class_age_treemap.png", width = 800, height = 600, res = 120)        # Zahlen anpassen!!!
print(class_age_treemap)
dev.off()

## Creating training------------------------------------------------------------
# Empty nodes-------------------------------------------------------------------
age_gaps_dt <- data.table(Kategorie = c(NA, "Erwachsen"),                        # for quiz mode both empty
                          Unterkategorie = c("Überlebt", NA, NA, "Gestorben"),
                          Häufigkeit = c(20, 15, 10, 5))                         # Zahlen anpassen!!!
age_treemap_gaps <- treemap(age_gaps_dt, index = c("Kategorie", "Unterkategorie"), 
                            vSize = "Häufigkeit", vColor = "Kategorie")
png("images/age_treemap_gaps.png", width = 800, height = 600, res = 120)         # Zahlen anpassen!!!
print(age_treemap_gaps)
dev.off()

age_dt <- data.table(Kategorie = c("Kind", "Erwachsen"),
                     Unterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                     Häufigkeit = c(20, 15, 10, 5))                              # Zahlen anpassen!!!
age_treemap <- treemap(age_dt, index = c("Kategorie", "Unterkategorie"), 
                       vSize = "Häufigkeit", vColor = "Kategorie")
png("images/age_treemap.png", width = 800, height = 600, res = 120)              # Zahlen anpassen!!!
print(age_treemap)
dev.off()

# Empty branches----------------------------------------------------------------
sex_gaps_dt <- data.table(Kategorie = c("Männlich", "Weiblich"),
                          Unterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                          Häufigkeit = c(NA, 20, 10, NA))                        # Zahlen anpassen!!!
sex_treemap_gaps <- treemap(sex_gaps_dt, index = c("Kategorie", "Unterkategorie"), 
                            vSize = "Häufigkeit", vColor = "Kategorie")
png("images/sex_treemap_gaps.png", width = 800, height = 600, res = 120)         # Zahlen anpassen!!!
print(sex_treemap_gaps)
dev.off()

sex_dt <- data.table(Kategorie = c("Männlich", "Weiblich"),
                     Unterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                     Häufigkeit = c(5, 20, 10, 15))                              # Zahlen anpassen!!!
sex_treemap <- treemap(sex_dt, index = c("Kategorie", "Unterkategorie"), 
                       vSize = "Häufigkeit", vColor = "Kategorie")
png("images/sex_treemap.png", width = 800, height = 600, res = 120)              # Zahlen anpassen!!!
print(sex_treemap)
dev.off()

## Quiz-------------------------------------------------------------------------
# Reading exercise--------------------------------------------------------------
sex_age_dt <- data.table(Kategorie = c("Männlich", "Weiblich"),
                           Unterkategorie = c("Kind", "Erwachsen", "Kind", "Erwachsen"),
                           Unterunterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben", 
                                                   "Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                           Häufigkeit = c(20, 15, 10, 5, 20, 15, 10, 5))         # Zahlen anpassen!!!
sex_age_treemap <- treemap(sex_age_dt, index = c("Kategorie", "Unterkategorie", "Unterunterkategorie"), 
                           vSize = "Häufigkeit", vColor = "Kategorie")
png("images/sex_age_treemap.png", width = 800, height = 600, res = 120)          # Zahlen anpassen!!!
print(sex_age_treemap)
dev.off()

# Creating exercise-------------------------------------------------------------
stop_gaps_dt <- data.table(Kategorie = c("Stadt1", "Stadt2"),                    # ersetzen!!!
                           Unterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                           Häufigkeit = c(NA, NA, 10, NA))                       # Zahlen anpassen!!!
stop_treemap_gaps <- treemap(stop_gaps_dt, index = c("Kategorie", "Unterkategorie"), 
                             vSize = "Häufigkeit", vColor = "Kategorie")
png("images/stop_treemap_gaps.png", width = 800, height = 600, res = 120)        # Zahlen anpassen!!!
print(stop_treemap_gaps)
dev.off()

stop_dt <- data.table(Kategorie = c("Stadt1", "Stadt2"),                         # ersetzen!!!
                      Unterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                      Häufigkeit = c(5, 20, 10, 15))                             # Zahlen anpassen!!!
stop_treemap <- treemap(stop_dt, index = c("Kategorie", "Unterkategorie"), 
                       vSize = "Häufigkeit", vColor = "Kategorie")
png("images/stop_treemap.png", width = 800, height = 600, res = 120)             # Zahlen anpassen!!!
print(stop_treemap)
dev.off()

stop_class_gaps_dt <- data.table(Kategorie = c("Stadt1", "Stadt2"),              # ersetzen!!!
                                 Unterkategorie = c("1. Klasse", "2. Klasse", "3. Klasse", "Personal", 
                                                    "1. Klasse", "2. Klasse", "3. Klasse", "Personal"),
                                 Unterunterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben", 
                                                         "Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                                 Häufigkeit = c(NA, NA, 10, NA, 5, 20, NA, NA))                       # Zahlen anpassen!!!
stop_class_treemap_gaps <- treemap(stop_class_gaps_dt, index = c("Kategorie", "Unterkategorie", 
                                                                 "Unterunterkategorie"), 
                             vSize = "Häufigkeit", vColor = "Kategorie")
png("images/stop_class_treemap_gaps.png", width = 800, height = 600, res = 120)  # Zahlen anpassen!!!
print(stop_class_treemap_gaps)
dev.off()

stop_class_dt <- data.table(Kategorie = c("Stadt1", "Stadt2"),                   # ersetzen!!!
                            Unterkategorie = c("1. Klasse", "2. Klasse", "3. Klasse", "Personal", 
                                               "1. Klasse", "2. Klasse", "3. Klasse", "Personal"),
                            Unterunterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben", 
                                                    "Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                            Häufigkeit = c(5, 20, 10, 15, 5, 20, 10, 15))        # Zahlen anpassen!!!
stop_class_treemap <- treemap(stop_class_dt, index = c("Kategorie", "Unterkategorie", "Unterunterkategorie"), 
                        vSize = "Häufigkeit", vColor = "Kategorie")
png("images/stop_class_treemap.png", width = 800, height = 600, res = 120)       # Zahlen anpassen!!!
print(stop_class_treemap)
dev.off()

### Create mosaic plots - ? on-site ?