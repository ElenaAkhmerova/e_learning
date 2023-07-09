### Import libraries------------------------------------------------------------
library(data.table)
library(treemap)
library(riskyr)
library(igraph)

### Read and explore data-------------------------------------------------------
## Bread basket-----------------------------------------------------------------
basket_dt <- fread("data/bread_basket.csv")

## Titanic----------------------------------------------------------------------
titanic_dt <- fread("titanic.csv")
n_total <- nrow(titanic_dt)

# Sex
maenner <- nrow(titanic_dt[titanic_dt$Sex == "männlich",])#"male",])
frauen <- nrow(titanic_dt[titanic_dt$Sex == "weiblich",])#"female",])

maenner_ueberlebt <- nrow(titanic_dt[(titanic_dt$Sex == "männlich")#"male") 
                                     & (titanic_dt$Survived == "überlebt"),])#1),])
#maenner_gestorben <- nrow(titanic_dt[(titanic_dt$Sex == "male") 
#                                     & (titanic_dt$Survived == 0),])
#frauen_ueberlebt <- nrow(titanic_dt[(titanic_dt$Sex == "female") 
#                                    & (titanic_dt$Survived == 1),])
frauen_gestorben <- nrow(titanic_dt[(titanic_dt$Sex == "weiblich")#"female") 
                                    & (titanic_dt$Survived == "nicht überlebt"),])#0),])

anteil_maenner <- maenner / n_total # prev
anteil_maenner_ueberlebt <- maenner_ueberlebt / maenner # sens
anteil_frauen_gestorben <- frauen_gestorben / frauen # spec

# Age
children <- nrow(titanic_dt[titanic_dt$Age == "Kind",]) #< 20,])
adults <- nrow(titanic_dt[titanic_dt$Age == "Erwachsene/r",]) #>= 20,])

children_survived <- nrow(titanic_dt[(titanic_dt$Age == "Kind") #< 20)
                                     & (titanic_dt$Survived == "überlebt"),])#1),])
adults_deceased <- nrow(titanic_dt[(titanic_dt$Age == "Erwachsene/r") #>= 20)
                                   & (titanic_dt$Survived == "nicht überlebt"),])#0),])

anteil_kinder <- children / n_total # prev
anteil_kinder_ueberlebt <- children_survived / children # sens
anteil_adults_deceased <- adults_deceased / adults # spec

### Create frequency trees------------------------------------------------------
## Explanation------------------------------------------------------------------
png("images/sex_tree.png", width = 800, height = 600, res = 120)              # Zahlen anpassen!!!
my_txt <- init_txt(cond_true_lbl = "Männlich", cond_false_lbl = "Weiblich",
                   hi_lbl = "Überlebt", mi_lbl = "Nicht Überlebt", 
                   fa_lbl = "Überlebt", cr_lbl = "Nicht Überlebt")
sex_tree <- plot_prism(prev = anteil_maenner, sens = anteil_maenner_ueberlebt, 
                       spec = anteil_frauen_gestorben, by = "cd", f_lbl = "nam", 
                       lbl_txt = my_txt, f_lwd = .5, arr_c = 2)
print(sex_tree)
dev.off()


## Reading training-------------------------------------------------------------
png("images/class_age_treemap.png", width = 800, height = 600, res = 120)        # Zahlen anpassen!!!
class_age_dt <- data.table(Kategorie = c("1. Klasse", "2. Klasse", "3. Klasse", "Personal"),
                           Unterkategorie = c("Kind", "Erwachsen", "Kind", "Erwachsen", 
                                              "Kind", "Erwachsen", "Kind", "Erwachsen"),
                           Unterunterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben", 
                                                   "Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                           Häufigkeit = c(20, 15, 10, 5, 20, 15, 10, 5))         # Zahlen anpassen!!!
class_age_treemap <- treemap(class_age_dt, index = c("Kategorie", "Unterkategorie", "Unterunterkategorie"), 
                             vSize = "Häufigkeit", vColor = "Kategorie")
dev.off()

## Creating training------------------------------------------------------------
# Empty nodes-------------------------------------------------------------------
png("images/age_tree_gaps.png", width = 800, height = 600, res = 120)         # Zahlen anpassen!!!
my_txt <- init_txt(cond_true_lbl = "Kind", cond_false_lbl = "(1)______",
                   hi_lbl = "Überlebt", mi_lbl = "Nicht Überlebt", 
                   fa_lbl = "(2)______", cr_lbl = "(3)______")
age_tree_gaps <- plot_prism(prev = anteil_kinder, sens = anteil_kinder_ueberlebt, 
                            spec = anteil_adults_deceased, by = "cd", f_lbl = "nam", 
                            lbl_txt = my_txt, f_lwd = .5, arr_c = 2)
dev.off()

png("images/age_tree.png", width = 800, height = 600, res = 120)              # Zahlen anpassen!!!
my_txt <- init_txt(cond_true_lbl = "Kind", cond_false_lbl = "Erwachsene/r",
                   hi_lbl = "Überlebt", mi_lbl = "Nicht Überlebt", 
                   fa_lbl = "Überlebt", cr_lbl = "Nicht Überlebt")
age_tree <- plot_prism(prev = anteil_kinder, sens = anteil_kinder_ueberlebt, 
                       spec = anteil_adults_deceased, by = "cd", f_lbl = "nam", 
                       lbl_txt = my_txt, f_lwd = .5, arr_c = 2)
dev.off()

# Empty branches----------------------------------------------------------------
png("images/class_tree_gaps.png", width = 800, height = 600, res = 120)       # Zahlen anpassen!!!
class_gaps_dt <- data.table(Kategorie = c("1. Klasse", "2. Klasse", "3. Klasse", "Personal"),
                          Unterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben", 
                                             "Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                          Häufigkeit = c(NA, 20, 10, NA, 30, 40, NA, NA))        # Zahlen anpassen!!! NA nur in Labels
class_treemap_gaps <- treemap(class_gaps_dt, index = c("Kategorie", "Unterkategorie"), 
                            vSize = "Häufigkeit", vColor = "Kategorie")
dev.off()

png("images/class_tree.png", width = 800, height = 600, res = 120)               # Zahlen anpassen!!!
class_dt <- data.table(Kategorie = c("1. Klasse", "2. Klasse", "3. Klasse", "Personal"),
                   Unterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben", 
                                      "Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                   Häufigkeit = c(20, 15, 10, 5, 20, 15, 10, 5))                 # Zahlen anpassen!!!
class_treemap <- treemap(class_dt, index = c("Kategorie", "Unterkategorie"), 
                         vSize = "Häufigkeit", vColor = "Kategorie")
dev.off()

## Quiz-------------------------------------------------------------------------
# Reading exercise--------------------------------------------------------------
png("images/sex_age_treemap.png", width = 800, height = 600, res = 120)          # Zahlen anpassen!!!
sex_age_dt <- data.table(Kategorie = c("Männlich", "Weiblich"),
                           Unterkategorie = c("Kind", "Erwachsen", "Kind", "Erwachsen"),
                           Unterunterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben", 
                                                   "Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                           Häufigkeit = c(20, 15, 10, 5, 20, 15, 10, 5))         # Zahlen anpassen!!!
sex_age_treemap <- treemap(sex_age_dt, index = c("Kategorie", "Unterkategorie", "Unterunterkategorie"), 
                           vSize = "Häufigkeit", vColor = "Kategorie")
dev.off()

# Creating exercise-------------------------------------------------------------
png("images/stop_treemap_gaps.png", width = 800, height = 600, res = 120)        # Zahlen anpassen!!!
stop_gaps_dt <- data.table(Kategorie = c("Stadt1", "Stadt2"),                    # ersetzen!!!
                           Unterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                           Häufigkeit = c(NA, NA, 10, NA))                       # Zahlen anpassen!!!
stop_treemap_gaps <- treemap(stop_gaps_dt, index = c("Kategorie", "Unterkategorie"), 
                             vSize = "Häufigkeit", vColor = "Kategorie")
dev.off()

png("images/stop_treemap.png", width = 800, height = 600, res = 120)             # Zahlen anpassen!!!
stop_dt <- data.table(Kategorie = c("Stadt1", "Stadt2"),                         # ersetzen!!!
                      Unterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                      Häufigkeit = c(5, 20, 10, 15))                             # Zahlen anpassen!!!
stop_treemap <- treemap(stop_dt, index = c("Kategorie", "Unterkategorie"), 
                       vSize = "Häufigkeit", vColor = "Kategorie")
dev.off()

png("images/stop_class_treemap_gaps.png", width = 800, height = 600, res = 120)  # Zahlen anpassen!!!
stop_class_gaps_dt <- data.table(Kategorie = c("Stadt1", "Stadt2"),              # ersetzen!!!
                                 Unterkategorie = c("1. Klasse", "2. Klasse", "3. Klasse", "Personal", 
                                                    "1. Klasse", "2. Klasse", "3. Klasse", "Personal"),
                                 Unterunterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben", 
                                                         "Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                                 Häufigkeit = c(NA, NA, 10, NA, 5, 20, NA, NA))                       # Zahlen anpassen!!!
stop_class_treemap_gaps <- treemap(stop_class_gaps_dt, index = c("Kategorie", "Unterkategorie", 
                                                                 "Unterunterkategorie"), 
                             vSize = "Häufigkeit", vColor = "Kategorie")
dev.off()

png("images/stop_class_treemap.png", width = 800, height = 600, res = 120)       # Zahlen anpassen!!!
stop_class_dt <- data.table(Kategorie = c("Stadt1", "Stadt2"),                   # ersetzen!!!
                            Unterkategorie = c("1. Klasse", "2. Klasse", "3. Klasse", "Personal", 
                                               "1. Klasse", "2. Klasse", "3. Klasse", "Personal"),
                            Unterunterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben", 
                                                    "Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                            Häufigkeit = c(5, 20, 10, 15, 5, 20, 10, 15))        # Zahlen anpassen!!!
stop_class_treemap <- treemap(stop_class_dt, index = c("Kategorie", "Unterkategorie", "Unterunterkategorie"), 
                        vSize = "Häufigkeit", vColor = "Kategorie")
dev.off()

### Create mosaic plots - ? on-site ?
## Explanation------------------------------------------------------------------
png("images/class_treemap.png", width = 800, height = 600, res = 120)            # Zahlen anpassen!!!
class_dt <- data.table(Kategorie = c("1. Klasse", "2. Klasse", "3. Klasse", "Personal"),
                       Unterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben", 
                                          "Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                       Häufigkeit = c(20, 15, 10, 5, 20, 15, 10, 5))             # Zahlen anpassen!!!
class_treemap <- treemap(class_dt, index = c("Kategorie", "Unterkategorie"), 
                         vSize = "Häufigkeit", vColor = "Kategorie")
dev.off()

## Reading training-------------------------------------------------------------
png("images/class_age_treemap.png", width = 800, height = 600, res = 120)        # Zahlen anpassen!!!
class_age_dt <- data.table(Kategorie = c("1. Klasse", "2. Klasse", "3. Klasse", "Personal"),
                           Unterkategorie = c("Kind", "Erwachsen", "Kind", "Erwachsen", 
                                              "Kind", "Erwachsen", "Kind", "Erwachsen"),
                           Unterunterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben", 
                                                   "Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                           Häufigkeit = c(20, 15, 10, 5, 20, 15, 10, 5))         # Zahlen anpassen!!!
class_age_treemap <- treemap(class_age_dt, index = c("Kategorie", "Unterkategorie", "Unterunterkategorie"), 
                             vSize = "Häufigkeit", vColor = "Kategorie")
dev.off()

## Creating training------------------------------------------------------------
# Empty nodes-------------------------------------------------------------------
png("images/age_treemap_gaps.png", width = 800, height = 600, res = 120)         # Zahlen anpassen!!!
age_gaps_dt <- data.table(Kategorie = c(NA, "Erwachsen"),                        # for quiz mode both empty
                          Unterkategorie = c("Überlebt", NA, NA, "Gestorben"),
                          Häufigkeit = c(20, 15, 10, 5))                         # Zahlen anpassen!!!
age_treemap_gaps <- treemap(age_gaps_dt, index = c("Kategorie", "Unterkategorie"), 
                            vSize = "Häufigkeit", vColor = "Kategorie")
dev.off()

png("images/age_treemap.png", width = 800, height = 600, res = 120)              # Zahlen anpassen!!!
age_dt <- data.table(Kategorie = c("Kind", "Erwachsen"),
                     Unterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                     Häufigkeit = c(20, 15, 10, 5))                              # Zahlen anpassen!!!
age_treemap <- treemap(age_dt, index = c("Kategorie", "Unterkategorie"), 
                       vSize = "Häufigkeit", vColor = "Kategorie")
dev.off()

# Empty branches----------------------------------------------------------------
png("images/sex_treemap_gaps.png", width = 800, height = 600, res = 120)         # Zahlen anpassen!!!
sex_gaps_dt <- data.table(Kategorie = c("Männlich", "Weiblich"),
                          Unterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                          Häufigkeit = c(NA, 20, 10, NA))                        # Zahlen anpassen!!!
sex_treemap_gaps <- treemap(sex_gaps_dt, index = c("Kategorie", "Unterkategorie"), 
                            vSize = "Häufigkeit", vColor = "Kategorie")
dev.off()

png("images/sex_treemap.png", width = 800, height = 600, res = 120)              # Zahlen anpassen!!!
sex_dt <- data.table(Kategorie = c("Männlich", "Weiblich"),
                     Unterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                     Häufigkeit = c(5, 20, 10, 15))                              # Zahlen anpassen!!!
sex_treemap <- treemap(sex_dt, index = c("Kategorie", "Unterkategorie"), 
                       vSize = "Häufigkeit", vColor = "Kategorie")
dev.off()

## Quiz-------------------------------------------------------------------------
# Reading exercise--------------------------------------------------------------
png("images/sex_age_treemap.png", width = 800, height = 600, res = 120)          # Zahlen anpassen!!!
sex_age_dt <- data.table(Kategorie = c("Männlich", "Weiblich"),
                         Unterkategorie = c("Kind", "Erwachsen", "Kind", "Erwachsen"),
                         Unterunterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben", 
                                                 "Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                         Häufigkeit = c(20, 15, 10, 5, 20, 15, 10, 5))         # Zahlen anpassen!!!
sex_age_treemap <- treemap(sex_age_dt, index = c("Kategorie", "Unterkategorie", "Unterunterkategorie"), 
                           vSize = "Häufigkeit", vColor = "Kategorie")
dev.off()

# Creating exercise-------------------------------------------------------------
png("images/stop_treemap_gaps.png", width = 800, height = 600, res = 120)        # Zahlen anpassen!!!
stop_gaps_dt <- data.table(Kategorie = c("Stadt1", "Stadt2"),                    # ersetzen!!!
                           Unterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                           Häufigkeit = c(NA, NA, 10, NA))                       # Zahlen anpassen!!!
stop_treemap_gaps <- treemap(stop_gaps_dt, index = c("Kategorie", "Unterkategorie"), 
                             vSize = "Häufigkeit", vColor = "Kategorie")
dev.off()

png("images/stop_treemap.png", width = 800, height = 600, res = 120)             # Zahlen anpassen!!!
stop_dt <- data.table(Kategorie = c("Stadt1", "Stadt2"),                         # ersetzen!!!
                      Unterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                      Häufigkeit = c(5, 20, 10, 15))                             # Zahlen anpassen!!!
stop_treemap <- treemap(stop_dt, index = c("Kategorie", "Unterkategorie"), 
                        vSize = "Häufigkeit", vColor = "Kategorie")
dev.off()

png("images/stop_class_treemap_gaps.png", width = 800, height = 600, res = 120)  # Zahlen anpassen!!!
stop_class_gaps_dt <- data.table(Kategorie = c("Stadt1", "Stadt2"),              # ersetzen!!!
                                 Unterkategorie = c("1. Klasse", "2. Klasse", "3. Klasse", "Personal", 
                                                    "1. Klasse", "2. Klasse", "3. Klasse", "Personal"),
                                 Unterunterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben", 
                                                         "Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                                 Häufigkeit = c(NA, NA, 10, NA, 5, 20, NA, NA))                       # Zahlen anpassen!!!
stop_class_treemap_gaps <- treemap(stop_class_gaps_dt, index = c("Kategorie", "Unterkategorie", 
                                                                 "Unterunterkategorie"), 
                                   vSize = "Häufigkeit", vColor = "Kategorie")
dev.off()

png("images/stop_class_treemap.png", width = 800, height = 600, res = 120)       # Zahlen anpassen!!!
stop_class_dt <- data.table(Kategorie = c("Stadt1", "Stadt2"),                   # ersetzen!!!
                            Unterkategorie = c("1. Klasse", "2. Klasse", "3. Klasse", "Personal", 
                                               "1. Klasse", "2. Klasse", "3. Klasse", "Personal"),
                            Unterunterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben", 
                                                    "Überlebt", "Gestorben", "Überlebt", "Gestorben"),
                            Häufigkeit = c(5, 20, 10, 15, 5, 20, 10, 15))        # Zahlen anpassen!!!
stop_class_treemap <- treemap(stop_class_dt, index = c("Kategorie", "Unterkategorie", "Unterunterkategorie"), 
                              vSize = "Häufigkeit", vColor = "Kategorie")
dev.off()