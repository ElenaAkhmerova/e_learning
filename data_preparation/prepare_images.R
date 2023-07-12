### Import libraries------------------------------------------------------------
library(data.table)
library(gridExtra)
library(treemap)
library(riskyr)
#library(igraph)
library(data.tree)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

### Read and explore data-------------------------------------------------------
## Bread basket-----------------------------------------------------------------
basket_dt <- fread("data/bread_basket.csv")

# absolute values
png("images/drink_daytime_contingency.png", width = 600, height = 300, res = 120)
basket1 <- table(basket_dt$Item[basket_dt$Item == "Coffee" | basket_dt$Item == "Tea"], 
                 basket_dt$period_day[basket_dt$Item == "Coffee" | basket_dt$Item == "Tea"]) 
grid.arrange(tableGrob(addmargins(basket1)))
dev.off()

png("images/bread_cake_weekday.png", width = 400, height = 300, res = 120)
basket2 <- table(basket_dt$Item[basket_dt$Item == "Bread" | basket_dt$Item == "Cake"], 
                 basket_dt$weekday_weekend[basket_dt$Item == "Bread" | basket_dt$Item == "Cake"]) 
grid.arrange(tableGrob(addmargins(basket2)))
dev.off()

# conditional relative values - note that prop.table() divides by total N -> unsuitable
basket1_dt <- data.table(basket1)
names(basket1_dt) <- c("Item", "Tageszeit", "Anzahl")
n_coffee <- sum(basket1_dt$Anzahl[basket1_dt$Item == "Coffee"])
n_tea <- sum(basket1_dt$Anzahl[basket1_dt$Item == "Tea"])
basket1_dt["Bedingte_relative_Häufigkeit"] := 

## Titanic----------------------------------------------------------------------
titanic_dt <- fread("data/titanic.csv")
n_total <- nrow(titanic_dt)

# Prepare the data.frame
titanic_dt$Survived[titanic_dt$Survived == 0] <- "nicht überlebt"
titanic_dt$Survived[titanic_dt$Survived == 1] <- "überlebt"
titanic_dt$Sex[titanic_dt$Sex == "male"] <- "männlich"
titanic_dt$Sex[titanic_dt$Sex == "female"] <- "weiblich"
titanic_dt$Age[titanic_dt$Age >= 20] <- "Erwachsene/r"
titanic_dt$Age[titanic_dt$Age != "Erwachsene/r"] <- "Kind"
titanic_dt$Pclass[titanic_dt$Pclass == 1] <- "1. Klasse"
titanic_dt$Pclass[titanic_dt$Pclass == 2] <- "2. Klasse"
titanic_dt$Pclass[titanic_dt$Pclass == 3] <- "3. Klasse"
titanic_dt$Embarked[titanic_dt$Embarked == "S"] <- "Southampton"
titanic_dt$Embarked[titanic_dt$Embarked == "C"] <- "Cherbourg"
titanic_dt$Embarked[titanic_dt$Embarked == "Q"] <- "Queenstown"
titanic_dt$Embarked[titanic_dt$Embarked == ""] <- "unbekannt"

# Sex
maenner <- nrow(titanic_dt[titanic_dt$Sex == "männlich",])
frauen <- nrow(titanic_dt[titanic_dt$Sex == "weiblich",])

maenner_ueberlebt <- nrow(titanic_dt[(titanic_dt$Sex == "männlich") 
                                     & (titanic_dt$Survived == "überlebt"),])
frauen_gestorben <- nrow(titanic_dt[(titanic_dt$Sex == "weiblich") 
                                    & (titanic_dt$Survived == "nicht überlebt"),])

anteil_maenner <- maenner / n_total # prev
anteil_maenner_ueberlebt <- maenner_ueberlebt / maenner # sens
anteil_frauen_gestorben <- frauen_gestorben / frauen # spec

# Age
children <- nrow(titanic_dt[titanic_dt$Age == "Kind",])
adults <- nrow(titanic_dt[titanic_dt$Age == "Erwachsene/r",])

children_survived <- nrow(titanic_dt[(titanic_dt$Age == "Kind")
                                     & (titanic_dt$Survived == "überlebt"),])
adults_deceased <- nrow(titanic_dt[(titanic_dt$Age == "Erwachsene/r")
                                   & (titanic_dt$Survived == "nicht überlebt"),])

anteil_kinder <- children / n_total # prev
anteil_kinder_ueberlebt <- children_survived / children # sens
anteil_adults_deceased <- adults_deceased / adults # spec

# Passenger Class
class1 <- nrow(titanic_dt[titanic_dt$Pclass == "1. Klasse",])
class2 <- nrow(titanic_dt[titanic_dt$Pclass == "2. Klasse",])
class3 <- nrow(titanic_dt[titanic_dt$Pclass == "3. Klasse",])

anteil_class1 <- round(class1 / n_total * 100, 2)
anteil_class2 <- round(class2 / n_total * 100, 2)
anteil_class3 <- round(class3 / n_total * 100, 2)

anteil_class1_survived <- round(nrow(titanic_dt[(titanic_dt$Pclass == "1. Klasse")
                                                & (titanic_dt$Survived == "überlebt"),])
                                / class1 * 100, 2)
anteil_class1_deceased <- 100 - anteil_class1_survived
anteil_class2_survived <- round(nrow(titanic_dt[(titanic_dt$Pclass == "2. Klasse")
                                                & (titanic_dt$Survived == "überlebt"),])
                                / class2 * 100, 2)
anteil_class2_deceased <- 100 - anteil_class2_survived
anteil_class3_survived <- round(nrow(titanic_dt[(titanic_dt$Pclass == "3. Klasse")
                                                & (titanic_dt$Survived == "überlebt"),])
                                / class3 * 100, 2)
anteil_class3_deceased <- 100 - anteil_class3_survived

### Create frequency trees------------------------------------------------------
## Explanation------------------------------------------------------------------
png("images/sex_tree.png", width = 800, height = 600, res = 120)
my_txt <- init_txt(cond_true_lbl = "Männlich", cond_false_lbl = "Weiblich",
                   hi_lbl = "Überlebt", mi_lbl = "Nicht Überlebt", 
                   fa_lbl = "Überlebt", cr_lbl = "Nicht Überlebt")
sex_tree <- plot_prism(prev = anteil_maenner, sens = anteil_maenner_ueberlebt, 
                       spec = anteil_frauen_gestorben, by = "cd", f_lbl = "nam", 
                       lbl_txt = my_txt, f_lwd = .5, arr_c = 2)
print(sex_tree)
dev.off()

## Creating training------------------------------------------------------------
# Empty nodes-------------------------------------------------------------------
png("images/age_tree_gaps.png", width = 800, height = 600, res = 120)
my_txt <- init_txt(cond_true_lbl = "(1)______", cond_false_lbl = "(2)______",
                   hi_lbl = "Überlebt", mi_lbl = "Nicht Überlebt", 
                   fa_lbl = "(3)______", cr_lbl = "(4)______")
age_tree_gaps <- plot_prism(prev = anteil_kinder, sens = anteil_kinder_ueberlebt, 
                            spec = anteil_adults_deceased, by = "cd", f_lbl = "nam", 
                            lbl_txt = my_txt, f_lwd = .5, arr_c = 2)
dev.off()

png("images/age_tree.png", width = 800, height = 600, res = 120)
my_txt <- init_txt(cond_true_lbl = "Kind", cond_false_lbl = "Erwachsene/r",
                   hi_lbl = "Überlebt", mi_lbl = "Nicht Überlebt", 
                   fa_lbl = "Überlebt", cr_lbl = "Nicht Überlebt")
age_tree <- plot_prism(prev = anteil_kinder, sens = anteil_kinder_ueberlebt, 
                       spec = anteil_adults_deceased, by = "cd", f_lbl = "nam", 
                       lbl_txt = my_txt, f_lwd = .5, arr_c = 2)
dev.off()

# Empty branches----------------------------------------------------------------
tree_class <- Node$new("N")
  class1 <- tree_class$AddChild("1. Klasse")
    survived1 <- class1$AddChild(" Überlebt")
    ceased1 <- class1$AddChild(" Nicht Überlebt")
  class2 <- tree_class$AddChild("2. Klasse")
    survived2 <- class2$AddChild("Überlebt ")
    ceased2 <- class2$AddChild("Nicht Überlebt ")
  class3 <- tree_class$AddChild("3. Klasse")
    survived3 <- class3$AddChild("Überlebt")
    ceased3 <- class3$AddChild("Nicht Überlebt")

# Customize edges - see "Plot with the data.tree plotting facility" in vignette
GetEdgeLabel <- function(node) {
  if (node$name == "1. Klasse") {
    label = anteil_class1
  } else if (node$name == "2. Klasse") {
    label = anteil_class2
  } else if (node$name == "3. Klasse") {
    label = anteil_class3
  } else if (node$name == " Überlebt"){
    label = "a)______"
  } else if (node$name == " Nicht Überlebt"){
    label = "b)______"
  } else if (node$name == "Überlebt "){
    label = "c)______"
  } else if (node$name == "Nicht Überlebt "){
    label = "d)______"
  } else if (node$name == "Überlebt"){
    label = "e)______"
  } else if (node$name == "Nicht Überlebt"){
    label = "f)______"
  }
  return (label)
}
SetEdgeStyle(tree_class, fontname = 'helvetica', label = GetEdgeLabel)
SetGraphStyle(tree_class, rankdir = "LR")
export_graph(ToDiagrammeRGraph(tree_class), "images/class_tree_gaps.png")  # otherwise blank png

#png("images/class_tree.png", width = 800, height = 600, res = 120)
GetEdgeLabel <- function(node) {
  if (node$name == "1. Klasse") {
    label = anteil_class1
  } else if (node$name == "2. Klasse") {
    label = anteil_class2
  } else if (node$name == "3. Klasse") {
    label = anteil_class3
  } else if (node$name == " Überlebt"){
    label = paste0("a) ", anteil_class1_survived)
  } else if (node$name == " Nicht Überlebt"){
    label = paste0("b) ", anteil_class1_deceased)
  } else if (node$name == "Überlebt "){
    label = paste0("c) ", anteil_class2_survived)
  } else if (node$name == "Nicht Überlebt "){
    label = paste0("d) ", anteil_class2_deceased)
  } else if (node$name == "Überlebt"){
    label = paste0("e) ", anteil_class3_survived)
  } else if (node$name == "Nicht Überlebt"){
    label = paste0("f) ", anteil_class3_deceased)
  }
  return (label)
}
SetEdgeStyle(tree_class, fontname = 'helvetica', label = GetEdgeLabel)
SetGraphStyle(tree_class, rankdir = "LR")
export_graph(ToDiagrammeRGraph(tree_class), "images/class_tree.png")  # otherwise blank png

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

## Reading training-------------------------------------------------------------
# png("images/class_age_treemap.png", width = 800, height = 600, res = 120)        # Zahlen anpassen!!!
# class_age_dt <- data.table(Kategorie = c("1. Klasse", "2. Klasse", "3. Klasse", "Personal"),
#                            Unterkategorie = c("Kind", "Erwachsen", "Kind", "Erwachsen", 
#                                               "Kind", "Erwachsen", "Kind", "Erwachsen"),
#                            Unterunterkategorie = c("Überlebt", "Gestorben", "Überlebt", "Gestorben", 
#                                                    "Überlebt", "Gestorben", "Überlebt", "Gestorben"),
#                            Häufigkeit = c(20, 15, 10, 5, 20, 15, 10, 5))         # Zahlen anpassen!!!
# class_age_treemap <- treemap(class_age_dt, index = c("Kategorie", "Unterkategorie", "Unterunterkategorie"), 
#                              vSize = "Häufigkeit", vColor = "Kategorie")
# dev.off()

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

### Create mosaic plots
## Explanation------------------------------------------------------------------
png("images/sex_mosaic.png", width = 800, height = 600, res = 120)
tbl1 <- prop.table(table(titanic_dt$Sex, titanic_dt$Survived))                   # redo !!!
mosaicplot(tbl1, main = "Überleben | Geschlecht")
dev.off()

png("images/sex_mosaic_shades.png", width = 800, height = 600, res = 120)
mosaicplot(tbl1, color = TRUE, main = "Überleben | Geschlecht")
dev.off()

## Reading training-------------------------------------------------------------
png("images/class_mosaic.png", width = 800, height = 600, res = 120)
tbl2 <- prop.table(table(titanic_dt$Pclass, titanic_dt$Survived))                # redo !!!
mosaicplot(tbl2, color=TRUE, main="Überleben | Passagierklasse")
dev.off()

## Creating training------------------------------------------------------------
# Empty nodes-------------------------------------------------------------------
png("images/age_mosaic_gaps.png", width = 800, height = 600, res = 120)
ttc <- read.csv("data/titanic.csv")
ttc$Age <- as.integer(ttc$Age)
ttc$Age[ttc$Age >= 20] <- "a) _________"
ttc$Age[ttc$Age < 20 | ttc$Age %in% c("4", "8", "3", "7", "5", "9", "6")] <- "b) _________"
tbl3 <- prop.table(table(ttc$Age, ttc$Survived))                                 # redo !!!
mosaicplot(tbl3, color=TRUE, main="Überleben | Alter")
dev.off()

png("images/age_mosaic.png", width = 800, height = 600, res = 120)
tbl4 <- prop.table(table(titanic_dt$Age, titanic_dt$Survived))                   # redo !!!
mosaicplot(tbl4, color=TRUE, main="Überleben | Alter")
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