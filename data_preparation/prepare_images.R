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
basket_dt$Item[basket_dt$Item == "Coffee"] <- "Kaffee"
basket_dt$Item[basket_dt$Item == "Tea"] <- "Tee"
basket_dt$Item[basket_dt$Item == "Bread"] <- "Brot"
basket_dt$Item[basket_dt$Item == "Cake"] <- "Kuchen"
basket_dt$period_day[basket_dt$period_day == "morning"] <- "Vormittag"
basket_dt$period_day[basket_dt$period_day == "afternoon"] <- "Nachmittag"
basket_dt$period_day[basket_dt$period_day == "evening"] <- "Abend"
basket_dt$weekday_weekend[basket_dt$weekday_weekend == "weekday"] <- "Arbeitstag"
basket_dt$weekday_weekend[basket_dt$weekday_weekend == "weekend"] <- "Wochenende"

# absolute values
png("images/drink_daytime_contingency.png", width = 600, height = 300, res = 120)
basket1 <- table(basket_dt$Item[basket_dt$Item == "Kaffee" | basket_dt$Item == "Tee"], 
                 basket_dt$period_day[basket_dt$Item == "Kaffee" | basket_dt$Item == "Tee"]) 
grid.arrange(tableGrob(addmargins(basket1)))
dev.off()

png("images/bread_cake_weekday.png", width = 300, height = 120, res = 70)
basket2 <- table(basket_dt$Item[basket_dt$Item == "Brot" | basket_dt$Item == "Kuchen"], 
                 basket_dt$weekday_weekend[basket_dt$Item == "Brot" | basket_dt$Item == "Kuchen"]) 
grid.arrange(tableGrob(addmargins(basket2)))
dev.off()

# conditional relative values - note that prop.table() divides by total N -> unsuitable
basket1_dt <- data.table(basket1)
names(basket1_dt) <- c("Item", "Tageszeit", "Anzahl")
n_coffee <- sum(basket1_dt$Anzahl[basket1_dt$Item == "Kaffee"])
n_tea <- sum(basket1_dt$Anzahl[basket1_dt$Item == "Tee"])
basket1_dt[, "Summe" := ifelse(Item == "Kaffee", n_coffee, NA)]
basket1_dt[, "Summe" := ifelse(Item == "Tee", n_tea, Summe)]
basket1_dt[, "Bedingte_relative_Häufigkeit" := round(basket1_dt$Anzahl 
                                                     / basket1_dt$Summe * 100, 2)]
coffee_tea_dt <- data.table(Item = c("Kaffee", "Tee"),
                            Vormittag = c(basket1_dt$Bedingte_relative_Häufigkeit[
                              basket1_dt$Item == "Kaffee" & basket1_dt$Tageszeit == "Vormittag"
                            ],
                            basket1_dt$Bedingte_relative_Häufigkeit[
                              basket1_dt$Item == "Tee" & basket1_dt$Tageszeit == "Vormittag"
                            ]),
                            Nachmittag = c(basket1_dt$Bedingte_relative_Häufigkeit[
                              basket1_dt$Item == "Kaffee" & basket1_dt$Tageszeit == "Nachmittag"
                            ],
                            basket1_dt$Bedingte_relative_Häufigkeit[
                              basket1_dt$Item == "Tee" & basket1_dt$Tageszeit == "Nachmittag"
                            ]),
                            Abend = c(basket1_dt$Bedingte_relative_Häufigkeit[
                              basket1_dt$Item == "Kaffee" & basket1_dt$Tageszeit == "Abend"
                            ],
                            basket1_dt$Bedingte_relative_Häufigkeit[
                              basket1_dt$Item == "Tee" & basket1_dt$Tageszeit == "Abend"
                            ]),
                            Summe = c(1, 1))
png("images/coffee_tea_conditional.png", width = 800, height = 300, res = 120)
grid.arrange(tableGrob(coffee_tea_dt))
dev.off()

tree_drinks <- Node$new("N")
  drink1 <- tree_drinks$AddChild("Kaffee")
    morning1 <- drink1$AddChild("Vormittag")
    afternoon1 <- drink1$AddChild("Nachmittag")
    evening1 <- drink1$AddChild("Abend")
  drink2 <- tree_drinks$AddChild("Tee")
    morning2 <- drink2$AddChild("Vormittag ")
    afternoon2 <- drink2$AddChild("Nachmittag ")
    evening2 <- drink2$AddChild("Abend ")
GetEdgeLabel <- function(node) {
  if (node$name == "Kaffee") {
    label = round(n_coffee / (n_coffee + n_tea) * 100, 2)
  } else if (node$name == "Tee") {
    label = round(n_tea / (n_coffee + n_tea) * 100, 2)
  } else if (node$name == "Vormittag") {
    label = basket1_dt$Bedingte_relative_Häufigkeit[basket1_dt$Item == "Kaffee" 
                                                    & basket1_dt$Tageszeit == "Vormittag"]
  } else if (node$name == "Nachmittag"){
    label = basket1_dt$Bedingte_relative_Häufigkeit[basket1_dt$Item == "Kaffee" 
                                                    & basket1_dt$Tageszeit == "Nachmittag"]
  } else if (node$name == "Abend"){
    label = basket1_dt$Bedingte_relative_Häufigkeit[basket1_dt$Item == "Kaffee" 
                                                    & basket1_dt$Tageszeit == "Abend"]
  } else if (node$name == "Vormittag "){
    label = basket1_dt$Bedingte_relative_Häufigkeit[basket1_dt$Item == "Tee" 
                                                    & basket1_dt$Tageszeit == "Vormittag"]
  } else if (node$name == "Nachmittag "){
    label = basket1_dt$Bedingte_relative_Häufigkeit[basket1_dt$Item == "Tee" 
                                                    & basket1_dt$Tageszeit == "Nachmittag"]
  } else if (node$name == "Abend "){
    label = basket1_dt$Bedingte_relative_Häufigkeit[basket1_dt$Item == "Tee" 
                                                    & basket1_dt$Tageszeit == "Abend"]
  }
  return (label)
}
SetEdgeStyle(tree_drinks, fontname = 'helvetica', label = GetEdgeLabel)
SetGraphStyle(tree_drinks, rankdir = "TB")
export_graph(ToDiagrammeRGraph(tree_drinks), "images/coffee_tea_tree.png")

png("images/coffee_tea_mosaic.png", width = 800, height = 600, res = 120)
basket1_dt[,"Zeit_f" := paste0(Tageszeit, ": ", Bedingte_relative_Häufigkeit, "%")]
treemap_test <- treemap(basket1_dt, index = c("Item", "Zeit_f"), 
                        vSize = "Bedingte_relative_Häufigkeit", vColor = "Item")
dev.off()

# For the quiz------------------------------------------------------------------
basket3_dt <- data.table(basket_dt$Item[basket_dt$Item %in% c("Hot chocolate", "Sandwich")],
                         basket_dt$weekday_weekend[basket_dt$Item %in% c("Hot chocolate", 
                                                                         "Sandwich")])
items_total <- nrow(basket3_dt)
weekday_total <- nrow(basket3_dt[basket3_dt$V2 == "weekday"])
weekend_total <- nrow(basket3_dt[basket3_dt$V2 == "weekend"])
sandwich_weekday <- nrow(basket3_dt[basket3_dt$V1 == "Sandwich" 
                                    & basket3_dt$V2 == "weekday"])
chocolate_weekend <- nrow(basket3_dt[basket3_dt$V1 == "Hot chocolate" 
                                   & basket3_dt$V2 == "weekend"])
anteil_weekday <- weekday_total / items_total
anteil_sandwich_weekday <- sandwich_weekday / weekday_total
anteil_chocolate_weekend <- chocolate_weekend / weekend_total
# tree
png("images/item_weekday_tree.png", width = 800, height = 600, res = 120)
my_txt <- init_txt(cond_true_lbl = "Arbeitstag", cond_false_lbl = "Wochenende",
                   hi_lbl = "Heiße Schokolade", mi_lbl = "Sandwich", 
                   fa_lbl = "Heiße Schokolade", cr_lbl = "Sandwich")
item_weekday_tree <- plot_prism(prev = anteil_weekday, sens = anteil_sandwich_weekday, 
                       spec = anteil_chocolate_weekend, by = "cd", f_lbl = "nam", 
                       lbl_txt = my_txt, f_lwd = .5, arr_c = 2)
dev.off()
# mosaic plot
basket3_mosaic_dt <- data.table(Item = c("c) ______________", 
                                         "d) Heiße Schokolade: _______ %", 
                                         "a) ______________", 
                                         "b) Sandwich: _______ %"),
                                Wochentag = c("Arbeitstag", "Wochenende"),
                                Bedingte_relative_Häufigkeit = c(1 - anteil_sandwich_weekday, 
                                                                 anteil_chocolate_weekend, 
                                                                 anteil_sandwich_weekday, 
                                                                 1 - anteil_chocolate_weekend))
png("images/item_weekday_mosaic.png", width = 800, height = 600, res = 120)
treemap_weekday <- treemap(basket3_mosaic_dt, index = c("Wochentag", "Item"), 
                        vSize = "Bedingte_relative_Häufigkeit", vColor = "Item")
dev.off()

# for the programming exercise
n_cakes <- nrow(basket_dt[basket_dt$Item == "Cake"])
n_bread <- nrow(basket_dt[basket_dt$Item == "Bread"])
anteil_cake_weekend <- round(nrow(basket_dt[basket_dt$Item == "Cake" 
                                            & basket_dt$weekday_weekend == "weekend"]) 
                             / n_cakes * 100, 2)
anteil_cake_weekday <- 100 - anteil_cake_weekend
bread_weekday <- nrow(basket_dt[basket_dt$Item == "Bread" 
                                & basket_dt$weekday_weekend == "weekday"])
anteil_bread_weekday <- round(bread_weekday / n_bread * 100, 2)
anteil_bread_weekend <- 100 - anteil_bread_weekday
dummy_dt <- data.table(Item = c("Kuchen", "Brot", "Summe"),
                       Arbeitstag = c("(1)", "(4)", "(7)"),
                       Wochenende = c("(2)", "(5)", "(8)"),
                       Summe = c("(3)", "(6)", " "))
png("images/test_dummy.png", width = 800, height = 300, res = 120)
grid.arrange(tableGrob(dummy_dt))
dev.off()

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
    deceased1 <- class1$AddChild(" Nicht Überlebt")
  class2 <- tree_class$AddChild("2. Klasse")
    survived2 <- class2$AddChild("Überlebt ")
    deceased2 <- class2$AddChild("Nicht Überlebt ")
  class3 <- tree_class$AddChild("3. Klasse")
    survived3 <- class3$AddChild("Überlebt")
    deceased3 <- class3$AddChild("Nicht Überlebt")

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

# Creating exercise-------------------------------------------------------------

png("images/tea_coffee_weekday.png", width = 300, height = 120, res = 70)
basket4 <- table(basket_dt$Item[basket_dt$Item == "Tee" | basket_dt$Item == "Kaffee"], 
                 basket_dt$weekday_weekend[basket_dt$Item == "Tee" | basket_dt$Item == "Kaffee"]) 
grid.arrange(tableGrob(addmargins(basket4)))
dev.off()

# mosaic erstellen result
anteil_kaffee_arbeitstag <- round(nrow(basket_dt[basket_dt$Item == "Kaffee" 
                                           & basket_dt$weekday_weekend == "Arbeitstag"]
                                       ) / nrow(basket_dt[basket_dt$Item == "Kaffee"]) * 100, 2)
anteil_tee_arbeitstag <- round(nrow(basket_dt[basket_dt$Item == "Tee" 
                                              & basket_dt$weekday_weekend == "Arbeitstag"]
                                    ) / nrow(basket_dt[basket_dt$Item == "Tee"])* 100, 2)

png("images/tea_coffee_day_mosaic.png", width = 800, height = 600, res = 120)
mosaic_dt <- data.table(Item = c(paste0("Kaffee: ", anteil_kaffee_arbeitstag, "%"), 
                                 paste0("Kaffee: ", 100 - anteil_kaffee_arbeitstag, "%"),
                                 paste0("Tee: ", anteil_tee_arbeitstag, "%"),
                                 paste0("Tee: ", 100 - anteil_tee_arbeitstag, "%")),
                        Wochentag = c("Arbeitstag", "Wochenende"),
                        Bedingte_relative_Häufigkeit = c(anteil_kaffee_arbeitstag, 
                                                         100 - anteil_kaffee_arbeitstag, 
                                                         anteil_tee_arbeitstag, 
                                                         100 - anteil_tee_arbeitstag))
treemap_weekday <- treemap(mosaic_dt, index = c("Wochentag", "Item"), 
                           vSize = "Bedingte_relative_Häufigkeit", vColor = "Item")
dev.off()
