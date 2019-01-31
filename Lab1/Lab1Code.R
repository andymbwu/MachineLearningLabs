#Correlation
library(corrplot)
# Load data
data("mtcars")

# print the first 6 rows
head(data, 6)
correlation <- cor(data)
round(correlation, 2)
corrplot(correlation, method="color")
#try the chart with "pie", "color", and "number"

drink_menu_1 <- ENSF_619_25_Lab01_TimiTomi_menu_nutrition_drinks 
drink_menu_2 <- TimiTomi_drinkMenu_expanded
food_menu <- TimiTomi_menu_nutrition_food

drink_menu <- rbind(drink_menu_1,drink_menu_2)

#load ggplot2
library(ggplot2)

drinks <- drink_menu_2[,c(2)]
sugar_levels <- drink_menu_2[,c(12)]

p <- ggplot(drink_menu_2,aes(x=drinks,y=sugar_levels),scale_x_discrete(),scale_y_discrete())
p+geom_bar()

drink_menu_df <- data.frame(drink_menu_2)

#drink_menu_df$total_fat <- paste(drink_menu_df$Total.Fat..g.,df$s)

p <- ggplot(data=drink_menu_df,aes(x=drinks,y=sugar_levels))+
  geom_bar(stat="identity") + scale_y_continuous() + scale_x_discrete() +
  theme(axis.text.x=element_text(angle=90,face="bold", color="#993333",hjust=1))
p

#Sugar level box plot
sugarlevel_boxplot <- ggplot(drink_menu_df, aes(x=drinks, y=sugar_levels)) +
        geom_boxplot()+ scale_y_continuous(name = "Sugar Levels (mg)") + scale_x_discrete(name ="Drinks")+
  theme(axis.text.x=element_text(angle=90,face="bold", color="#993333",hjust=1))
sugarlevel_boxplot

#Sugar level scatter plot
sugarlevel_scatterplot <- ggplot(drink_menu_df, aes(x=drinks, y=sugar_levels)) +
  geom_point()+ scale_y_continuous(name = "Sugar Levels (mg)") + scale_x_discrete(name ="Drinks")+
  theme(axis.text.x=element_text(angle=90,face="bold", color="#993333",hjust=1))
sugarlevel_scatterplotterplot

#Add menu_item column with combined names
drink_menu_df$Menu_item <- paste(drink_menu_df$Beverage,drink_menu_df$Beverage_prep)

#Drink menu with only overlapping parameters from food menu
drink_menu_df_limited <- data.frame(drink_menu_df$Menu_item,drink_menu_df$Calories,drink_menu_df$Total.Fat..g.,
                            drink_menu_df$Total.Carbohydrates..g.,drink_menu_df$Dietary.Fibre..g.,
                            drink_menu_df$Protein..g.)

colnames(drink_menu_df_limited) <- c("Menu.Items", "Calories","Fat..g.","Carb..g.","Fiber..g.","Protein..g.")
colnames(food_menu) <- c("Menu Items", "Calories","Fat (g)","Carb (g)","Fiber (g)","Protein (g)")
food_menu_df <- data.frame(food_menu,stringsAsFactors=FALSE)

#Combining food and drink menu data
all_items_menu_df <- rbind.data.frame(drink_menu_df_limited,food_menu_df)

drink_violin_plot <- ggplot(drink_menu_df,aes(x=Beverage_category,y=sugar_levels))+
  geom_violin()+
  theme(axis.text.x=element_text(angle=90,face="bold", color="#993333",hjust=1))

#Make dataframes for each drink size
tall_drinks <- data.frame(dplyr::filter(drink_menu_df, grepl('Tall', Beverage_prep)))
short_drinks <- data.frame(dplyr::filter(drink_menu_df, grepl('Short', Beverage_prep)))
grande_drinks <- data.frame(dplyr::filter(drink_menu_df, grepl('Grande', Beverage_prep)))
venti_drinks <- data.frame(dplyr::filter(drink_menu_df, grepl('Venti', Beverage_prep)))

#m[m[, "three"] == 11,]

#Create groups for each drink size in main drink table
drink_menu_df$Sizes <- ifelse(drink_menu_df$Beverage_prep %in% short_drinks$Beverage_prep, 'Short',
                                ifelse(drink_menu_df$Beverage_prep %in% tall_drinks$Beverage_prep,'Tall',
                                       ifelse(drink_menu_df$Beverage_prep %in% grande_drinks$Beverage_prep,'Grande',
                                              'Venti')))
drink_violin <- ggplot(drink_menu_df,aes(x=Beverage_category,y=Sugars..g.,fill=Sizes))+
  geom_violin()+ scale_fill_brewer(palette="Greens") + theme_classic()+
  scale_y_continuous(name = "Sugar Levels (g)") + scale_x_discrete(name ="Drinks")+
  ggtitle("Plot of Sugar Levels by Drink Categories")+
  theme(axis.text.x=element_text(angle=45,face="bold",hjust=1),plot.title = element_text(hjust = 0.5))
  
drink_violin

tall_violin <- ggplot(tall_drinks,aes(x=tall_drinks$Beverage_category,y=tall_drinks$Sugars..g.))+
  geom_violin()+
  theme(axis.text.x=element_text(angle=90,face="bold", color="#993333",hjust=1))+
  scale_y_continuous(name = "Sugar Levels (g)") + scale_x_discrete(name ="Tall Drinks")
tall_violin

short_violin <- ggplot(short_drinks,aes(x=short_drinks$Beverage_category,y=short_drinks$Sugars..g.))+
  geom_violin()+
  theme(axis.text.x=element_text(angle=90,face="bold", color="#993333",hjust=1))+
  scale_y_continuous(name = "Sugar Levels (g)") + scale_x_discrete(name ="Short Drinks")
short_violin

venti_violin <- ggplot(venti_drinks,aes(x=Beverage_category,y=Sugars..g.,fill=Calories))+
  geom_violin()+ theme(axis.text.x=element_text(angle=90,face="bold", color="#993333",hjust=1))+
  scale_y_continuous(name = "Sugar Levels (g)") + scale_x_discrete(name ="Venti Drinks")+
  scale_fill_grey() 
venti_violin



