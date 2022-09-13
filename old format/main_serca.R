#Install and load packages
if(!require('MESS')) 
{
  install.packages('MESS')
  library('MESS')
}
if(!require('readr')) 
{
  install.packages('readr')
  library('readr')
}
if(!require('ggplot2')) 
{
  install.packages('ggplot2')
  library('ggplot2')
}
if(!require('dplyr')) 
{
  install.packages('dplyr')
  library('dplyr')
}

#local variables
#sheet0 will be our main excel sheet
sheet0 = read_csv("clean_data.csv")

#create the main dataframe
main_df <- data.frame(sheet0$`wd#`, sheet0$gray_1, sheet0$gray_2, sheet0$gray_3)

#create the mCherry dataframe
mCh_df <- subset(sheet0, condition=="mCherry")
mCh_wd1 <- subset(mCh_df, `wd#`==1)
mCh_wd2 <- subset(mCh_df, `wd#`==2)
mCh_wd3 <- subset(mCh_df, `wd#`==3)

#create the SERCA dataframe
SERCA_df <- subset(sheet0, condition=="SERCA")
SERCA_wd1 <- subset(SERCA_df, `wd#`==1)
SERCA_wd2 <- subset(SERCA_df, `wd#`==2)
SERCA_wd3 <- subset(SERCA_df, `wd#`==3)

#assign the x dataset as the distance in microns values
x1 <- sheet0$`Distance_(microns)`
#mCh_x1_1 is the distance value for wd1 of the mCherry dataset
mCh_x1_1 <- x1[1:889]
#mCh_x1_2 is the distance value for wd2 of the mCherry dataset
mCh_x1_2 <- x1[890:1773]
#mCh_x1_3 is the distance value for wd3 of the mCherry dataset
mCh_x1_3 <- x1[1774:2659]
#SERCA_x1_1 is the distance values for wd1 of the SERCA dataset
SERCA_x1_1 <- x1[2660:3548]
#SERCA_x1_2 is the distance values for wd2 of the SERCA dataset
SERCA_x1_2 <- x1[3549:4433]
#SERCA_x1_3 is the distance values for wd3 of the SERCA dataset
SERCA_x1_3 <- x1[4434:5317]

#assign y1 dataset as the 1st set of gray values
y1 <- sheet0$gray_1
#mCh_y1_1 is the 1st set of gray values from wd1 of the mCherry dataset
mCh_y1_1 <- y1[1:889]
#mCh_y1_2 is the 1st set of gray values from wd2 of the mCherry dataset
mCh_y1_2 <- y1[890:1773]
#mCh_y1_3 is the 1st set of gray values from wd3 of the mCherry dataset
mCh_y1_3 <- y1[1774:2659]
#SERCA_y1_1 is the 1st set of gray values from wd1 of the SERCA dataset
SERCA_y1_1 <- y1[2660:3548]
#SERCA_y1_2 is the 1st set of gray values from wd2 of the SERCA dataset
SERCA_y1_2 <- y1[3549:4433]
#SERCA_y1_3 is the 1st set of gray values from wd3 of the SERCA dataset
SERCA_y1_3 <- y1[4434:5317]

#assign y2 dataset as the 2nd set of gray values
y2 <- sheet0$gray_2
#mCh_y2_1 is the 2nd set of gray values from wd1 of the mCherry dataset
mCh_y2_1 <- y2[1:889]
#mCh_y2_2 is the 2nd set of gray values from wd2 of the mCherry dataset
mCh_y2_2 <- y2[890:1773]
#mCh_y2_3 is the 2nd set of gray values from wd3 of the mCherry dataset
mCh_y2_3 <- y2[1774:2659]
#SERCA_y2_1 is the 2nd set of gray values from wd1 of the SERCA dataset
SERCA_y2_1 <- y2[2660:3548]
#SERCA_y2_2 is the 2nd set of gray values from wd2 of the SERCA dataset
SERCA_y2_2 <- y2[3549:4433]
#SERCA_y2_3 is the 2nd set of gray values from wd3 of the SERCA dataset
SERCA_y2_3 <- y2[4434:5317]

#assign y3 dataset as the 3rd set of gray values
y3 <- sheet0$gray_3
#mCh_y3_1 is the 3rd set of gray values from wd1 of the mCherry dataset
mCh_y3_1 <- y3[1:889]
#mCh_y3_2 is the 3rd set of gray values from wd2 of the mCherry dataset
mCh_y3_2 <- y3[890:1773]
#mCh_y3_3 is the 3rd set of gray values from wd3 of the mCherry dataset
mCh_y3_3 <- y3[1774:2659]
#SERCA_y3_1 is the 3rd set of gray values from wd1 of the SERCA dataset
SERCA_y3_1 <- y3[2660:3548]
#SERCA_y3_2 is the 3rd set of gray values from wd2 of the SERCA dataset
SERCA_y3_2 <- y3[3549:4433]
#SERCA_y3_3 is the 3rd set of gray values from wd3 of the SERCA dataset
SERCA_y3_3 <- y3[4434:5317]

#we will use the MESS packages AUC function to fit a spline to our curve
# https://www.rdocumentation.org/packages/MESS/versions/0.5.9/topics/auc
# to change the x minimum and maximum use the from and to variables
# this allows the user to specify areas on the graph to calculate auc
#auc(x,y, from = 0.1, to = 2, type = "spline")

#we will do an auc calculation for each x and y dataset
#mCh wd1 to wd3 for gray values 1 to 3
#mCherry wing disc 1 gray values dataset 1
auc_mCh_1_1 <- auc(mCh_x1_1, mCh_y1_1, from = 0, to = mCh_x1_1[length(mCh_x1_1)]
                   , type = "spline", subdivisions = 10000)
#mCherry wing disc 2 gray values dataset 1
auc_mCh_1_2 <- auc(mCh_x1_2, mCh_y1_2, from = 0, to = mCh_x1_2[length(mCh_x1_2)]
                   , type = "spline", subdivisions = 10000)
#mCherry wing disc 3 gray values dataset 1
auc_mCh_1_3 <- auc(mCh_x1_3, mCh_y1_3, from = 0, to = mCh_x1_3[length(mCh_x1_3)]
                   , type = "spline", subdivisions = 10000)
#mCherry wing disc 1 gray values dataset 2
auc_mCh_2_1 <- auc(mCh_x1_1, mCh_y2_1, from = 0, to = mCh_x1_1[length(mCh_x1_1)]
                   , type = "spline", subdivisions = 10000)
#mCherry wing disc 2 gray values dataset 2
auc_mCh_2_2 <- auc(mCh_x1_2, mCh_y2_2, from = 0, to = mCh_x1_2[length(mCh_x1_2)]
                   , type = "spline", subdivisions = 10000)
#mCherry wing disc 3 gray values dataset 2
auc_mCh_2_3 <- auc(mCh_x1_3, mCh_y2_3, from = 0, to = mCh_x1_3[length(mCh_x1_3)]
                   , type = "spline", subdivisions = 10000)
#mCherry wing disc 1 gray values dataset 3
auc_mCh_3_1 <- auc(mCh_x1_1, mCh_y3_1, from = 0, to = mCh_x1_1[length(mCh_x1_1)]
                   , type = "spline", subdivisions = 10000)
#mCherry wing disc 2 gray values dataset 3
auc_mCh_3_2 <- auc(mCh_x1_2, mCh_y3_2, from = 0, to = mCh_x1_2[length(mCh_x1_2)]
                   , type = "spline", subdivisions = 10000)
#mCherry wing disc 3 gray values dataset 3
auc_mCh_3_3 <- auc(mCh_x1_3, mCh_y3_3, from = 0, to = mCh_x1_3[length(mCh_x1_3)]
                   , type = "spline", subdivisions = 10000)

#create an array to keep up with all of the mCh auc values
mCh_auc_values <- c(auc_mCh_1_1, auc_mCh_1_2, auc_mCh_1_3, 
                   auc_mCh_2_1, auc_mCh_2_2, auc_mCh_2_3,
                   auc_mCh_3_1, auc_mCh_3_2, auc_mCh_3_3)

#create an array to keep the averaged gray SERCA auc values per wing disc
avg_mCh_auc_values <- c(mean(auc_mCh_1_1, auc_mCh_2_1, auc_mCh_3_1), 
                        mean(auc_mCh_1_2, auc_mCh_2_2, auc_mCh_3_2),
                        mean(auc_mCh_1_3, auc_mCh_2_3, auc_mCh_3_3))

#SERCA wd1 to wd3 for gray values 1 to 3
#SERCA wing disc 1 gray values dataset 1
auc_SERCA_1_1 <- auc(SERCA_x1_1, SERCA_y1_1, from = 0, 
                     to = SERCA_x1_1[length(SERCA_x1_1)]
                   , type = "spline", subdivisions = 10000)
#SERCA wing disc 2 gray values dataset 1
auc_SERCA_1_2 <- auc(SERCA_x1_2, SERCA_y1_2, from = 0, 
                     to = SERCA_x1_2[length(SERCA_x1_2)]
                   , type = "spline", subdivisions = 10000)
#SERCA wing disc 3 gray values dataset 1
auc_SERCA_1_3 <- auc(SERCA_x1_3, SERCA_y1_3, from = 0, 
                     to = SERCA_x1_3[length(SERCA_x1_3)]
                   , type = "spline", subdivisions = 10000)
#SERCA wing disc 1 gray values dataset 2
auc_SERCA_2_1 <- auc(SERCA_x1_1, SERCA_y2_1, from = 0, 
                     to = SERCA_x1_1[length(SERCA_x1_1)]
                   , type = "spline", subdivisions = 10000)
#SERCA wing disc 2 gray values dataset 2
auc_SERCA_2_2 <- auc(SERCA_x1_2, SERCA_y2_2, from = 0, 
                     to = SERCA_x1_2[length(SERCA_x1_2)]
                   , type = "spline", subdivisions = 10000)
#SERCA wing disc 3 gray values dataset 2
auc_SERCA_2_3 <- auc(SERCA_x1_3, SERCA_y2_3, from = 0, 
                     to = SERCA_x1_3[length(SERCA_x1_3)]
                   , type = "spline", subdivisions = 10000)
#SERCA wing disc 1 gray values dataset 3
auc_SERCA_3_1 <- auc(SERCA_x1_1, SERCA_y3_1, from = 0, 
                     to = SERCA_x1_1[length(SERCA_x1_1)]
                   , type = "spline", subdivisions = 10000)
#SERCA wing disc 2 gray values dataset 3
auc_SERCA_3_2 <- auc(SERCA_x1_2, SERCA_y3_2, from = 0, 
                     to = SERCA_x1_2[length(SERCA_x1_2)]
                   , type = "spline", subdivisions = 10000)
#SERCA wing disc 3 gray values dataset 3
auc_SERCA_3_3 <- auc(SERCA_x1_3, SERCA_y3_3, from = 0, 
                     to = SERCA_x1_3[length(SERCA_x1_3)], 
                     type = "spline", subdivisions = 10000)

#create an array to keep up with all of the SERCA auc values
SERCA_auc_values <- c(auc_SERCA_1_1, auc_SERCA_1_2, auc_SERCA_1_3, 
                      auc_SERCA_2_1, auc_SERCA_2_2, auc_SERCA_2_3,
                      auc_SERCA_3_1, auc_SERCA_3_2, auc_SERCA_3_3)

#create an array to keep the averaged gray SERCA auc values per wing disc
avg_SERCA_auc_values <- c(mean(auc_SERCA_1_1, auc_SERCA_2_1, auc_SERCA_3_1), 
                          mean(auc_SERCA_1_2, auc_SERCA_2_2, auc_SERCA_3_2),
                          mean(auc_SERCA_1_3, auc_SERCA_2_3, auc_SERCA_3_3))

#ggplots
#the original mCherry vs SERCA line plot
i <- ggplot() + 
  geom_line(data=sheet0, aes(`Distance_(microns)`, gray_avg, color=condition)) +
  labs(title="mCherry vs SERCA RNAi", x="Position (um)", y="Fluorescence (AU)")
print(i)

#save the original plot copy in the current directory
ggsave(file="SERCA_mCherry_gray_values_original.png", plot=i)

#the mCherry vs SERCA AUC dot plots
Identifier<-c("mCherry wd1", "mCherry wd1", "mCherry wd1", 
              "mCherry wd2", "mCherry wd2", "mCherry wd2", 
              "mCherry wd3", "mCherry wd3", "mCherry wd3",
              "SERCA wd1", "SERCA wd1", "SERCA wd1", 
              "SERCA wd2", "SERCA wd2", "SERCA wd2", 
              "SERCA wd3", "SERCA wd3", "SERCA wd3")

AUC_Value <- c(mCh_auc_values, SERCA_auc_values)

dotplot_df = data.frame(Identifier, AUC_Value)

j <- ggplot() + 
  geom_dotplot(data=dotplot_df, 
               aes(Identifier, AUC_Value, fill=Identifier),
               binaxis="y", stackdir="center") +
  labs(title="AUC mCherry vs SERCA RNAi", y="AUC Sum Value", x="Identifier")
print(j)

#save the AUC dot plot in the current directory
ggsave(file="AUC_SERCA_mCherry_gray_values_dotplot.png", plot=j)

#the averaged mCherry vs SERCA AUC dot plots
Condition<-c("mCherry", "mCherry", "mCherry",
              "SERCA", "SERCA", "SERCA")

Avg_AUC_Value <- c(avg_mCh_auc_values, avg_SERCA_auc_values)

avg_dotplot_df <- data.frame(Condition, Avg_AUC_Value)

k <- ggplot() + 
  geom_dotplot(data=avg_dotplot_df, 
               aes(Condition, Avg_AUC_Value, fill=Condition),
               binaxis="y", stackdir="center") +
  labs(title="AUC mCherry vs SERCA RNAi", y="AUC Average Sum Value",
       x="Condition")
print(k)

#save the AUC dot plot in the current directory
ggsave(file="avg_AUC_SERCA_mCherry_gray_values_dotplot.png", plot=k)
