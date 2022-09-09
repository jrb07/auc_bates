#Install and load packages using the if require template to reduce overhead
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
if(!require('tidyr')) 
{
  install.packages('tidyr')
  library('tidyr')
}

#sheet0 will be our main excel sheet
sheet0 = read_csv("clean_data_.csv")

#Use grepl to grab all column data that matches a search string to create dfs
position_df <- sheet0[ , grepl("POSITION", names(sheet0))]
old_mCherry_df <- sheet0[ , grepl("old_mCherry",names(sheet0))]
new_mCherry_df <- sheet0[ , grepl("new_mCherry",names(sheet0))]
old_orai_df <- sheet0[ , grepl("old_orai",names(sheet0))]
new_orai_df <- sheet0[ , grepl("new_orai",names(sheet0))]
stim_df <- sheet0[ , grepl("stim",names(sheet0))]
wt_df <- sheet0[ , grepl("WT",names(sheet0))]
best2_df <- sheet0[ , grepl("Best2",names(sheet0))]
serca_df <- sheet0[ , grepl("SERCA_",names(sheet0))]
sercamCherry_df <- sheet0[ , grepl("SERCAmCherry_",names(sheet0))]

#we will use the MESS packages AUC function to fit a spline to our curve
# https://www.rdocumentation.org/packages/MESS/versions/0.5.9/topics/auc
# to change the x minimum and maximum use the from and to variables
# this allows the user to specify areas on the graph to calculate auc
#auc(x,y, from = 0.1, to = 2, type = "spline")

#create an empty vector that is as long as the dataset dataframe
old_mCherry_aucs <- rep(0, ncol(old_mCherry_df))
zero_to_60_old_mCherry_aucs <- rep(0, ncol(old_mCherry_df))
sixty_to_end_old_mCherry_aucs <- rep(0, ncol(old_mCherry_df))
#we will do an auc calculation for distance vs each dataset
for(i in 1:ncol(old_mCherry_df))
{
  x <- position_df$POSITION
  y <- old_mCherry_df[, i, drop=TRUE] #drop to make a single column
  old_mCherry_aucs[i] <- auc(x, y, from = 0, type = "spline",
                             subdivisions = 1000, absolutearea = TRUE)
  zero_to_60_old_mCherry_aucs[i] <- auc(x, y, from = 0, to = 60 , 
                                        type = "spline", subdivisions = 1000,
                                        absolutearea = TRUE)
  sixty_to_end_old_mCherry_aucs[i] <- auc(x, y, from = 60, 
                                          type = "spline", subdivisions = 1000,
                                          absolutearea = TRUE)
}
new_mCherry_aucs <- rep(0, ncol(new_mCherry_df))
zero_to_60_new_mCherry_aucs <- rep(0, ncol(new_mCherry_df))
sixty_to_end_new_mCherry_aucs <- rep(0, ncol(new_mCherry_df))
for(i in 1:ncol(new_mCherry_df))
{
  x <- position_df$POSITION
  y <- new_mCherry_df[, i, drop=TRUE] #drop to make a single column
  new_mCherry_aucs[i] <- auc(x, y, from = 0, type = "spline",
                             subdivisions = 1000, absolutearea = TRUE)
  zero_to_60_new_mCherry_aucs[i] <- auc(x, y, from = 0, to = 60 , 
                                     type = "spline", subdivisions = 1000,
                                     absolutearea = TRUE)
  sixty_to_end_new_mCherry_aucs[i] <- auc(x, y, from = 60, 
                                       type = "spline", subdivisions = 1000,
                                       absolutearea = TRUE)
}
old_orai_aucs <- rep(0, ncol(old_orai_df))
zero_to_60_old_orai_aucs <- rep(0, ncol(old_orai_df))
sixty_to_end_old_orai_aucs <- rep(0, ncol(old_orai_df))
for(i in 1:ncol(old_orai_df))
{
  x <- position_df$POSITION
  y <- old_orai_df[, i, drop=TRUE] #drop to make a single column
  old_orai_aucs[i] <- auc(x, y, from = 0, type = "spline",
                             subdivisions = 1000, absolutearea = TRUE)
  zero_to_60_old_orai_aucs[i] <- auc(x, y, from = 0, to = 60 , 
                                     type = "spline", subdivisions = 1000,
                                     absolutearea = TRUE)
  sixty_to_end_old_orai_aucs[i] <- auc(x, y, from = 60, 
                                       type = "spline", subdivisions = 1000,
                                       absolutearea = TRUE)
}
new_orai_aucs <- rep(0, ncol(new_orai_df))
zero_to_60_new_orai_aucs <- rep(0, ncol(new_orai_df))
sixty_to_end_new_orai_aucs <- rep(0, ncol(new_orai_df))
for(i in 1:ncol(new_orai_df))
{
  x <- position_df$POSITION
  y <- new_orai_df[, i, drop=TRUE] #drop to make a single column
  new_orai_aucs[i] <- auc(x, y, from = 0, type = "spline",
                             subdivisions = 1000, absolutearea = TRUE)
  zero_to_60_new_orai_aucs[i] <- auc(x, y, from = 0, to = 60 , 
                                 type = "spline", subdivisions = 1000,
                                 absolutearea = TRUE)
  sixty_to_end_new_orai_aucs[i] <- auc(x, y, from = 60, 
                                   type = "spline", subdivisions = 1000,
                                   absolutearea = TRUE)
}
stim_aucs <- rep(0, ncol(stim_df))
zero_to_60_stim_aucs <- rep(0, ncol(stim_df))
sixty_to_end_stim_aucs <- rep(0, ncol(stim_df))
for(i in 1:ncol(stim_df))
{
  x <- position_df$POSITION
  y <- stim_df[, i, drop=TRUE] #drop to make a single column
  stim_aucs[i] <- auc(x, y, from = 0, type = "spline",
                             subdivisions = 1000, absolutearea = TRUE)
  zero_to_60_stim_aucs[i] <- auc(x, y, from = 0, to = 60 , 
                                  type = "spline", subdivisions = 1000,
                                  absolutearea = TRUE)
  sixty_to_end_stim_aucs[i] <- auc(x, y, from = 60, 
                                    type = "spline", subdivisions = 1000,
                                    absolutearea = TRUE)
}
best2_aucs <- rep(0, ncol(best2_df))
zero_to_60_best2_aucs <- rep(0, ncol(best2_df))
sixty_to_end_best2_aucs <- rep(0, ncol(best2_df))
for(i in 1:ncol(best2_df))
{
  x <- position_df$POSITION
  y <- best2_df[, i, drop=TRUE] #drop to make a single column
  best2_aucs[i] <- auc(x, y, from = 0, type = "spline",
                             subdivisions = 1000, absolutearea = TRUE)
  zero_to_60_best2_aucs[i] <- auc(x, y, from = 0, to = 60 , 
                               type = "spline", subdivisions = 1000,
                               absolutearea = TRUE)
  sixty_to_end_best2_aucs[i] <- auc(x, y, from = 60, 
                                 type = "spline", subdivisions = 1000,
                                 absolutearea = TRUE)
}
wt_aucs <- rep(0, ncol(wt_df))
zero_to_60_wt_aucs <- rep(0, ncol(wt_df))
sixty_to_end_wt_aucs <- rep(0, ncol(wt_df))
for(i in 1:ncol(wt_df))
{
  x <- position_df$POSITION
  y <- wt_df[, i, drop=TRUE] #drop to make a single column
  wt_aucs[i] <- auc(x, y, from = 0, 
                    type = "spline", subdivisions = 1000,
                    absolutearea = TRUE)
  zero_to_60_wt_aucs[i] <- auc(x, y, from = 0, to = 60 , 
                               type = "spline", subdivisions = 1000,
                               absolutearea = TRUE)
  sixty_to_end_wt_aucs[i] <- auc(x, y, from = 60, 
                                 type = "spline", subdivisions = 1000,
                                 absolutearea = TRUE)
}
serca_aucs <- rep(0, ncol(serca_df))
zero_to_60_serca_aucs <- rep(0, ncol(serca_df))
sixty_to_end_serca_aucs <- rep(0, ncol(serca_df))
for(i in 1:ncol(serca_df))
{
  x <- position_df$POSITION
  y <- serca_df[, i, drop=TRUE] #drop to make a single column
  serca_aucs[i] <- auc(x, y, from = 0, 
                    type = "spline", subdivisions = 1000,
                    absolutearea = TRUE)
  zero_to_60_serca_aucs[i] <- auc(x, y, from = 0, to = 60 , 
                               type = "spline", subdivisions = 1000,
                               absolutearea = TRUE)
  sixty_to_end_serca_aucs[i] <- auc(x, y, from = 60, 
                                 type = "spline", subdivisions = 1000,
                                 absolutearea = TRUE)
}
sercamCherry_aucs <- rep(0, ncol(sercamCherry_df))
zero_to_60_sercamCherry_aucs <- rep(0, ncol(sercamCherry_df))
sixty_to_end_sercamCherry_aucs <- rep(0, ncol(sercamCherry_df))
for(i in 1:ncol(sercamCherry_df))
{
  x <- position_df$POSITION
  y <- sercamCherry_df[, i, drop=TRUE] #drop to make a single column
  sercamCherry_aucs[i] <- auc(x, y, from = 0, 
                       type = "spline", subdivisions = 1000,
                       absolutearea = TRUE)
  zero_to_60_sercamCherry_aucs[i] <- auc(x, y, from = 0, to = 60 , 
                                  type = "spline", subdivisions = 1000,
                                  absolutearea = TRUE)
  sixty_to_end_sercamCherry_aucs[i] <- auc(x, y, from = 60, 
                                    type = "spline", subdivisions = 1000,
                                    absolutearea = TRUE)
}
#ggplots
#create an Identifier (x label) array that will mirror the AUC values array
Identifier<-c(rep("wt", length(wt_aucs)),
              rep("old mCherry", length(old_mCherry_aucs)),
              rep("new mCherry", length(new_mCherry_aucs)),
              rep("old orai", length(old_orai_aucs)),
              rep("new orai", length(new_orai_aucs)),
              rep("stim", length(stim_aucs)),
              rep("best2", length(best2_aucs)),
              rep("SERCA", length(serca_aucs)),
              rep("SERCA-mCherry", length(sercamCherry_aucs)))

AUC_Values <- c(wt_aucs, old_mCherry_aucs, new_mCherry_aucs,
                old_orai_aucs, new_orai_aucs, stim_aucs, best2_aucs,
                serca_aucs, sercamCherry_aucs)
# all distances in microns plot
#create a dataframe that will hold both arrays and be saved as a csv file
dotplot_df = data.frame(Identifier, AUC_Values)

j <- ggplot() + 
  geom_dotplot(data=dotplot_df, 
               aes(x = reorder(Identifier , -AUC_Values), AUC_Values,
                   fill=Identifier), binaxis="y", stackdir="center") + 
  labs(title="AUC of Distance vs Genotype", y="AUC Value") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x.bottom=element_blank(),
        legend.position="bottom")
print(j)

#save the AUC dot plot in the current directory
ggsave(file="AUC_gray_values_dotplot.png", plot=j,
       width=300, height=100, units="mm")
# 0 to 60 distance in microns plot
#create an Identifier (x label) array that will mirror the AUC values array
Identifier_0_to_60 <-c(rep("wt", length(zero_to_60_wt_aucs)),
                       rep("old mCherry", length(zero_to_60_old_mCherry_aucs)),
                       rep("new mCherry", length(zero_to_60_new_mCherry_aucs)),
                       rep("old orai", length(zero_to_60_old_orai_aucs)),
                       rep("new orai", length(zero_to_60_new_orai_aucs)),
                       rep("stim", length(zero_to_60_stim_aucs)),
                       rep("best2", length(zero_to_60_best2_aucs)),
                       rep("SERCA", length(zero_to_60_serca_aucs)),
                       rep("SERCA-mCherry", length(zero_to_60_sercamCherry_aucs)))

AUC_Values_0_to_60 <- c(zero_to_60_wt_aucs, zero_to_60_old_mCherry_aucs,
                        zero_to_60_new_mCherry_aucs, zero_to_60_old_orai_aucs,
                        zero_to_60_new_orai_aucs, zero_to_60_stim_aucs,
                        zero_to_60_best2_aucs,
                        zero_to_60_serca_aucs, zero_to_60_sercamCherry_aucs)
#create a dataframe that will hold both arrays and be saved as a csv file
zero_to_60_dotplot_df = data.frame(Identifier_0_to_60, AUC_Values_0_to_60)

r <- ggplot() + 
  geom_dotplot(data=zero_to_60_dotplot_df, 
               aes(x = reorder(Identifier_0_to_60 , -AUC_Values_0_to_60),
                   AUC_Values_0_to_60,
                   fill=Identifier_0_to_60), binaxis="y", stackdir="center") + 
  labs(title="AUC of Distance vs Genotype (Distance 0 to 60 microns)", y="AUC Value") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x.bottom=element_blank(),
        legend.position="bottom")
print(r)

#save the AUC dot plot in the current directory
ggsave(file="0_to_60_AUC_gray_values_dotplot.png", plot=r,
       width=300, height=100, units="mm")

#save the dotplot dataframe as a new table
write.csv(dotplot_df, file = "0_to_60_auc_out.csv")
# 60 to end distance in microns plot
#create an Identifier (x label) array that will mirror the AUC values array
Identifier_60_to_end <-c(rep("wt", length(sixty_to_end_wt_aucs)),
                       rep("old mCherry", length(sixty_to_end_old_mCherry_aucs)),
                       rep("new mCherry", length(sixty_to_end_new_mCherry_aucs)),
                       rep("old orai", length(sixty_to_end_old_orai_aucs)),
                       rep("new orai", length(sixty_to_end_new_orai_aucs)),
                       rep("stim", length(sixty_to_end_stim_aucs)),
                       rep("best2", length(sixty_to_end_best2_aucs)),
                       rep("SERCA", length(sixty_to_end_serca_aucs)),
                       rep("SERCA-mCherry", length(sixty_to_end_sercamCherry_aucs)))

AUC_Values_60_to_end <- c(sixty_to_end_wt_aucs, sixty_to_end_old_mCherry_aucs,
                          sixty_to_end_new_mCherry_aucs, sixty_to_end_old_orai_aucs,
                          sixty_to_end_new_orai_aucs, sixty_to_end_stim_aucs,
                          sixty_to_end_best2_aucs,
                          sixty_to_end_serca_aucs, sixty_to_end_sercamCherry_aucs)
#create a dataframe that will hold both arrays and be saved as a csv file
sixty_to_end_dotplot_df = data.frame(Identifier_60_to_end, AUC_Values_60_to_end)

b <- ggplot() + 
  geom_dotplot(data=sixty_to_end_dotplot_df, 
               aes(x = reorder(Identifier_60_to_end , -AUC_Values_60_to_end),
                   AUC_Values_60_to_end,
                   fill=Identifier_60_to_end), binaxis="y", stackdir="center") + 
  labs(title="AUC of Distance vs Genotype (Distance 60 to max in microns)", y="AUC Value") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x.bottom=element_blank(),
        legend.position="bottom")
print(b)

#save the AUC dot plot in the current directory
ggsave(file="60_to_end_AUC_gray_values_dotplot.png", plot=b,
       width=300, height=100, units="mm")

#save the dotplot dataframe as a new table
write.csv(dotplot_df, file = "total_distance_auc_out.csv")
write.csv(zero_to_60_dotplot_df, file = "0-60_distance_auc_out.csv")
write.csv(sixty_to_end_dotplot_df, file = "60-end_distance_auc_out.csv")

#generate citations
citation() # R citation
#package citations
citation("MESS")
citation("dplyr")
citation("tidyr")
citation("readr")
citation("ggplot2")
