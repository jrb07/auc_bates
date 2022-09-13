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

#assign cutoff values for the 1st peak which will be the start of peak 2
stim_peak1_cutoff <- 48
new_orai_peak1_cutoff <- 46
old_orai_peak1_cutoff <- 46
serca_peak1_cutoff <- 70
sercamCherry_peak1_cutoff <- 70
wt_peak1_cutoff <- 50
best2_peak1_cutoff <- 70
old_mCherry_peak1_cutoff <- 46
new_mCherry_peak1_cutoff <- 46




#we will use the MESS packages AUC function to fit a spline to our curve
# https://www.rdocumentation.org/packages/MESS/versions/0.5.9/topics/auc
# to change the x minimum and maximum use the from and to variables
# this allows the user to specify areas on the graph to calculate auc
#auc(x,y, from = 0.1, to = 2, type = "spline")

#create an empty vector that is as long as the dataset dataframe
old_mCherry_aucs <- rep(0, ncol(old_mCherry_df))
peak1_old_mCherry_aucs <- rep(0, ncol(old_mCherry_df))
peak2_old_mCherry_aucs <- rep(0, ncol(old_mCherry_df))
#we will do an auc calculation for distance vs each dataset
for(i in 1:ncol(old_mCherry_df))
{
  x <- position_df$POSITION
  y <- old_mCherry_df[, i, drop=TRUE] #drop to make a single column
  old_mCherry_aucs[i] <- auc(x, y, from = 0, type = "spline",
                             subdivisions = 1000, absolutearea = TRUE)
  peak1_old_mCherry_aucs[i] <- auc(x, y, from = 0, to = old_mCherry_peak1_cutoff, 
                                        type = "spline", subdivisions = 1000,
                                        absolutearea = TRUE)
  peak2_old_mCherry_aucs[i] <- auc(x, y, from = old_mCherry_peak1_cutoff, 
                                          type = "spline", subdivisions = 1000,
                                          absolutearea = TRUE)
}
new_mCherry_aucs <- rep(0, ncol(new_mCherry_df))
peak1_new_mCherry_aucs <- rep(0, ncol(new_mCherry_df))
peak2_new_mCherry_aucs <- rep(0, ncol(new_mCherry_df))
for(i in 1:ncol(new_mCherry_df))
{
  x <- position_df$POSITION
  y <- new_mCherry_df[, i, drop=TRUE] #drop to make a single column
  new_mCherry_aucs[i] <- auc(x, y, from = 0, type = "spline",
                             subdivisions = 1000, absolutearea = TRUE)
  peak1_new_mCherry_aucs[i] <- auc(x, y, from = 0, to = new_mCherry_peak1_cutoff, 
                                     type = "spline", subdivisions = 1000,
                                     absolutearea = TRUE)
  peak2_new_mCherry_aucs[i] <- auc(x, y, from = new_mCherry_peak1_cutoff, 
                                       type = "spline", subdivisions = 1000,
                                       absolutearea = TRUE)
}
old_orai_aucs <- rep(0, ncol(old_orai_df))
peak1_old_orai_aucs <- rep(0, ncol(old_orai_df))
peak2_old_orai_aucs <- rep(0, ncol(old_orai_df))
for(i in 1:ncol(old_orai_df))
{
  x <- position_df$POSITION
  y <- old_orai_df[, i, drop=TRUE] #drop to make a single column
  old_orai_aucs[i] <- auc(x, y, from = 0, type = "spline",
                             subdivisions = 1000, absolutearea = TRUE)
  peak1_old_orai_aucs[i] <- auc(x, y, from = 0, to = old_orai_peak1_cutoff , 
                                     type = "spline", subdivisions = 1000,
                                     absolutearea = TRUE)
  peak2_old_orai_aucs[i] <- auc(x, y, from = old_orai_peak1_cutoff, 
                                       type = "spline", subdivisions = 1000,
                                       absolutearea = TRUE)
}
new_orai_aucs <- rep(0, ncol(new_orai_df))
peak1_new_orai_aucs <- rep(0, ncol(new_orai_df))
peak2_new_orai_aucs <- rep(0, ncol(new_orai_df))
for(i in 1:ncol(new_orai_df))
{
  x <- position_df$POSITION
  y <- new_orai_df[, i, drop=TRUE] #drop to make a single column
  new_orai_aucs[i] <- auc(x, y, from = 0, type = "spline",
                             subdivisions = 1000, absolutearea = TRUE)
  peak1_new_orai_aucs[i] <- auc(x, y, from = 0, to = new_orai_peak1_cutoff , 
                                 type = "spline", subdivisions = 1000,
                                 absolutearea = TRUE)
  peak2_new_orai_aucs[i] <- auc(x, y, from = new_orai_peak1_cutoff, 
                                   type = "spline", subdivisions = 1000,
                                   absolutearea = TRUE)
}
stim_aucs <- rep(0, ncol(stim_df))
peak1_stim_aucs <- rep(0, ncol(stim_df))
peak2_stim_aucs <- rep(0, ncol(stim_df))
for(i in 1:ncol(stim_df))
{
  x <- position_df$POSITION
  y <- stim_df[, i, drop=TRUE] #drop to make a single column
  stim_aucs[i] <- auc(x, y, from = 0, type = "spline",
                             subdivisions = 1000, absolutearea = TRUE)
  peak1_stim_aucs[i] <- auc(x, y, from = 0, to = stim_peak1_cutoff , 
                                  type = "spline", subdivisions = 1000,
                                  absolutearea = TRUE)
  peak2_stim_aucs[i] <- auc(x, y, from = stim_peak1_cutoff, 
                                    type = "spline", subdivisions = 1000,
                                    absolutearea = TRUE)
}
best2_aucs <- rep(0, ncol(best2_df))
peak1_best2_aucs <- rep(0, ncol(best2_df))
peak2_best2_aucs <- rep(0, ncol(best2_df))
for(i in 1:ncol(best2_df))
{
  x <- position_df$POSITION
  y <- best2_df[, i, drop=TRUE] #drop to make a single column
  best2_aucs[i] <- auc(x, y, from = 0, type = "spline",
                             subdivisions = 1000, absolutearea = TRUE)
  peak1_best2_aucs[i] <- auc(x, y, from = 0, to = best2_peak1_cutoff, 
                               type = "spline", subdivisions = 1000,
                               absolutearea = TRUE)
  peak2_best2_aucs[i] <- auc(x, y, from = best2_peak1_cutoff, 
                                 type = "spline", subdivisions = 1000,
                                 absolutearea = TRUE)
}
wt_aucs <- rep(0, ncol(wt_df))
peak1_wt_aucs <- rep(0, ncol(wt_df))
peak2_wt_aucs <- rep(0, ncol(wt_df))
for(i in 1:ncol(wt_df))
{
  x <- position_df$POSITION
  y <- wt_df[, i, drop=TRUE] #drop to make a single column
  wt_aucs[i] <- auc(x, y, from = 0, 
                    type = "spline", subdivisions = 1000,
                    absolutearea = TRUE)
  peak1_wt_aucs[i] <- auc(x, y, from = 0, to = wt_peak1_cutoff, 
                               type = "spline", subdivisions = 1000,
                               absolutearea = TRUE)
  peak2_wt_aucs[i] <- auc(x, y, from = wt_peak1_cutoff, 
                                 type = "spline", subdivisions = 1000,
                                 absolutearea = TRUE)
}
serca_aucs <- rep(0, ncol(serca_df))
peak1_serca_aucs <- rep(0, ncol(serca_df))
peak2_serca_aucs <- rep(0, ncol(serca_df))
for(i in 1:ncol(serca_df))
{
  x <- position_df$POSITION
  y <- serca_df[, i, drop=TRUE] #drop to make a single column
  serca_aucs[i] <- auc(x, y, from = 0, 
                    type = "spline", subdivisions = 1000,
                    absolutearea = TRUE)
  peak1_serca_aucs[i] <- auc(x, y, from = 0, to = serca_peak1_cutoff , 
                               type = "spline", subdivisions = 1000,
                               absolutearea = TRUE)
  peak2_serca_aucs[i] <- auc(x, y, from = serca_peak1_cutoff, 
                                 type = "spline", subdivisions = 1000,
                                 absolutearea = TRUE)
}
sercamCherry_aucs <- rep(0, ncol(sercamCherry_df))
peak1_sercamCherry_aucs <- rep(0, ncol(sercamCherry_df))
peak2_sercamCherry_aucs <- rep(0, ncol(sercamCherry_df))
for(i in 1:ncol(sercamCherry_df))
{
  x <- position_df$POSITION
  y <- sercamCherry_df[, i, drop=TRUE] #drop to make a single column
  sercamCherry_aucs[i] <- auc(x, y, from = 0, 
                       type = "spline", subdivisions = 1000,
                       absolutearea = TRUE)
  peak1_sercamCherry_aucs[i] <- auc(x, y, from = 0, to = sercamCherry_peak1_cutoff , 
                                  type = "spline", subdivisions = 1000,
                                  absolutearea = TRUE)
  peak2_sercamCherry_aucs[i] <- auc(x, y, from = sercamCherry_peak1_cutoff, 
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
  labs(title="AUC of Distance vs Genotype for Peak 1 and 2", y="AUC Value") + 
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
Identifier_peak1 <-c(rep("wt", length(peak1_wt_aucs)),
                       rep("old mCherry", length(peak1_old_mCherry_aucs)),
                       rep("new mCherry", length(peak1_new_mCherry_aucs)),
                       rep("old orai", length(peak1_old_orai_aucs)),
                       rep("new orai", length(peak1_new_orai_aucs)),
                       rep("stim", length(peak1_stim_aucs)),
                       rep("best2", length(peak1_best2_aucs)),
                       rep("SERCA", length(peak1_serca_aucs)),
                       rep("SERCA-mCherry", length(peak1_sercamCherry_aucs)))

AUC_Values_peak1 <- c(peak1_wt_aucs, peak1_old_mCherry_aucs,
                        peak1_new_mCherry_aucs, peak1_old_orai_aucs,
                        peak1_new_orai_aucs, peak1_stim_aucs,
                        peak1_best2_aucs,
                        peak1_serca_aucs, peak1_sercamCherry_aucs)
#create a dataframe that will hold both arrays and be saved as a csv file
peak1_dotplot_df = data.frame(Identifier_peak1, AUC_Values_peak1)

r <- ggplot() + 
  geom_dotplot(data=peak1_dotplot_df, 
               aes(x = reorder(Identifier_peak1 , -AUC_Values_peak1),
                   AUC_Values_peak1,
                   fill=Identifier_peak1), binaxis="y", stackdir="center") + 
  labs(title="AUC of Distance vs Genotype for Peak 1", y="AUC Value") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x.bottom=element_blank(),
        legend.position="bottom")
print(r)

#save the AUC dot plot in the current directory
ggsave(file="peak1_AUC_gray_values_dotplot.png", plot=r,
       width=300, height=100, units="mm")

#save the dotplot dataframe as a new table
write.csv(dotplot_df, file = "peak1_auc_out.csv")
# 60 to end distance in microns plot
#create an Identifier (x label) array that will mirror the AUC values array
Identifier_peak2 <-c(rep("wt", length(peak2_wt_aucs)),
                       rep("old mCherry", length(peak2_old_mCherry_aucs)),
                       rep("new mCherry", length(peak2_new_mCherry_aucs)),
                       rep("old orai", length(peak2_old_orai_aucs)),
                       rep("new orai", length(peak2_new_orai_aucs)),
                       rep("stim", length(peak2_stim_aucs)),
                       rep("best2", length(peak2_best2_aucs)),
                       rep("SERCA", length(peak2_serca_aucs)),
                       rep("SERCA-mCherry", length(peak2_sercamCherry_aucs)))

AUC_Values_peak2 <- c(peak2_wt_aucs, peak2_old_mCherry_aucs,
                          peak2_new_mCherry_aucs, peak2_old_orai_aucs,
                          peak2_new_orai_aucs, peak2_stim_aucs,
                          peak2_best2_aucs,
                          peak2_serca_aucs, peak2_sercamCherry_aucs)
#create a dataframe that will hold both arrays and be saved as a csv file
peak2_dotplot_df = data.frame(Identifier_peak2, AUC_Values_peak2)

b <- ggplot() + 
  geom_dotplot(data=peak2_dotplot_df, 
               aes(x = reorder(Identifier_peak2 , -AUC_Values_peak2),
                   AUC_Values_peak2,
                   fill=Identifier_peak2), binaxis="y", stackdir="center") + 
  labs(title="AUC of Distance vs Genotype for Peak 2", y="AUC Value") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x.bottom=element_blank(),
        legend.position="bottom")
print(b)

#save the AUC dot plot in the current directory
ggsave(file="peak2_AUC_gray_values_dotplot.png", plot=b,
       width=300, height=100, units="mm")

#save the dataframes as new tables
write.csv(dotplot_df, file = "total_distance_auc_out.csv")
write.csv(peak1_dotplot_df, file = "peak1_distance_auc_out.csv")
write.csv(peak2_dotplot_df, file = "peak2_distance_auc_out.csv")

#generate citations
citation() # R citation
#package citations
citation("MESS")
citation("dplyr")
citation("tidyr")
citation("readr")
citation("ggplot2")
