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
sheet0 = read_csv("in/clean_data_.csv")

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
stim_peak1_cutoff <- 50
old_mCherry_peak1_cutoff <- 50
old_orai_peak1_cutoff <- 50

serca_peak1_cutoff <- 85
sercamCherry_peak1_cutoff <- 85

best2_peak1_cutoff <- 68
wt_peak1_cutoff <- 68

new_orai_peak1_cutoff <- 48
new_mCherry_peak1_cutoff <- 48

# stim is compared to old mCherry
# 
# best2 is compared to wt
# 
# old orai is compared to old mCherry
# 
# new orai is compared to new mCherry



#we will use the MESS packages AUC function to fit a spline to our curve
# https://www.rdocumentation.org/packages/MESS/versions/0.5.9/topics/auc
# to change the x minimum and maximum use the from and to variables
# this allows the user to specify areas on the graph to calculate auc
#auc(x,y, from = 0.1, to = 2, type = "spline")

#create empty vectors that are as long as each dataset dataframe
old_mCherry_aucs <- rep(0, ncol(old_mCherry_df))
peak1_old_mCherry_aucs <- rep(0, ncol(old_mCherry_df))
peak2_old_mCherry_aucs <- rep(0, ncol(old_mCherry_df))
old_mCherry_aucs_non_abs <- rep(0, ncol(old_mCherry_df))
peak2_old_mCherry_aucs_non_abs <- rep(0, ncol(old_mCherry_df))
peak1_old_mCherry_aucs_non_abs <- rep(0, ncol(old_mCherry_df))
new_mCherry_aucs <- rep(0, ncol(new_mCherry_df))
peak1_new_mCherry_aucs <- rep(0, ncol(new_mCherry_df))
peak2_new_mCherry_aucs <- rep(0, ncol(new_mCherry_df))
new_mCherry_aucs_non_abs <- rep(0, ncol(new_mCherry_df))
peak1_new_mCherry_aucs_non_abs <- rep(0, ncol(new_mCherry_df))
peak2_new_mCherry_aucs_non_abs <- rep(0, ncol(new_mCherry_df))
old_orai_aucs <- rep(0, ncol(old_orai_df))
peak1_old_orai_aucs <- rep(0, ncol(old_orai_df))
peak2_old_orai_aucs <- rep(0, ncol(old_orai_df))
old_orai_aucs_non_abs <- rep(0, ncol(old_orai_df))
peak1_old_orai_aucs_non_abs <- rep(0, ncol(old_orai_df))
peak2_old_orai_aucs_non_abs <- rep(0, ncol(old_orai_df))
new_orai_aucs <- rep(0, ncol(new_orai_df))
peak1_new_orai_aucs <- rep(0, ncol(new_orai_df))
peak2_new_orai_aucs <- rep(0, ncol(new_orai_df))
new_orai_aucs_non_abs <- rep(0, ncol(new_orai_df))
peak1_new_orai_aucs_non_abs <- rep(0, ncol(new_orai_df))
peak2_new_orai_aucs_non_abs <- rep(0, ncol(new_orai_df))
stim_aucs <- rep(0, ncol(stim_df))
peak1_stim_aucs <- rep(0, ncol(stim_df))
peak2_stim_aucs <- rep(0, ncol(stim_df))
stim_aucs_non_abs <- rep(0, ncol(stim_df))
peak1_stim_aucs_non_abs <- rep(0, ncol(stim_df))
peak2_stim_aucs_non_abs <- rep(0, ncol(stim_df))
best2_aucs <- rep(0, ncol(best2_df))
peak1_best2_aucs <- rep(0, ncol(best2_df))
peak2_best2_aucs <- rep(0, ncol(best2_df))
best2_aucs_non_abs <- rep(0, ncol(best2_df))
peak1_best2_aucs_non_abs <- rep(0, ncol(best2_df))
peak2_best2_aucs_non_abs <- rep(0, ncol(best2_df))
wt_aucs <- rep(0, ncol(wt_df))
peak1_wt_aucs <- rep(0, ncol(wt_df))
peak2_wt_aucs <- rep(0, ncol(wt_df))
wt_aucs_non_abs <- rep(0, ncol(wt_df))
peak1_wt_aucs_non_abs <- rep(0, ncol(wt_df))
peak2_wt_aucs_non_abs <- rep(0, ncol(wt_df))
serca_aucs <- rep(0, ncol(serca_df))
peak1_serca_aucs <- rep(0, ncol(serca_df))
peak2_serca_aucs <- rep(0, ncol(serca_df))
serca_aucs_non_abs <- rep(0, ncol(serca_df))
peak1_serca_aucs_non_abs <- rep(0, ncol(serca_df))
peak2_serca_aucs_non_abs <- rep(0, ncol(serca_df))
sercamCherry_aucs <- rep(0, ncol(sercamCherry_df))
peak1_sercamCherry_aucs <- rep(0, ncol(sercamCherry_df))
peak2_sercamCherry_aucs <- rep(0, ncol(sercamCherry_df))
sercamCherry_aucs_non_abs <- rep(0, ncol(sercamCherry_df))
peak1_sercamCherry_aucs_non_abs <- rep(0, ncol(sercamCherry_df))
peak2_sercamCherry_aucs_non_abs <- rep(0, ncol(sercamCherry_df))

#we will do an auc calculation for distance vs each wing disc set of values
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
  old_mCherry_aucs_non_abs[i] <- auc(x, y, from = 0, type = "spline",
                                     subdivisions = 1000, absolutearea = FALSE)
  peak1_old_mCherry_aucs_non_abs[i] <- auc(x, y, from = 0, to = old_mCherry_peak1_cutoff, 
                                   type = "spline", subdivisions = 1000,
                                   absolutearea = FALSE)
  peak2_old_mCherry_aucs_non_abs[i] <- auc(x, y, from = old_mCherry_peak1_cutoff, 
                                   type = "spline", subdivisions = 1000,
                                   absolutearea = FALSE)
}
for(i in 1:ncol(new_mCherry_df))
{
  x <- position_df$POSITION #our x values are the position in distance values
  y <- new_mCherry_df[, i, drop=TRUE] #drop transforms the data into 1 column
  new_mCherry_aucs[i] <- auc(x, y, from = 0, type = "spline",
                             subdivisions = 1000, absolutearea = TRUE)
  peak1_new_mCherry_aucs[i] <- auc(x, y, from = 0, to = new_mCherry_peak1_cutoff, 
                                     type = "spline", subdivisions = 1000,
                                     absolutearea = TRUE)
  peak2_new_mCherry_aucs[i] <- auc(x, y, from = new_mCherry_peak1_cutoff, 
                                       type = "spline", subdivisions = 1000,
                                       absolutearea = TRUE)
  new_mCherry_aucs_non_abs[i] <- auc(x, y, from = 0, type = "spline",
                             subdivisions = 1000, absolutearea = FALSE)
  peak1_new_mCherry_aucs_non_abs[i] <- auc(x, y, from = 0, to = new_mCherry_peak1_cutoff, 
                                   type = "spline", subdivisions = 1000,
                                   absolutearea = FALSE)
  peak2_new_mCherry_aucs_non_abs[i] <- auc(x, y, from = new_mCherry_peak1_cutoff, 
                                   type = "spline", subdivisions = 1000,
                                   absolutearea = FALSE)
}
for(i in 1:ncol(old_orai_df))
{
  x <- position_df$POSITION
  y <- old_orai_df[, i, drop=TRUE]
  old_orai_aucs[i] <- auc(x, y, from = 0, type = "spline",
                             subdivisions = 1000, absolutearea = TRUE)
  peak1_old_orai_aucs[i] <- auc(x, y, from = 0, to = old_orai_peak1_cutoff , 
                                     type = "spline", subdivisions = 1000,
                                     absolutearea = TRUE)
  peak2_old_orai_aucs[i] <- auc(x, y, from = old_orai_peak1_cutoff, 
                                       type = "spline", subdivisions = 1000,
                                       absolutearea = TRUE)
  old_orai_aucs_non_abs[i] <- auc(x, y, from = 0, type = "spline",
                          subdivisions = 1000, absolutearea = FALSE)
  peak1_old_orai_aucs_non_abs[i] <- auc(x, y, from = 0, to = old_orai_peak1_cutoff , 
                                type = "spline", subdivisions = 1000,
                                absolutearea = FALSE)
  peak2_old_orai_aucs_non_abs[i] <- auc(x, y, from = old_orai_peak1_cutoff, 
                                type = "spline", subdivisions = 1000,
                                absolutearea = FALSE)
}
for(i in 1:ncol(new_orai_df))
{
  x <- position_df$POSITION
  y <- new_orai_df[, i, drop=TRUE]
  new_orai_aucs[i] <- auc(x, y, from = 0, type = "spline",
                             subdivisions = 1000, absolutearea = TRUE)
  peak1_new_orai_aucs[i] <- auc(x, y, from = 0, to = new_orai_peak1_cutoff , 
                                 type = "spline", subdivisions = 1000,
                                 absolutearea = TRUE)
  peak2_new_orai_aucs[i] <- auc(x, y, from = new_orai_peak1_cutoff, 
                                   type = "spline", subdivisions = 1000,
                                   absolutearea = TRUE)
  new_orai_aucs_non_abs[i] <- auc(x, y, from = 0, type = "spline",
                          subdivisions = 1000, absolutearea = FALSE)
  peak1_new_orai_aucs_non_abs[i] <- auc(x, y, from = 0, to = new_orai_peak1_cutoff , 
                                type = "spline", subdivisions = 1000,
                                absolutearea = FALSE)
  peak2_new_orai_aucs_non_abs[i] <- auc(x, y, from = new_orai_peak1_cutoff, 
                                type = "spline", subdivisions = 1000,
                                absolutearea = FALSE)
}
for(i in 1:ncol(stim_df))
{
  x <- position_df$POSITION
  y <- stim_df[, i, drop=TRUE]
  stim_aucs[i] <- auc(x, y, from = 0, type = "spline",
                             subdivisions = 1000, absolutearea = TRUE)
  peak1_stim_aucs[i] <- auc(x, y, from = 0, to = stim_peak1_cutoff , 
                                  type = "spline", subdivisions = 1000,
                                  absolutearea = TRUE)
  peak2_stim_aucs[i] <- auc(x, y, from = stim_peak1_cutoff, 
                                    type = "spline", subdivisions = 1000,
                                    absolutearea = TRUE)
  stim_aucs_non_abs[i] <- auc(x, y, from = 0, type = "spline",
                      subdivisions = 1000, absolutearea = FALSE)
  peak1_stim_aucs_non_abs[i] <- auc(x, y, from = 0, to = stim_peak1_cutoff , 
                            type = "spline", subdivisions = 1000,
                            absolutearea = FALSE)
  peak2_stim_aucs_non_abs[i] <- auc(x, y, from = stim_peak1_cutoff, 
                            type = "spline", subdivisions = 1000,
                            absolutearea = FALSE)
}
for(i in 1:ncol(best2_df))
{
  x <- position_df$POSITION
  y <- best2_df[, i, drop=TRUE]
  best2_aucs[i] <- auc(x, y, from = 0, type = "spline",
                             subdivisions = 1000, absolutearea = TRUE)
  peak1_best2_aucs[i] <- auc(x, y, from = 0, to = best2_peak1_cutoff, 
                               type = "spline", subdivisions = 1000,
                               absolutearea = TRUE)
  peak2_best2_aucs[i] <- auc(x, y, from = best2_peak1_cutoff, 
                                 type = "spline", subdivisions = 1000,
                                 absolutearea = TRUE)
  best2_aucs_non_abs[i] <- auc(x, y, from = 0, type = "spline",
                       subdivisions = 1000, absolutearea = FALSE)
  peak1_best2_aucs_non_abs[i] <- auc(x, y, from = 0, to = best2_peak1_cutoff, 
                             type = "spline", subdivisions = 1000,
                             absolutearea = FALSE)
  peak2_best2_aucs_non_abs[i] <- auc(x, y, from = best2_peak1_cutoff, 
                             type = "spline", subdivisions = 1000,
                             absolutearea = FALSE)
}
for(i in 1:ncol(wt_df))
{
  x <- position_df$POSITION
  y <- wt_df[, i, drop=TRUE]
  wt_aucs[i] <- auc(x, y, from = 0, 
                    type = "spline", subdivisions = 1000,
                    absolutearea = TRUE)
  peak1_wt_aucs[i] <- auc(x, y, from = 0, to = wt_peak1_cutoff, 
                               type = "spline", subdivisions = 1000,
                               absolutearea = TRUE)
  peak2_wt_aucs[i] <- auc(x, y, from = wt_peak1_cutoff, 
                                 type = "spline", subdivisions = 1000,
                                 absolutearea = TRUE)
  wt_aucs_non_abs[i] <- auc(x, y, from = 0, 
                    type = "spline", subdivisions = 1000,
                    absolutearea = FALSE)
  peak1_wt_aucs_non_abs[i] <- auc(x, y, from = 0, to = wt_peak1_cutoff, 
                          type = "spline", subdivisions = 1000,
                          absolutearea = FALSE)
  peak2_wt_aucs_non_abs[i] <- auc(x, y, from = wt_peak1_cutoff, 
                          type = "spline", subdivisions = 1000,
                          absolutearea = FALSE)
}
for(i in 1:ncol(serca_df))
{
  x <- position_df$POSITION
  y <- serca_df[, i, drop=TRUE]
  serca_aucs[i] <- auc(x, y, from = 0, 
                    type = "spline", subdivisions = 1000,
                    absolutearea = TRUE)
  peak1_serca_aucs[i] <- auc(x, y, from = 0, to = serca_peak1_cutoff , 
                               type = "spline", subdivisions = 1000,
                               absolutearea = TRUE)
  peak2_serca_aucs[i] <- auc(x, y, from = serca_peak1_cutoff, 
                                 type = "spline", subdivisions = 1000,
                                 absolutearea = TRUE)
  serca_aucs_non_abs[i] <- auc(x, y, from = 0, 
                       type = "spline", subdivisions = 1000,
                       absolutearea = FALSE)
  peak1_serca_aucs_non_abs[i] <- auc(x, y, from = 0, to = serca_peak1_cutoff , 
                             type = "spline", subdivisions = 1000,
                             absolutearea = FALSE)
  peak2_serca_aucs_non_abs[i] <- auc(x, y, from = serca_peak1_cutoff, 
                             type = "spline", subdivisions = 1000,
                             absolutearea = FALSE)
}
for(i in 1:ncol(sercamCherry_df))
{
  x <- position_df$POSITION
  y <- sercamCherry_df[, i, drop=TRUE]
  sercamCherry_aucs[i] <- auc(x, y, from = 0, 
                       type = "spline", subdivisions = 1000,
                       absolutearea = TRUE)
  peak1_sercamCherry_aucs[i] <- auc(x, y, from = 0, to = sercamCherry_peak1_cutoff , 
                                  type = "spline", subdivisions = 1000,
                                  absolutearea = TRUE)
  peak2_sercamCherry_aucs[i] <- auc(x, y, from = sercamCherry_peak1_cutoff, 
                                    type = "spline", subdivisions = 1000,
                                    absolutearea = TRUE)
  sercamCherry_aucs_non_abs[i] <- auc(x, y, from = 0, 
                              type = "spline", subdivisions = 1000,
                              absolutearea = FALSE)
  peak1_sercamCherry_aucs_non_abs[i] <- auc(x, y, from = 0, to = sercamCherry_peak1_cutoff , 
                                    type = "spline", subdivisions = 1000,
                                    absolutearea = FALSE)
  peak2_sercamCherry_aucs_non_abs[i] <- auc(x, y, from = sercamCherry_peak1_cutoff, 
                                    type = "spline", subdivisions = 1000,
                                    absolutearea = FALSE)
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
Identifier_non_abs<-c(rep("wt_non_abs", length(wt_aucs_non_abs)),
              rep("old mCherry_non_abs", length(old_mCherry_aucs_non_abs)),
              rep("new mCherry_non_abs", length(new_mCherry_aucs_non_abs)),
              rep("old orai_non_abs", length(old_orai_aucs_non_abs)),
              rep("new orai_non_abs", length(new_orai_aucs_non_abs)),
              rep("stim_non_abs", length(stim_aucs_non_abs)),
              rep("best2_non_abs", length(best2_aucs_non_abs)),
              rep("SERCA_non_abs", length(serca_aucs_non_abs)),
              rep("SERCA-mCherry_non_abs", length(sercamCherry_aucs_non_abs)))

AUC_Values <- c(wt_aucs, old_mCherry_aucs, new_mCherry_aucs,
                old_orai_aucs, new_orai_aucs, stim_aucs, best2_aucs,
                serca_aucs, sercamCherry_aucs)
AUC_Values_non_abs <- c(wt_aucs_non_abs, old_mCherry_aucs_non_abs, new_mCherry_aucs_non_abs,
                old_orai_aucs_non_abs, new_orai_aucs_non_abs, stim_aucs_non_abs, best2_aucs_non_abs,
                serca_aucs_non_abs, sercamCherry_aucs_non_abs)
# all distances in microns plot
#create a dataframe that will hold two arrays and be saved as the out csv file
dotplot_df = data.frame(Identifier, AUC_Values)
dotplot_df_non_abs = data.frame(Identifier_non_abs, AUC_Values_non_abs)

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
ggsave(file="out/AUC_gray_values_dotplot.png", plot=j,
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
Identifier_peak1_non_abs <-c(rep("wt_non_abs", length(peak1_wt_aucs_non_abs)),
                     rep("old mCherry_non_abs", length(peak1_old_mCherry_aucs_non_abs)),
                     rep("new mCherry_non_abs", length(peak1_new_mCherry_aucs_non_abs)),
                     rep("old orai_non_abs", length(peak1_old_orai_aucs_non_abs)),
                     rep("new orai_non_abs", length(peak1_new_orai_aucs_non_abs)),
                     rep("stim_non_abs", length(peak1_stim_aucs_non_abs)),
                     rep("best2_non_abs", length(peak1_best2_aucs_non_abs)),
                     rep("SERCA_non_abs", length(peak1_serca_aucs_non_abs)),
                     rep("SERCA-mCherry_non_abs", length(peak1_sercamCherry_aucs_non_abs)))

AUC_Values_peak1 <- c(peak1_wt_aucs, peak1_old_mCherry_aucs,
                        peak1_new_mCherry_aucs, peak1_old_orai_aucs,
                        peak1_new_orai_aucs, peak1_stim_aucs,
                        peak1_best2_aucs,
                        peak1_serca_aucs, peak1_sercamCherry_aucs)
AUC_Values_peak1_non_abs <- c(peak1_wt_aucs_non_abs, peak1_old_mCherry_aucs_non_abs,
                      peak1_new_mCherry_aucs_non_abs, peak1_old_orai_aucs_non_abs,
                      peak1_new_orai_aucs_non_abs, peak1_stim_aucs_non_abs,
                      peak1_best2_aucs_non_abs,
                      peak1_serca_aucs_non_abs, peak1_sercamCherry_aucs_non_abs)
#create a dataframe that will hold both arrays and be saved as a csv file
peak1_dotplot_df = data.frame(Identifier_peak1, AUC_Values_peak1)
peak1_dotplot_df_non_abs = data.frame(Identifier_peak1_non_abs, AUC_Values_peak1_non_abs)

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
ggsave(file="out/peak1_AUC_gray_values_dotplot.png", plot=r,
       width=300, height=100, units="mm")

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
Identifier_peak2_non_abs <-c(rep("wt_non_abs", length(peak2_wt_aucs_non_abs)),
                     rep("old mCherry_non_abs", length(peak2_old_mCherry_aucs_non_abs)),
                     rep("new mCherry_non_abs", length(peak2_new_mCherry_aucs_non_abs)),
                     rep("old orai_non_abs", length(peak2_old_orai_aucs_non_abs)),
                     rep("new orai_non_abs", length(peak2_new_orai_aucs_non_abs)),
                     rep("stim_non_abs", length(peak2_stim_aucs_non_abs)),
                     rep("best2_non_abs", length(peak2_best2_aucs_non_abs)),
                     rep("SERCA_non_abs", length(peak2_serca_aucs_non_abs)),
                     rep("SERCA-mCherry_non_abs", length(peak2_sercamCherry_aucs_non_abs)))

AUC_Values_peak2 <- c(peak2_wt_aucs, peak2_old_mCherry_aucs,
                          peak2_new_mCherry_aucs, peak2_old_orai_aucs,
                          peak2_new_orai_aucs, peak2_stim_aucs,
                          peak2_best2_aucs,
                          peak2_serca_aucs, peak2_sercamCherry_aucs)
AUC_Values_peak2_non_abs <- c(peak2_wt_aucs_non_abs, peak2_old_mCherry_aucs_non_abs,
                      peak2_new_mCherry_aucs_non_abs, peak2_old_orai_aucs_non_abs,
                      peak2_new_orai_aucs_non_abs, peak2_stim_aucs_non_abs,
                      peak2_best2_aucs_non_abs,
                      peak2_serca_aucs_non_abs, peak2_sercamCherry_aucs_non_abs)
#create a dataframe that will hold both arrays and be saved as a csv file
peak2_dotplot_df = data.frame(Identifier_peak2, AUC_Values_peak2)
peak2_dotplot_df_non_abs = data.frame(Identifier_peak2_non_abs, AUC_Values_peak2_non_abs)

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
ggsave(file="out/peak2_AUC_gray_values_dotplot.png", plot=b,
       width=300, height=100, units="mm")

#save the dataframes as new tables
write.csv(dotplot_df, file = "out/total_distance_auc_out.csv")
write.csv(peak1_dotplot_df, file = "out/peak1_distance_auc_out.csv")
write.csv(peak2_dotplot_df, file = "out/peak2_distance_auc_out.csv")
write.csv(dotplot_df_non_abs, file = "out/total_distance_auc_out_non_abs.csv")
write.csv(peak1_dotplot_df_non_abs, file = "out/peak1_distance_auc_out_non_abs.csv")
write.csv(peak2_dotplot_df_non_abs, file = "out/peak2_distance_auc_out_non_abs.csv")

#generate citations
citation() # R citation
#package citations
citation("MESS")
citation("dplyr")
citation("tidyr")
citation("readr")
citation("ggplot2")
