if(!require("dplyr")) install.packages("dplyr")
if(!require("igraph")) install.packages("igraph")
if(!require("ggplot2")) install.packages("ggplot2")

#load required libraries
library(dplyr)
library(igraph)
library(ggplot2)


#######################################################
##Read in Data to set mydata, class (binary) and fullclass. Also to set constraints
#######################################################

##Read in simulated data with class labels
dat <- read.csv("D:\\CS Year 3\\FYP\\PTS code\\PTS 2 (using age and target as constraint)\\pca_transformed_cleveland.csv",row.names=NULL)
#the empty space before "," means - select ALL rows.
mydata=dat[,1:2] #mydata will contain the first 2 columns of the dataset. 
print(mydata)
fullclass = dat[,3]#vector of classes where 0 is control (selects third column which represents stages of disease).
print(fullclass)
class=1+(fullclass>0)#converts multiclasses into 2: 1 = control, 2 = others
print(class)
#constraints for Simulated Data
#constrain banfrom[k] -> banto[k] moves
banfrom = c(3,4);#NULL;#
print(banfrom)
banto = c(4,3);#NULL;#
print(banto)


###########################################
## MAIN PTS CODE
###########################################


#build full dist matrix
dis <- dist(mydata,method="euclidean")
dis=as.matrix(dis)
print(dis)

sampsize=nrow(mydata);
print(sampsize)
cons=dis;#matrix(0,sampsize,sampsize)
print(cons)
for (i in 1:sampsize)
{
  for (j in 1:sampsize)
  {
    for (k in 1:length(banfrom))
    {
      if ((fullclass[i]==banfrom[k]) & (fullclass[j]==banto[k]))
      {
        cons[i,j]=999;
        cons[j,i]=999;
      }
    }
  }
}



pcadat=(pc.cl  <- princomp(mydata))

plot(pcadat$scores[,1],pcadat$scores[,2],col=as.numeric(class)+1, pch=19)

#****PTS****
sampsize = 50;#determines the length of the pts
nreps = 100;#the number of pts generated
endclass=2;#endclass is represented by 2 and startclass by 1

pts = NULL
for (i in 1:nreps)
{
  #resample from data
  repeat
  {
    mysamp = sample(1:nrow(mydata), sampsize, replace=TRUE)
    if (is.element(1,class[mysamp]) & is.element(endclass,class[mysamp]))
    {
      break
    }
  }    
  dunesamp = mydata[mysamp,]
  classsamp = class(mysamp)
  
  repeat
  {
    startp = sample(1:sampsize,1)#sample(mysamp,1);
    endp = sample(1:sampsize,1)#sample(mysamp,1);
    if ((class[mysamp[startp]]==1) && (class[mysamp[endp]]==endclass))
    {
      break
    }
  }
  
  #build sampled distance matrix
  #dis <- dist(dunesamp,method="euclidean")
  #dis=as.matrix(dis)
  
  dissamp = cons[mysamp,mysamp]
  #dissamp = dis[mysamp,mysamp]
  
  
  #build minimum spanning tree
  mode(dissamp) <- "numeric"
  g <- graph_from_adjacency_matrix(dissamp,weighted=TRUE,mode = "undirected")
  
  datamst=mst(g)#, weights = TRUE, algorithm = NULL) ALSO mst
  E(datamst)$weight
  
  #get shortest path from class 1 to class 2
  datshort=shortest_paths(datamst, startp,endp,mode="all",output = "both")
  
  #plot the trajectories of each mst
  ptsind = datshort$vpath
  pts[i] = list(mysamp[ptsind[[1]]])
  
  #sample minimum spanning tree plots
  #if (i<11)
  #{
  #plot(datamst,vertex.color=4+class[mysamp],vertex.size=10*is.element(mysamp, mysamp[datshort$vpath[[1]]]),vertex.label=NA)
  #}
}


###PLOTTING
#scatter plot with legend
# Scatterplot of PCA scores colored by class
plot(
  pcadat$scores[, 1], pcadat$scores[, 2], 
  col = fullclass + 1, pch = 19, 
  xlab = "Principal Component 1", 
  ylab = "Principal Component 2", 
  main = "PCA Scores Colored by Class"
)

# Add legend to signify classes
legend(
  "topright", 
  legend = unique(fullclass), 
  col = unique(fullclass + 1), 
  pch = 19, 
  title = "Disease Stage"
)

# Overlay pseudo-time trajectories on PCA scatterplot
for (i in 1:nreps) {
  lines(
    pcadat$scores[pts[[i]], 1], 
    pcadat$scores[pts[[i]], 2], 
    col = i, lwd = 1.5  # Each trajectory in a different color
  )
}


#plotting only one patient
# Select a specific trajectory, e.g., the first trajectory from the pts list
selected_trajectory <- pts[[10]]  # Change the index to select any other trajectory

# Extract the PCA scores for the patients in the selected trajectory
selected_scores <- pcadat$scores[selected_trajectory, ]

# Scatterplot of PCA scores, only for patients in the selected trajectory
plot(
  selected_scores[, 1], selected_scores[, 2], 
  col = fullclass[selected_trajectory] + 1, pch =19,cex = 3,
  xlab = "Principal Component 1", 
  ylab = "Principal Component 2", 
  main = "Selected Trajectory in PCA Space"
)

# Add a legend to signify classes in the selected trajectory
legend(
  "bottomright", 
  legend = unique(fullclass[selected_trajectory]), 
  col = unique(fullclass[selected_trajectory] + 1), 
  pch = 19, 
  title = "Disease Stage"
)

# Overlay the trajectory line (connecting the points in order)
lines(
  selected_scores[, 1], selected_scores[, 2], 
  col = "blue", lwd = 1  # Blue line with width 2
)

###ggplots
FULLPTS=data.frame()
for (i in 1:nreps)
{
  t=1:length(pts[[i]])
  c=fullclass[pts[[i]]]
  pcas=pcadat$scores[pts[[i]],1:2]
  IDPTS=cbind(t,c,pcas)
  
  FULLPTS=rbind(FULLPTS,IDPTS)
}

#distribution of class over pseudo time
g <- ggplot(FULLPTS, aes(FULLPTS$t))
g + geom_density(aes(fill=factor(FULLPTS$c)), alpha=0.8) 


g <- ggplot(FULLPTS, aes(FULLPTS$t, FULLPTS$Comp.2))
g + geom_boxplot(aes(fill=factor(FULLPTS$c)))

ggplot(FULLPTS,
       aes(x=FULLPTS$t,
           y=FULLPTS$Comp.1, 
           color=FULLPTS$c))+
  geom_point()

ggplot(FULLPTS,
       aes(x=FULLPTS$t,
           y=FULLPTS$Comp.1, 
           fill=factor(FULLPTS$c)))+
  geom_smooth()

ggplot(FULLPTS,
       aes(x=FULLPTS$t,
           y=FULLPTS$Comp.2, 
           fill=factor(FULLPTS$c)))+
  geom_smooth()


#saving pts as csv for evaluation purposes
# Convert PTS list to a data frame for saving
pts_df <- do.call(rbind, lapply(1:length(pts), function(i) {
  data.frame(
    Rep = i,  # Identifier for the repetition
    PseudoTime = 1:length(pts[[i]]),  # Time point in the trajectory
    Index = pts[[i]],  # Indices from sampled data
    PC1 = pcadat$scores[pts[[i]], 1],  # PCA Component 1
    PC2 = pcadat$scores[pts[[i]], 2],  # PCA Component 2
    Age = age[pts[[i]]],  # Age of patients
    Class = class[pts[[i]]]  # Disease stage
  )
}))

write.csv(pts_df, "D:\\CS Year 3\\FYP\\PTS code\\pca_generated_pts_stageConstraints.csv", row.names = FALSE)

