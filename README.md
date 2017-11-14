# GISDAY-code

---
title: "UAV Registration Error Analysis"
author: "Thiago Sanna Freire Silva and Thais Medeiros"
output:
  word_document: default
  html_document:
    fig_caption: yes
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, cache = TRUE)
```

```{r Input Data}
### MODIFY THIS PART ###############################

### This is the working folder, change it to where the shapefiles are saved
wd <- "C:/GIS-DAY"
setwd(wd) #set the working folder to be wd

## We set out input images below
shp_reference <- "Base_CED.shp"
shp_original <- "CED_2016_02_23_NGO_2.shp"
shp_registered <- "CED_2016_02_23_GEO.shp"

# Here we specify the location and date (look at the file names)
loc_date <- "Cedro 2016-02-23"

#####################################################
```

This auto-generated report compares the registration errors between UAV images before and after georeferencing.

- Reference Image is: `r I(shp_reference)`
- Original Image is: `r I(shp_original)`
- Registered Image is: `r I(shp_registered)`


```{r Processing}
### Processing steps below


### Here we are loading the required packages
library(rgdal) # includes the spTransform() function to reproject the data
library(raster) # includes the shapefile() function to read shapefiles
library(ggplot2) # Cool plots
library(knitr) # Nicer tables on the report

### Now we read in the data

reference <- shapefile(shp_reference)
original <- shapefile(shp_original)
registered <- shapefile(shp_registered)

### Lets test if all files have the same number of points
### The script stops if they  don't
if (!all.equal(dim(reference),dim(original),dim(registered))){
    stop("Number of points is different for each file!")
}

### Do they have the same coordinate system?
### If not, then we reproject everybody to the reference
if (!identical(crs(reference),crs(original))){
    original <- spTransform(original, crs(reference))
}

if (!identical(crs(reference),crs(registered))){
    registered <- spTransform(registered, crs(reference))
}

### Now we can build a table with the x and y coordinates for each image
coord_df <- coordinates(reference) # extract the x and y coordinates as a new matrix
coord_df <- cbind(coord_df,coordinates(original)) # add the two coord columns from original
coord_df <- cbind(coord_df,coordinates(registered)) # add the two columns from registered

coord_df <- data.frame(coord_df) # convert from matrix to data frame, the correct table format for R
names(coord_df) <- c("ref.x","ref.y","ori.x","ori.y","reg.x","reg.y") # set the column names

### Good, now we have the data ready to work
### First, lets build a new table with the errors
### We will build the table column by column
errors_df <- data.frame('Initial.x' = coord_df$ori.x - coord_df$ref.x,
                        'Initial.y' = coord_df$ori.y - coord_df$ref.y,
                        'Final.x' = coord_df$reg.x - coord_df$ref.x,
                        'Final.y' = coord_df$reg.y - coord_df$ref.y)

### We can also calculate the diagonal distance between the points as a
### measure of error too. Lets add them to a new data frame

dist_df <- data.frame(Before = pointDistance(original, reference),
                      After = pointDistance(registered, reference))


### We can also calculate the azimuth (direction) of the distance between points
### We need to use some trigonomety: the arc-tangent of the x and y distances + 180

az_df <- data.frame(Before = atan(abs(errors_df$Initial.x / errors_df$Initial.y)) * 57.2958 + 180,
                    After = atan(abs(errors_df$Final.x / errors_df$Final.y)) * 57.2958 + 180)

### We can use this information to make a cool visualization
### But first we need to reformat the table to have three columns
### One specifiying initial vs final, one for distances and one for angles
### We call a table like that "long-format"

dist_long <- cbind(stack(dist_df),stack(az_df)[1])
names(dist_long) <- c("Distance","Source","Azimuth")
dist_long$x <- 0
dist_long$y <- 0
dist_long$Source <- factor(dist_long$Source,levels(dist_long$Source)[c(2,1)])
```

For reference, these are the first lines of the validaiton point coordinates (Table 1)

```{r Head Points}
kable(head(coord_df,3),
      caption = "Table 1. First lines of the validation point coordinates assessed.") 
```

### Horizontal and Vertical Errors

After georeferencing, average horizontal error decreased from `r round(mean(errors_df$Initial.x),2)` meters to `r round(mean(errors_df$Final.x),2)` meters, and average vertical error decreased from `r round(mean(errors_df$Initial.y),2)` to `r round(mean(errors_df$Final.x),2)` meters (Table 2 and Figure 1).

```{r HV Error Summary}
## Now we can get the summary of the errors
kable(summary(errors_df),
      caption = paste("Table 2. Summary of registration errors for image", loc_date, "before and after georeferencing."))
```

```{r Error Boxplot, fig.cap="Figure 1. Boxplot of horizontal and vertical registration errors, before and after georeferencing. Dashed red lines indicate the [-15cm,+15cm] range."}
### We can plot the errors as a boxplot, to show the distribution
### we also add a solid red line at 0 and dashed lines at 15 cm for reference
boxplot(errors_df, xlab = "Errors", ylab = "Meters")
abline(h=0,col="red")
abline(h=c(0.15,-0.15),col="red",lty=2)
```

### Euclidean Distance Errors

Average Euclidean distance errors decreased from `r round(mean(dist_df$Before),2)` meters to `r round(mean(dist_df$After),2)`meters (Table 3 and Figure 2).

```{r Dist Error Summary}
## Now we can get the summary of the errors
kable(summary(dist_df),
      caption = "Table 3. Summary of Euclidean distance errors before and after image georeferencing.")
```

```{r Dist Boxplot, fig.cap = "Figure 2. Boxplot of Euclidean distances between validation points before and after georeferencing."}
boxplot(dist_df, xlab = "Distance", ylab = "Meters")
```

### Overall Effect

The overall effect of registration can be seen on Figure 3.

```{r Spoke Plot, , fig.cap = "Figure 3. Radial plot of Euclidean distances and azimuths between validation points, before and after georeferencing."}
# Now we plot the distances and azimuths as a radial plot
ggplot(dist_long,aes(x,y)) +
    geom_spoke(aes(angle = Azimuth, radius = Distance),color='red',arrow = arrow(length = unit(0.2,"cm"))) +
    facet_wrap( ~ Source) +
    xlab('x error (meters)') +
    ylab('y error (meters)') +
    theme_bw(base_size = 12)
```
