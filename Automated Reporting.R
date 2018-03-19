rm(list = ls())

library(readxl)
library(e1071)
library(gplots)
library(ggplot2)
library(caret)
library(ggfortify)
library(gridExtra)
library(analogue)
library(ggrepel)
library(parallel)
library(dplyr)
library(vegan)
library(indicspecies)
library(reshape2)
library(plyr)
### library(r2excel)   ### Doesn't work at home. Can't install. Need to change this.


set.seed(2)

### Allow parallel computing (not useful on my computer since it's only dual core and we reserve a core to the OS)
# Calculate the number of cores
no_cores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(no_cores)

setwd("C:/Users/Émilie/Documents/Pierre/FA Data Exploration")
FA <- read_excel("Fatty Acids.xlsx", sheet = "Paste data on top left", trim_ws=TRUE)

###### First cleaning of the data
######
######


### Let's identify where our headers are
### We will define our headers based on the position of the cell named "Contributor"
### Let's find it's coordinates
index <- which(FA=="Contributor", arr.ind=TRUE)
### The index variable now contains a list of 2 items, the row number and the column number of the "Contributor" cell.
### We will define the column names of our data.frame to the content of the line where "Contributor" is.
colnames(FA)<- FA[index[1],]

### We will do 3 things at the same time :
### 1- remove the extra lines at the top
### 2- remove the extra columns at the left (instrument output)
### 3- remove the extra columns at the right (multivariate similarity index)

### More specifically, we will keep only the columns between "Date" and "Multivariate Similarity Index", the latter excluded.
### We will identify the position of those columns differently in order to return a single value and not a list of values.
### We will ask R to find a match to a regular expression in the column names.
### This will return the position of the column of a specific name in the list of column names.
### Note that we want to get rid of the column "Multivariate Similarity Index" so we will use it's index -1.

index_date<-grep("^Date$", colnames(FA))
index_msi<-grep("^Multivariate Similarity Index$", colnames(FA))-1

### Finally, we define our new dataset without the top rows and the extra columns to the left and right
FA<-FA[-c(1:index[1]),c(index_date:index_msi)]
### I overwrote my FA variable in order to lower memory usage.
### This way, we don't have the full dataset stored in memory anymore and we only have the trimed one.

### We will also remove the C19 variable since it's not usefull to report it
FA$`C19:0`<-NULL
### Just to make pretty, we will format the Date column to make it calendar year and not Posixt
FA$Date<-as.numeric(FA$Date)
FA$Date <- as.Date(FA$Date, origin = "1900-01-01")

###### Split the data by contributor
######
######


### First, we are going to need a list of the unique contributors.
### Let's make sure our Contributors column is lower case only.
### This will get rid of errors if some contributors are written both in upper and lower case.
### As a note, Excel doesn't show this with it's filters, so better handle it here.
FA$Contributor<-tolower(FA$Contributor)
### Now, we can see that the Contributor column is a character column and not a factor, so we have to set it accordingly.
FA$Contributor <- as.factor(FA$Contributor)
### Time for the list of unique contributors.
### To make this less confusing, we are going to store the list of unique contributors in the variable projects and not contributors.
projects<- unique(FA$Contributor)
projects <- projects[!is.na(projects)]

### We can now divide the data.frame by contributor and name each new data.frame after the name of that contributor.
### The for loop will go through each value of the list project (unique, lower case contributors) and subset according to this contributor.
### We will add all the dataframes to a list named l to use in the subsequent steps.
### To do this, we initialise a variable z that will increment with our loop and will be the index of the data.frame in the list
### Then, we initialise a list of the same length than the list of projects.
### Finally, the loop goes through creating the data.frames and adding them at the proper index of the l list.

z=1
l <- vector("list", length(projects))
for (i in projects){
 i=as.data.frame(FA [which (FA$Contributor==i),])
 l[[z]]=i
 z=z+1
}

### To make things easier, we will name the elements of the list l (the subsetted data.frames) to the name of the corresponding contributors.
names(l)<-projects


###### Final cleaning of the data
######
######

### What we want to do now is to remove the columns that have only missing values.
### We of course want to do this individually on each subsetted data frame.
### We define a function called data_cleaning that takes a data.frame as argument
### The function counts for each column the number of NA and if this number is different from the number of rows, it keeps it.
### This way, we can drop the columns that have only NA (missing values)

### We also want to remove the Contributors column since it provides no new information.
data_cleaning <- function(df){
  df= df[colSums(!is.na(df)) > 0]
  df$Contributor<-NULL
  df <- df[which(df$Material != "Sample Blank"),]
  return(df)
}

### Then, we use lapply (base r function) to apply our data_cleaning function to all elements of the l list.
### We overwrite our initial l list so that we now have a list of data.frames which are clean.
l<-lapply(l, data_cleaning)

### ### ### This function doesn't work because I can't load the package r2excel
### reporting <- function(df, name_of_df){
### filename<- paste (name_of_df ,"xlsx", sep = ".") ### Sets the name of the file to the name of the contributor and adds ".xlsx"
### wb<-createWorkbook(type="xlsx") ### Creates a new wxcel file with xlsx format
### sheet <- createSheet(wb, sheetName = "Fatty Acid Report") ### Creates and names the first sheet of our file
### xlsx.addHeader(wb, sheet, value="Fatty Acid Quantification Report",level=1, ### Adds the Title
            ###   color="black", underline=1)
### xlsx.addLineBreak(sheet, 1) ### Skips 1 line
### info=paste("Laboratory of Aquatic Sciences\n",
           ###  "UQAC",
           ###  "\n Analyst : Pierre Carrier-Corbeil\n",
          ### "Date of Report Generation: ", Sys.Date(), sep="")
### xlsx.addParagraph(wb, sheet,value=info, isItalic=TRUE, colSpan=4, 
                  ### rowSpan=5, fontColor="darkgray", fontSize=14)
### xlsx.addHeader(wb, sheet, value="For questions, please contact :",level=2, 
               ### color="black", underline=0)
### xlsx.addHyperlink(wb,sheet, "mailto:milla.rautio@uqac.ca", "Milla Rautio") 
### xlsx.addHyperlink(wb,sheet, "mailto:pierre.carrier-corbeil@uqac.ca", "Pierre Carrier-Corbeil") 
### xlsx.addLineBreak(sheet, 3)
### xlsx.addTable(wb, sheet, df, startCol=2)
### xlsx.addLineBreak(sheet, 2)
### saveWorkbook(wb, filename)
###}

### Now that we have a good function to create the reports, we need to apply it to our list of data.frames.
### We will use mapply, which is similar to lapply, but accepts to iterate over 2 sets.
### We pass our list of data.frames and the vector projects (to be used as file name) to our reporting function in mapply.
### This generates the output we wanted with clean reports for each contributor in the working directory.
#l<-mapply(reporting, l, projects)

### reporting(test_df, "Maxime")

### The tutorial for the excel document is http://www.sthda.com/english/wiki/r2excel-read-write-and-format-easily-excel-files-using-r-software

### Let's clean our environment to set the table for what's ahead.
### The following line of code removes everything except for the arguments.
### The packages are untouched and thus still available.

rm(list= ls()[!(ls() %in% c("cl", "l"))])

######### Here starts the automatic data exploration

### This function takes a data.frame as an argument and returns
### the indexes of the columns in Metadata, Univariate and Multivariate.
### The output is a list containing the objects Metadata, Univariate and Multivariate.

### Note that the list is named, so we can call Segmentation$Metadata for example
### instead of Segmentation[1]

test_df<-l[[1]]

data_segmentation<- function(df){
  headers_of_df <- names(df) ### Loading of the names of the columns)
  Total_FAME_Index<- match("Total FAME",headers_of_df) ### Position of column Total Fame
  Unconventional_FA_Index<- match("% Unconvential FA",headers_of_df) ### Position of column % Unconventional FA
  C13_Index<-match("C13:0",headers_of_df) ### Position of column C13:0
  
  Metadata<-headers_of_df[1:(Total_FAME_Index -1)] ### Definition of the range considered as Metadata
  Univariate<-headers_of_df[Total_FAME_Index:Unconventional_FA_Index]### Definition of the range considered as Univariate
  Multivariate<-headers_of_df[C13_Index:length(headers_of_df)]### Definition of the range considered as Multivariate
  Segmentation<-list(Metadata = Metadata, Univariate = Univariate, Multivariate = Multivariate) ### Creation of the list for output
  return(Segmentation)
}

test<-data_segmentation(test_df)
###### Correcting for the loss of type information from readxl.
###### When we import data from excel, we lose the information about the type
###### of data contained in each column. Everything is converted to character.
###### We have to correct for this or we can not run any analysis.

### Here, we define a function that looks up the position of the float columns
### and returns a vectors containing the indices of the float columns

### Note that the regular expression used doesn't match the scientific numbers
### used for certain fatty acids like 1.22-E3 for instance.
### This is why we will deal with the multivariate FA fata separately.
float_position_lookup<-function(df){
  floats_position<-list() ### Empty list that is populated during the loop
  for(column_index in c(1:length(names(df)))){ ### Ensures the loop goes through every column
  if((length(grep("^[0-9]+.[0-9]+$", df[,column_index])))>1){ ### If more then 1 item matches the regular expression,
    floats_position[column_index]<-TRUE} ### Then the list is populated with TRUE
    else{
      floats_position[column_index]<-FALSE ### Else it is populated with FALSE
    }
    
  }
  floats_position<-which(unlist(floats_position)) ### The which function extracts the indexes of the TRUE entry of our logical vector. unlist transforms the list to a vector.
  return(floats_position)
}

test<-float_position_lookup(test_df)
### This function converts the character columns to factors.
character_to_factor<-function(df){
  character_vars <- lapply(df, class) == "character" ### This identifies the character columns
  df[, character_vars] <- lapply(df[, character_vars], as.factor) ### This converts them to factors
  return(df)}
test<-character_to_factor(test_df)
### The following function returns a data.frame with the column types fixed.
### If converts to numerical the numerical columns in Metadata and Univariate.
### The Multivariate data are assumed to be all numerical and converted as such.
### The remaining character variables are converted to factors

Fixing_Types<-function(df){
  Segmentation<-data_segmentation(df)
  floats_position<-as.numeric(float_position_lookup(df)) ### Saves the position of floats
  df[,floats_position]<-lapply(df[,floats_position], as.numeric) ### Converts them to numerical
  df[,Segmentation$Multivariate]<-as.data.frame(lapply(df[,Segmentation$Multivariate], as.numeric)) ### Converts the Multivariate data to numerical
  df<-character_to_factor(df) ### COnverts the character variables to factor
  return(df)
}
test<-Fixing_Types(test_df)
### The following function will allow us to know the type of metadata columns
### We will have a list of list as output
### Each element of the master list will contain information for a different
### type of data
### Each type list (inner list) will contain indices of the columns of that type

### We have to define temporary function is.date since it's missing from base R

Detection_of_Metadata_Structure<-function(df){
  Segmentation<-data_segmentation(df)
  df<-Fixing_Types(df)
  Factors <- which(sapply(df[,Segmentation$Metadata], is.factor))
  Integers <- which(sapply(df[,Segmentation$Metadata], is.integer))
  Floats <- which(sapply(df[,Segmentation$Metadata], is.numeric))
  is.date <- function(x) inherits(x, 'Date')
  Dates <- which(sapply(df[,Segmentation$Metadata], is.date))
  Metadata_Structure<-list(Factors=Factors, Floats=Floats, Dates=Dates, Integers = Integers)
  return(Metadata_Structure)
}
test<-Detection_of_Metadata_Structure(test_df)
### Next is a function that will take a column of our data.frame and
### test for normality using the Shapiro-Wilk test.
### The output will be a list containing the pvalue as well as a statement.
### The statement will state if the variable is normal or not.

### Note, in order to make the statement work, the vector I test for must be
### named. Else, the names(variable_vector) will return NULL. 

### A good trick is to pass argument drop=FALSE when extracting a column from a
### data.frame like this test_df[,1, drop=FALSE].

### This will returned a vector whose name will be the name of the column it
### came from.

Normality_test<-function(variable_vector){
  normality_result<-list()
  normality<-shapiro.test(variable_vector)
  if(normality[2]>0.05){
    statement<-paste("The pvalue of",normality[2],"shows that the variable",names(variable_vector)," is normal, based on the Shapiro-Wilk test.")
  } else{statement<-paste("The pvalue of",normality[2],"shows that the variable",names(variable_vector)," is not normal, based on the Shapiro-Wilk test.")
  }
  statement<-statement[1]
  normality_result<-c(pvalue=normality[2], statement=statement)
  return(normality_result)
}


### This function will take a numeric vector and try to make it normal.
### It starts by looking at normality.
### If it's normal, then it generates a statement and stops there.
### If it's not normal, it transforms the vector and tests for normality again.
### If it's normal, it generates a statement and stops there.
### If it's not normal, it transforms the vector differently and tests for normality.

### The output of the function is a list containing the transformed vector
### as well as the statement.

### IMPORTANT NOTE : Returning actually breaks the function, meaning it stops it.
### This is extremely usefull for conditional functions where the function needs
### to stop at different moments depending on it's characteristics.

Normalify<-function(variable_vector){
  to_return<-list()
  normality<-Normality_test(variable_vector)
  if(normality[1]>0.05){
    statement<-"The variable is normal"
    to_return<-list(values=variable_vector, statement=statement)
    return(to_return)}
  else{
    variable_vector_transformed<-variable_vector^(-2)
  }
  normality_result<-Normality_test(variable_vector)
  if(normality[1]>0.05){
    statement<-"The variable was made normal with a power of -2"
    to_return<-list(values=variable_vector_transformed, statement=statement)
    return(to_return)}
  else{
    variable_vector_transformed<-variable_vector^(-0.5)
  }
  normality_result<-Normality_test(variable_vector)
  if(normality[1]>0.05){
    statement<-"The variable was made normal with a power of -0.5"
    to_return<-list(values=variable_vector_transformed, statement=statement)
    return(to_return)}
  else{
    variable_vector_transformed<-variable_vector^(-1)
  }
  normality_result<-Normality_test(variable_vector)
  if(normality[1]>0.05){
    statement<-"The variable was made normal with a power of -1"
    to_return<-list(values=variable_vector_transformed, statement=statement)
    return(to_return)}
  else{
    variable_vector_transformed<-variable_vector^(0.5)
  }
  normality_result<-Normality_test(variable_vector)
  if(normality[1]>0.05){
    statement<-"The variable was made normal with a power of -0.5"
    to_return<-list(values=variable_vector_transformed, statement=statement)
    return(to_return)}
  else{
    variable_vector_transformed<-variable_vector^(2)
  }
  normality_result<-Normality_test(variable_vector)
  if(normality[1]>0.05){
    statement<-"The variable was made normal with a power of 2"
    to_return<-list(values=variable_vector_transformed, statement=statement)
    return(to_return)}
  else{
    variable_vector_transformed<-log(variable_vector)
  }
  normality_result<-Normality_test(variable_vector)
  if(normality[1]>0.05){
    statement<-"The variable was made normal with a natural logarithm transformation"
    to_return<-list(values=variable_vector_transformed, statement=statement)
    return(to_return)}
  else{
    variable_vector_transformed<-log10(variable_vector)
  }
  normality_result<-Normality_test(variable_vector)
  if(normality[1]>0.05){
    statement<-"The variable was made normal with a common logarithm transformation"
    to_return<-list(values=variable_vector_transformed, statement=statement)
    return(to_return)}
  else{
    statement<-"The variable failed to become normal with transformation"
    to_return<-list(values=variable_vector, statement=statement)
    return(to_return)}
}

### I would like to implement the test for homscedasticity so I have tested all
### the assumptions.
### However, this test requires a group variable.
### I will thus create a new test vector containing group information.

Homoscedasticity<-function(variable_vector, group_vector){
  result<-bartlett.test(variable_vector ~ group_vector)
  if(result[3]<0.05){
    statement<-paste("The pvalue of",result[3],"shows that the variable",names(variable_vector)," is heteroscedastic, based on the Bartlett test.")
  }
  else{statement<-paste("The pvalue of",result[3],"shows that the variable",names(variable_vector)," is homoscedastic, based on the Bartlett test.")
    
  }
  return(c(pvalue=result[3], statement=statement))
}


### The following function will test for normality,
### Try to make the variable normal,
### Check for homoscedasticity,
### Tell if we should use parametric tests
### and finally make a unified statement about the assumptions.
### It will return the parametric value, the statement and the vector to use
### in the statistical tests.
Assumption_testing<-function(variable_vector, group_vector){
  normality<-Normality_test(variable_vector)
  normalify<-Normalify(variable_vector)
  homoscedasticity<-Homoscedasticity(variable_vector, group_vector)
  if(normalify$statement!="The variable failed to become normal with transformation" & homoscedasticity[1]>0.05){
    parametric<-TRUE
  }
  else{
    parametric<-FALSE}
  statement_assumptions<-paste(normality$statement, " ", normalify$statement, " ", homoscedasticity[2])
  return(list(parametric=parametric, statement_assumptions=statement_assumptions, vector_to_use=normalify[1]))
}

### Up next is a function that runs a t test and returns the pvalue and a statement
t_testing<-function(variable_vector, group_vector){
  results<-t.test(variable_vector ~ group_vector)
  if(results[3]>0.05){
    statement<-paste("The pvalue of",results[3],"shows that the variable",names(variable_vector)," is not different between groups based on t test.")
  } else{statement<-paste("The pvalue of",results[3],"shows that the variable",names(variable_vector)," is different between groups based on t test.")
  }
  statement<-statement[1]
  t_test_result<-c(pvalue=results[3], statement=statement)
  return(t_test_result)
}

### This function performs a non parametric t test and returns the pvalue and statement
non_parametric_t_testing<-function(variable_vector, group_vector){
  results<-wilcox.test(variable_vector ~ group_vector)
  if(results[3]>0.05){
    statement<-paste("The pvalue of",results[3],"shows that the variable",names(variable_vector)," is not different between groups based on Mann-Whitney-Wilcoxon test.")
  } else{statement<-paste("The pvalue of",results[3],"shows that the variable",names(variable_vector)," is different between groups based on Mann-Whitney-Wilcoxon test.")
  }
  statement<-statement[1]
  t_test_result<-c(pvalue=results[3], statement=statement)
  return(t_test_result)
}

### Here, we will define a function to run an Anova and return a pvalue and a statement.
Anova_test<-function(variable_vector, group_vector){
fit <- aov(variable_vector ~ group_vector)
test<-summary(fit)
pvalue<-test[[1]]$'Pr(>F)'
pvalue<-pvalue[1]
if(pvalue>0.05){
  statement<-paste("The pvalue of",pvalue,"shows that the variable",names(variable_vector)," is not different between groups based on a one way ANOVA.")
} else{statement<-paste("The pvalue of",pvalue,"shows that the variable",names(variable_vector)," is different between groups based on a one way ANOVA.")
}
statement<-statement[1]
to_return<-as.list(c(pvalue=pvalue, statement=statement))
return(to_return)
}

non_parametric_anova<-function(variable_vector, group_vector){
  fit <- kruskal.test(variable_vector ~ group_vector)
  pvalue<-fit[3]
  if(pvalue>0.05){
    statement<-paste("The pvalue of",pvalue,"shows that the variable",names(variable_vector)," is not different between groups based on a Kruskal Wallis test.")
  } else{statement<-paste("The pvalue of",pvalue,"shows that the variable",names(variable_vector)," is different between groups based on a Kruskal Wallis test.")
  }
  statement<-statement[1]
  to_return<-as.list(c(pvalue=pvalue, statement=statement))
  return(to_return)
}

### This function could be used instead of boxplots.
### It's simpler, but doesn't give as much information on the plots.
### And they don't look as good as ggplot.

###group_comparison_plot_quick<-function(variable_vector, group_vector){
###plotmeans(variable_vector~group_vector,xlab=names(group_vector),
###          ylab=names(variable_vector))}
###group_comparison_plot_quick(test_variable, test_group)

### This function will produce a boxplot for the group comparison.
### It would be nice to print the test used and the p value as well.
### I could also print star symbols or the HSD Tukey contrast results
Box_plots<-function(variable_vector, group_vector){
  plot_data.frame<-as.data.frame(variable_vector,group_vector)
  ggplot(plot_data.frame, aes(x = group_vector, y = variable_vector)) + geom_boxplot()+ scale_x_discrete(name = "")+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=0, hjust=1),
                                                                                                                           panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank())+ scale_y_continuous(name=names(variable_vector))}

### Now, it's time to wrap up our univariate comparison function in a single,
### more convenient function. The function will take a numeric variable
### and a factor. It will then look at the number of values in the factor.

### If there is only one value (excluding NAs), then it will do nothing.

### If there is 2 values of the factor, it will run the assumption test method.
### Afterwards, it will use the given vector (possibly transformed) to run
### a t test or non parametric t test. It will then produce the boxplots
### with the non transformed vector.The function will finally return 
### a list containing the boxplot and the unified text.

### If there is more than 2 values of the factor, it will run the
### assumption test method as well. Then, it will use the possibly transformed
### vector to run an Anova or non parametric Anova. It will use the original
### vector to produce the boxplots. Finally, the function will return the 
### unified text as well as the boxlpot.

### It works fine with 2 or 3 factors when non parametric, but gives an error
### when given normal numerical vector.

group_comparison<-function(variable_vector, group_vector){
  number_of_levels<-nlevels(group_vector)
  if(number_of_levels<2){
    return(NULL)}
  else if (number_of_levels==2) {
    to_use<-Assumption_testing(variable_vector, group_vector)
    to_use_variable<-unlist(to_use[3])
    if (to_use$parametric==TRUE){
      results<-t_testing(to_use_variable,group_vector)}
    else {
      results<-non_parametric_t_testing(to_use_variable, group_vector)
    }
    graph<-as.list(Box_plots(variable_vector, group_vector))
    statement=paste(to_use$statement_assumptions, results$statement)
    to_return<-list()
    to_return[[1]]<-graph
    to_return[2]<-statement
    return(to_return)
  }
    else {
      to_use<-Assumption_testing(variable_vector, group_vector)
      to_use_variable<-unlist(to_use[3])
      if (to_use$parametric==TRUE){
        results<-Anova_test(to_use_variable,group_vector)}
      else {
        results<-non_parametric_anova(to_use_variable, group_vector)
      }
      graph<-as.list(Box_plots(variable_vector, group_vector))
      statement=paste(to_use$statement_assumptions, results$statement)
      to_return<-list()
      to_return[[1]]<-graph
      to_return[2]<-statement
      return(to_return)
    }
}

### Here we define a function that will create a data.frame containing
### all of the factors. It will remove all factors with a single level.
### It will then make all possible combinations of 2 factors and append the
### new factors with correct names to the original factor data.frame.

### Try catch will handle the cases where no factor contains more than one level.
### It will then return an empty factors object, thus aborting the analysis.

factors_data.frame<-function(df){
  structure<-Detection_of_Metadata_Structure(df)
  factors<-df[,structure$Factors]
  factors<-droplevels(factors)
  tryCatch({
  factors<-factors[, sapply(factors, nlevels) > 1]
  length_of_factors<-1:ncol(factors)
  for (i in length_of_factors) {
    for (j in length_of_factors){
      if (i<j){
        factors$ij<-interaction(factors[,c(i,j)])
        factor_name<-paste(colnames(factors)[i], ".", colnames(factors[j]))
        colnames(factors)[colnames(factors)=="ij"] <- factor_name
      }
    }
  }
  factors<-droplevels(factors)
  factors<-factors[, sapply(factors, nlevels) > 1]
  return(factors)
  } , error = function(e) return("ERROR"))
  }

test<-factors_data.frame(test_df)
### Here, we will define a function to prepare a good set of multivariate
### data for a given factor.

Multivariate_prep<-function(df, factor_index){
  ###We start by segmenting the data frame to know where is our multivariate data
  Segmentation<-data_segmentation(df)
  ### We then extract this multivariate data
  multivariate<-df[,Segmentation$Multivariate]
  ### We transform the NA values to 0 since they are 0 in the end.
  ### It would also cause problem with the Wisconsin transformation applied later
  ### to keep the NAs.
  multivariate[is.na(multivariate)] <- 0
  ### We extract and augment our factors.
  factors<-factors_data.frame(df)
  ### We remove from the multivariate data all the rows where the factor at the
  ### specified index has no values.
  if (sum(is.na(factors[,factor_index]))>0) {
  multivariate<- as.data.frame(multivariate[-which(is.na(factors[,factor_index])), ])
  ### We also delete these rows in the factors data.frame
  factors<-as.data.frame(factors[-which(is.na(factors[,factor_index])), ])}
  ### We identify the columns in the multivariate data that have near zero variance
  nzv_cols <- nearZeroVar(multivariate)
  ### and we delete those columns.
  if(length(nzv_cols) > 0) multivariate <- multivariate[, -nzv_cols]
  to_return<-list(multivariate=multivariate, factors=factors)
  return(to_return)
}
  

### Here, we will set our function that will perform Permanova and Permdist
### tests. It will also run the NMDS and plot it with ellipses.
### The only problem is that it can not plot more than 6 groups correctly
### on the NMDS so it does nothing with the groups 7, 8, 9, etc...
### This should not be a problem because it would be impossible to read a plot
### with more than 6 colors anyway.

### The output will be a statement and the plot.

### trycatch will handle errors.


Permanova_Permdist_NMDS<-function(df, factor_index){
tryCatch({
results<-Multivariate_prep(df, factor_index)
multivariate<-results$multivariate
factors<-results$factors
data_numerical_tran<-tran(multivariate,method="wisconsin")
tryCatch({
  Permanova_FA_euclidean<-adonis(data_numerical_tran~factors[,factor_index], method = "euclidean")
pvalue_permanova<-Permanova_FA_euclidean$aov.tab$`Pr(>F)`[1]

### Permdist2
distance_structure_euclidean<-vegdist(data_numerical_tran, method="euclidean")
betadisper_euclidean<-betadisper(distance_structure_euclidean, factors[,factor_index], type = c("centroid"))
permutest_euclidean<-permutest(betadisper_euclidean, pairwise = FALSE)
pvalue_Permdist<-permutest_euclidean$tab$`Pr(>F)`[1]

statement_Permanova<-if(pvalue_permanova>0.05){
  statement<-paste("The permanova pvalue of",pvalue_permanova,"shows that the variable",names(factors[factor_index])," is not different between groups in the euclidean multivariate space.")
} else{statement<-paste("The permanova pvalue of",pvalue_permanova,"shows that the variable",names(factors[factor_index])," is different between groups in the euclidean multivariate space.")
}

statement_Permdist<-if(pvalue_Permdist>0.05){
  statement<-paste("The permdist pvalue of",pvalue_Permdist,"shows that the variance of the variable",names(factors[factor_index])," is not different between groups in the euclidean multivariate space.")
} else{statement<-paste("The permdist pvalue of",pvalue_Permdist,"shows that the variance of the variable",names(factors[factor_index])," is different between groups in the euclidean multivariate space.")
}

statement<-paste(statement_Permanova, statement_Permdist)
}
, error=function(e){
  statement<-NULL
})

tryCatch({
NMDS=metaMDS(multivariate,k=,trymax=100, autotransform =TRUE, distance = "euclidean")
treat<-factors[,factor_index]

data.scores <- as.data.frame(scores(NMDS))
data.scores$site <- rownames(data.scores)
data.scores$grp<- treat
species.scores <- as.data.frame(scores(NMDS, "species"))
species.scores$species <- rownames(species.scores)

hull.data<-data.frame()
for (level in levels(treat)){
  grp.treat<-grp.a <- data.scores[data.scores$grp == level, ][chull(data.scores[data.scores$grp == 
                                                                                  level, c("NMDS1", "NMDS2")]), ]
  hull.data <- rbind(hull.data, grp.treat)}
NMDS_plot<-as.list(ggplot() + 
  geom_polygon(data=hull.data,aes(x=NMDS1,y=NMDS2,fill=grp,group=grp),alpha=0.30) + # add the convex hulls
  geom_point(data=data.scores,aes(x=NMDS1,y=NMDS2,shape=grp,colour=grp),size=4)+ # add the point markers
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank()))
}, error = function(f){
  NMDS_plot<-NULL
  return(NMDS_plot)
})
to_return<-list()
to_return[1]<-statement
to_return[[2]]<-NMDS_plot

### The following code snippet was made as comment
### It runs the indicator species analysis and adds the result to the output
### of this function.
### The reason for it's removal is that it is extremely demanding computationnaly
### It would make the script about 100 times longer to run, if not more.

### indval <- multipatt(multivariate, factors[,factor_index])
### to_return[[3]]<-indval

return(to_return)
}
, error = function(g){
  return(vector("list", 2))
})
}

### This is a modification to the multivariate_prep function that does the same
### to prepare a good set of univariate data according to a factor index.
Univariate_prep<-function(df, factor_index){
  Segmentation<-data_segmentation(df)
  ### We then extract this univariate data
  univariate<-df[,Segmentation$Univariate]
  Date<-as.data.frame(df$Date)
  ### We transform the NA values to 0 since they are 0 in the end.
  univariate[is.na(univariate)] <- 0
  ### We extract and augment our factors.
  factors<-factors_data.frame(df)
  ### We remove from the univariate data all the rows where the factor at the
  ### specified index has no values.
  if (sum(is.na(factors[,2]))) {
    univariate<- as.data.frame(univariate[-which(is.na(factors[factor_index])), ])
    ### We will also create a date column and clean it in the same way than 
    ### factors and univariate.
    ### This will be necessary for the time series plots.
    Date<-Date[-which(is.na(factors[factor_index])),]
    ### We also delete these rows in the factors data.frame
    factors<-as.data.frame(factors[-which(is.na(factors[factor_index])), ])}
  ### We identify the columns in the multivariate data that have near zero variance
  nzv_cols <- nearZeroVar(univariate)
  ### and we delete those columns.
  if(length(nzv_cols) > 0) univariate <- univariate[, -nzv_cols]
  to_return<-list(univariate=univariate, factors=factors, Date= Date)
  return(to_return)}

### Here we define a function to produce time series plots of our data.
time_series<-function(df, factor_index){
  tryCatch({
  results<-Univariate_prep(df, factor_index)
  univariate<-results$univariate
  factors<-results$factors
  groups<-univariate
  Date<-results$Date
  if(length(Date)>0){
  names(Date)<-"Date"
  split_factor<-factors[, factor_index]
  groups<-cbind(groups, Date)
  groups<-cbind(groups, split_factor)
  groups_to_plot <- split(groups, split_factor)
  plots<-list()
  
  for (i in (1:length(groups_to_plot))){
    to_melt<-groups_to_plot[[i]]
    to_melt$split_factor<-NULL
    melt_groups<-melt(to_melt, id="Date")
    
    Area_plot<-ggplot(melt_groups,aes(x=Date,y=value,colour=variable,group=variable)) + 
      geom_area(aes(colour = variable, fill= variable))+ scale_colour_brewer(palette = "Paired") + scale_fill_brewer(palette = "Paired") + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(angle=45, hjust=1),
            panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.title = element_blank()) + 
      scale_x_date(name="", date_labels = "%b %y") +
      scale_y_continuous()
    plots[[i]]<-Area_plot
  }
  names_of_plots<-names(groups_to_plot)
  names(plots)<-names_of_plots
  return(plots)
  }
  else{
    plots<-list()}}
  , error = function(e){
    return(list())})
    }

### Before moving on, we need an extension to the univariate function
### group_comparison. We need to make it work for all the univariate variables
### at the same time. We will also make it work by using a factor_index argument.

univariate_group_comparison<-function(df, factor_index){
  tryCatch({results<-Univariate_prep(df)
  univariate<-results$univariate
  factors<-results$factors
  factors<-droplevels(factors)
  group_vector<-factors[,factor_index]
  univariate<-univariate[!(as.numeric(group_vector) %in% which(table(group_vector)<2)),]
  factors <- factors[!(as.numeric(group_vector) %in% which(table(group_vector)<2)),]
  group_vector<-factors[,factor_index]
  
  if (sum(is.na(factors[,factor_index]))>0) {
    univariate<- as.data.frame(univariate[-which(is.na(factors[,factor_index])), ])
    ### We also delete these rows in the factors data.frame
    factors<-as.data.frame(factors[-which(is.na(factors[,factor_index])), ])}
  group_vector<-factors[,factor_index]
  
  total_results<-list()
  z=1
  for(i in c(1:length(results$univariate))){
    variable_vector<-univariate[,i]
    names(variable_vector)<-names(univariate[,i,drop=FALSE])
    comparison<-group_comparison(variable_vector, group_vector)
    total_results[[z]]<-as.list(comparison)
    z=z+1}
  names(total_results)<-names(univariate)
  return(total_results)
  }, error=function(e) return("TryCatch caught an error"))
}



### Here, we define a master function that will take the whole data.frame and
### run the group comparisons in Univariate and Multivariate for all identified
### groups. It will also produce all of the time series plots for each levels of
### each factor.
### The output will be a very long list containing statements and plots.
### This list could be used later to produce a report in pdf form.

Complete_analysis<-function(df){
  df<-Fixing_Types(df)
  factors<-factors_data.frame(df)
  factors_length<-length(factors)
  Univariate_Comparison_list<-list()
  Multivariate_comparison_list<-list()
  time_series_plots_list<-list()
  z=1
  for(i in c(1:factors_length)){
   univariate_comparison<-as.list(univariate_group_comparison(df, i))
   Univariate_Comparison_list[[z]]<-univariate_comparison
   z=z+1}
  names(Univariate_Comparison_list)<-names(factors)
  z=1
  for(i in c(1:factors_length)){
    Multivariate_comparison<-Permanova_Permdist_NMDS(df, i)
    Multivariate_comparison_list[[z]]<-Multivariate_comparison
    z=z+1}
  names(Multivariate_comparison_list)<-names(factors)
  z=1
  for(i in c(1:factors_length)){
    time_series_plots<-time_series(df, i)
    time_series_plots_list[[z]]<-time_series_plots
    z = z+1}
  names(time_series_plots_list)<-names(factors)
  complete_results<-list()
  complete_results[[1]]<-Univariate_Comparison_list
  complete_results[[2]]<-Multivariate_comparison_list
  complete_results[[3]]<-time_series_plots_list
  names(complete_results)<-c("Univariate", "Multivariate", "Time Series")
  return(complete_results)
}

### Here, we will iterate over our list of clean project data.frames l and 
### apply the master function to each of these data.frames. We will store the
### individual projects list to disc.

setwd("C:/Users/Émilie/Documents/Pierre/FA Data Exploration/automated_reports")
for(i in c(1:length(l))){
  df<-l[[i]]
  full_results<-Complete_analysis(df)
  name_of_file<-paste(names(l[i]), ".RDS", sep ="")
  saveRDS(full_results, file = name_of_file)
#  readline(prompt="Press [enter] to continue") ### This line asks for the user to press enter
  ### after each iteration of the loop. Usefull for debugging.
}


### Now that we can run the whole analysis on the datasets, we want to iterate
### over all levels of factors present in our dataset. This means that we will do
### All of the analysis on seston samples only, then on zooplankton only, etc...

### In order to achieve this, we will have to create a new function that will
### take a df as an input, perform all the possible splits and then return
### a list of lists containing all the results of a specific project.
### Then, we will be able to iterate over projects.

setwd("C:/Users/Émilie/Documents/Pierre/FA Data Exploration/automated_reports_split")

### STATUS
### I think the worst is over.
### I only need to format the output of the split_and_analyse function
### and to make it fit with the for loop to give nested lists.

split_and_analyse<-function(df){
  df<-Fixing_Types(df)
  structure<-Detection_of_Metadata_Structure(df)
  factors<-df[,structure$Factors]
  factors<-droplevels(factors)
  number_of_factors<-ncol(factors)
  list_of_results<-list()
  z=1
  for(i in c(1:number_of_factors)){
    list_of_results[[z]]<-as.list(by(data = df, INDICES = factors[,z], FUN = Complete_analysis, simplify = FALSE))
    z=z+1
  }
  names(list_of_results)<-names(factors[,1:number_of_factors])
  return(list_of_results)
}

for(i in c(1:length(l))){
  name_of_file<-paste(names(l[i]), ".RDS", sep ="")
  tryCatch({
  df<-l[[i]]
  full_results<-list()
  full_results[[1]]<-Complete_analysis(df)
  full_results[[2]]<-as.list(split_and_analyse(df))
  names(full_results)<-c("No split", "With split")
  saveRDS(full_results, file = name_of_file)
  }, error=function(e) return(saveRDS("Doesn't fit in memory", file = name_of_file)))
  gc()}

### Ok, it look like I have accomplished what I wanted to do!!!
### What are the next steps?

### It runs well, being a single click execution, but the output requires
### importing an R object and exploring it through R.
### Altough easy, it might be a problem for non R users or users on the go.
### It would be a good idea to transform this script as an R shiny app.
### It would require asking the user for the delimiters of the data to 
### perform the segmentation, but afterwards, the full analysis could be done
### on the server. The result could be explored interactively in the app or 
### downloaded (totally or partially).

### It could also be extended to other analysis as well.
### We could calculate correlation matrices for instance.
### We could have tukey HSD tests for the Anovas and Permanovas.
### We could have t-SNE and PCA in addition to NMDS.
### We could add symbols on the graphs according to the results of the 
### statistical tests.

### We could also work to have more ways of importing the data.
### The shiny R could support csv files for instance.

### I could fix my regular expression for numerical vectors to flag the
### scientific notation as well. It works now, but could be a problem for the 
### shiny R app.

### A shiny R app is a web based app running on an R logic.
### I would need to implement the user interface and tweak the back-end to make
### it a bit more flexible, probably only rewriting the segmentation function 
### to accomodate different delimiters.

### We could promote this to an open source project as well with a github page
### This would allow contributors to pitch in with bug fixes and features implementations.

### We could host the app either on a public server (free or monthly cost)
### or host it ourselves on a dedicated machine.
### Not sure if it would get past UQAC's firewall...

### If we host it ourselves, it will be important to have a capable machine.
### One option is to host it on Amazon Web Service.
### To cover the costs, we could ask for donations or offer the app as a service.
### The second option might be a littl touchy with the open source philosophy,
### altough not totally incompatible.