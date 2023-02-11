### TEMPLATE SCRIPT ###



##### PART 1 - PREPARATION #####
 
  ### If you have previous data still loaded, use the broomstick icon in the environment pane to clear your session

  ### Set working directory 
  # In the top menu, under "Session", then "Set Working Directory", then "Choose directory...", select your folder
  # Now a line of code has been run in the console below. Copy it to your script. It should look something like this:
    setwd("C:/Users/xxx/yyy/Datasets_RManual")
  # Check the working directory
    getwd()
    
  ### Load (basic) packages
    # Install if used for the first time. Only needs to be run once on your computer, do not put this in your script!
      install.packages("EpiStats")
    # Load package before you can use it. Needs to be run once on every new session, so put it in the script. Frequently used packages are:
      # to import Excel files
      library(readxl)
      # to calculate OR or RR
      library(EpiStats)
    
  ### Import data
    # Under "Environment" in the top right, click on "Import Dataset", then choose the option that corresponds with the format of your data.
    # Make the necessary configurations, and after it has run, copy the code from the console to your script.
    # Here are the most common options, with the code that is produced.
      # From Text (.csv)
        data <- read.csv("H:/Teaching/My_Courses/IIH-epistat/R/Sharm_Exercise/mysharm.csv")
      # from Excel file (.xls, .xlsx)
        library(readxl)
        data <- read_excel("H:/Teaching/My_Courses/IIH-epistat/R/Sharm_Exercise/mysharm.xlsx")
      # from STATA file (.dta)
        library(haven)
        data <- read_dta("H:/Teaching/My_Courses/IIH-epistat/R/Sharm_Exercise/mysharm.dta")
        
##### PART 2 - DATA EXPLORATION AND CLEANING #####
  ### Define structure of the dataset
      # Dataframe
      data <- as.data.frame(data)
        
  ### Exploring the data ###
    # Look in the environment pane. Click on the name of the data set to open a spreadsheet view, or click on the arrow to show a list of variables. Or use the commands below:
      # names of variables
      names(data)
      # number of rows
      nrow(data)
      # number of columns
      ncol(data)
      # number of rows and columns
      dim(data)
      # type of variables (integer, character,...)
      str(data)
      
  ### Check if all variables are coded as the correct class ; if not you need to recode ###
      
      # summary of each variable
        summary(data)
        summary(data$var)
        class(data$var)
        str(data$var)
      
      # Categorical variable coded as numbers? -> convert to a factor variable
          data$var_f <- factor(data$var)


  ### Check for missing or duplicate data 
      # missing variables 
            # number of missing variables for a specific variable
              table(is.na(data$variable))
              
            # Make sure empty text is coded as NA so that R knows they are missing (and treats them correctly)
              data$var[data$var==""] <- NA

            # Exclude participants/rows with missing data for one specific variable
              data <- data[!is.na(data$var),]
              
      # duplicates
            # check whether or not there are duplicates in a specific variable
              anyDuplicated(data$var)
              duplicates <- data.frame(table(data$var))
            # Make list with double data only
              doubles <- data.frame(table(data$var))
              doubles[doubles$Freq > 1 ,]
          
              
  ### Make new variables based on other variables
      # e.g. creating a new variable 'tall' that is 1 if variable height >200 cm; if not (if height is not >200) I want this new variable tall to be coded as 0
          data$tall <- ifelse(data$height>200, 1, 0)
          
      # Regrouping (e.g. age in months to agegroup)
          data$agegrp <- NA
          data$agegrp[data$age<12] <- "1"
          data$agegrp[data$age>11 & data$age<24] <- "2"
          data$agegrp[data$age>23 & data$age<36] <- "3"
          data$agegrp[data$age>35 & data$age<48] <- "4"
          data$agegrp[data$age>47 & data$age<60] <- "5"
          # don't forget to convert agegrp to a factor variable
          data$agegrp <- factor(data$agegrp)
          
      # Dichotomize (e.g. nutrition status M,N,S -> malnutrition TRUE/FALSE)
          data$maln <- NA
          data$maln[data$nutr=="N"] <- FALSE
          data$maln[data$nutr=="M"] <- TRUE
          data$maln[data$nutr=="S"] <- TRUE
          
  ### Rename variables
      data$newname <- data$oldname
      # Optionally remove the old variable
      data$oldname <- NULL
          
          
      # Save clean dataset for analysis (make sure you have set your working directory, so R can save your clean dataset there!)
          write.csv(data, file="cleaned_data.csv")
      # However, on a new session, it is best to run all previous code in the script (that is, importing the raw data and running all the data cleaning commands), rather than importing a cleaned dataset
          
      
##### PART 3 - DESCRIPTIVE STATISTICS #####
      
  ### Univariate analysis #####
      # Numerical variables
          # graphical representation -> histogram/frequency polygon
                  hist(data$var)
                  hist(data$var, 
                     col="red",           # color of the graph
                     breaks = 5,          # specify the number of breaks
                     xlab = "Title")      # title of the graph 

          # Summary 
                # measure of central tendancy (mean, median)
                  summary(data$var)
                  mean(data$var)
                    # for confidence interval around the mean
                      t.test(data$var)
                # measure of dispersion (standard deviation, interquartile range)
                  summary(data$var)
                  sd(data$var)
                # graphical representation of the summary = boxplot
                  boxplot(data$var)
                  boxplot(data$var,
                          col = "blue",                    # color of the graph
                          main = "title",                  # title of the graph
                          ylab = "title on the y axis"     # title on the y axis
                          )
                  
                  
      # Categorical variables
          # graphical representation -> frequency distribution (bar chart, pie chart)
                  table(data$var_f)
                  barplot(table(data$var_f))
                    barplot(table(data$var_f,
                                  col="red",                # color of the graph
                                  xlab = "title x axis",     # title on the x-axis
                                  ylab = "title y axis"     # title on the y axis
                                  ))
                  pie(table(data$var_f))
          # Summary = frequency table
                  table(data$var_f)
                  prop.table(table(data$var_f))
      

  ### Bivariate analysis #####
      # NUMERICAL - NUMERICAL -> make a scatterplot
          # Make a scatterplot
            plot(data$var1, data$var2, main="Title")
          # add a regression line
            abline(lm(var2~ var1, data = data), col = "blue")
          # calculate correlation coefficient
            cor(data$var1, data$var2)
          # check r for correlation coefficient
            
      # CATEGORICAL - CATEGORICAL -> calculate OR or RR
          # make a 2x2 table
            table(data$var1, data$var2)
                  # add row %  
                  prop.table (table(data$var1,data$var2), margin=1)
                  # add column %  
                  prop.table (table(data$var1,data$var2), margin=2)
                  # change order of the rows/independent variable 
                  table(data$var1,data$var2)[c(2,1),]
                  # add row %
                  prop.table (table(data$var1,data$var2)[c(2,1),], margin=1)
                  # change order of the columns/dependent variable 
                  table(data$var1,data$var2)[,c(2,1)]  
                  
          # calculate Odds Ratio
                  CC(data, "dependent_var","independent_var")
                  # check interpretation with original 2x2 table
                    table(data$dependent_var, data$independent_var)
          # calculate Risk Ratio
                  CS(sharm, "dependent_var","independent_var")
                  # check interpretation with original 2x2 table
                  table(data$dependent_var, data$independent_var)
                  
      # NUMERICAL - CATEGORICAL -> summary of the numerical variable for each of the categories of the qualitative variable
                  tapply(data$quantitative, data$qualitative, summary)
                  boxplot(data$quantitative ~ data$qualitative)
                  # compare two means (in case of normal distribution of numerical variable) -> use t.test
                    t.test(data$dependent_var ~data$independent_var)
                  # compare two medians (in case of non-normal distribution of numerical variable) -> use kruskal wallis test
                    kruskal.test(data$dependent_var ~ data$independent_var)
                    

            
                    
##### PART 4 - STRATIFIED ANALYSIS #####
  # identify a potential confounder
      # association with outcome?
      # association with risk factor?
      # not on the causal pathway?
  # If POTENTIAL confounder identified -> verify whether this is a TRUE confounder that should be controlled for
      # Perform stratified analysis
          # Note that all variables (exposure, outcome and potential confounder) need to be numeric and binary and coded as "0" and 1".
                # The third variable (potential confounder) needs to be numeric, but may have more categories, such as "0", "1" and "2".
                    # recode
                      data$var_bin[data$var=="Pos"]<-1
                      data$var_bin[data$var=="Neg"]<-0
                      table(data$var_bin)
          # Stratified analysis
                CCInter(data, "outcome", "exposure", "potential_confounder") 
                    
