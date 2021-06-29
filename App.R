library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
                  # Output: Data file--
                  tabPanel("Data", tableOutput("contents")) # Data as datatable

      )  
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  # Regression output
  output$summary <- renderPrint ({
    
    library(stringr)
    library(dplyr)
    library(plyr)
    library(tidyverse)
    library(fastDummies)
    library(caret)
    library(randomForest)
    files<-list.files(path='C:/Users/zhangxinhua/Desktop/Housing/Data/original', pattern='*.csv', full.names=TRUE, recursive = FALSE)
    i<-0
    for (file in files){
      i<-i+1
      t<-read.csv(file, header = FALSE)
      t<-t[-1,]
      colnames(t)<-as.character(unlist(t[1,]))
      t<-t[-1,]
      t<-t[t$`Date of Sale`!='',]
      write.csv(t, file = paste("file_",i,".csv"))
    }
    # bind together with ldply
    myfiles<-list.files(path='C:/Users/zhangxinhua/Desktop/Housing/Data', pattern='*.csv', full.names=TRUE, recursive = FALSE)
    estate_raw<-ldply(myfiles, read_csv)
    estate<-estate_raw[,-c(1,2,20:35)]
    # rename the variables to remove spaces so that it's easier to type
    colnames(estate) <- c("Project_Name",  "Street_Name", "Type", "Postal_District",
                          "Market_Segment", "Tenure", "Type_of_Sale", "No_of_Units",
                          "Price", "Nett_Price", "Area_Sqft", "Type_of_Area", 
                          "Floor_Level", "Unit_Price_psf", "Date_of_Sale", "Area_Sqm",
                          "Unit_Price_psm")
    estate1<-estate[, c(1:10, 12, 13, 15, 11, 14, 16, 17)]
    # last thing to hanle in raw data is that some area units are using sqft and some sqm
    # same issue for unit price units. need to convert and drop the extra 2 columns
    # convert rate is 10.764 as indicated in the file
    estate1$Area_Sqft_new<-round(estate1$Area_Sqm*10.764)
    estate1$Area_Sqm_new<-round(estate1$Area_Sqft/10.764)
    estate1$Area_Sqft<-pmax(estate1$Area_Sqft, estate1$Area_Sqft_new, na.rm=TRUE)
    estate1$Area_Sqm<-pmax(estate1$Area_Sqm, estate1$Area_Sqm_new, na.rm=TRUE)
    estate1$Unit_Price_psf<-round(estate1$Price/estate1$Area_Sqft)
    estate1$Unit_Price_psm<-round(estate1$Price/estate1$Area_Sqm)
    estate<-estate1[, -c(18, 19)]
    es_df<-distinct(estate)
    
    # Data preprocessing ------------------------------------------------------
    # drop columns: Nett_Price, Area_Sqft, Unit_Price_psf
    keeps=c("Project_Name",  "Street_Name", "Type", "Postal_District",
            "Market_Segment", "Tenure", "Type_of_Sale", "No_of_Units",
            "Price", "Type_of_Area", "Floor_Level", "Date_of_Sale", 
            "Area_Sqm","Unit_Price_psm")
    estate_df = es_df[,keeps]
    NA_Tenure_Proj<-estate_df%>%
      filter(is.na(Tenure))%>%
      distinct(Project_Name, Street_Name)
    Ref<-estate_df%>%
      filter(Project_Name %in% NA_Tenure_Proj$Project_Name)%>%
      filter(!is.na(Tenure))%>%
      distinct(Project_Name, Tenure,Street_Name)
    Ref_tbl<-merge(NA_Tenure_Proj, Ref, by=c("Project_Name","Street_Name"))
    # can directly merge back to original dataset if the merged list is distinct
    !Ref_tbl %in% Ref_tbl[duplicated(Ref_tbl)]
    colnames(Ref_tbl)[3]<-'Tenure_Filled'
    estate_df1<-merge(estate_df, Ref_tbl,by=c("Project_Name","Street_Name"), all.x = TRUE)
    # # this does not work, so do the 1 on 1 assignment codes in the later part
    # estate_df1[is.na(estate_df1$Tenure),]$Tenure->estate_df1[is.na(estate_df1$Tenure),]$Tenure_Filled
    estate_df1$Tenure<-pmax(estate_df1$Tenure, estate_df1$Tenure_Filled, na.rm=TRUE)
    estate_df1%>%
      filter(is.na(Tenure))
    estate_df<-estate_df1
    
    # get the tail of the string, it's the TOP year
    estate_df$TOP<-word(estate_df$Tenure[],-1)
    # get the head of the string, it's the lease period
    estate_df$Lease<-word(estate_df$Tenure[],1)
    
    #! to be updated, in here i just simple hardcode as 2017. need to be flexible
    estate_df$TOP[estate_df$TOP=="01/11/2017"] <- "2017"
    
    # it's ok for Freehold to be without TOP year since it has unlimited tenure, but for leasehold one, we need to check
    estate_df %>% 
      filter(estate_df$TOP=="leasehold")
    
    # anyways, checked the online the starting year is 2019 https://esingaporeproperty.sg/property/view-at-kismis-condo/
    estate_df$TOP[estate_df$TOP=="leasehold"] <- "2019"
    
    ## NEED TO MAKE HERE FLEXIBLE e.g. when the input data is empty
    # categorize tenure into limited and freehold
    unlimited <- c('999', '956', '945', '946', '929', '947', '9999', '993', '998', '999999', '940', 'Freehold')
    limited <- c('99', '103', '60', '102', '100', '101', '110', '70', '85', '104', '89')
    # create new column with mutate function
    estate_df<-estate_df %>% mutate(Lease_Cat = case_when(Lease %in% limited ~ "Limited", 
                                                          Lease %in% unlimited ~ "Unlimited"))
    # NEED TO MAKE HERE FLEXIBLE 
    
    # categorise accordingly: Before 1900,	1901-1990,	1991-2000,	2001-2010,	2011-2015,	2016-2020,	Freehold
    estate_df$TOP_Period[estate_df$TOP=='Freehold']<-'Freehold'
    estate_df$TOP_Period[estate_df$TOP<1900]<-'Before 1900'
    estate_df$TOP_Period[estate_df$TOP>1900 & estate_df$TOP<=1990]<-'1901-1990'
    estate_df$TOP_Period[estate_df$TOP>1900 & estate_df$TOP<=2000]<-'1991-2000'
    estate_df$TOP_Period[estate_df$TOP>2000 & estate_df$TOP<=2010]<-'2001-2010'
    estate_df$TOP_Period[estate_df$TOP>2010 & estate_df$TOP<=2015]<-'2011-2015'
    estate_df$TOP_Period[estate_df$TOP>2015 & estate_df$TOP<=2020]<-'2016-2020'
    
    # Date of sales, split to year and month
    estate_df$Sales_Month<-substr(estate_df$Date_of_Sale, 1, 3)
    estate_df$Sales_Year<-substr(estate_df$Date_of_Sale, 5, 8)
    
    Multiple_units<-estate_df%>%
      filter(estate_df$No_of_Units>1)
    
    # We create an indicator for the single unit/multi-unit in case there are nulk buy discount 
    estate_df$Multi_Units[estate_df$No_of_Units==1]<-'Single Unit'
    estate_df$Multi_Units[estate_df$No_of_Units>1]<-'Multiple Units'
    
    estate_df$Price_Per_Unit<-estate_df$Price/estate_df$No_of_Units
    estate_df$Area_Sqm_Per_Unit<-estate_df$Area_Sqm/estate_df$No_of_Units
    
    #Floor Level also needs classification 
    estate_df$Floor_Level_Cat<-estate_df$Floor_Level
    estate_df$Floor_Level_Cat[estate_df$Floor_Level=='-']<-'Unknown'
    estate_df$Floor_Level_Cat[estate_df$Floor_Level=='B1 to B5'|estate_df$Floor_Level=='01 to 05']<-'B5-05'
    estate_df$Floor_Level_Cat[estate_df$Floor_Level=='16 to 20'|estate_df$Floor_Level=='21 to 25'|estate_df$Floor_Level=='26 to 30']<-'16-30'
    estate_df$Floor_Level_Cat[estate_df$Floor_Level=='31 to 35'|estate_df$Floor_Level=='36 to 40'|
                                estate_df$Floor_Level=='41 to 45'|estate_df$Floor_Level=='46 to 50'|
                                estate_df$Floor_Level=='51 to 55'|estate_df$Floor_Level=='56 to 60'|
                                estate_df$Floor_Level=='61 to 65'|estate_df$Floor_Level=='66 to 70'|
                                estate_df$Floor_Level=='71 to 75']<-'Above 70'
    
    model_vars<-c('Type','Postal_District','Market_Segment','Type_of_Sale', 'Price_Per_Unit','Type_of_Area',
                  'Floor_Level_Cat', 'Area_Sqm_Per_Unit', 'Unit_Price_psm', 'TOP_Period', 'Lease_Cat', 'Sales_Month',
                  'Sales_Year', 'Multi_Units')
    estate_lite<-estate_df[,model_vars]
    
    estate_df<-subset(estate_df, select = -Tenure_Filled)
    
    estate_lite$Log_Unit_Price_psm<-log(estate_lite$Unit_Price_psm)
    estate_lite$Log_Area_Sqm_Per_Unit<-log(estate_lite$Area_Sqm_Per_Unit)
    
    # tapply(estate_lite$Unit_Price_psm, estate_lite$Area_Sqm_Per_Unit, summary)
    
    # dummy the cat variables
    estate_md_dt<-dummy_cols(estate_lite, select_columns = c("Type", "Postal_District", "Market_Segment","Type_of_Sale",
                                                             "Type_of_Area", "Floor_Level_Cat","TOP_Period", "Lease_Cat",
                                                             "Sales_Month", "Sales_Year", "Multi_Units"),remove_first_dummy = TRUE)
    colnames(estate_md_dt)<-make.names(names(estate_md_dt))
    
    X = estate_md_dt[,c(16:81)] #16-82 are dummy variables + "Log_Area_Sqm_Per_Unit"
    y = estate_md_dt[,15]
    
    set.seed(123) #randomization
    
    test_inds = createDataPartition(y = 1:length(y), p = 0.2, list = F) 
    
    # Split data into test/train using indices
    X_test = X[test_inds, ]; y_test = y[test_inds] 
    X_train = X[-test_inds, ]; y_train = y[-test_inds]
    train_all<-cbind(X_train, y_train)
    test_all<-cbind(X_test, y_test)
    
    linearMod <- lm(y_train ~., data=train_all)
    prediction_lm <- predict(linearMod,X_test)
    RMSE_lm <- RMSE(y_test, prediction_lm)
    RFMod <- randomForest(y_train ~.,
                          data = train_all)
    prediction <- predict(RFMod,X_test)
    
    # Evaluation RMSE function
    RMSE_rf <- RMSE(y_test, prediction)
    RMSE_comp <- as.data.frame(cbind(RMSE_lm, RMSE_rf))
    colnames(RMSE_comp) <-c("Linear Regression", "Random Forest")
    print(linearMod)
    importance <- importance(RFMod)
    
  })
  
  
}

# Create Shiny app ----
shinyApp(ui, server)