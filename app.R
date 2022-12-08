library(tidyverse)
library(dplyr)
library(shiny)
library(psych)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(DT)
library(shinydashboard)
options(scipen = 10000000) # to get numerical value
crop_production=read.csv("crop_production.csv")
crop_production=crop_production %>% drop_na(Production) # to drop null value
crop_production= crop_production[crop_production$Crop != "Coconut ",]
# because coconut production was creating a big hindrance in our dataset
header= dashboardHeader(title = "Crop Production in India", titleWidth = 750
)
sidebar=dashboardSidebar(
  sidebarMenu(
    id="sidebar",
    menuItem("Inroduction", tabName = "Data", icon=icon("database")),
    menuItem("Data Visualization", icon= icon("chart-line"),
             menuItem("Productivity", tabName = "j"),
             menuItem("Seasonal Productivity", tabName = "k"),
             menuItem("Use of Agricultural Land", tabName="l"),
             menuItem("Total Production in a Year", tabName = "m")),
    menuItem("Conclusion", tabName = "conc")
  )
)
body=  dashboardBody(
  tabItems(
    tabItem(tabName = "Data",
            tabBox(id="t1", width=12, 
                   tabPanel("Abstract", icon= icon("address-card"),h5("With a population of 1.27 billion India is the world's second most populous country. It is the seventh largest country in the world with an area of 3.288 million sq kms. It has a long coastline of over 7,500 kms. India is a diverse country where over 22 major languages and 415 dialects are spoken. With the highest mountain range in the world, the Himalayas to its North, the Thar desert to its West, the Gangetic delta to its East and the Deccan Plateau in the South, the country is home to vast agro-ecological diversity. India is the world's largest producer of milk, pulses and jute, and ranks as the second largest producer of rice, wheat, sugarcane, groundnut, vegetables, fruit and cotton. It is also one of the leading producers of spices, fish, poultry, livestock and plantation crops. Worth $ 2.1 trillion, India is the world's third largest economy after the US and China.
                                                                     Agriculture, with its allied sectors, is the largest source of livelihoods in India. 70 percent of its rural households still depend primarily on agriculture for their livelihood, with 82 percent of farmers being small and marginal. In 2017-18, total food grain production was estimated at 275 million tonnes (MT).  India is the largest producer (25% of global production), consumer (27% of world consumption) and importer (14%) of pulses in the world. India's annual milk production was 165 MT (2017-18), making India the largest producer of milk, jute and pulses, and with world's second-largest cattle population 190 million in 2012.[153] It is the second-largest producer of rice, wheat, sugarcane, cotton and groundnuts, as well as the second-largest fruit and vegetable producer, accounting for 10.9% and 8.6% of the world fruit and vegetable production, respectively.")),
                   tabPanel("Sample Data", icon= icon("address-card"),dataTableOutput("dataT")),
                   tabPanel("About Data and Aim",h5("In our dataset we have 7 variable and 246091 data values, Crop production is given on various parameters like Statewise and seasonwise as well. This dataset provides a huge amount of information on crop production in India ranging from several years. Based on the Information our the ultimate goal is to visualize the crop production in india across their states on various parameters like year season and many more using the R shinny application "),icon= icon("address-card")),
                   tabPanel("SummaryStats", icon= icon("address-card"), verbatimTextOutput("summary")))),
    #second tab item
    tabItem(tabName= "j",
            fluidRow(
              selectInput("var1",label= "Select the State", choices = as.factor(unique(crop_production$State_Name)), selected = "Andhra Pradesh"),
              plotOutput("p1")
            )
    ),
    tabItem( tabName = "k",
             fluidRow(
               selectInput("var2",label= "Select the State accordingly", choices = as.factor(unique(crop_production$State_Name)), selected = "Andhra Pradesh"),
               plotOutput("p2")
             )
             
             
    ),
    tabItem(tabName = "l",
            fluidRow(
              selectInput("var3", label= "Select the State", choices =as.factor(unique(crop_production$State_Name)), selected = "Andhra Pradesh" ),
              plotOutput("p3")
            )
            
    ),
    tabItem(tabName = "m",
            fluidRow(
              selectInput("var4", label= "Select the State", choices =as.factor(unique(crop_production$State_Name)), selected = "Telangana" ),
              plotOutput("p4")
            )
    ),
    
    tabItem(tabName = "conc",
            fluidRow(
              uiOutput("desc")
            ))
  )
)
ui<- dashboardPage(skin="green",header,sidebar, body)
server= function(input,output){
  # summary
  output$summary= renderPrint(
    describe(crop_production, fast= TRUE)
    
  )
  output$dataT= renderDataTable(
    crop_production
  )
  output$p1= renderPlot(
    {
      xc= crop_production[crop_production$State_Name== input$var1,]
      season_df1=xc %>% group_by(Crop_Year) %>% summarise(
        Total_area=sum(Area),
        Total_Production=sum(Production)
        ,Production_Per_Unit_Area= Total_Production/Total_area)
      pp_plot= ggplot(season_df1, aes(Crop_Year, Production_Per_Unit_Area))+
        geom_smooth(se= FALSE)+
        ggtitle("Productivity by Total Area in States of India")
      pp_plot+labs(x= "Year",y= "Total Productivity")
      
      
    })
  output$p2= renderPlot(
    {
      xj= crop_production[crop_production$State_Name== input$var2,]
      season_df2= xj[xj$Season== "Kharif     " |xj$Season=="Rabi       ",] %>% group_by(Crop_Year, Season) %>% summarise(total_production= sum(Production),
                                                                                                                         total_area = sum(Area),production_per_unit_area= total_production/total_area)                         
      season_df2$Crop_Year=as.character(season_df2$Crop_Year)
      gk_g=ggplot(season_df2, aes(Crop_Year,production_per_unit_area, fill= Season ))+
        geom_bar(stat= "identity", position= "dodge")+
        theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))
      gk_g + labs(x="Year", y= "Productivity")+
        ggtitle("Productivity Analysis of Kharif and Rabi Season")
    }
  )
  output$p3= renderPlot(
    {
      cc= crop_production[crop_production$State_Name== input$var3,]
      season_df1=cc %>% group_by(Crop_Year) %>% summarise(
        tot_area=sum(Area),
        tot_prod=sum(Production)
        ,production_per_unit_area= tot_prod/tot_area)
      ggplot(season_df1, aes(Crop_Year,tot_area))+
        geom_smooth(se= FALSE)+
        ggtitle("Land used for Agriculture in India by States")+
        labs(x="Year", y= "Agricultural Land(In Acres)")
    }
  )
  output$p4= renderPlot(
    {
      dk= crop_production[crop_production$State_Name== input$var4,]
      season_df3= dk[dk$Crop_Year== 2013, ] %>% group_by(Crop) %>% summarise(tot_prd= sum(Production), tot_area= sum(Area), pro= tot_prd/tot_area)
      ggplot(season_df3, aes(x=Crop,y=tot_prd))+
        geom_col()+
        theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust=1))+labs(y= "Total Production(In Tones)")+ggtitle("Crop Production in 2014")
      
    }
  )
  output$desc= renderUI(
    tags$div(
      tags$br(),
      tags$br(),
      tags$ul(
        tags$li(h4("In the data set, productivity is increasing for all the Indian states except Uttrakhand since 1997.")),
        
        tags$ul(
          tags$li(h4("We define productivity as total producation divided by area that was used for production."))
        ),
        tags$br(),
        tags$li(h4("Seasonal productivity has increased in both  Kharif as well rabi season. ")),
        
        tags$ul(
          tags$li(h4("Increasing rate of productivity is higher in Kharif season.")),
          
          tags$li(h4("Most of the states has 3-4 times high productivity in kharif season than rabi season."))
        ),
        tags$br(),
        tags$li(h4("Some states has increased agricultural land and some has decreased:")),
        
        tags$ul(
          tags$li(h4("Uttar Pradesh, Jharkhand, Meghlay like states has decreased their agricultural land.")),
          
          tags$li(h4("Andhra Pradesh, Assam , Haryana like states has increased their agricultural land."))
        ),
        tags$br(),
        tags$li(h4("Each state has its main seasonal crops, so production of each crop varies from  state to state:")),
        tags$ul(
          tags$li(h4("Andhra Pradesh has major production in Sugar cane and Rice.")),
          tags$li(h4("Bihar has most of the production in Rice and wheat."),
                  tags$li(h4("Kerala has heighest production in Taploca.")),
                  tags$li(h4("West Bangal has major production in jute, Rice and Potato."))
          )
        )
        
        
        
      )
    )
  )
  
  
}
shinyApp(ui = ui, server = server)