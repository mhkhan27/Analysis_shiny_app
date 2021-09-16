
# library ------------------------------------------------------------------

library(shiny)
library(illuminate)
library(srvyr)
library(stringr)
library(openxlsx)
library(readxl)
library(purrr)
library(dplyr)
library(shinythemes)
library(shinyWidgets)
library(DT)



# UI ----------------------------------------------------------------------

ui <- fluidPage(
  
  titlePanel("Data Analysis App"),
  hr(),
  
  # setBackgroundImage(
  #   src = "https://www.freepik.com/free-vector/diagonal-motion-lines-white-background_17564647.htm"
  # ),
  
  actionButton("run", "Run"), 
  downloadButton("download_data", label = "Download Data", class = NULL),
  downloadButton("download_analysis", label = "Download Analysis", class = NULL),
  
  hr(),
    
    tabPanel( "Inputs",
    
    sidebarLayout(
        sidebarPanel(
            fileInput("data","Upload dataset*"),
            fileInput("survey","Upload the kobo survey sheet*"), ## need to check the capital
            
            hr(),
            fileInput("sample","Upload sample frame*"),
            
            pickerInput("sample_strata",
                        label = "Define strata column name in sample frame:",   
                        choices = NULL,
                        selected = NULL,
                        multiple = FALSE,
                        options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
            ),
            
            
            pickerInput("population",
                        label = "Define population column name in sample frame:",   
                        choices = NULL,
                        selected = NULL,
                        multiple = FALSE,
                        options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
            ),
            
            hr(),
            
            pickerInput("weighted",
                        label = "Do you want to use weight in your analysis?",   
                        choices = c("yes","no"),
                        selected = "no",
                        multiple = F,
                        options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
            ),
            
            pickerInput("select_strata",
                        label = "Define strata column name in dataset:",   
                        choices = NULL,
                        selected = NULL,
                        multiple = FALSE,
                        options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
            ),
            
            hr(),
            
            # pickerInput("select_weights",
            #             label = "Define weight column:",   
            #             choices = NULL,
            #             selected = "NULL",
            #             multiple = FALSE,
            #             options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
            # ),
            
            pickerInput("select_col_not_to_analyse",
                        label = "Select all unnecessary variables from the dataset (all date/text column will be removed from the analysis autometically):",   
                        choices = NULL,
                        selected = NULL,
                        multiple = T,
                        options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
            ),
            
            
            pickerInput("repeat_variable",
                        label = "Disagregation level",   
                        choices = NULL,
                        selected = NULL,
                        multiple = T,
                        options = pickerOptions(title = "Select", actionsBox = TRUE, liveSearch = TRUE)
            ),
            
            
    ),
    
   
    
    
 mainPanel(
       DTOutput("d"),
     # tableOutput("input_survey_Sheet"),
     # tableOutput("input_choice_Sheet")
 )
)),

)# fluidpage

hr()





# server ------------------------------------------------------------------

server <- function(input, output,session) {
    
############################# read data #################################################
    
    data_upload <- reactive({
        req(input$data)
        tryCatch(
            {
                data_upload <- read.csv(input$data$datapath,
                                        na.strings=c("", "NA", " "),
                                        stringsAsFactors = F)
            },
            error = function(e) {
                stop(safeError(e))
            }
        )
    })



############################# read survey sheet #################################################

survey_df <- reactive({
    req(input$survey)
    tryCatch(
        {
            data_upload <- read.csv(input$survey$datapath,
                                    na.strings=c("", "NA", " "),
                                    stringsAsFactors = F)
        },
        error = function(e) {
            stop(safeError(e))
        }
    )
})


############################# read choice #################################################

sample_frame_df <- reactive({
    req(input$sample)
    tryCatch(
        {
            data_upload <- read.csv(input$sample$datapath,
                                    na.strings=c("", "NA", " "),
                                    stringsAsFactors = F)
        },
        error = function(e) {
            stop(safeError(e))
        }
    )
})

 ################################## show read data ##################################################

# output$d <- renderTable({data_upload()}) # show read data
# output$input_survey_Sheet <- renderTable({survey_df()}) # show read data
# output$input_choice_Sheet <- renderTable({choice_df()}) # show read data

##################################FIX Data format ####################################################
 df_fixed_formt <- reactive({fix_data_type_frm_kobo(df = data_upload(),kobo_servey_sheet = survey_df())})

 
 ############## list of variable ########################################    
 indicators <- reactive({names(df_fixed_formt())})
 sample_fram_indicatiors <- reactive({names(sample_frame_df())})
 
 ########################################################################
 
 observe({
     input$data
     updatePickerInput(session, "select_strata", choices = indicators())
     updatePickerInput(session, "select_weights", choices = indicators())
     updatePickerInput(session, "select_col_not_to_analyse", choices = indicators())
     updatePickerInput(session, "repeat_variable", choices = indicators())
     
     updatePickerInput(session, "sample_strata", choices = sample_fram_indicatiors())
     updatePickerInput(session, "population", choices = sample_fram_indicatiors())

 })
 
 
 
 ###################################### weights #######################################################
 
 name1 <- reactive({input$select_strata})
 name2 <- reactive({input$sample_strata})
 population_c <- reactive({input$population})
 
 weights_df <- eventReactive(input$run, {
   
   df_fixed_formt() %>% group_by_(input$select_strata) %>% summarise(
   survey_count = n()
 ) %>% left_join(sample_frame_df(),by = setNames(name2(),name1())) %>% 
   mutate(sample_global=sum(survey_count),
          pop_global=sum(as.integer(!!sym(population_c()))),
          survey_weight= (as.integer(!!sym(population_c()))/pop_global)/(survey_count/sample_global)) %>% select(input$select_strata,survey_weight)
   })
 
 final_data <-  eventReactive(input$run, {df_fixed_formt() %>% left_join(weights_df())})
 

 ###########################################################################################################
 
 svy_design <- eventReactive(input$run, {
   
   if(input$weighted == "yes"){

 svy_design <- as_survey(final_data(),strata = input$select_strata, weights = survey_weight)

   }
   
   # if(input$weighted == "yes"){ 
   #   
   #   svy_design <- as_survey(final_data(), weights = survey_weight)
   #   
   # }
   
 
 if(input$weighted == "no"){ 
   
   svy_design <- as_survey(final_data())
   }
   return(svy_design)
 })
 
 ############################################ cols_to_ana  ##############################################
  
 text_cols <- reactive((survey_df() %>% dplyr::filter(type == "text"))$name) # remove all text column from the analysis
 note_cols <- reactive((survey_df() %>% dplyr::filter(type == "note"))$name) # remove all note column from the analysis
 date_cols <- reactive((survey_df() %>% dplyr::filter(type == "date"))$name)# remove all date column from the analysis
 starts_with_x <- reactive(final_data() %>% select(starts_with("X_")) %>% names()) # remove all columns that starts_With X_ (X_uuid,X_index.......)
 
 
 cols_not_to_ana <-reactive(c(text_cols(),starts_with_x(),date_cols(),note_cols(),input$select_col_not_to_analyse,"survey_weight") %>% unique())
 
 cols_not_to_ana2 <- reactive(cols_not_to_ana()[cols_not_to_ana() %in% names(final_data())])
 
 col_to_analysis <- reactive(final_data() %>% dplyr::select(-cols_not_to_ana2()) %>% names())
 
 
 ################################################# Analysis ###########################################################
 
 
 analysis <- eventReactive(input$run,{
 
if(is.null(input$repeat_variable)){
analysis <- survey_analysis(df = svy_design(),vars_to_analyze = col_to_analysis(),
                                     sm_sep = ".",question_lable = F)
 }
 
 
if(!is.null(input$repeat_variable)){
     analysis <- survey_analysis(df = svy_design(),vars_to_analyze = col_to_analysis(),
                                 sm_sep = ".",question_lable = F,disag = input$repeat_variable)
   }   
   
return(analysis)   
   
 })
  
 
 # output$d <- renderTable({analysis()})
 
 
 output$d <- DT::renderDataTable(
   
   #data
   analysis(), rownames = FALSE,
   
   # column filter on the top
   filter = 'top', server = TRUE,
   
   # autoWidth
   options = list(autoWidth = TRUE,
                  pageLength = 25))
 
 
 
 output$download_analysis <- downloadHandler(
   filename = function() {
     paste('Analysis', Sys.Date(), '.csv', sep='')
   },
   content = function(con) {
     write.csv(analysis(), con)
   }
 )
 
 
 
 output$download_data <- downloadHandler(
   filename = function() {
     paste('Data', Sys.Date(), '.csv', sep=',')
   },
   content = function(con) {
     write.csv(final_data(), con)
   }
 )
 
  
}


# Run the application 
shinyApp(ui = ui, server = server)
