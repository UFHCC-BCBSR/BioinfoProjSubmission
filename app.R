library(shiny)
library(readxl)
library(writexl)
library(DT)
library(glue)
library(janitor)
library(shinyjs)
source("helpers.R")
print("✅ This is the NEW version of the app!")

ui <- fluidPage(
  useShinyjs(), 
  uiOutput("page_title"),
    mainPanel(
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Project Metadata",
                 helpText("All Fun fields below are required in order to submit a project for ATAC-seq analysis."),
                 helpText("Please contact your bioinformatics collaborator if you need assistance filling out this form."),
                 helpText("When all fields are complete and your sample information is uploaded and validated, you will be able to submit the project using the submit button on the top right."),
                 
                 textInput("report_title", "Report Title", placeholder = "e.g. ATAC-seq Analysis of NS3982"),
                 helpText("This title appears at the top of the generated report."),
                 
                 textInput("seqID", "Sequencing ID (SeqID)", placeholder = "e.g. NS3984"),
                 helpText("The unique ID for the sequencing run given by the sequencing facility."),
                 
                 textInput("pi", "Principal Investigator Name"),
                 textInput("pi_email", "Principal Investigator Email"),
                 
                 textInput("study_contact", "Study Contact Name"),
                 textInput("study_contact_email", "Study Contact Email"),
                 
                 textInput("institution", "Institution"),
                 textInput("department", "Department"),
                 textInput("project_title", "Project Title"),
                 textInput("study_summary", "Study Summary"),
                 textInput("sample_types", "Sample Types"),
                 
                 selectInput("spike", "Spike-In Control", choices = c("Drosophila melanogaster", "None")),
                 helpText("If a spike-in control was used, we will assemble sequence reads to the spike-in reference genome and perform spike-in normalization"),
                
                 selectInput("organism", "Organism", choices = c("Homo sapiens" = "hsa", "Mus musculus" = "mmu")),
                 selectInput("batch_var", "Batch Variable",choices=c("None","Batch")),
                 helpText("If data were processed in batches, you must include a variable named \"Batch\" in your Sample Information sheet so that we can assess batch effects and perform batch correction if needed."),
                 
                 #actionButton("submit_button", "Submit Project", class = "btn-primary"),

        ),
        
        tabPanel("Sample Metadata",
                 uiOutput("validation_box"),
                 br(),
                 helpText("Download the example file and carefully enter your sample information and desired contrasts in exactly this format."),
                 helpText("You will not be able to submit your project until your sample information and contrasts pass validation."),
                 helpText("Please contact your bioinformatics collaborator if you need assistance formatting and uploading this file."),
                 downloadButton("download_template", "Download Excel Template"),
                 fileInput("upload_excel", "Upload Excel File (Sample Information + Contrasts)", accept = ".xlsx"),
                 tabsetPanel(
                   tabPanel("Sample Information", DTOutput("sample_table")),
                   tabPanel("Contrasts", DTOutput("contrast_table"))
                 )
        )
      )
    )
    
  ,
  actionButton("submit_button", "Submit Metadata", class = "btn-primary")
)

server <- function(input, output, session) {
  output$page_title <- # Define a safe reactive assay accessor
    assay <- reactive({
      query <- getQueryString()
      if (!is.null(query$assay) && nzchar(query$assay)) {
        return(query$assay)
      }
      return("ATACseq")  # default fallback
    })
  
  output$page_title <- renderUI({
    selected_assay <- assay()
    
    title <- switch(selected_assay,
                    "RNAseq" = "RNA-seq Project Submission",
                    "CUTandRUN" = "CUT&RUN Project Submission",
                    "ATACseq"  # default
    )
    
    titlePanel(title)
  })
  
  
  observe({
    # Required form inputs
    required_fields <- list(
      input$report_title, input$seqID, input$pi, input$pi_email,
      input$study_contact, input$study_contact_email, input$institution,
      input$department, input$project_title, input$study_summary,
      input$sample_types, input$organism
    )
    
    all_filled <- all(sapply(required_fields, function(x) !is.null(x) && x != ""))
    validation_ok <- !is.null(data$validation) && data$validation == "Validation passed successfully."
    
    shinyjs::toggleState("submit_button", condition = all_filled && validation_ok)
  })
  output$validation_box <- renderUI({
    req(data$validation)
    
    if (data$validation == "Validation passed successfully.") {
      div(
        style = "background-color: #d4edda; padding: 10px; border-radius: 5px; color: #155724; font-weight: bold;",
        icon("check-circle"), " Excel validation passed."
      )
    } else {
      div(
        style = "background-color: #f8d7da; padding: 10px; border-radius: 5px; color: #721c24;",
        icon("exclamation-triangle"), strong("Validation failed:"), 
        tags$br(),
        tags$pre(data$validation)
      )
    }
  })
  
  data <- reactiveValues(sample = NULL, contrast = NULL, validation = NULL)
  
  observeEvent(input$upload_excel, {
    req(input$upload_excel)
    path <- input$upload_excel$datapath
    
    data$sample <- readxl::read_excel(path, sheet = "SampleInformation") |> janitor::clean_names(case = "none")
    data$contrast <- readxl::read_excel(path, sheet = "Contrasts") |> janitor::clean_names(case = "none")
    
    # Run validation immediately
    data$validation <- validate_inputs(data$sample, data$contrast)
  })
  
  
  output$sample_table <- renderDT({ datatable(data$sample) })
  output$contrast_table <- renderDT({ datatable(data$contrast) })
  
  observeEvent(input$validate_btn, {
    req(data$sample, data$contrast)
    data$validation <- validate_inputs(data$sample, data$contrast)
  })
  
  output$validation_results <- renderPrint({
    req(data$validation)
    cat(data$validation)
  })
  
  output$download_template <- downloadHandler(
    filename = function() { "Template.xlsx" },
    content = function(file) {
      file.copy("template.xlsx", file, overwrite = TRUE)
    }
  )
  
  
  output$download_params <- downloadHandler(
    filename = function() {
      paste0("params_", input$seqID, ".txt")
    },
    content = function(file) {
      required_fields <- list(
        report_title = input$report_title,
        seqID = input$seqID,
        pi = input$pi,
        institution = input$institution,
        department = input$department,
        project_title = input$project_title,
        study_summary = input$study_summary,
        sample_types = input$sample_types,
        organism = input$organism,
        study_contact = input$study_contact
      )
      
      missing <- names(required_fields)[sapply(required_fields, function(x) is.null(x) || x == "")]
      
      if (length(missing) > 0) {
        shiny::showModal(modalDialog(
          title = "Missing Required Fields",
          paste("The following required fields are missing or empty:", 
                paste(missing, collapse = ", ")),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return(NULL)
      }
      
      
      # Proceed if all inputs are valid
      params <- generate_params(
        input$report_title, input$seqID, input$pi, input$institution, input$department,
        input$project_title, input$study_summary, input$sample_types, input$organism, input$study_contact,
        input$test_mode,input$batch_var,input$spike_in_control
      )
      
      writeLines(params, con = file)
    }
  )
  
  
  
  output$download_excel <- downloadHandler(
    filename = function() { paste0("Final_Metadata_", input$seqID, ".xlsx") },
    content = function(file) {
      req(data$sample, data$contrast)
      write_xlsx(list(SampleInformation = data$sample, Contrasts = data$contrast), path = file)
    }
  )
  
  observe({
    required_fields <- list(
      input$report_title, input$seqID, input$pi, input$institution, input$department,
      input$project_title, input$study_summary, input$sample_types, input$organism, input$study_contact
    )
    
    any_missing <- any(sapply(required_fields, function(x) is.null(x) || x == ""))
    
    shinyjs::toggleState("download_params", condition = !any_missing)
  })
  observeEvent(input$submit_button, {

      req(data$validation)
      
      if (data$validation != "Validation passed successfully.") {
        showModal(modalDialog(
          title = "Cannot Submit",
          "The uploaded sample and contrast sheets did not pass validation. Please fix the issues before submitting.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        return(NULL)
      }
    req(data$sample, data$contrast)
    
    # Generate unique folder
    uuid <- uuid::UUIDgenerate()
    folder_id <- paste0(gsub(" ", "_", input$pi), "_", input$seqID, "_", uuid)
   #output_dir <- file.path(tempdir(), "data_submissions", assay(), folder_id)
     output_dir <- file.path("/pubapps/jobrant/bcb-sr/dev/apps/ProjectReporting", "data_submissions", assay(), folder_id)
    
dir.create(output_dir, recursive = TRUE,showWarnings=TRUE)
    
    # Define filenames
    sample_file <- paste0(folder_id, "_samples.txt")
    contrast_file <- paste0(folder_id, "_contrasts.txt")
    param_file <- paste0(folder_id, "_params.txt")
    
    # Write sample and contrast tables
    write.table(data$sample, file = file.path(output_dir, sample_file),
                sep = "\t", quote = FALSE, row.names = FALSE)
    
    write.table(data$contrast, file = file.path(output_dir, contrast_file),
                sep = "\t", quote = FALSE, row.names = FALSE)
    
    # Generate updated params with relative paths
    params <- generate_params(
      report_title = input$report_title,
      seqID = input$seqID,
      pi = input$pi,
      pi_email = input$pi_email,
      institution = input$institution,
      department = input$department,
      project_title = input$project_title,
      study_summary = input$study_summary,
      sample_types = input$sample_types,
      organism = input$organism,
      study_contact = input$study_contact,
      study_contact_email = input$study_contact_email,
      prepared_by = input$prepared_by,
      reviewed_by = input$reviewed_by,
      test_mode = input$test_mode,
      sample_file = sample_file,
      contrast_file = contrast_file,
      batch_var=input$batch_var,
      spike_in_control=input$spike_in_control
    )
    
    writeLines(params, file.path(output_dir, param_file))
    # Define the static file server directory
    #public_base <- "/orange/cancercenter-dept/web/public/BCB-SR/project_submissions"
    #public_dir <- file.path(public_base, folder_id)
    
    output$download_submission <- downloadHandler(
      filename = function() {
        paste0("submission_", input$seqID, ".zip")
      },
      content = function(file) {
        # Determine the path to the PI_UUID folder
        pi_uuid_path <- file.path(output_dir)
        
        # List all files within the PI_UUID folder
        files_to_zip <- list.files(pi_uuid_path, full.names = TRUE)
        
        # Create the ZIP file containing only the files from PI_UUID
        zip::zipr(zipfile = file, files = files_to_zip, root = pi_uuid_path)
      }
    )
    
    
    showModal(modalDialog(
      title = "Submission Ready",
      "Your submission was saved and can be downloaded below.",
      downloadButton("download_submission", "Download ZIP"),
      easyClose = TRUE
    ))
    
    
  })
  
  
}

shinyApp(ui = ui, server = server)
