validate_inputs <- function(sample_df, contrast_df) {
  errors <- c()
  
  # ---- Check required columns exist ----
  if (!"SampleID" %in% colnames(sample_df)) {
    errors <- c(errors, "Missing 'SampleID' column in SampleInformation.")
  }
  if (!"SeqID" %in% colnames(sample_df)) {
    errors <- c(errors, "Missing 'SeqID' column in SampleInformation.")
  }
  if (!all(c("Variable", "Contrast") %in% colnames(contrast_df))) {
    errors <- c(errors, "Contrast file must contain 'Variable' and 'Contrast' columns.")
    return(paste(errors, collapse = "\n")) # Can't proceed without this
  }
  
  # ---- Check that all contrast variables are present in sample metadata ----
  missing_vars <- setdiff(contrast_df$Variable, colnames(sample_df))
  if (length(missing_vars) > 0) {
    errors <- c(errors, paste("These variables from contrast file are missing in sample file:", 
                              paste(missing_vars, collapse = ", ")))
  }
  
  # ---- Validate contrasts against sample_df ----
  for (i in seq_len(nrow(contrast_df))) {
    var <- contrast_df$Variable[i]
    contrast <- contrast_df$Contrast[i]
    
    if (!var %in% colnames(sample_df)) next
    
    # Split contrast string
    if (!grepl("_vs_", contrast)) {
      errors <- c(errors, paste("Contrast", contrast, "is not properly formatted with '_vs_'"))
      next
    }
    
    parts <- unlist(strsplit(contrast, "_vs_"))
    if (length(parts) != 2) {
      errors <- c(errors, paste("Contrast", contrast, "does not split cleanly into 2 parts."))
      next
    }
    
    g1 <- parts[1]
    g2 <- parts[2]
    levels_var <- unique(sample_df[[var]])
    
    # Check levels exist
    if (!(g1 %in% levels_var)) {
      errors <- c(errors, sprintf("'%s' (from contrast %s) not found in levels of '%s'", g1, contrast, var))
    }
    if (!(g2 %in% levels_var)) {
      errors <- c(errors, sprintf("'%s' (from contrast %s) not found in levels of '%s'", g2, contrast, var))
    }
    
    # Check replicates (at least 2 per group)
    n_g1 <- sum(sample_df[[var]] == g1)
    n_g2 <- sum(sample_df[[var]] == g2)
    
    if (n_g1 < 2) {
      errors <- c(errors, sprintf("Less than 2 replicates for group '%s' in variable '%s'", g1, var))
    }
    if (n_g2 < 2) {
      errors <- c(errors, sprintf("Less than 2 replicates for group '%s' in variable '%s'", g2, var))
    }
  }
  
  # ---- Sample naming and character constraints ----
  bad_names <- c(sample_df$SeqID, contrast_df$Contrast,contrast_df$Variable,colnames(sample_df))
  if (any(grepl("^[0-9]", bad_names))) {
    errors <- c(errors, "Some SeqIDs, Variables, or Contrast names start with a number, which is not allowed.")
  }
  if (any(grepl("-", bad_names))) {
    errors <- c(errors, "SeqIDs, Variables, or Contrast names may not contain dashes ('-').")
  }
  
  # ---- Duplicates ----
  if (any(duplicated(sample_df$SampleID))) {
    dupes <- unique(sample_df$SampleID[duplicated(sample_df$SampleID)])
    errors <- c(errors, paste("Duplicate SampleIDs found:", paste(dupes, collapse = ", ")))
  }
  
  # ---- Return result ----
  if (length(errors) == 0) {
    return("Validation passed successfully.")
  } else {
    return(paste(errors, collapse = "\n"))
  }
}


generate_params <- function(
    report_title, seqID, pi, pi_email, institution, department,
    project_title, study_summary, sample_types, organism,
    annotation_db, spike_in_control, study_contact, study_contact_email,
    prepared_by, reviewed_by, test_mode,
    sample_file, contrast_file,batch_var
)

{
  annotation_db <- switch(
    organism,
    "hsa" = "org.Hs.eg.db",
    "mmu" = "org.Mm.eg.db",
    stop("Unsupported organism")
  )
  required_values <- list(
    report_title = report_title,
    seqID = seqID,
    pi = pi,
    institution = institution,
    department = department,
    project_title = project_title,
    study_summary = study_summary,
    sample_types = sample_types,
    organism = organism,
    annotation_db = annotation_db,
    study_contact = study_contact
  )
  
  missing_fields <- names(required_values)[sapply(required_values, function(x) is.null(x) || x == "")]
  if (length(missing_fields) > 0) {
    stop(paste("The following required fields are missing or empty:", 
               paste(missing_fields, collapse = ", ")))
  }
  
  glue_lines <- glue::glue(
    "--report_title \"{report_title}\"\n",
    "--group_var \"Condition\"\n",
    "--filter_samples\n",
    "--pipeline_output \"/path/to/pipeline/OUTPUT\"\n",
    "--contrasts \"{contrast_file}\"\n",
    "--sample_data \"{sample_file}\"\n",
    "--genome \"/path/to/genome/\"\n",
    "--outdir \"/path/to/output/files/\"\n",
    "--organism \"{organism}\"\n",
    "--annotation_db \"{annotation_db}\"\n",
    "--Study_Contact \"[{study_contact}](mailto:{study_contact_email})\"\n",
    "--PI \"[{pi}](mailto:{pi_email})\"\n",
    "--Institution \"{institution}\"\n",
    "--Department \"{department}\"\n",
    "--Project_Title \"{project_title}\"\n",
    "--Study_Summary \"{study_summary}\"\n",
    "--Sample_Types \"{sample_types}\"\n",
    "--Organism \"Homo sapiens\"\n",
    "--Analysis_Goals \"\"\n",
    "--Report_Prepared_By \"\"\"\n",
    "--Report_Reviewed_By \"\"\"\n",
    "--raw_seq_URL \"\"\n",
    "--multiqc_html \"/path/to/multiqc_report.html\"\n",
    "--nfcore_output_dir \"/path/to/pipeline/OUTPUT/\"\n",
    "--nfcore_spikein_dir \"/path/to/pipeline/OUTPUT_DMel/\"\n",
    "--seqID \"{seqID}\"\n",
    "--test_mode {tolower(test_mode)}\n",
    "--spike_in \"{spike_in_control}\"\n",
    "--batch_var \"{batch_var}\"\n"
  )
  
  return(glue_lines)
}

