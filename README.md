# ATAC-seq Metadata Submission App

This Shiny application provides a structured, user-friendly interface for submitting project and sample metadata required for ATAC-seq analysis. It ensures completeness and consistency of input while enforcing validation rules based on your internal analysis pipeline.

---

## Features

- Interactive form for entering project-level metadata
- File upload for structured Excel metadata sheets (`SampleInformation` and `Contrasts`)
- Live validation with detailed error feedback
- Downloadable parameter file (`params.txt`) for pipeline integration
- Automatic submission output to a server-side folder with UUID-based tracking
- Public-facing static URL generation for each submission

---

## Folder Structure

Upon submission, the app generates a folder named like `PI_SeqID_UUID`, containing:

- `PI_SeqID_UUID_params.txt`: structured parameters for downstream pipeline
- `PI_SeqID_UUID_samples.txt`: sample metadata
- `PI_SeqID_UUID_contrasts.txt`: contrast definitions
- (Optional) `README.html` or linkable metadata stub

This folder is copied to a public web directory (e.g. `/orange/cancercenter-dept/web/public/BCB-SR/project_submissions`) and a corresponding link is displayed to the user.

---

## Excel Template

The app expects an uploaded Excel file with two sheets:

### Sheet 1: `SampleInformation`

Required columns:
- `Sample_Name`: Internal sample label
- `SeqID`: Must match sequencing FASTQ naming
- Other variables: Used for grouping, contrasts, etc.
- `Notes`: Optional field for QC or comments

### Sheet 2: `Contrasts`

Required columns:
- `Variable`: Must match a column name in `SampleInformation`
- `Group1` / `Group2`: Must match values in the specified variable column

You can download an example file by clicking **"Download Excel Template"** in the app.

---

## Validation

Validation is triggered automatically upon file upload and includes:
- Matching contrast variables to sample columns
- Ensuring all contrast levels exist in sample metadata
- Disallowing names that start with numbers or contain dashes
- Ensuring all required fields are completed before enabling submission

Validation errors are shown prominently at the top of the form and must be resolved before submission.

---

## Server Requirements

- R + Shiny
- Packages:
  - `shiny`, `shinyjs`, `readxl`, `writexl`, `DT`, `janitor`, `glue`, `uuid`
- A writable output directory for storing submission data
- An Apache- or NGINX-accessible static file directory (e.g. `/orange/cancercenter-dept/web/public`)
- Optional: scriptable permission fixes (e.g., `chmod o+rx` on new submission folders)

---

## Customization

To adapt this app for other sequencing assays or metadata formats:

- Update the `validate_inputs()` function in `helpers.R`
- Modify `generate_params()` to reflect different pipeline arguments
- Replace `template.xlsx` with your preferred format

---

## License

This project is developed for internal use by the [UF Health Cancer Center Bioinformatics Core](https://cancer.ufl.edu/), but may be adapted or extended under standard academic sharing principles. No warranty is provided.


