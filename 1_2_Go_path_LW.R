
############ Go_path function ############

Go_path <- function(project, pdf = TRUE, table = FALSE, path = NULL){
  # If a TRUE is assigned to "pdf" or "table" argument, the Go_path function will generate the folder directory, respectively.
  # If a FALSE is assigned to "pdf" or "table" argument, no folder directory will be generated. The if loops under "# Main pdf" / "# Main table" will return nothing.
  
  if(pdf %notin% c(TRUE, FALSE)) { stop("Please input 'pdf = TRUE' or 'pdf = FALSE'.") }
  if(table %notin% c(TRUE, FALSE)) { stop("Please input 'table = TRUE' or 'table = FALSE'.") }
  
  # Main directory
  # This creates a different sub-folder path if analysis was conducted every day, providing a "ProjectName_Date" format.
  out <- file.path(sprintf("%s_%s", project, format(Sys.Date(), "%y%m%d")))   # sprintf() provides a C-style string format.
  if(!file_test("-d", out)) { dir.create(out) }     # "-d" in file_test() tests existence and directory.
  
  # Main pdf.
  # "pdf" variable passed from the function call is a character variable, but "pdf" acts like a logical one.
  if(pdf){
    out_pdf <- file.path(sprintf("%s/pdf", out))
    if(!file_test("-d", out_pdf)) { dir.create(out_pdf) }
    print(sprintf("pdf is in your working dir. Use '%s' to save.", out_pdf))
  } else if(!pdf){
    print("pdf argument is set FALSE. No pdf dir created.")
  } else print("No pdf dir.")   # "No pdf dir." actually tells that "pdf" is null when calling this function.
  
  # Main table
  if(table){
    out_tab <- file.path(sprintf("%s/table", out)) 
    if(!file_test("-d", out_tab)) { dir.create(out_tab) }
    print(sprintf("table is in your working dir. Use '%s' to save.", out_tab))
  } else if(!table){
    print("table argument is set FALSE. No table dir created.")
  } else print("No table dir.")
  
  # "path" is a character string.
  if(is.null(path)){
    print("No another dir.")
  } else {
    out_path <- file.path(sprintf("%s/%s", out, path))
    if(!file_test("-d", out_path)) { dir.create(out_path) }
    print(sprintf("Another path (%s) is in your working dir. Use `out_path` to save.", path))
  }
  
  # Write a function that returns two values
  functionReturningTwoValues <- function() {
    dirs <- list()
    if(is.null(pdf)) {
    } else if(pdf){
      dirs$pdf <- out_pdf
    }
    
    if(is.null(table)){
    } else if(table){
      dirs$tab <- out_tab
    }
    
    if(is.null(path)){
    } else {
      dirs$path <- out_path
    }
    return(dirs)
  }
  
  functionReturningTwoValues()
}

