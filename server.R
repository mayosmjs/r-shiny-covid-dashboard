
directory <- function(path, recursive = FALSE, local = TRUE) {
    if (!dir.exists(path)) {
        warning(paste(path, "Directory not valid"))
        return(NULL)
    }
    
    if (is.logical(local) && local) { env <- parent.frame() }
    else if (is.logical(local) && !local) { env <- globalenv() }
    else if (is.environment(local)) { env <- local }
    else { stop("'local' must be TRUE, FALSE or an environment") }
    files <- list.files(path = path, pattern = ".*\\.R", all.files = F, full.names = TRUE, recursive = recursive)
    for (fileToSource in files) {
        tryCatch(
            {
                source(fileToSource, local = env)
                cat(fileToSource, "sourced.\n")
            },
            
            error = function(cond) {
                message("Loading of file \"", fileToSource, "\" failed.")
                message(cond)
            }
            
        )
    }
}

#Run files for the server recursively
anotherServer <- function(input, output) {
    directory("content", recursive = TRUE)
    
    #Check for hopkins git updates hourly
    loadGit <- reactiveTimer(3600000)
    
    observeEvent(loadGit, {
        updateData()
    })
    
    observe({
        data <- presentData(input$timeSlider)
    })
   
}