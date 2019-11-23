## RevBayes
eng_rb <- function(options) {
  # options - variables from knitr - called herein:
  # options$code - string, the code for that chunk
  # options$error - logical, should it fail on an error
  # options$eval - logical, should the code be evaluated
  # options$engine - should be == 'rb'
  # options$engine.path - path to rb
  #
  # options$rbHistoryDirPath - directory to put history files
  # options$refreshHistoryRB - remove existing history files if this is a new knitr doc
  # options$rbDiagnosticMode - run diagnostic mode
  ################################
  # set rbDiagnosticMode
  if(is.null(options$rbDiagnosticMode)){
    options$rbDiagnosticMode <- FALSE
  }
  # early exit if evaluated output not requested
  if (!options$eval){
    options$results = 'asis'
    return(engine_output(options, options$code, ''))
  }
  # set up path to rb
  rbPath <- knitr:::get_engine_path(options$engine.path, 'rb')
  # options$engine.opts - opts for engines that should include 'rb'
  # use get_engine_opts to pull out rb options
  opts <- get_engine_opts(options$engine.opts, 'rb')
  # engine specific options
  #
  # options$refreshHistoryRB
  # logical
  # Controls whether previous .eng_rb.knitr.cache files
  # should be deleted if this is the first rb chunk
  # If not defined, default is TRUE
  if(is.null(options$refreshHistoryRB)){
    options$refreshHistoryRB <- TRUE
  }
  # options$rbHistoryDirPath
  # string - path and name for rb history directory
  # default is ".eng_rb.knitr.cache" in working dir
  if(is.null(options$rbHistoryDirPath)){
    options$rbHistoryDirPath <- ".eng_rb.knitr.history/"
  }
  #############
  rbOutPath <- paste0(options$rbHistoryDirPath, '/.eng_rb_out')
  rbCodePath <- paste0(options$rbHistoryDirPath, '/.eng_rb_code')
  rbOutPath <- normalizePath(rbOutPath, mustWork = FALSE)
  rbCodePath <- normalizePath(rbCodePath, mustWork = FALSE)
  #if(options$rbDiagnosticMode){ message("rb_chunk_counter = ", rb_chunk_counter(-1) )}
  # check (and simultaneously update) rb_chunk_counter()
  if(knitr:::rb_chunk_counter() == 1L){
    # this is the first time an rb code-chunk is run for this document
    # set prev_out artificially to 13
    prev_out <- 13
    #
    if(dir.exists(options$rbHistoryDirPath) & options$refreshHistoryRB){
      # need to get rid of old history
      unlink(options$rbHistoryDirPath, recursive = TRUE)
    }
    # once old files are cleared (if they exist)
    # Set up history directories
    dir.create(options$rbHistoryDirPath, showWarnings = TRUE)
    # get code to run
    code_to_run <- options$code
    # change options$refreshHistoryRB
    options$refreshHistoryRB <- FALSE
  }else{
    # if FALSE, then this isn't the first chunk in a document
    # error if history dir doesn't already exist (?)
    if(!dir.exists(options$rbHistoryDirPath)){
      stop("RevBayes history directory not found at specified path for later rb chunks")
    }
    # get length of old out file
    prev_out <- length(readLines(rbOutPath))
    # get old code history
    old_code <- readLines(rbCodePath)
    print(old_code)
    # combine
    code_to_run <- c(old_code, options$code)
  }
  # write code to history file
  xfun::write_utf8(code_to_run, con = rbCodePath)
  # make a temporary file of rb code to execute
  # don't need to one-string code
  tempF <- knitr:::wd_tempfile('.rb', '.Rev')
  tempF <- normalizePath(
    tempF,
    winslash = "/",
    mustWork = FALSE
  )
  # write to file and add q() line
  xfun::write_utf8(c(code_to_run, "q()"), con = tempF)
  # setup to delete temporary files for execution when done
  if(!options$rbDiagnosticMode){
    on.exit(unlink(tempF), add = TRUE)
  }
  # correct order for rb is options + cmdArg + file
  cmdArg = paste(opts, '-b', tempF)
  # execute code
  out = if (options$eval) {
    message(paste0('running Revbayes with cmd: rb ', cmdArg))
    tryCatch(
      system2(rbPath, cmdArg,
              stdout = TRUE,
              stderr = TRUE,
              env = options$engine.env
      ),
      error = function(e) {
        if (!options$error) stop(e)
        paste('Error in running command rb:')
      }
    )
  } else {''}
  # chunk option error=FALSE means we need to signal the error
  if (!options$error && !is.null(attr(out,'status'))) {
    stop(knitr:::one_string(out))
  }
  # write new out to rb out
  xfun::write_utf8(out, con = rbOutPath)
  # remove unwanted prev header+code from out
  out = out[-(1:prev_out)]
  # remove unwanted leading + trailing white space
  out = trimws(out)
  # remove empty lines
  out = out[!(out == "")]
  if(length(out)>1){
    # add numbers to each line
    out = paste0("[",1:length(out),"] ",out)
  }
  # return output via engine_output
  knitr::engine_output(options, code = options$code, out = out)
}
