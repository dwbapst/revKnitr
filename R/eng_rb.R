#' A \code{knitr} engine for RevBayes
#' 
#' This function provides users with a way to execute code written
#' from the Bayesian phylogenetic program RevBayes as knitted chunks 
#' in a document, such as a chunk in a n ]code{rmarkdown} chunk.

#' @details
#' There are a number of limitations of RevBayes that make this execution difficult:
#' there is presently no way to save environments and reload them across RevBayes instances,
#' and no apparent way to keep a RevBayes session open while getting the \code{stdout}, and
#' telling R that the process has terminated.

#' @param options Variables that can be specified within \code{knitr} chunk
#' headers to control how RevBayes code is executed. 
#' Those relevant to use \code{eng_rb} can be broken into three groups:

#' }

#' \emph{Generic Options}
#' \describe{

#' \item{options$code}{A string; the code for that chunk.}
#' \item{options$error}{A logical; should knitr fail on an error.}
#' \item{options$eval}{A logical; should the code be evaluated.}

#' \emph{Generic Options to specify when using RevBayes}
#' \describe{

#' \item{options$engine}{A string; Should be 'rb' to use RevBayes}
#' \item{options$engine.path}{A string; The path to the rb executable if not in the PATH}
#' \item{options$engine.opts}{A string; Command-line modifications for when rb is executed}

#' }

#' \emph{options for specifically for revbayes}
#' \describe{

#' \item{options$rbHistoryDirPath}{A string giving the path to the
#' directory where history files will be stored as RevBayes code is crunched.
#' The default path is ".eng_rb.knitr.cache" in the current working directory.
#' }

#' \item{options$refreshHistoryRB}{A logical. If not defined, the default
#' is \code{TRUE}. If \code{options$refreshHistoryRB = TRUE} and the hidden
#' chunk count function suggests this is the first RevBayes chunk
#' of the \code{knitr} document being knitted, then existing history files
#' found in \code{options$rbHistoryDirPath} from previous RevBayes analyses
#' using \code{knitr} will be removed. 
#' }

#' \item{options$rbDiagnosticMode}{A logical; default is \code{FALSE}. 
#' This option controlls whether to run diagnostic mode to
#' identify issues with the RevBayes engine.
#' }

#' }

#' @return
#' Output from applying RevBayes to the input code.

#' @seealso
#' \code{\link{knitr::engine_output}}

#' @author April Wright, David W. Bapst

#' @examples
#' # no examples yet

#' @importFrom xfun write_utf8

#' @name eng_rb
#' @rdname eng_rb
#' @export
eng_rb <- function(options) {
  ## knitr engine for RevBayes
    
  ###########################
  # options - variables given in knitr chunk headers
  #######################
  
  ##############
  # generic options called by eng_rb
  
  # options$code - string, the code for that chunk
  # options$error - logical, should knitr fail on an error
  # options$eval - logical, should the code be evaluated
  
  ############
  # options that are generic but related to revbayes
  
  # options$engine - should be == 'rb'
  # options$engine.path - path to rb  
  # options$engine.opts - command-line modifications for when rb is executed
  
  ############
  # options for specifically for revbayes
  
  # options$rbHistoryDirPath
    # string - path to directory for rb history files
    # default is ".eng_rb.knitr.cache" in working dir
      # may need "/" at the end??
  
  # options$refreshHistoryRB 
    # a logical - If not defined, default is TRUE 
    # if TRUE & this is a new knitr document, remove existing history files 
    # Thus controls whether previous .eng_rb.knitr.cache files
      # should be deleted if this is the first rb chunk
  
  # options$rbDiagnosticMode
    # a logical, default is FALSE
    # controlling whether to run diagnostic mode
  
  ################################
  # early exit if evaluated output not requested
  if (!options$eval){
    options$results = 'asis'
    return(engine_output(options, options$code, ''))
  }
  
  # now set up path to rb
  rbPath <- RevKnitr:::get_rb_path(options$engine.path)

  #############
  # check RevBayes engine specific options
  
  # use get_rb_opts to pull out rb options (see above)
  opts <- RevKnitr:::get_rb_opts(options$engine.opts)
  
  # set rbDiagnosticMode if not already set
  if(is.null(options$rbDiagnosticMode)){
    options$rbDiagnosticMode <- FALSE
  }
  
  # options$refreshHistoryRB 
  if(is.null(options$refreshHistoryRB)){
    options$refreshHistoryRB <- TRUE
  }
  
  # options$rbHistoryDirPath 
  if(is.null(options$rbHistoryDirPath)){
    options$rbHistoryDirPath <- ".eng_rb.knitr.history"
    # should the above have a '/' at end or not?
  }
  
  #############
  # set paths for output, code in history directory
  rbOutPath <- paste0(options$rbHistoryDirPath, '/.eng_rb_out')
  rbCodePath <- paste0(options$rbHistoryDirPath, '/.eng_rb_code') 
  
  # do these paths need to be normalized?
  # rbOutPath <- normalizePath(rbOutPath, mustWork = FALSE)
  # rbCodePath <- normalizePath(rbCodePath, mustWork = FALSE)  
  
  # for diagnostic purporses:
  # if(options$rbDiagnosticMode){
  # message("rb_chunk_counter = ", rb_chunk_counter(-1) )
  # }
  
  # check (and simultaneously update) rb_chunk_counter()
  if(.rb_chunk_counter() == 1L){
    
    # this is the first time an rb code-chunk is run for this document
    # set prev_out artificially to 13
    prev_out <- 13
    
    # if an old history directory exists, get rid of it
    if(dir.exists(options$rbHistoryDirPath) & options$refreshHistoryRB){
      unlink(options$rbHistoryDirPath, recursive = TRUE)
    }
    
    # Set up new history directories 
    dir.create(options$rbHistoryDirPath, showWarnings = FALSE)
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
    # combine
    code_to_run <- c(old_code, options$code)

  }
  
  # write history file for code based on this chunk
  xfun::write_utf8(code_to_run, con = rbCodePath)
    
  # make a temporary file of rb code to execute
    # don't need to one-string code
  tempF <- rb_wd_tempfile('.rb', '.Rev')
  
  # does tempF need to be normalized?
  # tempF <- normalizePath(
  #   tempF,
  #   winslash = "/",
  #   mustWork = FALSE
  # )    
  
  # write to file and add q() line
  xfun::write_utf8(c(code_to_run, "q()"), con = tempF)
  
  # setup to delete temporary files created for execution when done
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
        if (!options$error){stop(e)}
        paste('Error in running command rb:')
      }
    )
  } else {
    # if eval is FALSE, then NOTHING
    ''
    }
  
  # chunk option error=FALSE means we need to signal the error
  if (!options$error && !is.null(attr(out,'status'))) {
    stop(rb_one_string(out))
  }
  
  # write new out to rb out
  xfun::write_utf8(out, con = rbOutPath)
  # remove unwanted prev header+code from out
  out = out[-(1:prev_out)]
  # remove unwanted leading + trailing white space 
  out = trimws(out)
  # remove empty lines
  out = out[!(out == "")]
  # if output is longer than 1 line, add numbers to each line
  if(length(out)>1){
    out = paste0("[",1:length(out),"] ",out)
  }
  
  # return output via engine_output in knitr
  knitr::engine_output(
    options, 
    code = options$code, 
    out = out
    )
  
}

