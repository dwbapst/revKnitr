# rb chunk counter

rb_knit_counter = function(init = 0L) {
  n = init
  function(reset = FALSE) {
    if (reset) return(n <<- init)
    n <<- n + 1L
    n - 1L
  }
}

# count revbayes chunks
.rb_chunk_counter <<- rb_knit_counter(1L)

###########################################################
# the following internal support functions shamelessly stolen from knitr and lightly rebranded:
##############################################

# options$engine.path can be list(name1 = path1, name2 = path2, ...); similarly,
# options$engine.opts can be list(name1 = opts1, ...)
get_rb_opts = function(opts, engine = 'rb' , fallback = '') {
  if (is.list(opts)) opts = opts[[engine]]
  opts %n% fallback
}

# tempfile under the current working directory
rb_wd_tempfile = function(...) basename(tempfile(tmpdir = '.', ...))

# collapse by \n
rb_one_string = function(x, ...) paste(x, ..., collapse = '\n')

get_rb_path <- function (engine_path_from_options, engine = 'rb'){
  # need to set up path to revbayes
  # base off of knitr:::get_engine_path(options$engine.path, 'rb')
  # typical use: rbPath <- RevKnitr::get_rb_path(options$engine.path)
  # 
  get_rb_opts(engine_path_from_options, engine, engine)
}
