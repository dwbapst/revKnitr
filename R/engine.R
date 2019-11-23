#' An Output Wrapper for Language Engine Output for RevBayes
#'
#' If you have designed a language engine, you may call this function in the end
#' to format and return the text output from your engine.
#'
#' For expert users, an advanced usage of this function is
#' \code{engine_output(options, out = LIST)} where \code{LIST} is a list that
#' has the same structure as the output of \code{evaluate::evaluate()}. In this
#' case, the arguments \code{code} and \code{extra} are ignored, and the list is
#' passed to an internal function \code{knitr:::wrap()} to return a character
#' vector of final output.

#' @import knitr

#' @param options 
#' A list of chunk options. Usually this is just the object
#'   \code{options} passed to the engine function; see
#'   \code{\link{knit_engines}}.

#' @param code 
#' Source code of the chunk, to which the output hook
#'   \code{source} is applied, unless the chunk option \code{echo} is \code{FALSE}.

#' @param out 
#' Text output from the engine, to which the hook \code{output}
#'   is applied, unless the chunk option \code{results} is \code{'hide'}

#' @param extra 
#' Any additional text output that you want to include.

#' @return A character string generated from the source code and output using
#'   the appropriate output hooks.


#' @examples 
#' library(knitr)
#' 
#' engine_output(
#'    options= opts_chunk$merge(list(engine = rb)), 
#'    code = '1 + 1', 
#'    out = '[1] 2'
#'    )
#' 
#' engine_output(
#'    options = opts_chunk$merge(
#'       list(echo = FALSE, engine = rb)
#'       ), 
#'    code = '1 + 1', 
#'    out = '[1] 2'
#'    )
#'
#' # expert use only
#' engine_output(
#'    options = opts_chunk$merge(list(engine = rb)), 
#'    out = list(
#'       structure(list(src = '1 + 1'), 
#'       class = 'source'),
#'       '2')
#'    )



#' RevBayes engine
#'
#' This engine allows users to run RevBayes in the RStudio window
#' @import knitr
#' @export





# additional engines
knit_engines$set(
  rb = eng_rb
)



# engine_output






#' @export
engine_output = function(options, code, out, extra = NULL) {
  if (missing(code) && is.list(out)) return(unlist(wrap(out, options)))
  if (!is.logical(options$echo)) code = code[options$echo]
  if (length(code) != 1L) code = knitr:::one_string(code)
  if (options$engine == 'sas' && length(out) > 1L && !grepl('[[:alnum:]]', out[2]))
    out = tail(out, -3L)
  if (length(out) != 1L) out = knitr:::one_string(out)
  out = sub('([^\n]+)$', '\\1\n', out)
  # replace the engine names for markup later, e.g. ```Rscript should be ```r
  options$engine = switch(
    options$engine, rb = "RevBayes",
    options$engine
  )
  knitr:::one_string(c(
    if (length(options$echo) > 1L || options$echo) knit_hooks$get('source')(code, options),
    if (options$results != 'hide' && !knitr:::is_blank(out)) {
      if (options$engine == 'highlight') out else knitr:::wrap.character(out, options)
    },
    extra
  ))
}

###########################################################################

# old eng_rb




##########################################################################

# functions that aren't eng_rb from old knitr fork but have rb in it

# additional engines
knit_engines$set(
  highlight = eng_highlight, Rcpp = eng_Rcpp, tikz = eng_tikz, dot = eng_dot,
  c = eng_shlib, fortran = eng_shlib, fortran95 = eng_shlib, asy = eng_dot,
  cat = eng_cat, asis = eng_asis, stan = eng_stan, rb = eng_rb, block = eng_block,
  block2 = eng_block2, js = eng_js, css = eng_css, sql = eng_sql, go = eng_go,
  python = eng_python, julia = eng_julia, sass = eng_sxss, scss = eng_sxss
)




engine_output = function(options, code, out, extra = NULL) {
  if (missing(code) && is.list(out)) return(unlist(wrap(out, options)))
  if (!is.logical(options$echo)) code = code[options$echo]
  if (length(code) != 1L) code = one_string(code)
  if (options$engine == 'sas' && length(out) > 1L && !grepl('[[:alnum:]]', out[2]))
    out = tail(out, -3L)
  if (length(out) != 1L) out = one_string(out)
  out = sub('([^\n]+)$', '\\1\n', out)
  # replace the engine names for markup later, e.g. ```Rscript should be ```r
  options$engine = switch(
    options$engine, mysql = 'sql', node = 'javascript', psql = 'sql', Rscript = 'r', rb = "RevBayes",
    options$engine
  )
  if (options$engine == 'stata') {
    out = gsub('\n+running.*profile.do', '', out)
    out = sub('...\n+', '', out)
    out = sub('\n. \nend of do-file\n', '', out)
  }
  one_string(c(
    if (length(options$echo) > 1L || options$echo) knit_hooks$get('source')(code, options),
    if (options$results != 'hide' && !is_blank(out)) {
      if (options$engine == 'highlight') out else wrap.character(out, options)
    },
    extra
  ))
}











#####################################################################

# from april's fork
# no apparent mention of rb
# why do we need any of this - test this



# options$engine.path can be list(name1 = path1, name2 = path2, ...); similarly,
# options$engine.opts can be list(name1 = opts1, ...)
get_engine_opts = function(opts, engine, fallback = '') {
  if (is.list(opts)) opts = opts[[engine]]
  opts %in% fallback
}

get_engine_path = function(path, engine) {
  get_engine_opts(path, engine, engine)
}


## output the code without processing it
eng_asis = function(options) {
  if (options$echo && options$eval) knitr:::one_string(options$code)
}

# write a block environment according to the output format
eng_block = function(options) {
  if (isFALSE(options$echo)) return()
  code = knitr:::one_string(options$code)
  to = pandoc_to()
  is_pandoc = !is.null(to)
  if (!is_pandoc) {
    # not in R Markdown v2
    to = out_format()
    if (!(to %in% c('latex', 'html', 'markdown'))) to = NULL
  }
  if (is.null(to)) return(code)
  if (to == 'beamer') to = 'latex'
  if (is_html_output(to)) to = 'html'
  type = options$type
  if (is.null(type)) return(code)
  # convert the chunk content to HTML or LaTeX (ideally I only need to specify
  # the markdown extension, but it is not implemented yet for LaTeX:
  # https://github.com/jgm/pandoc/issues/2453)
  if (is_pandoc) code = pandoc_fragment(code, if (to == 'html') 'html4' else to)
  l1 = options$latex.options
  if (is.null(l1)) l1 = ''
  h2 = options$html.tag %n% 'div'
  h3 = options$html.before %n% ''
  h4 = options$html.after %n% ''
  # e.g. type = c(latex = 'marginfigure', html = 'marginnote')
  if (to %in% names(type)) type = type[to]
  # block level tags? this is an incomplete list, but should work for most cases
  if (to == 'html') if (h2 %in% c('div', 'p', 'blockquote')) {
    code = paste0('\n', code, '\n')
  } else {
    code = gsub('<p>', '<span style="display: block;">', code)
    code = gsub('</p>', '</span>', code)
  }
  switch(
    to,
    latex = sprintf('\\begin{%s}%s\n%s\n\\end{%s}', type, l1, code, type),
    html =  sprintf('%s<%s class="%s">%s</%s>%s', h3, h2, type, code, h2, h4),
    code
  )
}

# helper to create engines the wrap embedded html assets (e.g. css,js)
eng_html_asset = function(prefix, postfix) {
  function(options) {
    out = if (options$eval && is_html_output(excludes = 'markdown')) {
      knitr:::one_string(c(prefix, options$code, postfix))
    }
    options$results = 'asis'
    engine_output(options, options$code, out)
  }
}

get_engine = function(name) {
  fun = knit_engines$get(name)
  if (is.function(fun)) return(fun)
  warning(
    "Unknown language engine '", name,
    "' (must be registered via knit_engines$set())."
  )
  function(options) {
    engine_output(options, options$code, '')
  }
}

cache_engine = function(options) {
  cache_fun = cache_engines$get(options$engine)
  if (!is.function(cache_fun)) return()
  cache_fun(options)
}
