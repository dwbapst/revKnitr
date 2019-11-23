

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

