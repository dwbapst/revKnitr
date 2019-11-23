
# options$engine.path can be list(name1 = path1, name2 = path2, ...); similarly,
# options$engine.opts can be list(name1 = opts1, ...)
get_engine_opts = function(opts, engine, fallback = '') {
  if (is.list(opts)) opts = opts[[engine]]
  opts %n% fallback
}




rb_chunk_counter

