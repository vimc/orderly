orderly_batch <- function(name = NULL, parameters = NULL, ...) {
  batch_id <- ids::random_id()
  ids <- lapply(seq_len(nrow(params)), function(row) {
    orderly_run(name, parameters = params[row, ], ..., batch_id = batch_id)
  })
  ids
}
