#' fastq file information
#'
#' @export

fastq <- S7::new_class("fastq",
  package = "dataLIMS",
  properties = list(
    id = S7::class_character, # unique id
    pid = S7::class_character, # parent id
    name = S7::class_character,
    type = S7::class_character,
    interleaved = S7::class_logical,
    path = S7::class_character, # could be a list with either ipath, or fpath/rpath depending on whether interleaved is true or false
    # processed = S7::class_logical,
    # notes = S7::class_character,
    read_count = S7::class_integer,
    read_length = S7::class_integer
  ),
  constructor = function(path,
                         pid,
                         name,
                         type,
                         interleaved) {

    id = stringi::stri_rand_strings(1, 8, "[a-f0-9]")

    S7::new_object(S7::S7_object(),
                   id = id,
                   pid = pid,
                   name = name,
                   type = type,
                   path = path,
                   interleaved = interleaved,
                   read_count = integer(),
                   read_length = integer())
  },
  validator = function(self) {
    # make sure there is a file at path
    if (!file.exists(self@path)) {
      stop("file at <path> does not seem to exist", call. = FALSE)
    }

    # interleaved must be either TRUE or FALSE, it cannot be NA
    if (!is.logical(self@interleaved) | is.na(self@interleaved)) {
      stop("interleaved must be either TRUE or FALSE", call. = FALSE)
    }
  }
)

#' fastq helper
#'
#' @export

fastq_helper <- function(data) {
  data <- as.vector(data)
  fastq(
    name = data$name,
    path = data$path,
    pid = data$pid,
    type = data$type,
    interleaved = data$interleaved
  )
}

#' Create a fastq object from an excel template
#'
#' @export

fastq_from_template = function(fastq_template) {

  df = readxl::read_excel(fastq_template) |>
    dplyr::mutate(rowid = dplyr::row_number()) |>
    tidyr::nest(data = -rowid)

  j2 <- df |>
    dplyr::mutate(fastq = purrr::map(data, fastq_helper))

  return(j2)
}


