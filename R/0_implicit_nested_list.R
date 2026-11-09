#' A flat list which acts as a nested list with nesting controlled by dots.
#' @rdname implicit_nested_list
#' @description
#'   All values are stored in a one-dimensional named list. Dots in names define
#'   virtual groups (the "implied" nesting) of the vector.
#'
#' @details
#'   Named vectors or lists are recursively flattened on construction and assignment
#'   to keep the underlying structure fully one-dimensional.
#'   Reading a group returns an ordinary named vector if every immediate child
#'   is a scalar of the same atomic type; otherwise it returns an ordinary list.
#'
#' @param x (`list`)\cr
#'   Named values to store. Named elements are recursively flattened.
#' @return (`implicit_nested_list`)\cr
#'   A flat list with virtual nested access.
#' @export
#' @examples
#'   x <- implicit_nested_list(list(compartments = c("E" = 2L, "I" = 3L)))
#'   x$compartments[["I"]] <- 4L
#'   stopifnot(identical(x[["compartments.I"]], 4L))
implicit_nested_list <- function(x = list()) {

  if (is.null(x)) {
    return(NULL)
  }

  checkmate::assert_list(x)

  # Name collision must be checked in this structure
  .implicit_nested_list_validate_names(names(x), length(x))

  # Normalise every input through the same expansion rule, including inputs
  # that already contain dotted names.
  flat <- list()
  for (key in names(x)) {
    flat <- c(flat, .implicit_nested_list_flatten(x[[key]], key))
  }

  # Names must be re-checked after flattening
  .implicit_nested_list_validate_names(names(flat), length(flat))
  class(flat) <- "implicit_nested_list"
  return(flat)
}

.implicit_nested_list_validate_names <- function(keys, size) {
  if (size == 0L) {
    return(invisible(NULL))
  }

  if (is.null(keys) || anyNA(keys) || !all(nzchar(keys)) || anyDuplicated(keys)) {
    stop("Elements must have unique, nonempty names.", call. = FALSE)
  }

  if (any(grepl(r"{(^\.|\.\.|\.$)}", keys))) {
    stop("Names cannot start or end with a dot or contain '..'", call. = FALSE)
  }

  prefixes <- lapply(
    strsplit(keys, ".", fixed = TRUE),
    function(parts) {
      if (length(parts) < 2L) {
        return(character(0))
      }

      vapply(
        seq_len(length(parts) - 1L),
        function(index) paste(parts[seq_len(index)], collapse = "."),
        character(1)
      )
    }
  ) |>
    unlist(use.names = FALSE)

  if (any(keys %in% prefixes)) {
    stop("A name cannot be both a leaf and a group prefix.", call. = FALSE)
  }

  return(invisible(NULL))
}

# Expand named plain lists and vectors at every level. Unnamed values, arrays,
# and objects remain leaves, so their internal structure is not reinterpreted.
.implicit_nested_list_flatten <- function(value, key) {
  child_names <- names(value)
  can_expand <- (is.list(value) || is.atomic(value)) &&
    !is.object(value) && is.null(dim(value)) && length(value) > 0L &&
    !is.null(child_names)

  if (!can_expand) {
    return(stats::setNames(list(value), key))
  }

  .implicit_nested_list_validate_names(child_names, length(value))

  # Distribute parent attributes to children, preserving existing attributes
  metadata <- attributes(value)
  metadata$names <- NULL

  flat <- list()
  for (child_name in child_names) {
    child <- value[[child_name]]

    for (attribute in names(metadata)) {
      if (is.null(attr(child, attribute, exact = TRUE))) {
        attr(child, attribute) <- metadata[[attribute]]
      }
    }

    flat <- c(flat, .implicit_nested_list_flatten(child, paste0(key, ".", child_name)))
  }

  return(flat)
}

#' @name implicit_nested_list
#' @details
#'  Simplification is strict: integer + double and numeric + character stay lists.
#' @param flat (`implicit_nested_list(1)`)\cr
#'   1D list to transform
#' @export
explicit_nested_list <- function(flat) {
  return(.explicitly_nest_list(flat))
}

#' @export
.explicitly_nest_list <- function(flat, level = 0) {
  keys <- names(flat)
  roots <- unique(sub("\\..*$", "", keys))
  branches <- !(roots %in% keys)

  values <- lapply(
    seq_along(roots),
    function(index) {
      root <- roots[[index]]
      if (!branches[[index]]) return(flat[[root]])

      prefix <- paste0(root, ".")
      children <- flat[startsWith(keys, prefix)]
      names(children) <- substring(names(children), nchar(prefix) + 1L)
      return(.explicitly_nest_list(children, level = level + 1))
    }
  )
  names(values) <- roots

  # Only simplify scalar values without additional metadata
  is_scalar <- vapply(
    values,
    FUN = function(value) {
      is.atomic(value) && !is.object(value) && length(value) == 1L &&
        length(setdiff(names(attributes(value)), "names")) == 0L
    },
    FUN.VALUE = logical(1)
  )

  if (
    length(values) > 0L && !any(branches) && all(is_scalar) &&
      length(unique(vapply(values, typeof, character(1)))) == 1L
  ) {
    out <- unlist(values, recursive = FALSE, use.names = FALSE)
    names(out) <- roots

    # Top-level objects must always be ordinary lists
    if (level == 0L) {
      return(as.list(out))
    }

    return(out)
  }

  return(values)
}

#' @export
`$.implicit_nested_list` <- function(x, name) {
  flat <- unclass(x)
  flat_names <- names(flat) %||% character(0)

  if (name %in% flat_names) {
    return(flat[[name]])
  }

  prefix <- paste0(name, ".")
  children <- flat[startsWith(flat_names, prefix)]
  if (length(children) == 0L) return(NULL)

  names(children) <- substring(names(children), nchar(prefix) + 1L)

  return(.explicitly_nest_list(children, level = 1)) # Level > 0 to prevent casting to list
}

#' @export
`$<-.implicit_nested_list` <- function(x, name, value) {
  flat <- unclass(x)
  keys <- names(flat) %||% character(0)
  prefix <- paste0(name, ".")
  scope <- keys == name | startsWith(keys, prefix)

  # NULL removes a key or its complete virtual subtree, matching list $<-.
  if (is.null(value)) {
    return(implicit_nested_list(flat[!scope]))
  }

  replacement <- .implicit_nested_list_flatten(value, name)


  # Preserve unchanged leaf metadata when replacing a virtual group.
  # Direct assignments to existing leaves must retain the supplied attributes.
  if (!(name %in% keys)) {
    for (key in intersect(names(replacement), keys[scope])) {
      previous <- flat[[key]]
      incoming <- replacement[[key]]

      # Only restore previous metadata when the incoming value has none
      if (!is.null(attributes(incoming))) next

      attributes(previous) <- NULL

      if (identical(previous, incoming)) {
        replacement[key] <- flat[key]
      }
    }
  }

  # Keep the group at its original position, instead of moving it to the end
  # on each assignment. New groups append like new ordinary list entries.
  if (any(scope)) {
    position <- which(scope)[[1L]]
    left <- if (position > 1L) flat[seq_len(position - 1L)] else list()
    right <- if (position <= length(flat)) flat[seq.int(position, length(flat))] else list()
    right <- right[!names(right) %in% keys[scope]]
    flat <- c(left[!names(left) %in% keys[scope]], replacement, right)
  } else {
    flat <- c(flat, replacement)
  }

  return(implicit_nested_list(flat))
}

#' @export
`[[.implicit_nested_list` <- function(x, i, ..., exact = TRUE) {
  if (is.character(i) && length(i) == 1L && length(list(...)) == 0L) {
    return(`$.implicit_nested_list`(x, i))
  }

  return(base::`[[`(unclass(x), i, ..., exact = exact))
}

#' @export
`[[<-.implicit_nested_list` <- function(x, i, ..., value) {
  if (is.character(i) && length(i) == 1L && length(list(...)) == 0L) {
    return(`$<-.implicit_nested_list`(x, i, value))
  }

  flat <- base::`[[<-`(unclass(x), i, ..., value = value)
  return(implicit_nested_list(flat))
}

#' @export
`[.implicit_nested_list` <- function(x, ...) {
  return(implicit_nested_list(base::`[`(unclass(x), ...)))
}

#' @export
`[<-.implicit_nested_list` <- function(x, ..., value) {
  flat <- base::`[<-`(unclass(x), ..., value = value)
  return(implicit_nested_list(flat))
}

#' @export
print.implicit_nested_list <- function(x, ...) {
  print(explicit_nested_list(x), ...)

  return(invisible(x))
}
