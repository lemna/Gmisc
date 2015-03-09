#' A function for converting a table generated from the \pkg{tablone} package
#'
#' This function translates the \code{tableone}'s descriptive table into an
#' \code{\link[htmlTable]{htmlTable}}-object. The function uses the raw output
#' from the \code{print.TableOne} and looks for \code{header}, \code{rgroup},
#' and \code{n.rgroup} elements.
#'
#' @param x The TableOne object
#' @param ... Passed onto \code{\link[htmlTable]{htmlTable}}
#' @param t1_args Arguments passed to the \code{\link[tableone]{print.TableOne}}
#' @param css.rgroup Overrides the default bold text. See \code{\link[htmlTable]{htmlTable}} for details.
#' @return Returns an \code{\link[htmlTable]{htmlTable}} object
#' @rdname htmlTable_tableone
#' @export
#' @import htmlTable
#' @import tableone
#' @importFrom stringr str_trim
#' @example inst/examples/htmlTable.TableOne_ex.R
htmlTable.TableOne <- function(x,
                               ...,
                               css.rgroup = "",
                               t1_args = list(printToggle = FALSE)){
  if (!"printToggle" %in% names(t1_args))
    t1_args[["printToggle"]] <- FALSE

  t1 <- fastDoCall(print, c(x=list(x), t1_args))

  if (colnames(t1)[NCOL(t1)] == "test" &&
        all(str_trim(t1[,NCOL(t1)]) == ""))
    t1 <- t1[,-NCOL(t1),drop=FALSE]

  header <- prGetTableoneHeader(t1)
  t1 <- header$t1
  header <- header$header

  rgroup_elmnts <- grep("   ", rownames(t1))
  if (length(rgroup_elmnts) > 0){
    rg <- prGetTableoneRgroups(t1)
    htmlTable(x = rg$t1,
              ...,
              header = header,
              rgroup = rg$rgroup, n.rgroup = rg$n.rgroup,
              css.rgroup = css.rgroup)
  }else{
    htmlTable(x = t1, ..., header = header)
  }
}

#' @rdname htmlTable_tableone
#' @keywords internal
htmlTable.CatTable <- function(...){
  return(htmlTable.TableOne(...))
}

#' @rdname htmlTable_tableone
#' @keywords internal
htmlTable.ContTable <- function(...){
  return(htmlTable.TableOne(...))
}

prGetTableoneHeader <- function(t1){
  header <- mapply(function(cn, n) {
    if (nchar(str_trim(n)) > 0)
      txtMergeLines(cn, paste("n =", n))
    else
      cn
  }, cn = colnames(t1), n = t1[1,,drop=FALSE])
  t1 <- t1[-1,,drop=FALSE]
  return(list(t1 = t1, header = header))
}

prGetTableoneRgroups <- function(t1){
  rgroup_elmnts <- grep("^   ", rownames(t1))

  rgroup_rows <- which("" == str_trim(t1[,1]))
  if (sum(colnames(t1) == "p") == 1){
    p_col <- which(colnames(t1) == "p")
    add <- t1[1*rgroup_rows, "p"]
    add <- lapply(add, function(x) {
      x <- list(x)
      names(x) = p_col
      x
    })
    names(add) = rownames(t1)[rgroup_rows]

  }else{
    add <- NULL
  }
  t1 <- t1[-1*rgroup_rows, ,drop=FALSE]

  if (rgroup_rows[1] > 1){
    rgroup <- c("")
    n.rgroup <- c(rgroup_rows[1] - 1)
  }else{
    rgroup <- c()
    n.rgroup <- c()
  }
  for (i in 1:length(rgroup_rows)){
    if (rgroup_rows[[i]] == 1){
      actual_row <- 1
    }else{
      actual_row <-
        rgroup_rows[[i]] -
        sum(rgroup != "")
    }
    rgroup_elmnts <- rgroup_elmnts[rgroup_elmnts > rgroup_rows[[i]]]
    if (actual_row >
          sum(n.rgroup) + 1){
      # Add rgroup without indentation
      rgroup <- c(rgroup, "")
      n.rgroup <- c(n.rgroup,
                    actual_row -
                      sum(n.rgroup) -
                      1)
    }else if (actual_row <
                sum(n.rgroup) + 1){
      stop("Something wrong with the autocalculation of rgroups")
    }
    rgroup <- c(rgroup, names(rgroup_rows)[i])
    n <- 1
    for (ii in 1:(length(rgroup_elmnts)-1)){
      steps <- rgroup_elmnts[ii + 1] -
        rgroup_elmnts[ii]
      if (steps == 1){
        n <- n + 1
      }else if (ii == length(rgroup_elmnts)-1){
        n <- n + 1
      }else{
        break;
      }
    }
    # Save n value to n.rgoup
    n.rgroup <- c(n.rgroup, n)
  }
  if (sum(n.rgroup) < nrow(t1)){
    rgroup <- c(rgroup, "")
    n.rgroup <- c(n.rgroup, nrow(t1) - sum(n.rgroup))
  }else if (sum(n.rgroup) > nrow(t1)){
    print(cbind(rn = rep(rownames(t1), length.out = sum(n.rgroup)),
                rgroups = rep(rgroup, times = n.rgroup)))
    stop("Invalid n.rgroup - the sum is bigger than the provided number of rows")
  }
  if (!is.null(add)){
    names(add) <- sapply(names(add),
                         function(x) which(x == rgroup),
                         USE.NAMES = FALSE)
    attr(rgroup, "add") <- add
  }

  return(list(t1 = t1,
              rgroup = rgroup,
              n.rgroup = n.rgroup))
}
