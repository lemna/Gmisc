#' A function for converting a table generated from the \pkg{tablone} package
#' 
#' This function translates the \code{tableone}'s descriptive table into an
#' \code{\link[htmlTable]{htmlTable}}-object. The function uses the raw output
#' from the \code{print.TableOne} and looks for \code{header}, \code{rgroup},
#' and \code{n.rgroup} elements.
#' 
#' @param x The TableOne object
#' @param ... Passed onto \code{\link[htmlTable]{htmlTable}}
#' @return Returns an \code{\link[htmlTable]{htmlTable}} object
#' @rdname htmlTable_tableone
#' @export
#' @import htmlTable
#' @import tableone
#' @importFrom stringr str_trim
htmlTable.TableOne <- function(x, ..., css.rgroup = ""){
  t1 <- print(x, printToggle = FALSE)

  if (colnames(t1)[NCOL(t1)] == "test")
    t1 <- t1[,-NCOL(t1)]
  
  header <- mapply(function(cn, n) {
    if (nchar(str_trim(n)) > 0)
      txtMergeLines(cn, n)
    else
      cn
  }, cn = colnames(t1), n = t1[1,])
  t1 <- t1[-1,]
  
  rgroup_elmnts <- grep("   ", rownames(t1))
  if (length(rgroup_elmnts) > 0){
    rgroup_rows <- which("" == stringr::str_trim(t1[,1]))
    if (sum(colnames(t1) == "p") == 1){
      add <- t1[1*rgroup_rows, "p"]
    }else{
      add <- NULL
    }
    t1 <- t1[-1*rgroup_rows, ]

    if (rgroup_rows[1] > 1){
      rgroup <- c("")
      n.rgroup <- c(rgroup_rows[1] - 1)
    }else{
      rgroup <- c()
      n.rgroup <- c()
    }
    for (i in 1:length(rgroup_rows)){
      actual_row <- 
        rgroup_rows[[i]] - 
        sum(rgroup != "") - 
        (rgroup[1] != "")
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
      add <- as.list(add)
      names(add) <- sapply(names(add), 
                           function(x) which(x == rgroup), 
                           USE.NAMES = FALSE)
      attr(rgroup, "add") <- add
    }
    htmlTable(x = t1, header = header, rgroup = rgroup, n.rgroup = n.rgroup,
              css.rgroup = css.rgroup)
  }else{
    htmlTable(x = t1, header = header, 
              css.rgroup = css.rgroup)
  }
}