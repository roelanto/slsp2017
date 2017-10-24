

anonymize_atomic <- function(group, name, groups, names, unique) {
  groupno <- which(groups == group)
  nameno <- which(names[[as.character(group)]] == name)
  namesCount <- 0
  translatedGroupName <- list(control="Control", alzheimer="AD", parkinson="PD", ppa="PPA-NF", ppasd="PPA-SD")
  if (unique == TRUE) {
    if (groupno > 1) {
      for (i in c(1:(groupno-1))) {
        namesCount <- namesCount + length(names[[groups[i]]])
      }
    }
  } else {
    namesCount <- 0
  }
  if (is.null(translatedGroupName[[as.character(group)]])) {
    groupname <- group
  } else {
    groupname <- translatedGroupName[[as.character(group)]]
  }
  return(paste0(groupname, "_", namesCount + nameno))
}

