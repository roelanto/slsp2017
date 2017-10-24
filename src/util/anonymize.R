anonymize <- function(group, name, groups, names, unique=TRUE) {
  if (length(group) != length(name) || length(group) < 1) {
    message(paste("Error, group and name vectors should be >0 and same size, length(group)=",length(group)," length(name)=",length(name)))
  } else {
    if (length(group) == 1) {
      return(anonymize_atomic(group, name, groups, names, unique))
    } else {
      retVal <- vector()
      for (i in c(1:length(group))) {
        theGroup <- group[i]
        theName <- name[i]
        retVal <- append(retVal, anonymize_atomic(theGroup, theName, groups, names, unique))
      }
      return(retVal)
    }
  }
}

testAnonymize <- function() {
  names <- list(a=c("A", "B", "C"), b=c("X", "Y", "Z"), c=c("D", "E", "F"))
  groups <- c("a", "b", "c")
  retVal <- anonymize(group="b", name="Y", unique=FALSE, groups=groups, names=names)
  print(paste("#1 expected 'b #2', got: ", retVal))
  retVal <- anonymize(group="b", name="Y", unique=TRUE, groups=groups, names=names)
  print(paste("#1 expected 'b #5', got: ", retVal))
  retVal <- anonymize(group=c("a", "b"), name=c("A", "Y"), groups=groups, names=names)
  print(paste("#1 expected 'a #1' 'b #5', got: ", retVal))
}

testAnonymize()

