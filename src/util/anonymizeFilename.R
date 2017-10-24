anonymizeFilename <- function(filename) {
    af <- NULL
    if (startsWith(x=filename, "P")) {
        af <- anonymize(group="pd", name=filename, groups=groups, names=names)
    } else if (startsWith(x=filename, "FTDL PPA H")) {
        af <- anonymize(group="ppa1", name=filename, groups=groups, names=names)
    } else if (startsWith(x=filename, "FTDL PPA Sa")) {
        af <- anonymize(group="ppa2", name=filename, groups=groups, names=names)
    } else if (startsWith(x=filename, "FTDL PPA SD")) {
        af <- anonymize(group="sd", name=filename, groups=groups, names=names)
    } else if (startsWith(x=filename, "FTDL FTD ")) {
        af <- anonymize(group="bv", name=filename, groups=groups, names=names)
    } else if (startsWith(x=filename, "Alzheimer")) {
        af <- anonymize(group="ad", name=filename, groups=groups, names=names)
    } else if (startsWith(x=filename, "Kontrollgru")) {
        af <- anonymize(group="c", name=filename, groups=groups, names=names)
    } else {
        message("Don't know how to anonymize ", filename)
    }
    af
}
