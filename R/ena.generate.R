##
# @title Accumulate and Generate
#
# @description Accumulate and Generate
#
# @details [TBD]
#
# @param file [TBD]
# @param window.size.back [TBD]
# @param units.by [TBD]
# @param conversations.by [TBD]
# @param code [TBD]
# @param units.used [TBD]
#
# @return list containing the accumulation and set
##
ena.generate <- function(
  file,
  window.size.back,
  units.by,
  conversations.by,
  code,
  units.used = NULL
) {
  accum = ena.accumulate.data.file(
    file = file,
    window.size.back = window.size.back,
    units.by = units.by,
    units.used = units.used,
    conversations.by = conversations.by,
    codes = code
  )
  set = ena.make.set(
    enadata = accum
  )

  return( set );
}
