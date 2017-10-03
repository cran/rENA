##
# @title Alternate log function
#
# @description adds 1 before computing to avoid indef. results from zeros
# @param x [TBD]
# @details [TBD]
##
log = function(x) {

   x = base::log(x + 1);

   x;

}
