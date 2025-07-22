MODULE sort_real_dp_m

IMPLICIT NONE

INTEGER, PRIVATE, PARAMETER :: dp = SELECTED_REAL_KIND(15)

CONTAINS

#define DATA_TYPE REAL(dp)
#include "isort.inc"
#include "qsort.inc"


END MODULE