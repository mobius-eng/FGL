MODULE sort_real_sp_m

IMPLICIT NONE

INTEGER, PRIVATE, PARAMETER :: sp = SELECTED_REAL_KIND(6)

CONTAINS

#define DATA_TYPE REAL(sp)
#include "isort.inc"
#include "qsort.inc"


END MODULE