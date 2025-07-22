MODULE utest_m

USE, INTRINSIC :: iso_fortran_env, only : output_unit
IMPLICIT NONE
PRIVATE
PUBLIC start_test_group, end_test_group, test_true, set_test_output

CHARACTER(8), PARAMETER :: &
  green_attr = CHAR(27)//'[1;32m', &
  red_attr   = CHAR(27)//'[1;31m', &
  blue_attr  = CHAR(27)//'[1;94m', &
  reset_attr = CHAR(27)//'[0m'

INTEGER, PARAMETER :: tgname_len = 64

CHARACTER(len=tgname_len) :: tgname = ''
INTEGER                   :: unit = output_unit
LOGICAL                   :: colors = .TRUE.


CONTAINS


SUBROUTINE set_test_output(newunit)
  INTEGER, INTENT(IN), OPTIONAL :: newunit
  IF (PRESENT(newunit)) THEN
    unit = newunit
  ELSE
    unit = output_unit
  END IF
  IF (unit /= output_unit) THEN
    colors = .FALSE.
  ELSE
    colors = .TRUE.
  END IF
END SUBROUTINE


SUBROUTINE start_test_group(name)
  CHARACTER(len=*), INTENT(IN) :: name
  INTEGER name_len
  name_len = LEN_TRIM(name)
  name_len = MIN(name_len, tgname_len)
  tgname = ''
  tgname(1:name_len) = name(1:name_len)
  WRITE(unit, '(/)')
  IF (colors) WRITE(unit, '(A)', advance='no') TRIM(blue_attr)
  WRITE(unit, '(A)') REPEAT('=', 72)
  WRITE(unit, '(A, A)') 'Start testing ', TRIM(tgname)
  IF (colors) WRITE(unit, '(A)', advance='no') TRIM(reset_attr)
  WRITE(unit, '(/)')
END SUBROUTINE


SUBROUTINE end_test_group
  IF (colors) WRITE(unit, '(A)', advance='no') TRIM(blue_attr)
  WRITE(unit, '(A, A)') 'End testing ', TRIM(tgname)
  WRITE(unit, '(A)') REPEAT('=', 72)
  IF (colors) WRITE(unit, '(A)', advance='no') TRIM(reset_attr)
  WRITE(unit, '(/)')
END SUBROUTINE


SUBROUTINE test_true(cond, name)
  LOGICAL, INTENT(IN) :: cond
  CHARACTER(*), INTENT(IN) :: name
  IF (cond) THEN
    IF (colors) WRITE(unit,'(A)',advance='no') TRIM(green_attr)
    WRITE (unit, '(A)', advance='no') 'OK'
    IF (colors) WRITE (unit,'(A)', advance='no') TRIM(reset_attr)
    WRITE (unit, '(T4,A)') TRIM(name)
  ELSE
    IF (colors) WRITE(unit,'(A)',advance='no') TRIM(red_attr)
    WRITE (unit, '(A)', advance='no') 'FAIL'
    IF (colors) WRITE (unit,'(A)', advance='no') TRIM(reset_attr)
    WRITE (unit, '(T4,A)') TRIM(name)
  END IF
END SUBROUTINE


END MODULE