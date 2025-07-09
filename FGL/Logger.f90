MODULE logger_m

USE iso_fortran_env, ONLY: error_unit,                    &
                           int8, int16, int32, int64,     &
                           real32, real64, real128
USE error_m
USE optional_m
USE string_m

IMPLICIT NONE
PRIVATE
PUBLIC print_log, set_log_output, get_log_output, fmt

! Log output file. Defaults to error stream
INTEGER :: log_output = error_unit

INTERFACE fmt
    ! Simple formatting INTERFACE
    MODULE PROCEDURE :: fmt_real32, fmt_real64, fmt_real128,          &
                        fmt_int8, fmt_int16, fmt_int32, fmt_int64
END INTERFACE fmt

CHARACTER(len=*), PARAMETER :: fmt_str_format = '("(A,",A,",A)")'
CHARACTER(len=*), PARAMETER :: set_log_output_err_msg =     &
  'Error IN setting logger output: cannot access file'


CONTAINS


SUBROUTINE set_log_output(new_output, status)
  !* Change the file of log output. No argument: sets default.
  INTEGER, INTENT(IN), OPTIONAL  :: new_output
  INTEGER, INTENT(OUT), OPTIONAL :: status
  LOGICAL                        :: op
  CHARACTER(len=7)               :: writable
  
  CALL set_optional(NO_ERROR, opt=status)
  
  IF (present(new_output)) THEN
    ! Check IF there is a writable file associated with `new_output` unit
    INQUIRE (unit=new_output, opened=op, WRITE=writable)
    ! Lower the case
    CALL to_lower(writable)
    IF (op .and. writable == 'yes') THEN
      log_output = new_output
      RETURN
    END IF

    ! Getting here only IF invalid `new_output` was passed
    IF (present(status)) THEN
      status = IO_FILE_ACCESS_ERROR
      CALL set_error(IO_FILE_ACCESS_ERROR, set_log_output_err_msg)
    ELSE
      WRITE (error_unit, *) set_log_output_err_msg
      WRITE (error_unit, *) 'Setting logger to output to error unit'
      CALL set_error(IO_FILE_ACCESS_ERROR, set_log_output_err_msg)
      log_output = error_unit
    END IF
  ELSE
    ! Re-setting default value
    log_output = error_unit
  END IF
END SUBROUTINE set_log_output


FUNCTION get_log_output()
  ! Returns file ID of the current output.
  INTEGER get_log_output
  get_log_output = log_output
END FUNCTION


FUNCTION fmt_real32(pref, num, suff, num_format) RESULT(s)
  ! Formats single precision REAL with prefix & suffix.
  CHARACTER(len=*), INTENT(IN) :: pref, suff
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: num_format
  REAL(real32), INTENT(IN) :: num
  CHARACTER(len=len_trim(pref)+len_trim(suff)+40) :: s
  CHARACTER(len=30) :: full_format
  IF (present(num_format)) THEN
    WRITE(unit=full_format, fmt=fmt_str_format) num_format
    WRITE(unit=s, fmt=full_format) pref, num, suff
  ELSE
    WRITE(unit=s, fmt='(A,G0.6,A)') pref, num, suff
  END IF
END FUNCTION


FUNCTION fmt_real64(pref, num, suff, num_format) RESULT(s)
  ! Formats double precision REAL with prefix & suffix.
  CHARACTER(len=*), INTENT(IN) :: pref, suff
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: num_format
  REAL(real64), INTENT(IN) :: num
  CHARACTER(len=len_trim(pref)+len_trim(suff)+40) :: s
  CHARACTER(len=30) :: full_format
  IF (present(num_format)) THEN
    WRITE(unit=full_format, fmt=fmt_str_format) num_format
    WRITE(unit=s, fmt=full_format) pref, num, suff
  ELSE
    WRITE(s, '(A,G0.15,A)') pref, num, suff
  END IF
END FUNCTION


FUNCTION fmt_real128(pref, num, suff, num_format) RESULT(s)
  ! Formats double precision REAL with prefix & suffix.
  CHARACTER(len=*), INTENT(IN) :: pref, suff
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: num_format
  REAL(real128), INTENT(IN) :: num
  CHARACTER(len=len_trim(pref)+len_trim(suff)+40) :: s
  CHARACTER(len=30) :: full_format
  IF (present(num_format)) THEN
    WRITE(unit=full_format, fmt=fmt_str_format) num_format
    WRITE(unit=s, fmt=full_format) pref, num, suff
  ELSE
    WRITE(s, '(A,G0.20,A)') pref, num, suff
  END IF
END FUNCTION


FUNCTION fmt_int8(pref, num, suff, num_format) RESULT(s)
  ! Formats default INTEGER with prefix & suffix.
  CHARACTER(len=*), INTENT(IN) :: pref, suff
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: num_format
  INTEGER(int8), INTENT(IN) :: num
  CHARACTER(len=len_trim(pref)+len_trim(suff)+40) :: s
  CHARACTER(len=30) :: full_format
  IF (present(num_format)) THEN
    WRITE(unit=full_format, fmt=fmt_str_format) num_format
    WRITE(unit=s, fmt=full_format) pref, num, suff
  ELSE
    WRITE(s, '(A,I5,A)') pref, num, suff
  END IF
END FUNCTION


FUNCTION fmt_int16(pref, num, suff, num_format) RESULT(s)
  ! Formats default INTEGER with prefix & suffix.
  CHARACTER(len=*), INTENT(IN) :: pref, suff
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: num_format
  INTEGER(int16), INTENT(IN) :: num
  CHARACTER(len=len_trim(pref)+len_trim(suff)+40) :: s
  CHARACTER(len=30) :: full_format
  IF (present(num_format)) THEN
    WRITE(unit=full_format, fmt=fmt_str_format) num_format
    WRITE(unit=s, fmt=full_format) pref, num, suff
  ELSE
    WRITE(s, '(A,I7,A)') pref, num, suff
  END IF
END FUNCTION


FUNCTION fmt_int32(pref, num, suff, num_format) RESULT(s)
  ! Formats default INTEGER with prefix & suffix.
  CHARACTER(len=*), INTENT(IN) :: pref, suff
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: num_format
  INTEGER(int32), INTENT(IN) :: num
  CHARACTER(len=len_trim(pref)+len_trim(suff)+40) :: s
  CHARACTER(len=30) :: full_format
  IF (present(num_format)) THEN
    WRITE(unit=full_format, fmt=fmt_str_format) num_format
     WRITE(unit=s, fmt=full_format) pref, num, suff
  ELSE
    WRITE(s, '(A,I12,A)') pref, num, suff
  END IF
END FUNCTION


FUNCTION fmt_int64(pref, num, suff, num_format) RESULT(s)
  ! Formats default INTEGER with prefix & suffix.
  CHARACTER(len=*), INTENT(IN) :: pref, suff
  CHARACTER(len=*), INTENT(IN), OPTIONAL :: num_format
  INTEGER(int64), INTENT(IN) :: num
  CHARACTER(len=len_trim(pref)+len_trim(suff)+40) :: s
  CHARACTER(len=30) :: full_format
  IF (present(num_format)) THEN
    WRITE(unit=full_format, fmt=fmt_str_format) num_format
    WRITE(unit=s, fmt=full_format) pref, num, suff
  ELSE
    WRITE(s, '(A,I22,A)') pref, num, suff
  END IF
END FUNCTION


SUBROUTINE print_log(msg, prefix, timestamp)
  ! Outputs log message. The message is prefixed & optionally time stamped.
  CHARACTER(len=*), INTENT(IN) :: msg, prefix
  LOGICAL, INTENT(IN), OPTIONAL :: timestamp
  CHARACTER(len=8) :: date
  CHARACTER(len=10) :: time
  CHARACTER(len=20) :: full_time
  IF (default_or_optional(.false., timestamp)) THEN
    CALL date_and_time(date, time)
    WRITE (full_time, '(" ", A,"/",A,"/",A," ",A,":",A,":",A)') &
      date(1:4), date(5:6), date(7:8), time(1:2), time(3:4), time(5:6)
  ELSE
    full_time = ''
  END IF
  WRITE(log_output, '("[",A,A,"] ",A)') trim(prefix), trim(full_time), trim(msg)
END SUBROUTINE


END MODULE
