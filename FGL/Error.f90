MODULE error_m
!
! (c) 2024 Alexey V. Cherkaev
!
! Error (exception) handling routines & methods
!

IMPLICIT NONE

PRIVATE

! Errors are identified by the error number

INTEGER, PARAMETER, PUBLIC    :: no_error = 0

! These numbers (errors) are split into classes
! Classes of errors
INTEGER, PARAMETER, PUBLIC    :: ioerror_class = 0
INTEGER, PARAMETER, PUBLIC    :: memerror_class = -100
INTEGER, PARAMETER, PUBLIC    :: arrayerror_class = -200
INTEGER, PARAMETER, PUBLIC    :: argerror_class = -300
INTEGER, PARAMETER, PUBLIC    :: valerror_class = -400
INTEGER, PARAMETER, PUBLIC    :: usererror_class = -1000

! I/O Errors
INTEGER, PARAMETER, PUBLIC    :: io_unspecified_error = ioerror_class - 1
INTEGER, PARAMETER, PUBLIC    :: io_open_error = ioerror_class - 2
INTEGER, PARAMETER, PUBLIC    :: io_output_error = ioerror_class - 3
INTEGER, PARAMETER, PUBLIC    :: io_input_error = ioerror_class - 4
INTEGER, PARAMETER, PUBLIC    :: io_file_access_error = ioerror_class - 5

! Memory access and allocation
INTEGER, PARAMETER, PUBLIC    :: mem_unspecified_error = memerror_class - 1
INTEGER, PARAMETER, PUBLIC    :: mem_alloc_error = memerror_class - 2
INTEGER, PARAMETER, PUBLIC    :: mem_access_error = memerror_class - 3

! Array related errors
INTEGER, PARAMETER, PUBLIC    :: array_unspecified_error = arrayerror_class - 1
INTEGER, PARAMETER, PUBLIC    :: array_size_error = arrayerror_class - 2
INTEGER, PARAMETER, PUBLIC    :: array_size_mismatch_error = arrayerror_class - 3

! Argument errors
INTEGER, PARAMETER, PUBLIC    :: arg_unspecified_error = argerror_class - 1
INTEGER, PARAMETER, PUBLIC    :: arg_not_provided_error = argerror_class - 2
INTEGER, PARAMETER, PUBLIC    :: arg_type_error = argerror_class - 3

! Value errors
INTEGER, PARAMETER, PUBLIC    :: val_unspecified_error = valerror_class - 1
INTEGER, PARAMETER, PUBLIC    :: val_domain_error = valerror_class - 2

! Use USERERROR_CLASS value as general unspecified error
INTEGER, PARAMETER, PUBLIC    :: unspecified_error = usererror_class

INTEGER, PARAMETER            :: max_err_msg_length = 1024

! Store information on last error
INTEGER                       :: last_error_code = no_error
CHARACTER(max_err_msg_length) :: last_error_message

PUBLIC                        :: get_last_error_code,               &
                                 get_last_error_message, set_error, &
                                 release_error

CONTAINS

FUNCTION get_last_error_code() result (res)
  INTEGER :: res
  res = last_error_code
END FUNCTION

FUNCTION get_last_error_message() result (msg)
  CHARACTER(:), ALLOCATABLE :: msg
  msg = trim(last_error_message)
END FUNCTION

SUBROUTINE set_error(code, msg)
  INTEGER, INTENT(IN) :: code
  CHARACTER(*), INTENT(IN) :: msg
  INTEGER :: n
  last_error_code = code
  n = len_trim(msg)
  n = min(n, max_err_msg_length)
  last_error_message(1:n) = msg(1:n)
END SUBROUTINE


SUBROUTINE release_error()
  last_error_code = no_error
  last_error_message = ''
END SUBROUTINE

END MODULE