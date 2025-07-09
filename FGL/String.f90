MODULE string_m
!
! (c) 2024 Alexey V. Cherkaev
!
! Additional string functions
!    
USE iso_c_binding, ONLY : c_ptr, c_size_t, c_loc, c_null_char
USE error_m, ONLY       : usererror_class, set_error

IMPLICIT NONE
PRIVATE
PUBLIC :: to_lower, to_upper,get_all_cmd_arg, char_array_to_string, &
          associate_c_string, rm_null_from_fstring

CHARACTER(len=26), PARAMETER :: alph(2) = &
  ['ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz']

INTEGER, PARAMETER :: cmd_arg_max_length = 1024
INTEGER, PARAMETER :: cmd_error = usererror_class - 2000 - 1

INTERFACE
  FUNCTION strlen(s) BIND (C, name='strlen')
    IMPORT                         :: c_ptr, c_size_t
    TYPE(c_ptr), INTENT(IN), VALUE :: s
    INTEGER(c_size_t)              :: strlen
  END FUNCTION
END INTERFACE


CONTAINS


SUBROUTINE to_lower(s)
  ! TO_LOWER
  !   Convert string to lower-CASE (only latin symbols)
  !
  !   Parameters
  !       S       (INOUT) String to convert (IN place)
  !
  ! Details
  !   In-place convert string to lower-CASE. Conversion is
  !   independent of a particular encoding.
  CHARACTER(len=*), INTENT(INOUT) :: s
  INTEGER                         :: ia, ic, ialph
  ! Use conversion to ASCII CHARACTER since they are
  ! one after another
  ia = iachar('A')
  
  DO ic=1,len(s)
    
    SELECT CASE (s(ic:ic))
      CASE ('A':'Z')
        ialph = iachar(s(ic:ic)) - ia + 1
        s(ic:ic) = alph(2)(ialph:ialph)
    END SELECT
  
  END DO

END SUBROUTINE


SUBROUTINE to_upper(s)
  ! TO_UPPER
  !   Convert string to upper-CASE (only latin symbols)
  !
  !   Parameters
  !       S       (INOUT) String to convert (IN place)
  !
  ! Details
  !   In-place convert string to upper-CASE. Conversion is
  !   independent of a particular encoding.
  CHARACTER(len=*), INTENT(INOUT) :: s
  INTEGER                         :: ia, ic, ialph
  ia = iachar('a')

  DO ic = 1,len(s)

    SELECT CASE (s(ic:ic))
      CASE ('a':'z')
        ialph = iachar(s(ic:ic)) - ia + 1
        s(ic:ic) = alph(1)(ialph:ialph)
    END SELECT

  END DO

END SUBROUTINE


SUBROUTINE get_all_cmd_arg(string)
  CHARACTER(len=*), INTENT(OUT)       :: string

  CHARACTER(len=cmd_arg_max_length*2) :: full_command
  CHARACTER(len=cmd_arg_max_length)   :: cmd_name
  INTEGER                             :: full_cmd_len, cmd_len
  CHARACTER(len=64), PARAMETER        :: err_msg = &
    'Cannot parse command line arguments: unknown format'

  CALL get_command(full_command, full_cmd_len)
  CALL get_command_argument(0, cmd_name, cmd_len)

  ! This can happen IF command name was escaped with ""
  IF (full_command(1:1) /= cmd_name(1:1)) THEN

    IF (full_command(1:1) == '"' .and. &
        full_command(cmd_len+2:cmd_len+2) == '"') THEN
      ! Adjust command name length to include ""
      cmd_len = cmd_len + 2
    ELSE
      CALL set_error(cmd_error, err_msg)
      string = ''
      RETURN
    END IF

  END IF
  
  string = full_command(cmd_len+1:full_cmd_len)

END SUBROUTINE


SUBROUTINE char_array_to_string(arr, str)
  ! A utility FUNCTION: from C get array of characters
  ! Need to convert them to Fortran string
  CHARACTER(len=1) :: arr(:)
  CHARACTER(len=*) :: str
  INTEGER          :: n, i
  
  n = min(size(arr), len(str))
  DO i = 1, n
      str(i:i) = arr(i)
  END DO

END SUBROUTINE


FUNCTION associate_c_string(fstring, addnull) result(s)
  ! Helpers to deal with Fortran & C strings
  CHARACTER(len=*), INTENT(INOUT), target :: fstring
  logical, INTENT(IN), optional           :: addnull
  TYPE(c_ptr)                             :: s
  INTEGER                                 :: n

  n = len_trim(fstring)
  IF (n < len(fstring) .and. present(addnull)) THEN
    IF (addnull) fstring(n+1:n+1) = c_null_char
  END IF
  s = c_loc(fstring)
END FUNCTION


SUBROUTINE rm_null_from_fstring(fstring)
  CHARACTER(len=*), INTENT(INOUT) :: fstring
  INTEGER                         :: n, i
  
  n = len(fstring)
  i = 1
  DO
    IF (i > n) EXIT

    IF (fstring(i:i) == c_null_char) THEN
      fstring(i:i) = ' '
      i = i + 1
      EXIT
    END IF
    
    i = i + 1

  END DO

  DO i = i, n
      fstring(i:i) = ' '
  END DO

END SUBROUTINE


END MODULE