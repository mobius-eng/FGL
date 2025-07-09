MODULE optional_m

USE iso_fortran_env, ONLY: int8, int16, int32, int64, real32, real64, real128

IMPLICIT NONE
PRIVATE
PUBLIC default_or_optional, set_optional

INTERFACE default_or_optional
  ! Return OPTIONAL value (IF present) or default value.
  ! Important: although the default value is marked as `OPTIONAL` IN
  ! PROCEDURE INTERFACE, it must be always present!
  MODULE PROCEDURE :: default_or_optional_int8, default_or_optional_int16,    &
                      default_or_optional_int32, default_or_optional_int64,   &
                      default_or_optional_real32, default_or_optional_real64, &
                      default_or_optional_real128, default_or_optional_logical
END INTERFACE

INTERFACE set_optional
  MODULE PROCEDURE :: set_optional_int8,                        &
                      set_optional_int16, set_optional_int32,   &
                      set_optional_int64, set_optional_real32,  &
                      set_optional_real64, set_optional_real128 
END INTERFACE


CONTAINS


FUNCTION default_or_optional_logical(defval, optval) RESULT (res)
  LOGICAL, INTENT(IN) :: defval
  LOGICAL, INTENT(IN), OPTIONAL :: optval
  LOGICAL :: res
  IF (present(optval)) THEN
    res = optval
  ELSE
    res = defval
  END IF
END FUNCTION


FUNCTION default_or_optional_int8(defval, optval) RESULT (res)
  INTEGER(int8), INTENT(IN) ::  defval
  INTEGER(int8), INTENT(IN), OPTIONAL :: optval
  INTEGER(int8) :: res
  IF (present(optval)) THEN
    res = optval
  ELSE
    res = defval
  END IF
END FUNCTION


FUNCTION default_or_optional_int16(defval, optval) RESULT (res)
  INTEGER(int16), INTENT(IN) ::  defval
  INTEGER(int16), INTENT(IN), OPTIONAL :: optval
  INTEGER(int16) :: res
  IF (present(optval)) THEN
    res = optval
  ELSE
    res = defval
  END IF
END FUNCTION


FUNCTION default_or_optional_int32(defval, optval) RESULT (res)
  INTEGER(int32), INTENT(IN) ::  defval
  INTEGER(int32), INTENT(IN), OPTIONAL :: optval
  INTEGER(int32) :: res
  IF (present(optval)) THEN
    res = optval
  ELSE
    res = defval
  END IF
END FUNCTION


FUNCTION default_or_optional_int64(defval, optval) RESULT (res)
  INTEGER(int64), INTENT(IN) ::  defval
  INTEGER(int64), INTENT(IN), OPTIONAL :: optval
  INTEGER(int64) :: res
  IF (present(optval)) THEN
    res = optval
  ELSE
    res = defval
  END IF
END FUNCTION


FUNCTION default_or_optional_real32(defval, optval) RESULT (res)
  REAL(real32), INTENT(IN) ::  defval
  REAL(real32), INTENT(IN), OPTIONAL :: optval
  REAL(real32) :: res
  IF (present(optval)) THEN
    res = optval
  ELSE
    res = defval
  END IF
END FUNCTION


FUNCTION default_or_optional_real64(defval, optval) RESULT (res)
  REAL(real64), INTENT(IN) ::  defval
  REAL(real64), INTENT(IN), OPTIONAL :: optval
  REAL(real64) :: res
  IF (present(optval)) THEN
    res = optval
  ELSE
    res = defval
  END IF
END FUNCTION


FUNCTION default_or_optional_real128(defval, optval) RESULT (res)
  REAL(real128), INTENT(IN) ::  defval
  REAL(real128), INTENT(IN), OPTIONAL :: optval
  REAL(real128) :: res
  IF (present(optval)) THEN
    res = optval
  ELSE
    res = defval
  END IF
END FUNCTION


SUBROUTINE set_optional_int8(val, opt)
  INTEGER(int8), INTENT(IN) :: val
  INTEGER(int8), INTENT(inout), OPTIONAL :: opt
  IF (present(opt)) THEN
    opt = val
  END IF
END SUBROUTINE


SUBROUTINE set_optional_int16(val, opt)
  INTEGER(int16), INTENT(IN) :: val
  INTEGER(int16), INTENT(inout), OPTIONAL :: opt
  IF (present(opt)) THEN
    opt = val
  END IF
END SUBROUTINE


SUBROUTINE set_optional_int32(val, opt)
  INTEGER(int32), INTENT(IN) :: val
  INTEGER(int32), INTENT(inout), OPTIONAL :: opt
  IF (present(opt)) THEN
    opt = val
  END IF
END SUBROUTINE


SUBROUTINE set_optional_int64(val, opt)
  INTEGER(int64), INTENT(IN) :: val
  INTEGER(int64), INTENT(inout), OPTIONAL :: opt
  IF (present(opt)) THEN
    opt = val
  END IF
END SUBROUTINE


SUBROUTINE set_optional_real32(val, opt)
  REAL(real32), INTENT(IN) :: val
  REAL(real32), INTENT(inout), OPTIONAL :: opt
  IF (present(opt)) THEN
    opt = val
  END IF
END SUBROUTINE


SUBROUTINE set_optional_real64(val, opt)
  REAL(real64), INTENT(IN) :: val
  REAL(real64), INTENT(inout), OPTIONAL :: opt
  IF (present(opt)) THEN
    opt = val
  END IF
END SUBROUTINE


SUBROUTINE set_optional_real128(val, opt)
  REAL(real128), INTENT(IN) :: val
  REAL(real128), INTENT(inout), OPTIONAL :: opt
  IF (present(opt)) THEN
    opt = val
  END IF
END SUBROUTINE


END MODULE