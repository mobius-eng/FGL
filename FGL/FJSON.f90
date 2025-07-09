MODULE fjson_m

USE iso_c_binding, ONLY : c_ptr, c_int32_t, c_double, c_char, c_loc, c_null_ptr
USE error_m, ONLY       : usererror_class
USE string_m, ONLY      : associate_c_string, rm_null_from_fstring

IMPLICIT NONE

PRIVATE
PUBLIC :: fjson_error_class, invalid_input_error, file_error, &
          parse_error, access_error, fjson_t

! Error code defined IN CJSON
INTEGER, PARAMETER :: &
  CJSON_SUCCESS             =  0,     &
  CJSON_INVALID_INPUT_ERROR = -1,     &
  CJSON_FILE_ERROR          = -2,     &
  CJSON_PARSE_ERROR         = -3,     &
  CJSON_ACCESS_ERROR        = -4

! Fortran error codes
INTEGER, PARAMETER :: fjson_error_class   = usererror_class - 1000,  &
                      invalid_input_error = fjson_error_class - 1,   &
                      file_error          = fjson_error_class - 2,   &
                      parse_error         = fjson_error_class - 3,   &
                      access_error        = fjson_error_class - 4

! Interface to CJSON static library
INTERFACE

  SUBROUTINE cjson_new_from_file(j, file_name, istat) BIND (c)
    IMPORT c_ptr
    TYPE(c_ptr), VALUE :: j, file_name, istat
  END SUBROUTINE

  FUNCTION cjson_new_empty() RESULT(j) BIND (c)
    IMPORT c_ptr
    TYPE(c_ptr) :: j
  END FUNCTION

  SUBROUTINE cjson_delete(j, istat) BIND (c)
    IMPORT c_ptr
    TYPE(c_ptr), VALUE :: j, istat
  END SUBROUTINE


  SUBROUTINE cjson_sub(newj, j, path, istat) BIND (c)
    IMPORT c_ptr
    TYPE(c_ptr), VALUE :: newj, j, istat
    TYPE(c_ptr), dimension(*) :: path
  END SUBROUTINE

  SUBROUTINE cjson_at(newj, j, index, istat) BIND (c)
    IMPORT c_ptr, c_int32_t
    TYPE(c_ptr), VALUE        :: newj, j, istat
    INTEGER(c_int32_t), VALUE :: index
  END SUBROUTINE

  SUBROUTINE cjson_get_num(x, j, istat) BIND (c)
    IMPORT c_ptr
    TYPE(c_ptr), VALUE :: x, j, istat
  END SUBROUTINE

  SUBROUTINE cjson_get_int(n, j, istat) BIND (c)
    IMPORT c_ptr
    TYPE(c_ptr), VALUE :: n, j, istat
  END SUBROUTINE

  SUBROUTINE cjson_get_str(dest, j, max_char, istat, nlen) BIND (c)
    IMPORT                    :: c_ptr, c_int32_t
    TYPE(c_ptr), VALUE        :: dest, j, istat, nlen
    INTEGER(c_int32_t), VALUE :: max_char
  END SUBROUTINE

END INTERFACE

! Fortran-ize C-INTERFACE
! Use OOP approach to encapsulate JSON internals
TYPE fjson_t
  TYPE(c_ptr) :: json
  LOGICAL               :: initialized = .false.
  LOGICAL               :: needs_delete = .false.
  CONTAINS
  FINAL                 :: fjson_delete
  PROCEDURE, PASS(self) :: init_empty => fjson_init_empty
  PROCEDURE, PASS(self) :: init_from_file => fjson_init_from_file
  PROCEDURE, PASS(self) :: sub => fjson_sub
  PROCEDURE, PASS(self) :: get_num => fjson_get_num
  PROCEDURE, PASS(self) :: get_int => fjson_get_int
  PROCEDURE, PASS(self) :: get_str => fjson_get_str
  PROCEDURE, PASS(self) :: at      => fjson_at
END TYPE


CONTAINS

PURE FUNCTION conv_error_code(ccode) RESULT (fcode)
  INTEGER, INTENT(IN) :: ccode
  INTEGER             :: fcode

  IF (ccode == 0) THEN
    fcode = 0
    RETURN
  END IF

  fcode = ccode - fjson_error_class

END FUNCTION


SUBROUTINE fjson_init_empty(self)
  CLASS(fjson_t) :: self
  self%json         = cjson_new_empty()
  self%initialized  = .true.
  self%needs_delete = .true.
END SUBROUTINE


SUBROUTINE fjson_init_from_file(self, file_name, istat)
    
  CHARACTER(len=*), INTENT(IN)          :: file_name
  CLASS(fjson_t), TARGET, INTENT(INOUT) :: self
  INTEGER, INTENT(OUT)                  :: istat

  CHARACTER(len=len(file_name)+1)       :: cfname
  INTEGER(c_int32_t), TARGET            :: cistat
  TYPE(c_ptr)                           :: pfname, pj, pistat

  cfname = trim(file_name)
  pfname = associate_c_string(cfname, .true.)
  pistat = c_loc(cistat)
  pj     = c_loc(self%json)
  
  CALL cjson_new_from_file(pj, pfname, pistat)

  IF (cistat == CJSON_SUCCESS) THEN
    self%initialized  = .true.
    self%needs_delete = .true.
  END IF

  istat = conv_error_code(cistat)

END SUBROUTINE


SUBROUTINE f_c_char_array(fsa, cha)
  CHARACTER(len = *) :: fsa(:)
  TYPE(c_ptr)        :: cha(*)
  INTEGER            :: i

  DO i = 1, size(fsa)
    cha(i) = associate_c_string(fsa(i), addnull=.true.)
  END DO
  cha(size(fsa)+1) = c_null_ptr
END SUBROUTINE


SUBROUTINE fjson_sub(self, newj, path, istat)
  CLASS(fjson_t), INTENT(INOUT)      :: self
  CHARACTER(len=*), INTENT(IN)       :: path(:)
  INTEGER, INTENT(OUT)               :: istat
  TYPE(fjson_t), INTENT(OUT), TARGET :: newj

  CHARACTER(kind=c_char, len=256)    :: cpath(size(path))
  TYPE(c_ptr)                        :: ppath(size(path)+1)
  INTEGER(c_int32_t), TARGET         :: cstat
  TYPE(c_ptr)                        :: pj, pstat
  INTEGER                            :: i

  DO i = 1, size(path)
    cpath(i) = trim(path(i))
  END DO
  CALL f_c_char_array(cpath, ppath)

  pj    = c_loc(newj%json)
  pstat = c_loc(cstat)

  CALL cjson_sub(pj, self%json, ppath, pstat)
  
  IF (cstat == CJSON_SUCCESS) THEN
    newj%initialized  = .true.
    newj%needs_delete = .false.
  END IF

  istat = conv_error_code(cstat)

END SUBROUTINE


SUBROUTINE fjson_get_num(self, x, istat)
  CLASS(fjson_t), INTENT(INOUT)       :: self
  real(c_double), INTENT(OUT), TARGET :: x
  INTEGER, INTENT(OUT)                :: istat

  INTEGER(c_int32_t), TARGET          :: cstat
  TYPE(c_ptr)                         :: px, pstat

  px    = c_loc(x)
  pstat = c_loc(cstat)

  CALL cjson_get_num(px, self%json, pstat)
  istat = conv_error_code(cstat)

END SUBROUTINE


SUBROUTINE fjson_delete(self)
  TYPE(fjson_t)              :: self
  INTEGER(c_int32_t), TARGET :: cstat
  TYPE(c_ptr)                :: pstat

  pstat = c_loc(cstat)

  IF (self%initialized .and. self%needs_delete) THEN
    CALL cjson_delete(self%json, pstat)
  END IF
  self%initialized  = .false.
  self%needs_delete = .false.

END SUBROUTINE


SUBROUTINE fjson_get_int(self, x, istat)
  CLASS(fjson_t), INTENT(INOUT) :: self
  INTEGER, INTENT(OUT)          :: x
  INTEGER, INTENT(OUT)          :: istat

  INTEGER(c_int32_t), TARGET    :: cstat, cx
  TYPE(c_ptr)                   :: px, pstat

  px    = c_loc(cx)
  pstat = c_loc(cstat)

  CALL cjson_get_int(px, self%json, pstat)
  istat = conv_error_code(cstat)
  x = cx

END SUBROUTINE


SUBROUTINE fjson_get_str(self, dest, istat)
  CLASS(fjson_t), INTENT(IN)                      :: self
  CHARACTER(len=*), INTENT(INOUT)                 :: dest
  INTEGER, INTENT(OUT)                            :: istat

  INTEGER(c_int32_t)                              :: n
  INTEGER(c_int32_t), TARGET                      :: cstat, nlen
  CHARACTER(kind=c_char, len=len(dest)+1), TARGET :: cdest
  TYPE(c_ptr)                                     :: pdest, pstat, plen
  
  n     = len(dest, c_int32_t)
  pdest = c_loc(cdest)
  pstat = c_loc(cstat)
  plen  = c_loc(nlen)
  
  CALL cjson_get_str(pdest, self%json, n, pstat, plen)
  dest = ''
  dest(1:nlen) = cdest(1:nlen)
  istat = conv_error_code(cstat)
  
END SUBROUTINE


SUBROUTINE fjson_at(self, newj, index, istat)
  CLASS(fjson_t), INTENT(IN)         :: self
  INTEGER, INTENT(IN)                :: index
  TYPE(fjson_t), INTENT(OUT), TARGET :: newj
  INTEGER, INTENT(OUT)               :: istat
  
  INTEGER(c_int32_t)                 :: ci
  INTEGER(c_int32_t), TARGET         :: cstat
  TYPE(c_ptr)                        :: pstat, pj

  ci    = index
  pstat = c_loc(cstat)
  pj    = c_loc(newj%json)

  CALL cjson_at(pj, self%json, ci, pstat)
  IF (cstat == CJSON_SUCCESS) THEN
    newj%initialized  = .true.
    newj%needs_delete = .false.
  END IF

  istat = conv_error_code(cstat)

END SUBROUTINE


END MODULE