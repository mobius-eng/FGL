module fjson_m
    
use iso_c_binding, only: c_ptr, c_int32_t, c_double, c_char, c_loc, c_null_ptr
use error_m
use string_m, only: associate_c_string, rm_null_from_fstring
implicit none

! Interface to CJSON static library
interface

    function cjson_new_from_file(file_name) result (j) bind (c)
        import c_ptr
        type(c_ptr)        :: j
        type(c_ptr), value :: file_name
    end function

    function cjson_new_empty() result(j) bind (c)
        import c_ptr
        type(c_ptr) :: j
    end function

    subroutine cjson_delete(j) bind (c)
        import c_ptr
        type(c_ptr), value :: j
    end subroutine


    function cjson_sub(j, path) result(newj) bind (c)
        import c_ptr
        type(c_ptr), value :: j
        type(c_ptr), dimension(*) :: path
        type(c_ptr)        :: newj
    end function

    function cjson_at(j, index) result(newj) bind (c)
        import c_ptr, c_int32_t
        type(c_ptr), value        :: j
        integer(c_int32_t), value :: index
        type(c_ptr)               :: newj
    end function

    function cjson_get_num(j) result (x) bind (c)
        import c_ptr, c_double
        type(c_ptr), value :: j
        real(c_double)     :: x
    end function

    function cjson_get_int(j) result(n) bind (c)
        import c_ptr, c_int32_t
        type(c_ptr), value :: j
        integer(c_int32_t) :: n
    end function

    subroutine cjson_get_str(dest, j, max_char) bind (c)
        import c_ptr, c_int32_t
        type(c_ptr), value :: dest, j
        integer(c_int32_t) :: max_char
    end subroutine
    
end interface

! Fortran-ize C-interface

type fjson_t
    type(c_ptr) :: json
    logical :: initialized = .false.
    logical :: needs_delete = .false.
contains
    final :: fjson_delete
    procedure, pass(self) :: init_empty => fjson_init_empty
    procedure, pass(self) :: init_from_file => fjson_init_from_file
    procedure, pass(self) :: sub => fjson_sub
    procedure, pass(self) :: get_num => fjson_get_num
    procedure, pass(self) :: get_int => fjson_get_int
    procedure, pass(self) :: get_str => fjson_get_str
    procedure, pass(self) :: at      => fjson_at
end type

contains


subroutine fjson_init_empty(self)
    class(fjson_t) :: self
    self%json = cjson_new_empty()
    self%initialized = .true.
    self%needs_delete = .true.
end subroutine

subroutine fjson_init_from_file(self, file_name)
    character(len=*), intent(in) :: file_name
    class(fjson_t) :: self
    character(len=len(file_name)+1) :: fname
    type(c_ptr) :: c_file_name
    fname = trim(file_name)
    c_file_name = associate_c_string(fname, .true.)
    self%json = cjson_new_from_file(c_file_name)
    self%initialized = .true.
    self%needs_delete = .true.
end subroutine

subroutine f_c_char_array(fsa, cha)
    character(len = *), dimension(:)     :: fsa
    character(kind = c_char, len=256), &
            dimension(size(fsa)), target :: cfsa
    type(c_ptr), dimension(*)            :: cha
    integer                              :: i

    do i = 1, size(fsa)
        cfsa(i) = trim(fsa(i))
        cha(i) = associate_c_string(cfsa(i), addnull = .true.)
    end do
    cha(size(fsa)+1) = c_null_ptr
end subroutine

function fjson_sub(self, path) result (newj)
    
    class(fjson_t)                             :: self
    character(len=*), dimension(:), intent(in) :: path
    type(fjson_t)                              :: newj
    type(c_ptr), dimension(size(path)+1)       :: cpath

    call f_c_char_array(path, cpath)
    newj%json = cjson_sub(self%json, cpath)
    newj%initialized = .true.
    newj%needs_delete = .false.

end function

function fjson_get_num(self) result (x)
    class(fjson_t) :: self
    real(c_double) :: x
    x = cjson_get_num(self%json)
end function

subroutine fjson_delete(self)
    type(fjson_t) :: self
    if (self%initialized .and. self%needs_delete) call cjson_delete(self%json)
    self%initialized = .false.
    self%needs_delete = .false.
end subroutine

function fjson_get_int(self) result (x)
    class(fjson_t)     :: self
    integer            :: x
    x = cjson_get_int(self%json)
end function

subroutine fjson_get_str(self, dest)
    class(fjson_t)                          :: self
    character(len=*), intent(inout), target :: dest
    integer(c_int32_t)                      :: n
    type(c_ptr)                             :: cdest

    n = len(dest, c_int32_t)
    cdest = c_loc(dest)
    call cjson_get_str(cdest, self%json, n)
    call rm_null_from_fstring(dest)

end subroutine

function fjson_at(self, index) result (newj)
    class(fjson_t)      :: self
    integer, intent(in) :: index
    type(fjson_t)       :: newj
    integer(c_int32_t)  :: ci
    
    ci = index
    newj%json = cjson_at(self%json, ci)
    newj%initialized = .true.
    newj%needs_delete = .false.

end function


end module