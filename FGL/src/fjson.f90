module fjson_m
    
    use iso_c_binding
    use error_m
    use string_m, only: associate_c_string
    implicit none
    
    ! Interface to CJSON static library
    interface
    
        function cjson_read_from_file(file_name) bind (c)
            use iso_c_binding, only: c_ptr
            type(c_ptr) :: cjson_read_from_file
            type(c_ptr), value :: file_name
        end function
        
        function cjson_make_empty() bind (c)
            use iso_c_binding, only: c_ptr
            type(c_ptr) :: cjson_make_empty
        end function
        
        subroutine cjson_write_to_file(json, file_name) bind (c)
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: json, file_name
        end subroutine
        
        subroutine cjson_delete(json) bind (c)
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: json
        end subroutine
        
        function cjson_read_number_g(json, spath, ipath, index_selector) result (x) bind (c)
            use iso_c_binding, only: c_ptr, c_double, c_int
            type(c_ptr), value :: json
            type(c_ptr), dimension(*) :: spath
            integer(c_int), dimension(*) :: ipath
            integer(c_int), dimension(*) :: index_selector
            real(c_double) :: x
        end function
        
        subroutine cjson_put_number_g(json, spath, ipath, index_selector, number) bind (c)
            use iso_c_binding, only: c_ptr, c_double
            type(c_ptr), value :: json, spath, ipath, index_selector
            real(c_double), value :: number
        end subroutine
        
        subroutine cjson_read_string_g(json, spath, ipath, index_selector, dest, max_count) bind (c)
            use iso_c_binding, only: c_ptr, c_int
            type(c_ptr), value :: json, spath, ipath, index_selector, dest
            integer(c_int), value :: max_count
        end subroutine
        
        subroutine cjson_put_string_g(json, spath, ipath, index_selector, dest) bind (c)
            use iso_c_binding, only: c_ptr
            type(c_ptr), value :: json, spath, ipath, index_selector, dest
        end subroutine
        
    end interface
    
    ! Fortran-ize C-interface
    
    type fjson_t
        type(c_ptr) :: json
        logical :: initialized = .false.
    contains
        final :: fjson_clean
        procedure, pass(self) :: write_to_file => fjson_write_to_file
    end type
    
    type fjson_index_t(slen, ilen)
        integer, len :: slen, ilen
        character(len=256), dimension(slen) :: spath
        integer, dimension(ilen) :: ipath
        integer, dimension(slen + ilen) :: isel
        integer :: slast = 0, ilast = 0
    contains
        generic :: push => push_string, push_integer
        procedure, pass(self) :: push_string => fjson_index_push_string
        procedure, pass(self) :: push_integer => fjson_index_push_integer
    end type
    
contains

    subroutine fjson_index_push_string(self, string)
        class(fjson_index_t(slen = *, ilen = *)) :: self
        character(len=*), intent(in) :: string
        integer :: n
        if (self%slast >= self%slen) then
            call set_error(MEM_ACCESS_ERROR, 'FJSON_INDEX_PUSH_STRING: buffer is full')
            return
        end if
        n = min(len_trim(string), 256)
        self%slast = self%slast + 1
        self%spath(self%slast) = string(1:n)
        self%isel(self%ilast + self%slast) = 0
    end subroutine
    
    subroutine fjson_index_push_integer(self, number)
        class(fjson_index_t(slen = *, ilen = *)) :: self
        integer, intent(in) :: number
        if (self%ilast >= self%ilen) then
            call set_error(MEM_ACCESS_ERROR, 'FJSON_INDEX_PUSH_INTEGER: buffer is full')
            return
        end if
        self%ilast = self%ilast + 1
        self%ipath(self%ilast) = number
        self%isel(self%ilast + self%slast) = 1
    end subroutine

    function fjson_make_empty() result (json)
        type(fjson_t) :: json
        json%json = cjson_make_empty()
        json%initialized = .true.
    end function
    
    function fjson_read_from_file(file_name) result (json)
        character(len=*), intent(in) :: file_name
        type(fjson_t) :: json
        character(len=len(file_name)) :: fname
        type(c_ptr) :: c_file_name
        fname = trim(file_name)
        c_file_name = associate_c_string(fname, .true.)
        json%json = cjson_read_from_file(c_file_name)
        json%initialized = .true.
    end function
    
    subroutine fjson_write_to_file(self, file_name)
        class(fjson_t) :: self
        character(len=*), intent(in) :: file_name
        character(len=len(file_name)) :: fname
        type(c_ptr) :: c_file_name
        fname = trim(file_name)
        c_file_name = associate_c_string(fname, .true.)
        call cjson_write_to_file(self%json, c_file_name)
    end subroutine
    
    subroutine f_c_char_array(fsa, cha)
        character(kind = c_char, len = *), dimension(:), target :: fsa
        type(c_ptr), dimension(:) :: cha
        integer :: i
        do i = 1, size(cha)
            cha(i) = c_loc(fsa(i))
        end do
    end subroutine
    
    function fjson_read_number(json, path) result (x)
        class(fjson_t) :: json
        type(fjson_index_t(slen = *,ilen = *)) :: path
        real(c_double) :: x
        type(c_ptr), dimension(path%slen) :: c_spath
        call f_c_char_array(path%spath, c_spath)
        x = cjson_read_number_g(json%json, c_spath, path%ipath, path%isel)
    end function

    subroutine fjson_clean(self)
        type(fjson_t) :: self
        if (self%initialized) call cjson_delete(self%json)
        self%initialized = .false.
    end subroutine
    
end module