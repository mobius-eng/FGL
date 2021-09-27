module bitstring_m
    
    use iso_c_binding, only: c_int8_t
    
    implicit none
    
    type bitstring_t
        integer :: slen
        integer(c_int8_t), dimension(:), allocatable :: buffer
    contains
        procedure, pass(self) :: set => bitstring_set
        procedure, pass(self) :: get => bitstring_get
        procedure, pass(self) :: convert_to_logical_array => bitstring_convert_to_logical_array
        procedure, pass(self) :: custom_write => bitstring_write
        generic :: write(formatted) => custom_write
    end type
    
contains
    
    function make_zero_bitstring(slen) result(bs)
        integer, intent(in) :: slen
        type(bitstring_t) :: bs
        allocate (bs%buffer((slen+7)/8))
        bs%buffer(:) = 0
        bs%slen = slen
    end function
    
    function make_bitstring_from_logical_array(larray) result(bs)
        logical, dimension(:), intent(in) :: larray
        type(bitstring_t) :: bs
        integer :: i
        allocate (bs%buffer((size(larray)+7)/8))
        bs%slen = size(larray)
        do i = 1, bs%slen
            call bs%set(i, ltoi(larray(i)))
        end do
    contains
    
        pure function ltoi(v)
            logical, intent(in) :: v
            integer :: ltoi
            if (v) then
                ltoi = 1
            else
                ltoi = 0
            end if
        end function
    
    end function

    subroutine bitstring_set(self, index, val)
        class(bitstring_t), intent(inout) :: self
        integer, intent(in) :: index, val
        ! array & bit indices; new value for all 8 bits
        integer :: iarray, ibit, newval
        integer(c_int8_t), dimension(size(self%buffer)) :: dummy_array
        iarray = (index - 1) / 8 + 1
        ibit = mod(index - 1, 8)
        if (val == 0) then
            self%buffer(iarray) = ibclr(self%buffer(iarray), ibit)
        else
            self%buffer(iarray) = ibset(self%buffer(iarray), ibit)
        end if
    end subroutine
    
    pure integer function bitstring_get(self, index)
        class(bitstring_t), intent(in) :: self
        integer, intent(in) :: index
        integer :: iarray, ibit
        iarray = (index - 1) / 8 + 1
        ibit = mod(index - 1, 8)
        bitstring_get = ibits(self%buffer(iarray), ibit, 1)
    end function
    
    function bitstring_convert_to_logical_array(self) result (larray)
        class(bitstring_t), intent(in) :: self
        logical, dimension(:), allocatable :: larray
        integer :: i
        allocate(larray(self%slen))
        do i = 1, self%slen
            larray(i) = self%get(i) /= 0
        end do
    end function
    
    subroutine bitstring_write(self, unit, iotype, v_list, iostat, iomsg)
        class(bitstring_t), intent(in) :: self
        integer, intent(in) :: unit         ! Internal unit to write to.
        character(*), intent(in) :: iotype  ! LISTDIRECTED or DTxxx
        integer, intent(in) :: v_list(:)    ! parameters from fmt spec.
        integer, intent(out) :: iostat      ! non zero on error, etc.
        character(*), intent(inout) :: iomsg  ! define if iostat non zero.
        character(len=256) :: fmt
        integer :: nfull, noverbits, i, n
        nfull = self%slen / 8
        noverbits = mod(self%slen, 8)
        do i = 1, nfull
            call write_binary(self%buffer(i))
        end do
        if (noverbits > 0) then
            n = self%buffer(nfull+1)
            !n = shiftl(n, 8 - noverbits)
            !n = ishftc(n, 1)
            do i = 1, noverbits
                write (unit, '(I1)', iostat = iostat, iomsg = iomsg) iand(n, 1)
                n = ishft(n, -1)
            end do
        end if
    contains
    
        subroutine write_binary(n)
            integer(c_int8_t), intent(in) :: n
            integer(c_int8_t) :: m, i
            m = n
            do i = 1, 8
                write (unit, '(I1)', iostat = iostat, iomsg = iomsg) iand(m, 1)
                m = ishft(m, -1)
            end do
        end subroutine
        
    end subroutine
    
end module