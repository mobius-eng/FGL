module hash_m
    
    use iso_fortran_env
    implicit none
    private
    
    integer, parameter, public :: ihash = int32
    
    public hash
    interface hash
        module procedure murmur_hash
    end interface
    
contains
    
    integer(ihash) function get_chunk(nstr, str, i) result(n)
        integer(ihash), intent(in) :: nstr
        integer(ihash), intent(inout) :: i
        character(len=nstr), intent(in) :: str
        n = 0
        n = ichar(str(i:i), ihash)
        n = ishft(n, 8)
        if (i + 3 <= nstr) then
            n = n + ichar(str(i+1:i+1), ihash)
            n = ishft(n, 8)
            n = n + ichar(str(i+2:i+2), ihash)
            n = ishft(n, 8)
            n = n + ichar(str(i+3:i+2), ihash)
            i = i + 4
        elseif (i + 2 <= nstr) then
            n = n + ichar(str(i+1:i+1), ihash)
            n = ishft(n, 8)
            n = n + ichar(str(i+2:i+2), ihash)
            n = ishft(n, 8)
            i = nstr + 1
        elseif (i + 1 <= nstr) then
            n = n + ichar(str(i+1:i+1), ihash)
            n = ishft(n, 16)
            i = nstr + 1
        else
            n = ishft(n, 16)
            i = nstr + 1
        end if
    end function
        
    
    integer(ihash) function murmur_hash(str, seed) result(hsh)
        character(len=*), intent(in) :: str
        integer(ihash), intent(in) :: seed
        integer(ihash) :: nstr
        integer(ihash) :: c1, c2, r1, r2, m, n, istr, chunk, k
        ! May need some improvements
        c1 = z'cc9e2d51'
        c2 = z'1b873593'
        r1 = 15
        r2 = 13
        m = 5
        n = z'e6546b64'
        hsh = seed
        nstr = len_trim(str, ihash)
        istr = 1
        do while (istr <= nstr)
            chunk = get_chunk(nstr, str, istr)
            k = chunk
            k = k * c1
            k = ishftc(k, r1)
            k = k * c2
            hsh = ixor(hsh, k)
            hsh = ishftc(hsh, r2)
            hsh = hsh * m + n
        end do
        hsh = ixor(hsh, nstr)
        hsh = ixor(hsh, ishft(hsh, -16))
        hsh = hsh * z'85ebca6b'
        hsh = ixor(hsh, ishft(hsh, -13))
        hsh = hsh * z'c2b2ae35'
        hsh = ixor(hsh, ishft(hsh, -16))
        ! Fortran doesn't have unsigned numbers
        ! Ensure we get positive 32-bit number
        ! Effectively, hash is 31-bit.
        hsh = iand(hsh, z'7FFFFFFF')
    end function

end module