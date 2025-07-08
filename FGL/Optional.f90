module optional_m

    use iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128
    implicit none
    private

    interface default_or_optional
        !> Return optional value (if present) or default value.
        !  Important: although the default value is marked as `optional` in
        ! procedure interface, it must be always present!
        module procedure :: default_or_optional_int8, default_or_optional_int16, &
            & default_or_optional_int32, default_or_optional_int64, &
            & default_or_optional_real32, default_or_optional_real64, &
            & default_or_optional_real128, default_or_optional_logical
    end interface

    interface set_optional
        module procedure :: set_optional_int8,  &
            & set_optional_int16, set_optional_int32, set_optional_int64, &
            & set_optional_real32, set_optional_real64, set_optional_real128
    end interface
    
    public default_or_optional, set_optional

contains

    function default_or_optional_logical(defval, optval) result (res)
        logical, intent(in) :: defval
        logical, intent(in), optional :: optval
        logical :: res
        if (present(optval)) then
            res = optval
        else
            res = defval
        end if
    end function
    
    function default_or_optional_int8(defval, optval) result (res)
        integer(int8), intent(in) ::  defval
        integer(int8), intent(in), optional :: optval
        integer(int8) :: res
        if (present(optval)) then
            res = optval
        else
            res = defval
        end if
    end function
    
    function default_or_optional_int16(defval, optval) result (res)
        integer(int16), intent(in) ::  defval
        integer(int16), intent(in), optional :: optval
        integer(int16) :: res
        if (present(optval)) then
            res = optval
        else
            res = defval
        end if
    end function
    
    function default_or_optional_int32(defval, optval) result (res)
        integer(int32), intent(in) ::  defval
        integer(int32), intent(in), optional :: optval
        integer(int32) :: res
        if (present(optval)) then
            res = optval
        else
            res = defval
        end if
    end function
    
    function default_or_optional_int64(defval, optval) result (res)
        integer(int64), intent(in) ::  defval
        integer(int64), intent(in), optional :: optval
        integer(int64) :: res
        if (present(optval)) then
            res = optval
        else
            res = defval
        end if
    end function

    function default_or_optional_real32(defval, optval) result (res)
        real(real32), intent(in) ::  defval
        real(real32), intent(in), optional :: optval
        real(real32) :: res
        if (present(optval)) then
            res = optval
        else
            res = defval
        end if
    end function

    function default_or_optional_real64(defval, optval) result (res)
        real(real64), intent(in) ::  defval
        real(real64), intent(in), optional :: optval
        real(real64) :: res
        if (present(optval)) then
            res = optval
        else
            res = defval
        end if
    end function

    function default_or_optional_real128(defval, optval) result (res)
        real(real128), intent(in) ::  defval
        real(real128), intent(in), optional :: optval
        real(real128) :: res
        if (present(optval)) then
            res = optval
        else
            res = defval
        end if
    end function

    subroutine set_optional_int8(val, opt)
        integer(int8), intent(in) :: val
        integer(int8), intent(inout), optional :: opt
        if (present(opt)) then
            opt = val
        end if
    end subroutine
    
    subroutine set_optional_int16(val, opt)
        integer(int16), intent(in) :: val
        integer(int16), intent(inout), optional :: opt
        if (present(opt)) then
            opt = val
        end if
    end subroutine

    subroutine set_optional_int32(val, opt)
        integer(int32), intent(in) :: val
        integer(int32), intent(inout), optional :: opt
        if (present(opt)) then
            opt = val
        end if
    end subroutine
    
    subroutine set_optional_int64(val, opt)
        integer(int64), intent(in) :: val
        integer(int64), intent(inout), optional :: opt
        if (present(opt)) then
            opt = val
        end if
    end subroutine

    subroutine set_optional_real32(val, opt)
        real(real32), intent(in) :: val
        real(real32), intent(inout), optional :: opt
        if (present(opt)) then
            opt = val
        end if
    end subroutine
    
    subroutine set_optional_real64(val, opt)
        real(real64), intent(in) :: val
        real(real64), intent(inout), optional :: opt
        if (present(opt)) then
            opt = val
        end if
    end subroutine

    subroutine set_optional_real128(val, opt)
        real(real128), intent(in) :: val
        real(real128), intent(inout), optional :: opt
        if (present(opt)) then
            opt = val
        end if
    end subroutine

end module