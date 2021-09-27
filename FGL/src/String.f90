module string_m
    
    use iso_c_binding
    
    implicit none
    
    character(len=26), parameter, dimension(2), private :: alph = &
        &   ['ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz']
    
    integer, parameter :: cmd_arg_max_length = 1024
    
    interface
        function strlen(s) bind (c, name = 'strlen')
            use iso_c_binding, only: c_ptr, c_size_t
            type(c_ptr), intent(in), value :: s
            integer(c_size_t) :: strlen
        end function
    end interface

contains

    subroutine to_lower(s)
        character(len=*), intent(inout) :: s
        integer :: ia, ic, ialph
        ! Use conversion to ASCII character since they are
        ! one after another
        ia = iachar('A')
        do ic=1,len(s)
            select case (s(ic:ic))
            case ('A':'Z')
                ialph = iachar(s(ic:ic)) - ia + 1
                s(ic:ic) = alph(2)(ialph:ialph)
            end select
        end do
    end subroutine

    subroutine to_upper(s)
        character(len=*), intent(inout) :: s
        integer :: ia, ic, ialph
        ia = iachar('a')
        do ic=1,len(s)
            select case (s(ic:ic))
            case ('a':'z')
                ialph = iachar(s(ic:ic)) - ia + 1
                s(ic:ic) = alph(1)(ialph:ialph)
            end select
        end do
    end subroutine
    
    subroutine get_all_cmd_arg(string)
        character(len=*), intent(out) :: string
        character(len=cmd_arg_max_length*2) :: full_command
        character(len=cmd_arg_max_length) :: cmd_name
        integer :: full_cmd_len, cmd_len
        call get_command(full_command, full_cmd_len)
        call get_command_argument(0, cmd_name, cmd_len)
        ! This can happen if command name was escaped with ""
        if (full_command(1:1) /= cmd_name(1:1)) then
            if (full_command(1:1) == '"' .and. &
                &   full_command(cmd_len+2:cmd_len+2) == '"') then
                ! Adjust command name length to include ""
                cmd_len = cmd_len + 2
            else
                write (*,*) 'Parsing command line arguments: unknown cmd format'
                stop
            end if
        end if
        string = full_command(cmd_len+1:full_cmd_len)     
    end subroutine
    
    ! A utility function: from C get array of characters
    ! Need to convert them to Fortran string
    subroutine char_array_to_string(arr, str)
        character(len=1), dimension(:) :: arr
        character(len=*) :: str
        integer :: n, i
        n = min(size(arr), len(str))
        do i = 1, n
            str(i:i) = arr(i)
        end do
    end subroutine
    
    ! Helpers to deal with Fortran & C strings
    function associate_c_string(fstring, addnull) result(s)
        character(len=*), intent(inout) :: fstring
        logical, intent(in), optional :: addnull
        type(c_ptr) :: s
        integer :: n
        n = len_trim(fstring)
        if (n < len(fstring) .and. present(addnull)) then
            if (addnull) fstring(n+1:n+1) = c_null_char
        end if
        s = c_loc(fstring)
    end function
    
    subroutine rm_null_from_fstring(fstring)
        character(len=*), intent(inout) :: fstring
        integer :: n, i
        n = len(fstring)
        i = 1
        do
            if (i > n) exit
            if (fstring(i:i) == c_null_char) then
                fstring(i:i) = ' '
                i = i + 1
                exit
            end if
            i = i + 1
        end do
        do i = i, n
            fstring(i:i) = ' '
        end do
    end subroutine
    
    
    
end module