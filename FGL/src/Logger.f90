module logger_m
    
    use iso_fortran_env, only: error_unit, &
        &   int8, int16, int32, int64,     &
        &   real32, real64, real128
    use error_m
    use optional_m
    use string_m

    implicit none

    private

    integer :: log_output = error_unit
        !* Log output file. Defaults to error stream
  
    interface fmt
        !* Simple formatting interface
        module procedure :: fmt_real32, fmt_real64, fmt_real128, &
            &   fmt_int8, fmt_int16, fmt_int32, fmt_int64
    end interface fmt

    character(len=*), parameter :: fmt_str_format = '("(A,",A,",A)")'
    character(len=*), parameter :: set_log_output_err_msg = &
        &   'Error in setting logger output: cannot access file'

    public print_log, set_log_output, get_log_output, fmt

contains

    subroutine set_log_output(new_output, status)
        !* Change the file of log output. No argument: sets default.
        integer, intent(in), optional :: new_output
        integer, intent(out), optional :: status
        logical :: op
        character(len=7) :: writable
        call set_optional(NO_ERROR, opt=status)
        if (present(new_output)) then
            ! Check if there is a writable file associated with `new_output` unit
            inquire (unit=new_output, opened=op, write=writable)
            ! Lower the case
            call to_lower(writable)
            if (op .and. writable == 'yes') then
                log_output = new_output
                return
            end if
            ! Getting here only if invalid `new_output` was passed
            if (present(status)) then
                status = IO_FILE_ACCESS_ERROR
                call set_error(IO_FILE_ACCESS_ERROR, set_log_output_err_msg)
            else
                write (error_unit, *) set_log_output_err_msg
                write (error_unit, *) 'Giving up'
                stop
            end if
        else
            ! Re-setting default value
            log_output = error_unit
        end if
    end subroutine set_log_output

    function get_log_output()
        !* Returns file ID of the current output.
        integer get_log_output
        get_log_output = log_output
    end function


    function fmt_real32(pref, num, suff, num_format) result(s)
        !* Formats single precision real with prefix & suffix.
        character(len=*), intent(in) :: pref, suff
        character(len=*), intent(in), optional :: num_format
        real(real32), intent(in) :: num
        character(len=len_trim(pref)+len_trim(suff)+40) :: s
        character(len=30) :: full_format
        if (present(num_format)) then
            write(unit=full_format, fmt=fmt_str_format) num_format
            write(unit=s, fmt=full_format) pref, num, suff
        else
            write(unit=s, fmt='(A,G0.6,A)') pref, num, suff
        end if
    end function

    function fmt_real64(pref, num, suff, num_format) result(s)
        !* Formats double precision real with prefix & suffix.
        character(len=*), intent(in) :: pref, suff
        character(len=*), intent(in), optional :: num_format
        real(real64), intent(in) :: num
        character(len=len_trim(pref)+len_trim(suff)+40) :: s
        character(len=30) :: full_format
        if (present(num_format)) then
            write(unit=full_format, fmt=fmt_str_format) num_format
            write(unit=s, fmt=full_format) pref, num, suff
        else
            write(s, '(A,G0.15,A)') pref, num, suff
        end if
    end function
    
    function fmt_real128(pref, num, suff, num_format) result(s)
        !* Formats double precision real with prefix & suffix.
        character(len=*), intent(in) :: pref, suff
        character(len=*), intent(in), optional :: num_format
        real(real128), intent(in) :: num
        character(len=len_trim(pref)+len_trim(suff)+40) :: s
        character(len=30) :: full_format
        if (present(num_format)) then
            write(unit=full_format, fmt=fmt_str_format) num_format
            write(unit=s, fmt=full_format) pref, num, suff
        else
            write(s, '(A,G0.20,A)') pref, num, suff
        end if
    end function

    function fmt_int8(pref, num, suff, num_format) result(s)
        !* Formats default integer with prefix & suffix.
        character(len=*), intent(in) :: pref, suff
        character(len=*), intent(in), optional :: num_format
        integer(int8), intent(in) :: num
        character(len=len_trim(pref)+len_trim(suff)+40) :: s
        character(len=30) :: full_format
        if (present(num_format)) then
            write(unit=full_format, fmt=fmt_str_format) num_format
            write(unit=s, fmt=full_format) pref, num, suff
        else
            write(s, '(A,I5,A)') pref, num, suff
        end if
    end function
    
    function fmt_int16(pref, num, suff, num_format) result(s)
        !* Formats default integer with prefix & suffix.
        character(len=*), intent(in) :: pref, suff
        character(len=*), intent(in), optional :: num_format
        integer(int16), intent(in) :: num
        character(len=len_trim(pref)+len_trim(suff)+40) :: s
        character(len=30) :: full_format
        if (present(num_format)) then
            write(unit=full_format, fmt=fmt_str_format) num_format
            write(unit=s, fmt=full_format) pref, num, suff
        else
            write(s, '(A,I7,A)') pref, num, suff
        end if
    end function
    
    function fmt_int32(pref, num, suff, num_format) result(s)
        !* Formats default integer with prefix & suffix.
        character(len=*), intent(in) :: pref, suff
        character(len=*), intent(in), optional :: num_format
        integer(int32), intent(in) :: num
        character(len=len_trim(pref)+len_trim(suff)+40) :: s
        character(len=30) :: full_format
        if (present(num_format)) then
            write(unit=full_format, fmt=fmt_str_format) num_format
            write(unit=s, fmt=full_format) pref, num, suff
        else
            write(s, '(A,I12,A)') pref, num, suff
        end if
    end function

    function fmt_int64(pref, num, suff, num_format) result(s)
        !* Formats default integer with prefix & suffix.
        character(len=*), intent(in) :: pref, suff
        character(len=*), intent(in), optional :: num_format
        integer(int64), intent(in) :: num
        character(len=len_trim(pref)+len_trim(suff)+40) :: s
        character(len=30) :: full_format
        if (present(num_format)) then
            write(unit=full_format, fmt=fmt_str_format) num_format
            write(unit=s, fmt=full_format) pref, num, suff
        else
            write(s, '(A,I22,A)') pref, num, suff
        end if
    end function

    subroutine print_log(msg, prefix, timestamp)
        !* Outputs log message. The message is prefixed & optionally time stamped.
        character(len=*), intent(in) :: msg, prefix
        logical, intent(in), optional :: timestamp
        character(len=8) :: date
        character(len=10) :: time
        character(len=20) :: full_time
        if (default_or_optional(.false., timestamp)) then
            call date_and_time(date, time)
            write (full_time, '(" ", A,"/",A,"/",A," ",A,":",A,":",A)') &
                date(1:4), date(5:6), date(7:8), time(1:2), time(3:4), time(5:6)
        else
            full_time = ''
        end if
        write(log_output, '("[",A,A,"] ",A)') trim(prefix), trim(full_time), trim(msg)
    end subroutine

end module
