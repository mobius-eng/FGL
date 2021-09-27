module error_m
    
    implicit none
    private
    ! Classes of errors
    integer, parameter, public :: IOERROR_CLASS = 0
        !! Base value for I/O operation errors
    integer, parameter, public :: MEMERROR_CLASS = -100
        !! Base value for memory operation errors
    integer, parameter, public :: ARRAYERROR_CLASS = -200
        !! Base value for array operation errors
    integer, parameter, public :: ARGERROR_CLASS = -300
        !! Base value for general argument passing errors
    integer, parameter, public :: VALERROR_CLASS = -400
        !! Base value for incorrect value errors
    integer, parameter, public :: USERERROR_CLASS = -1000
        !! Base value for user-defined errors

    integer, parameter, public :: NO_ERROR = 0
    ! I/O Errors
    integer, parameter, public :: IO_UNSPECIFIED_ERROR = IOERROR_CLASS - 1
    integer, parameter, public :: IO_OPEN_ERROR = IOERROR_CLASS - 2
    integer, parameter, public :: IO_OUTPUT_ERROR = IOERROR_CLASS - 3
    integer, parameter, public :: IO_INPUT_ERROR = IOERROR_CLASS - 4
    integer, parameter, public :: IO_FILE_ACCESS_ERROR = IOERROR_CLASS - 5
    ! Memory access (?) and allocation
    integer, parameter, public :: MEM_UNSPECIFIED_ERROR = MEMERROR_CLASS - 1
    integer, parameter, public :: MEM_ALLOC_ERROR = MEMERROR_CLASS - 2
    integer, parameter, public :: MEM_ACCESS_ERROR = MEMERROR_CLASS - 3
    ! Array related errors
    integer, parameter, public :: ARRAY_UNSPECIFIED_ERROR = ARRAYERROR_CLASS - 1
    integer, parameter, public :: ARRAY_SIZE_ERROR = ARRAYERROR_CLASS - 2
    integer, parameter, public :: ARRAY_SIZE_MISMATCH_ERROR = ARRAYERROR_CLASS - 3
    ! Argument errors
    integer, parameter, public :: ARG_UNSPECIFIED_ERROR = ARGERROR_CLASS - 1
    integer, parameter, public :: ARG_NOT_PROVIDED_ERROR = ARGERROR_CLASS - 2
    integer, parameter, public :: ARG_TYPE_ERROR = ARGERROR_CLASS - 3
        !! This is relevant for `class` arguments.
    ! Value errors
    integer, parameter, public :: VAL_UNSPECIFIED_ERROR = VALERROR_CLASS - 1
    integer, parameter, public :: VAL_DOMAIN_ERROR = VALERROR_CLASS - 2
    ! Use USERERROR_CLASS value as general unspecified error
    integer, parameter, public :: UNSPECIFIED_ERROR = USERERROR_CLASS

    integer, parameter :: exception_message_max_length = 1024
    type, public :: exception
        integer :: error_code
        character(len = exception_message_max_length) :: message
    end type
    
    class(exception), pointer, private :: last_error => null()
    
    interface set_error
        module procedure set_error_exception, set_error_code_msg
    end interface
    
    public :: get_last_error_code, get_last_error_message, set_error, &
        &   release_error, get_last_error_object

contains

    function get_last_error_code() result (res)
        integer :: res
        if (associated(last_error)) then
            res = last_error%error_code
        else
            res = 0
        end if
    end function

    subroutine get_last_error_message(msg)
        character(len=*), intent(inout) :: msg
        if (associated(last_error)) then
            msg = trim(last_error%message)
        else
            msg = ''
        end if
    end subroutine

    subroutine set_error_exception(err)        
        class(exception), intent(in) :: err
        call release_error()
        allocate(last_error, source=err)
    end subroutine
    
    subroutine set_error_code_msg(code, msg)
        integer, intent(in) :: code
        character(len=*), intent(in) :: msg
        type(exception) err
        err = exception(code, msg)
        call release_error()
        allocate(last_error, source = err)
    end subroutine

    subroutine release_error()
        if (associated(last_error)) then
            deallocate(last_error)
        end if
        last_error => null()
    end subroutine
    
    subroutine get_last_error_object(errobj)
        class(exception), pointer, intent(inout) :: errobj
        errobj => last_error
    end subroutine
    
    
end module error_m