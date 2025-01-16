! Include this file together with
! adding 'use string_m' 
! This command loads passed arguments in NAMELIST format
! NAMELIST name is passed as ARG argument
! IOS is the status of parsing: non-zero means an error
#define _GET_CMD_ARGS_(ARG, IOS) \
    block; \
    character(len=cmd_arg_max_length) :: cmd__args__string; \
    call get_all_cmd_arg(cmd__args__string); \
    cmd__args__string = '&' // #ARG // trim(cmd__args__string) // ' /'; \
    read (cmd__args__string, nml=ARG, iostat = IOS); \
    end block
