program main
    use iso_fortran_env
    use parser_module
    implicit none
    integer, parameter :: BUFFER_MAX = 255
    real, parameter :: pi = 4*atan(1.)

    real :: t1, t0, ans
    integer :: stat
    character(BUFFER_MAX) :: line_buffer

    type(parser_t) :: p
    type(token_t) :: ret

    call p % register_function(["sin ", "sqrt"])
    call p % register_operator(["+","-","*","/"])
    call p % register_operator(["^"], is_right_assoc=.true.)
    call p % ignore_tokens([" ", "&", new_line(' ')])

    p % on_operator => on_operator
    p % on_function => on_function
    p % on_operand  => on_operand

    write(*,'(1000(" "))')
    write(*,'("FORTRAN BC - Example of program using the expression parser")')
    write(*,'("Some invalid syntax may freeze your terminal (we are not checking)")')
    write(*,'("Type some basic expr. like: 1/2")')

    do
        write(output_unit,'("fortran-bc> ")',advance='no')
        read(input_unit,'(a)',iostat=stat) line_buffer
        if (stat < 0) exit
        if (len_trim(line_buffer) > 0) then
            call cpu_time(t0)
            ret = p % parse(line_buffer)
            call cpu_time(t1)

            print*, "Time spent ", t1 - t0
            select type(ret => ret % object)
            type is (real)
                print*, ret
            end select
        end if
    end do

contains

    function on_operand(self, opr) result(ans)
        class(parser_t) :: self
        type(token_t) :: opr
        type(token_t) :: ans

        select case(opr % string)
        case('pi'); ans % object = 4*atan(1.)
        case default
        block
            integer :: info
            real :: value
            read(opr % string,*,iostat=info) value
            if (info == 0) ans % object = value
        end block
        end select
    end function

    function on_function(self, fun, arg) result(ans)
        class(parser_t) :: self
        type(token_t) :: fun
        type(token_t) :: arg
        type(token_t) :: ans

        select type(arg => arg % object)
        type is (real)
            select case(fun % string)
            case('sqrt'); ans % object = sqrt(arg)
            case('sin');  ans % object = sin(arg)
            end select
        end select

    end function

    function on_operator(self, lhs, opr, rhs) result(ans)
        class(parser_t) :: self
        type(token_t) :: opr
        type(token_t) :: lhs
        type(token_t) :: rhs
        type(token_t) :: ans

        select type(lhs => lhs % object)
        type is (real)
            select type(rhs => rhs % object)
            type is (real)
                select case(opr % string)
                case('+'); ans % object = lhs+rhs
                case('-'); ans % object = lhs-rhs
                case('*'); ans % object = lhs*rhs
                case('/'); ans % object = lhs/rhs
                case('^'); ans % object = lhs**rhs
                end select
            end select
        end select
    end function
end program
