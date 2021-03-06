program main
    use iso_fortran_env
    use parser_module
    use parser_listitem
    implicit none
    integer, parameter :: BUFFER_MAX = 255
    real, parameter :: pi = 4*atan(1.)

    real :: t1, t0, ans
    integer :: stat
    character(BUFFER_MAX) :: line_buffer

    type(Parser) :: p
    class(*), allocatable :: ret

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
            select type(ret)
            type is (real)
                print*, ret
            end select
        end if
    end do

contains

    function on_operand(self, opr) result(ans)
        class(Parser) :: self
        class(*) :: opr
        class(*), allocatable :: ans

        select type(opr)
        type is (character(*))
            select case(opr)
            case('pi'); allocate(ans, source=4*atan(1.))
            case default; block
                integer :: info
                real :: value
                read(opr,*,iostat=info) value
                if (info == 0) allocate(ans, source=value)
            end block; end select
        end select
    end function

    function on_function(self, fun, arg) result(ans)
        class(Parser) :: self
        class(*) :: fun
        class(*) :: arg
        class(*), allocatable :: ans

        select type(fun)
        type is (character(*))
            select type(arg)
            type is (real)
                select case(fun)
                case('sqrt'); allocate(ans, source=sqrt(arg))
                case('sin');  allocate(ans, source=sin(arg))
                end select
            end select
        end select

    end function

    function on_operator(self, lhs, opr, rhs) result(ans)
        class(Parser) :: self
        class(*) :: opr
        class(*) :: lhs
        class(*) :: rhs
        class(*), allocatable :: ans
        real :: lhs_val, rhs_val

        select type(lhs)
        type is (real)
            lhs_val = lhs
        end select

        select type(rhs)
        type is (real)
            rhs_val = rhs
        end select

        select type(opr)
        type is (character(*))
            select case(opr)
            case('+'); allocate(ans, source=lhs_val+rhs_val)
            case('-'); allocate(ans, source=lhs_val-rhs_val)
            case('*'); allocate(ans, source=lhs_val*rhs_val)
            case('/'); allocate(ans, source=lhs_val/rhs_val)
            case('^'); allocate(ans, source=lhs_val**rhs_val)
            end select
        end select
    end function
end program
