program main
    use parser_module
    use parser_listitem
    implicit none
    real, parameter :: pi = 4*atan(1.)

    real :: t1, t0, ans
    integer :: file_size
    character(:), allocatable :: buffer

    type(Parser) :: p
    class(*), allocatable :: ret

    call p % register_function(["sin ", "sqrt"])
    call p % register_operator(["+","-","*","/"])
    call p % register_operator(["^"], is_right_assoc=.true.)
    call p % ignore_tokens([" ", "&", new_line(' ')])

    p % on_operator => on_operator
    p % on_function => on_function
    p % on_operand  => on_operand


    open(unit=404, file="include/big-simple-expression.inc", &
        form='unformatted', access='stream', status='old')
    inquire(file="include/big-simple-expression.inc",size=file_size)
    print*, file_size
    allocate(character(file_size) :: buffer)
    read(404) buffer

    call cpu_time(t0)
    ret = p % parse(buffer(len("ans = & "):))
    call cpu_time(t1)
    print*, "Time spent ", t1 - t0
    select type(ret)
    type is (real)
        print*, ret
    end select

    call cpu_time(t0)
    include 'big-simple-expression.inc'
    call cpu_time(t1)
    print*, "Time spent ", t1 - t0
    print*, ans

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
