program test_benchmark
    use parser_module
    implicit none
    real, parameter :: pi = 4*atan(1.)

    real :: t1, t0, ans
    integer :: file_size
    character(:), allocatable :: filename, buffer

    type(parser_t) :: p
    type(token_t) :: ret

    call p % register_function(["sin ", "sqrt"])
    call p % register_operator(["+","-","*","/","="])
    call p % register_operator(["^"], is_right_assoc=.true.)
    call p % ignore_tokens([" ", "&", new_line(' ')])

    p % on_operator => on_operator
    p % on_function => on_function
    p % on_operand  => on_operand

    filename = "include/big-simple-expression.inc"

    print '(a)', "test_benchmark"

    open(unit=404, file=filename, form='unformatted', access='stream', status='old')
    inquire(file=filename, size=file_size)
    allocate(character(file_size) :: buffer)
    read(404) buffer

    print '("Loaded file ", a, " with size ", g0)', filename, file_size

    call cpu_time(t0)
    ret = p % parse(trim(buffer))
    call cpu_time(t1)
    print '("Time spent to parse file ", g0)', t1 - t0
    select type(ret => ret % object)
    type is (real)
        print '("Value returned: ", g0)', ret
    end select

    call cpu_time(t0)
    include 'big-simple-expression.inc'
    call cpu_time(t1)
    print '("Time spent to parse file ", g0)', t1 - t0
    print '("Value returned: ", g0)', ans

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
                case('='); ans % object = rhs
                end select
            end select
        end select
    end function
end program
