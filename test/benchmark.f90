program test_benchmark
    use parser_module
    implicit none
    real, parameter :: pi = 4*atan(1.)

    real :: t1, t0, fortran_time, parser_time, ans
    integer :: file_size
    character(:), allocatable :: filename, buffer

    type(parser_t) :: p
    type(token_t) :: ret

    call p % register_function(["sin ","sqrt"])
    call p % register_operator(["+   ","-   ",&
                                "*   ","/   ","=   "])
    call p % register_operator(["**  "], is_right_assoc=.true.)
    call p % ignore_tokens([new_line(' '), &
                            "&", &
                            " "])

    p % on_unary => on_unary
    p % on_binary => on_binary
    p % on_operand => on_operand

    call cpu_time(t0)
    include 'big-simple-expression.inc'
    call cpu_time(t1)
    fortran_time = t1 - t0

    filename = "include/big-simple-expression.inc"

    open(unit=404, file=filename, form='unformatted', access='stream', status='old')
    inquire(file=filename, size=file_size)
    allocate(character(file_size) :: buffer)
    read(404) buffer

    print '("Loaded file ", a, " with size ", g0)', filename, file_size

    call cpu_time(t0)
    ret = p % parse(trim(buffer))
    call cpu_time(t1)
    parser_time = t1 - t0


    select type(ret => ret % object)
    type is (real)
        print '("              ", g20.10, " | ", g20.10)','native fortran','parser'
        print '("elapsed time: ", g20.10, " | ", g20.10)', fortran_time, parser_time
        print '("returned:     ", g20.10, " | ", g20.10)', ans,          ret
        if (ans /= ret) error stop
    class default
        error stop
    end select

contains

    function on_operand(self, opr) result(ans)
        class(parser_t) :: self
        type(token_t) :: opr
        type(token_t) :: ans

        select case(opr % string)
        case('pi');  ans % object = pi
        case('ans'); ans % object = "ans" 
        case default
        block
            integer :: info
            real :: value
            read(opr % string,*,iostat=info) value
            if (info == 0) ans % object = value
            if (info /= 0) error stop "Unknown operand: " // opr % string
        end block
        end select
    end function

    function on_unary(self, opr, arg) result(ans)
        class(parser_t) :: self
        type(token_t) :: opr
        type(token_t) :: arg
        type(token_t) :: ans

        select type(arg => arg % object)
        type is (real)
            select case(opr % string)
            case('sqrt'); ans % object = sqrt(arg)
            case('sin');  ans % object = sin(arg)
            end select
        end select

    end function

    function on_binary(self, lhs, opr, rhs) result(ans)
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
                case('+');  ans % object = lhs+rhs
                case('-');  ans % object = lhs-rhs
                case('*');  ans % object = lhs*rhs
                case('/');  ans % object = lhs/rhs
                case('**'); ans % object = lhs**rhs
                end select
            end select
        type is (character(*))
            select type(rhs => rhs % object)
            type is (real)
                select case(opr % string)
                case('='); ans % object = rhs
                end select
            end select
        end select
    end function
end program
