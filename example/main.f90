program main
    use parser_module
    implicit none
    type(parser_t) :: p
    type(token_t) :: ans
    real, parameter :: PI = 4*atan(1.)

    call p % register_function(["sin ", "sqrt"])
    call p % register_operator(["+","-","*","/"])
    call p % register_operator(["^"], is_right_assoc=.true.)

    p % on_operator => on_operator
    p % on_function => on_function
    p % on_operand  => on_operand

    !print*, sqrt(2.0)*sin(pi/4.0) + 3.0 + 4.0 * 2.0 / ( 1.0 - 5.0 ) ** 2.0 ** 3.0
    print*, sqrt(2.0)*sin(pi/4.0) + 3.0 + 4.0 * 2.0 / ( 1.0 - 5.0 ) ** 2 ** 3
    ans = p % parse("sqrt(2)*sin(pi/4) + 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3")
    select type(ans => ans % object)
    type is (real)
        print*, ans
    end select

contains

    function on_operand(self, opr) result(ans)
        class(parser_t) :: self
        type(token_t) :: opr
        type(token_t) :: ans

        select case(opr % string)
        case('pi'); ans % object = PI
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
