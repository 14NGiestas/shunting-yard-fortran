program main
    use parser_module
    implicit none
    type(Parser) :: p

    call p % register_function(["sin ", "sqrt"])
    call p % register_operator(["+","-","*","/"])
    call p % register_operator(["^"], is_right_assoc=.true.)

    p % on_operator => on_operator
    p % on_function => on_function
    p % on_operand  => on_operand

    call p % parse("sqrt(2)*sin(pi/4) + 3 + 4 * 2 / ( 1 - 5 ) ^ 2 ^ 3")

contains

    function on_operand(self, lhs, opr, rhs) result(ans)
        class(Parser) :: self
        class(*) :: opr
        class(*), optional :: lhs
        class(*), optional :: rhs
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
        class default
            allocate(ans, source=opr)
        end select
    end function

    function on_function(self, lhs, opr, rhs) result(ans)
        class(Parser) :: self
        class(*) :: opr
        class(*), optional :: lhs
        class(*), optional :: rhs
        class(*), allocatable :: ans

        select type(opr)
        type is (character(*))
            select type(rhs)
            type is (real)
                select case(opr)
                case('sqrt'); ans = sqrt(rhs)
                case('sin');  ans = sin(rhs)
                end select
            end select
        end select
    end function

    function on_operator(self, lhs, opr, rhs) result(ans)
        class(Parser) :: self
        class(*) :: opr
        class(*), optional :: lhs
        class(*), optional :: rhs
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
