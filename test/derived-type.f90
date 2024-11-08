module derived_type
    use iso_fortran_env
    integer, parameter :: wp = REAL64

    type :: fraction_t
        integer :: numerator
        integer :: denominator
    contains
        procedure :: as_decimal
        procedure :: multiply
        procedure :: subtract
        procedure :: addition
        procedure :: division
        procedure :: write
        generic :: operator(+) => addition
        generic :: operator(-) => subtract
        generic :: operator(*) => multiply
        generic :: operator(/) => division
        generic :: write(formatted) => write
    end type

contains

    real(wp) pure function as_decimal(self)
        class(fraction_t), intent(in) :: self
        as_decimal = real(self % numerator,wp) &
                   / real(self % denominator,wp)
    end function

    function multiply(lhs, rhs) result(ans)
        class(fraction_t), intent(in) :: lhs
        type(fraction_t),  intent(in) :: rhs
        type(fraction_t)              :: ans
        ans % numerator   = lhs % numerator   * rhs % numerator
        ans % denominator = lhs % denominator * rhs % denominator
    end function

    function division(lhs, rhs) result(ans)
        class(fraction_t), intent(in) :: lhs
        type(fraction_t),  intent(in) :: rhs
        type(fraction_t)              :: ans

        ans % numerator   = lhs % numerator   * rhs % denominator
        ans % denominator = lhs % denominator * rhs % numerator
    end function

    function addition(lhs, rhs) result(ans)
        class(fraction_t), intent(in) :: lhs
        type(fraction_t),  intent(in) :: rhs
        type(fraction_t)              :: ans

        ans % numerator   = lhs % numerator   * rhs % denominator &
                          + lhs % denominator * rhs % numerator
        ans % denominator = lhs % denominator * rhs % denominator
    end function

    function subtract(lhs, rhs) result(ans)
        class(fraction_t), intent(in) :: lhs
        type(fraction_t),  intent(in) :: rhs
        type(fraction_t)              :: ans

        ans % numerator   = lhs % numerator   * rhs % denominator &
                          - lhs % denominator * rhs % numerator
        ans % denominator = lhs % denominator * rhs % denominator
    end function

    subroutine write(self, unit, iotype, v_list, iostat, iomsg)
        class(fraction_t), intent(in)    :: self
        integer,           intent(in)    :: unit
        character(*),      intent(in)    :: iotype
        integer,           intent(in)    :: v_list(:)
        integer,           intent(out)   :: iostat
        character(*),      intent(inout) :: iomsg
        write(unit,'(g0,"/",g0)',iostat=iostat) &
           self % numerator, &
           self % denominator
    end subroutine

end module

program test_derived_type
    use parser_module
    use derived_type
    implicit none

    type(parser_t) :: p

    call p % register_unary(["+","-"])
    call p % register_binary(["+","-","*","/"])

    p % on_unary    => on_unary
    p % on_binary   => on_binary
    p % on_operand  => on_operand

    call test("-(-2)*(-2)",        - (-2.0_wp) * (-2.0_wp) )
    call test("--(-2)*(-2)",       - - (-2.0_wp) * (-2.0_wp) )
    call test("--(-2)/(-3)",       - - (-2.0_wp) / (-3.0_wp) )
    call test("--(-2/-3)",         - - (-2.0_wp / -3.0_wp) )
    call test("-+(-2/-3)",         - + (-2.0_wp / -3.0_wp) )
    call test("+5/3*4+3/2",        5.0_wp/3.0_wp*4.0_wp+3.0_wp/2.0_wp)
    call test("57/311*433+39/27", 57.0_wp/311.0_wp*433.0_wp+39.0_wp/27.0_wp)
    call test("2/3*4+3/2",         2.0_wp/3.0_wp*4.0_wp+3.0_wp/2.0_wp)

contains

    subroutine assert_equal(expected, value)
        real(wp), parameter :: eps = epsilon(1.0_wp) * 10
        real(wp) :: value
        real(wp) :: expected
        if (abs(value - expected) > eps) then
           write(error_unit,'("Expected ",g0," but found ",g0)') expected, value
           error stop
        end if
    end subroutine

    subroutine test(string, value)
        character(*) :: string
        real(wp) :: value
        type(token_t), allocatable :: ret

        ret = p % parse(string)
        select type(ret => ret % object)
        type is (fraction_t)
            print '(A20," = ",sp,g0," = ",DT)', string, ret % as_decimal(), ret
            call assert_equal(value, ret % as_decimal())
        end select
    end subroutine

    function on_operand(self, opr) result(ans)
        class(parser_t) :: self
        type(token_t) :: opr
        type(token_t) :: ans

        integer :: info
        integer :: value
        type(fraction_t) :: fract

        read(opr % string,*,iostat=info) value

        if (info == 0) ans % object = fraction_t(value,1)
    end function

    function on_unary(self, opr, arg) result(ans)
        class(parser_t) :: self
        type(token_t) :: opr 
        type(token_t) :: arg
        type(token_t) :: ans

        select type(arg => arg % object)
        type is (real)
            select case(opr % string)
            case('-');    ans % object = -(arg)
            case('+');    ans % object = +(arg)
            end select
        type is (fraction_t)
            select case(opr % string)
            case('-');    ans % object = fraction_t(-1,1) * (arg)
            case('+');    ans % object = fraction_t(+1,1) * (arg)
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
        type is (fraction_t)
            select type(rhs => rhs % object)
            type is (fraction_t)
                select case(opr % string)
                case('+'); ans % object = lhs+rhs
                case('-'); ans % object = lhs-rhs
                case('*'); ans % object = lhs*rhs
                case('/'); ans % object = lhs/rhs
                end select
            end select
        end select
    end function

end program
