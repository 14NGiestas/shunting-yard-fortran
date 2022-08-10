module derived_type

    type :: aritimetic_t
        integer :: numerator
        integer :: denominator
    contains
        procedure :: multiply
        procedure :: subtract
        procedure :: addition
        procedure :: division
        generic :: operator(+) => addition
        generic :: operator(-) => subtract
        generic :: operator(*) => multiply
        generic :: operator(/) => division
    end type

contains

    function multiply(lhs, rhs) result(ans)
        class(aritimetic_t), intent(in) :: lhs
        type(aritimetic_t),  intent(in) :: rhs
        type(aritimetic_t)              :: ans
        ans % numerator   = lhs % numerator   * rhs % numerator
        ans % denominator = lhs % denominator * rhs % denominator
    end function

    function division(lhs, rhs) result(ans)
        class(aritimetic_t), intent(in) :: lhs
        type(aritimetic_t),  intent(in) :: rhs
        type(aritimetic_t)              :: ans

        ans % numerator   = lhs % numerator   * rhs % denominator
        ans % denominator = lhs % denominator * rhs % numerator
    end function

    function addition(lhs, rhs) result(ans)
        class(aritimetic_t), intent(in) :: lhs
        type(aritimetic_t),  intent(in) :: rhs
        type(aritimetic_t)              :: ans

        ans % numerator   = lhs % numerator   * rhs % denominator &
                          + lhs % denominator * rhs % numerator
        ans % denominator = lhs % denominator * rhs % denominator
    end function

    function subtract(lhs, rhs) result(ans)
        class(aritimetic_t), intent(in) :: lhs
        type(aritimetic_t),  intent(in) :: rhs
        type(aritimetic_t)              :: ans

        ans % numerator   = lhs % numerator   * rhs % denominator &
                          - lhs % denominator * rhs % numerator
        ans % denominator = lhs % denominator * rhs % denominator
    end function

end module

program test_derived_type
    use parser_module
    use derived_type
    use iso_fortran_env
    implicit none

    type(parser_t) :: p
    type(token_t), allocatable :: ret

    call p % register_operator(["+","-","*","/"])

    p % on_operator => on_operator
    p % on_function => on_function
    p % on_operand  => on_operand

    print*, "5/3*4+3/2"
    ret = p % parse("5/3*4+3/2")
    select type(ret => ret % object)
    type is (aritimetic_t)
        print*, ret
        print*, real(ret % numerator,   kind=REAL64) &
              / real(ret % denominator, kind=REAL64)
        print*, 5.0d0/3.0d0*4.0d0+3.0d0/2.0d0
    end select

    print*, "57/311*433+39/27"
    ret = p % parse("57/311*433+39/27")
    select type(ret => ret % object)
    type is (aritimetic_t)
        print*, ret
        print*, real(ret % numerator,   kind=REAL64) &
              / real(ret % denominator, kind=REAL64)
        print*, 57.d0/311.d0*433.d0+39.d0/27.d0
    end select

    print*, "2/3*4+3/2"
    ret = p % parse("2/3*4+3/2")
    select type(ret => ret % object)
    type is (aritimetic_t)
        print*, ret
        print*, real(ret % numerator,   kind=REAL64) &
              / real(ret % denominator, kind=REAL64)
        print*, 2.0d0/3.0d0*4.0d0+3.0d0/2.0d0
    end select

contains

    function on_operand(self, opr) result(ans)
        class(parser_t) :: self
        type(token_t) :: opr
        type(token_t) :: ans

        integer :: info
        integer :: value
        type(aritimetic_t) :: fract

        read(opr % string,*,iostat=info) value

        if (info == 0) ans % object = aritimetic_t(value,1)
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
        type is (aritimetic_t)
            select type(rhs => rhs % object)
            type is (aritimetic_t)
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
