module derived_type_and_intrinsics

    type :: aritimetic_t
        integer :: numerator
        integer :: denominator
        real, allocatable :: pure_evil(:)
    contains
        procedure, pass(rhs) :: multiply
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
        class(*),            intent(in) :: lhs
        class(aritimetic_t), intent(in) :: rhs
        type(aritimetic_t)              :: ans
        select type(lhs)
        type is (real)
            ans % pure_evil   = [ lhs, rhs % pure_evil ]
            ans % numerator   = lhs * rhs % numerator
            ans % denominator = rhs % denominator
        type is (aritimetic_t)
            ans % pure_evil   = [ lhs % pure_evil, rhs % pure_evil ]
            ans % numerator   = lhs % numerator   * rhs % numerator
            ans % denominator = lhs % denominator * rhs % denominator
        class default
            error stop "multiply: something else"
        end select
    end function

    function division(lhs, rhs) result(ans)
        class(aritimetic_t), intent(in) :: lhs
        type(aritimetic_t),  intent(in) :: rhs
        type(aritimetic_t)              :: ans

        ans % pure_evil   = [ lhs % pure_evil, rhs % pure_evil ]
        ans % numerator   = lhs % numerator   * rhs % denominator
        ans % denominator = lhs % denominator * rhs % numerator
    end function

    function addition(lhs, rhs) result(ans)
        class(aritimetic_t), intent(in) :: lhs
        type(aritimetic_t),  intent(in) :: rhs
        type(aritimetic_t)              :: ans

        ans % pure_evil   = [ lhs % pure_evil, rhs % pure_evil ]
        ans % numerator   = lhs % numerator   * rhs % denominator &
                          + lhs % denominator * rhs % numerator
        ans % denominator = lhs % denominator * rhs % denominator
    end function

    function subtract(lhs, rhs) result(ans)
        class(aritimetic_t), intent(in) :: lhs
        type(aritimetic_t),  intent(in) :: rhs
        type(aritimetic_t)              :: ans

        ans % pure_evil   = [ lhs % pure_evil, rhs % pure_evil ]
        ans % numerator   = lhs % numerator   * rhs % denominator &
                          - lhs % denominator * rhs % numerator
        ans % denominator = lhs % denominator * rhs % denominator
    end function

end module

program test_derived_type_and_intrinsics
    use parser_module
    use derived_type_and_intrinsics
    use iso_fortran_env
    implicit none

    type(Parser) :: p
    class(*), allocatable :: ret

    call p % register_operator(["+","-","*","/"])

    p % on_operator => on_operator
    p % on_function => on_function
    p % on_operand  => on_operand

    print '(a)', "test_derived_type_and_intrinsics"

    print '(a)', "5/3*4.0+3/2"
    ret = p % parse("5/3*4.0+3/2")
    select type(ret)
    type is (aritimetic_t)
        print '("=", g0, "/", g0)', ret % numerator, ret % denominator
        print *, ret % pure_evil
        print*, real(ret % numerator,   kind=REAL64) &
              / real(ret % denominator, kind=REAL64)
        print*, 5.0d0/3.0d0*4.0d0+3.0d0/2.0d0
    end select

    print '(a)', "57/311*433+39/27"
    ret = p % parse("57/311*433+39/27")
    select type(ret)
    type is (aritimetic_t)
        print '("=", g0, "/", g0)', ret % numerator, ret % denominator
        print *, ret % pure_evil
        print*, real(ret % numerator,   kind=REAL64) &
              / real(ret % denominator, kind=REAL64)
        print*, 57.d0/311.d0*433.d0+39.d0/27.d0
    end select

    print '(a)', "2/3*4+3/2"
    ret = p % parse("2/3*4+3/2")
    select type(ret)
    type is (aritimetic_t)
        print '("=", g0, "/", g0)', ret % numerator, ret % denominator
        print *, ret % pure_evil
        print*, real(ret % numerator,   kind=REAL64) &
              / real(ret % denominator, kind=REAL64)
        print*, 5.0d0/3.0d0*4.0d0+3.0d0/2.0d0
    end select

contains

    function on_operand(self, opr) result(ans)
        class(Parser) :: self
        class(*) :: opr
        class(*), allocatable :: ans

        select type(opr)
        type is (character(*))
            if (scan(opr,'.') > 0) then
                block
                integer :: info
                real(REAL64) :: value
                type(aritimetic_t) :: fract
                read(opr,*,iostat=info) value
                if (info == 0) allocate(ans, source=value)
                end block
            else
                block
                integer :: info
                integer(INT64) :: value
                type(aritimetic_t) :: fract

                read(opr,*,iostat=info) value

                fract = aritimetic_t(value, 1, [ value ])
                if (info == 0) allocate(ans, source=fract)
                end block
            end if
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
        type(aritimetic_t) :: lhs_val, rhs_val

        select type(lhs)
        type is (aritimetic_t)
            lhs_val = lhs
        end select

        select type(rhs)
        type is (aritimetic_t)
            rhs_val = rhs
        end select

        select type(opr)
        type is (character(*))
            select case(opr)
            case('+'); allocate(ans, source=lhs_val+rhs_val)
            case('-'); allocate(ans, source=lhs_val-rhs_val)
            case('*'); allocate(ans, source=lhs_val*rhs_val)
            case('/'); allocate(ans, source=lhs_val/rhs_val)
            end select
        end select
    end function

end program
