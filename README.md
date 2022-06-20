# shunting-yard-fortran
Small Expression Parser Using Shunting Yard Algorithm.

## Getting Started

First get the code, by cloning the repo:

```sh
git clone https://github.com/14NGiestas/shunting-yard-fortran.git
cd shunting-yard-fortran 
```

## FPM

This project was designed to be built using the [Fortran Package Manager](https://github.com/fortran-lang/fpm).
Follow the directions on that page to install FPM if you haven't already.

To build, type:

```sh
fpm build
```

## Usage

In the example program provided in [`example/main.f90`](https://github.com/14NGiestas/shunting-yard-fortran/blob/main/example/main.f90) you will find the basic usage of this package.

The user of this package should tell the parser what is each token, by using the routines `register_*` 

```fortran
call p % register_function(["sin ", "sqrt"])
call p % register_operator(["+","-","*","/"])
call p % register_operator(["^"], is_right_assoc=.true.)
```

By default, spaces and newlines will be ignored, if you want some other character to be ignored you can use the `ignore_tokens` routine

```fortran
! This will ignore this characters
call p % ignore_tokens([" ", "&", new_line(' ')])
```

Then, you will need to bind the `on_*` functions that will be called to make sense of the tokens

```fortran
p % on_operator => on_operator
p % on_function => on_function
p % on_operand  => on_operand
```

Such functions must respect their related interfaces
```fortran
abstract interface
    function interface_on_operator(self, lhs, opr, rhs) result(ans)
        import :: Parser
        class(Parser)  :: self
        class(*) :: lhs
        class(*) :: opr
        class(*) :: rhs
        class(*), allocatable :: ans
    end function

    function interface_on_operand(self, opr) result(ans)
        import :: Parser
        class(Parser)  :: self
        class(*) :: opr
        class(*), allocatable :: ans
    end function

    function interface_on_function(self, fun, arg) result(ans)
        import :: Parser
        class(Parser)  :: self
        class(*) :: fun
        class(*) :: arg
        class(*), allocatable :: ans
    end function
end interface
```

For example if you want to make sense of "pi" in a expression you must implement the following function.

```fortran
function on_operand(self, opr) result(ans)
    class(Parser) :: self
    class(*) :: opr
    class(*), allocatable :: ans

    select type(opr)
    type is (character(*))
        select case(opr)
        case('pi')
            allocate(ans, source=4*atan(1.))
        end select
    end select
end function
```
