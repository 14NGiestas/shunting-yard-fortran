# shunting-yard-fortran
Small Expression Parser Using Shunting Yard Algorithm.

# About

This is just a toy project I started as a way to parse Derived Types (my hamiltonian model) from a file/string (and eventually I gave up as it didn't worked well - but I've published anyway), so the point here is not be super performant but flexible enough to allow you write your own parser with some ease, reusing operator overloaded structures that you may have already.

After a while (2022) I finally got it to work with Derived Types by reworking the API to use a `token_t` and `token_list` that I made a separate package for, [fortran-tokenizer](https://github.com/14NGiestas/fortran-tokenizer).

Although it works as a way to write parsers I do not recommend you using it as a dependency as there are a lot of design issues:

1. No Thread safety: I register valid tokens as a global singleton, so multiple parsers are not supported. 
2. Hard-coded precedence: the operator precedence is currently fixed for a few tokens (desynced with registered ones).
3. No unary support (no ambiguity): unary `-`/`+` operations must be done defining a function.
4. As it uses an array of strings all tokens must be padded with spaces so they are same-size.
5. The current app provides no way to check syntax errors 

That being said, I am using it anyway right now and the current public API feels ok, so I will be working on a proper package with a generic version based on this idea soon:tm:.

## Getting Started

First get the code, by cloning the repo:

```sh
git clone https://github.com/14NGiestas/shunting-yard-fortran.git
cd shunting-yard-fortran 
```

## FPM

This project was designed to be built using the [Fortran Package Manager](https://github.com/fortran-lang/fpm).
Follow the directions on that page to install FPM if you haven't already.

To build and run, type:

```sh
fpm run
```

to run the tests type

```sh
fpm test 
```

to run the example type
```
fpm run --example
```

## Usage

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
        import :: parser_t
        class(parser_t) :: self
        type(token_t) :: lhs
        type(token_t) :: opr
        type(token_t) :: rhs
        type(token_t) :: ans
    end function

    function interface_on_operand(self, opr) result(ans)
        import :: parser_t 
        class(parser_t) :: self
        type(token_t) :: opr
        type(token_t) :: ans
    end function

    function interface_on_function(self, fun, arg) result(ans)
        import :: parser_t 
        class(parser_t) :: self
        type(token_t) :: fun
        type(token_t) :: arg
        type(token_t) :: ans
    end function
end interface
```

For example if you want to make sense of "pi" in a expression you must implement the following function.

```fortran
function on_operand(self, opr) result(ans)
    class(parser_t) :: self
    type(token_t) :: opr
    type(token_t) :: ans

    ! the `% string` will get the string representation of a token
    select case(opr % string)
    case('pi')
        ! the `% object` is a unlimeted polymorphic `class(*)`
        ! that you can store and retrieve  arbitrary information
        ans % object = 4*atan(1.)
    end select
end function
```

See more using the example program provided in [`example/main.f90`](https://github.com/14NGiestas/shunting-yard-fortran/blob/main/example/main.f90) where you will find the basic usage of this package.
