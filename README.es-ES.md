# shunting-yard-fortran
Pequeño analizador de expresiones utilizando el algoritmo Shunting Yard.

# Acerca de

Este es solo un proyecto experimental que comencé como una forma de analizar Tipos Derivados (mi modelo hamiltoniano) a partir de un archivo o cadena (y eventualmente desistí ya que no funcionaba bien, pero lo publiqué de todos modos), por lo que el objetivo aquí no es ser súper eficiente, sino lo suficientemente flexible para permitirte escribir tu propio analizador con cierta facilidad, reutilizando estructuras de sobrecarga de operadores que ya puedas tener.

Después de un tiempo (2022), finalmente logré que funcionara con Tipos Derivados rediseñando la API para utilizar un `token_t` y una `token_list` para los cuales creé un paquete separado, [fortran-tokenizer](https://github.com/14NGiestas/fortran-tokenizer).

Aunque funciona como una forma de escribir analizadores, no recomiendo usarlo como dependencia ya que presenta varios problemas de diseño:

1. Sin seguridad de hilos (Thread safety): Registro los tokens válidos como un singleton global, por lo que no se admiten múltiples analizadores.
2. Precedencia hard-coded: la precedencia de los operadores está actualmente fija para algunos tokens (desincronizada con los registrados).
3. Como utiliza un array de cadenas, todos los tokens deben estar rellenos con espacios para que tengan el mismo tamaño.
4. La aplicación actual no proporciona ninguna forma de verificar errores de sintaxis.

Dicho esto, lo estoy utilizando de todos modos en este momento y la API pública actual se siente bien, por lo que pronto trabajaré en un paquete adecuado con una versión genérica basada en esta idea:tm:.

## Primeros Pasos

Primero obtén el código, clonando el repositorio:

```sh
git clone https://github.com/14NGiestas/shunting-yard-fortran.git
cd shunting-yard-fortran 
```

## FPM

Este proyecto fue diseñado para ser construido utilizando el [Fortran Package Manager](https://github.com/fortran-lang/fpm).
Sigue las instrucciones de esa página para instalar FPM si aún no lo has hecho.

Para construir y ejecutar, escribe:

```sh
fpm run
```

Para ejecutar las pruebas, escribe:

```sh
fpm test 
```

## Uso

El usuario de este paquete debe indicar al analizador qué es cada token, utilizando las rutinas `register_*` 

```fortran
call p % register_function(["sin ", "sqrt"])
call p % register_operator(["+   ", "-   ",&
                            "*   ", "/   "])
call p % register_operator(["^   "], is_right_assoc=.true.)
```

Por defecto, los espacios y los saltos de línea serán ignorados; si deseas que se ignore algún otro carácter, puedes usar la rutina `ignore_tokens`

```fortran
! Esto ignorará estos caracteres
call p % ignore_tokens([" ", "&", new_line(' ')])
```

Luego, deberás vincular las funciones `on_*` que serán llamadas para dar sentido a los tokens

```fortran
p % on_unary    => on_unary
p % on_binary   => on_binary
p % on_operand  => on_operand
```

Dichas funciones deben respetar sus interfaces relacionadas

```fortran
abstract interface
    function interface_on_binary(self, lhs, opr, rhs) result(ans)
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

    function interface_on_unary(self, opr, arg) result(ans)
        import :: parser_t 
        class(parser_t) :: self
        type(token_t) :: opr
        type(token_t) :: arg
        type(token_t) :: ans
    end function
end interface
```

Por ejemplo, si quieres darle sentido a "pi" en una expresión, debes implementar la siguiente función.

```fortran
function on_operand(self, opr) result(ans)
    class(parser_t) :: self
    type(token_t) :: opr
    type(token_t) :: ans

    ! el `% string` obtiene la representación en cadena de un token
    select case(opr % string)
    case('pi')
        ! el `% object` es una `class(*)` polimórfica ilimitada
        ! en la que puedes almacenar y recuperar información arbitraria
        ans % object = 4*atan(1.)
    end select
end function
```

Consulta más detalles utilizando el programa de ejemplo proporcionado en [`example/main.f90`](https://github.com/14NGiestas/shunting-yard-fortran/blob/main/example/main.f90) donde encontrarás el uso básico de este paquete.
