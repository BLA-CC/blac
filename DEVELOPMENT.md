# Etapa 1
- Álvaro escribió documentación, parseo de argumentos y estableció la infraestructura para tests.
- Lucía diseñó casos de test, e implementó un "visitor" y un "printer" para el AST.
- Bruno implementó las estructuras y código necesarios para el AST y el parser,
    basándose en el pre-proyecto.

# Etapa 2
- Álvaro aportó en el desarrollo de la symbol table, escribió casos de test y actualizó la interfaz de usuario para que se ejecute el chequeo de tipos en la etapa de parseo.
- Lucía aportó en el desarrollo de la symbol table y se encargó de reportar los errores de tipos.
- Bruno aportó en el desarrollo de la symbol table e implementó el recorrido del AST que hace el chequeo de tipos. También mejoró el "printer" del AST.

# Etapa 3
- Álvaro se encargó de la generación de código para el `while`, hizo un intérprete para que las variables globales se calculen en tiempo de compilación y testeó el parseo de argumentos
- Lucía se encargó de la generación de código para las expresiones (sin llamadas a funciones) y las instrucciones `if` y testeó la generación de código intermedio
- Bruno se encargó de agregar soporte para la generación de código en la tabla de símbolos, hizo la generación de llamadas a métodos, agregó más chequeos semánticos a las variables globales y se encargó de hacer el "printer" del código intermedio.
