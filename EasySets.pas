PROGRAM EasySets (input, output);

{Constantes}

CONST MAX     = 10; {Numéro máximo de elementos en una secuencia.}
      CANTSEC = 3;  {Numéro máximo de secuencias.}

{***********************************************************************************************************************}

{Definición de tipos}

TYPE  Natural   = 0..MAXINT;
      rangoSecs = 1..CANTSEC;

      Secuencia = RECORD
                    valores : ARRAY [1..MAX] OF Natural;
                    tope : 0..MAX;
                  END;

      Coleccion = ^Celda;
      Celda = RECORD
                sec : Secuencia;
                sig : Coleccion;
              END;

      TipoResultado = (Fallo, Creado, Agregado);
      Resultado = RECORD
                    CASE quePaso : TipoResultado OF
                      Fallo: ();
                      Creado: ();
                      Agregado: (posicion: Natural)
                  END;

{***********************************************************************************************************************}

{Variables}

VAR opcion : Char;  
    res: Resultado;
    secs : ARRAY [rangoSecs] OF Secuencia;
    idSec1, idSec2, idSec: rangoSecs;
    col: Coleccion;
    valor: Natural;
    pos, pos1, pos2: Natural;

{***********************************************************************************************************************}

{Procedimientos y funciones}

PROCEDURE CrearSecuencia (VAR sec : Secuencia);

{Crea una secuencia de números naturales vacía.}

BEGIN
    sec.tope := 0
END;

{-----------------------------------------------------------------------------------------------------------------------}

PROCEDURE AgregarValor (VAR sec : Secuencia; valor : Natural; VAR res: Resultado);

{Inserta ordenadamente (orden creciente) el parámetro "valor" en el parámetro
"sec", y devuelve "Agregado" en "res.quePaso" (campo correspondiente del
parámetro "res") y la posición en la cual fue insertado en "res.posicion" (campo
correspondiente del parámetro "res"). Si el parámetro "sec" está lleno o el
parámetro "valor" ya pertenece al mismo, devuelve "Fallo" en "res.quePaso" (campo
correspondiente del parámetro "res").}

VAR Indice, i : 1..MAX + 1;

BEGIN
    IF sec.tope = MAX THEN
        res.quePaso := Fallo

    ELSE
        IF sec.tope = 0 THEN
            BEGIN
                sec.tope := 1;
                sec.valores [1] := valor;            
                res.quePaso := Agregado;
                res.posicion := 1
            END

        ELSE
            BEGIN
                Indice := 1;
                
                WHILE (Indice <= sec.tope) AND (sec.valores [Indice] < valor) DO
                    Indice := Indice + 1;
                
                IF (Indice <= sec.tope) AND (sec.valores [Indice] = valor) THEN
                    res.quePaso := Fallo

                ELSE
                    BEGIN
                        sec.tope := sec.tope + 1;

                        FOR i := sec.tope DOWNTO Indice + 1 DO
                            sec.valores [i] := sec.valores [i - 1];

                        sec.valores [Indice] := valor;
                        res.quePaso := Agregado;
                        res.posicion := Indice
                    END
            END
END;

{-----------------------------------------------------------------------------------------------------------------------}

FUNCTION SecuenciasIguales (sec1, sec2 : Secuencia) : Boolean;

{Determina si los parámetros "sec1" y "sec2"
tienen los mismos valores y posiciones.}

VAR Indice : 1..MAX + 1;

BEGIN
    IF sec1.tope = sec2.tope THEN
        BEGIN
            Indice := 1;

            WHILE (Indice <= sec1.tope) AND (sec1.valores [Indice] = sec2.valores [Indice]) DO
                Indice := Indice + 1;

            SecuenciasIguales := Indice > sec1.tope
        END
    
    ELSE
        SecuenciasIguales := FALSE
END;

{-----------------------------------------------------------------------------------------------------------------------}

PROCEDURE SecuenciaFusionada (sec : Secuencia; VAR secF : Secuencia; VAR res: Resultado);

{Le asigna al parámetro "secF" el parámetro "sec", y devuelve "Creado"
en "res.quePaso" (campo correspondiente del parámetro "res").}

BEGIN
    secF := sec;
    res.quePaso := Creado
END;

{-----------------------------------------------------------------------------------------------------------------------}

PROCEDURE AgregarValores (sec1, sec2 : Secuencia; VAR sec3 : Secuencia; VAR res : Resultado);

{Devuelve en el parámetro "sec3" la "unión" (entre comillas porque técnicamente no se está
trabajando con conjuntos) en orden creciente de los parámetros "sec1" y "sec2", y devuelve
"Creado" en "res.quePaso" (campo correspondiente del parámetro "res").}

{Precondición: (sec1.tope <> 0) AND (sec2.tope <> 0) AND (sec1.tope + sec2.tope <= MAX)}

VAR i : 2..MAX + 1;
    Indice, Posicion: 1..MAX + 1;    

BEGIN
    Indice   := 1;
    Posicion := 1;
    
    REPEAT                               
        WHILE (Indice <= sec1.tope) AND (sec1.valores [Indice] < sec2.valores [Posicion]) DO
            Indice := Indice + 1;
                            
        IF (Indice <= sec1.tope) AND (sec1.valores [Indice] = sec2.valores [Posicion]) THEN
            Posicion := Posicion + 1

        ELSE
            BEGIN
                sec1.tope := sec1.tope + 1;

                FOR i := sec1.tope DOWNTO Indice + 1 DO
                    sec1.valores [i] := sec1.valores [i - 1];

                sec1.valores [Indice] := sec2.valores [Posicion];
                Posicion := Posicion + 1;
                Indice := Indice + 1           
            END;

    UNTIL Posicion > sec2.tope;

    SecuenciaFusionada (sec1, sec3, res)
END;

{-----------------------------------------------------------------------------------------------------------------------}

PROCEDURE AgregarValoresRigurosamente (sec1, sec2 : Secuencia; VAR sec3 : Secuencia; VAR res : Resultado);

{Análogo al procedimiento "AgregarValores", pero en caso de que el parámetro "sec3" no tenga
espacio suficiente para almacenar todos los valores, devuelve "Fallo" en "res.quePaso"
(campo correspondiente del parámetro "res"), y el parámetro "sec3" queda indeterminado.}

{Precondición: (sec1.tope <> 0) AND (sec2.tope <> 0)}

VAR i : 2..MAX + 1;
    Indice, Posicion: 1..MAX + 1;    

BEGIN
    Indice   := 1;
    Posicion := 1;
    
    REPEAT                               
        WHILE (Indice <= sec1.tope) AND (sec1.valores [Indice] < sec2.valores [Posicion]) DO
            Indice := Indice + 1;
                            
        IF (Indice <= sec1.tope) AND (sec1.valores [Indice] = sec2.valores [Posicion]) THEN
            Posicion := Posicion + 1

        ELSE
            IF sec1.tope < MAX THEN
                BEGIN
                    sec1.tope := sec1.tope + 1;

                    FOR i := sec1.tope DOWNTO Indice + 1 DO
                        sec1.valores [i] := sec1.valores [i - 1];

                    sec1.valores [Indice] := sec2.valores [Posicion];
                    Posicion := Posicion + 1;
                    Indice := Indice + 1           
                END

    UNTIL (Posicion > sec2.tope) OR (Indice >= MAX) OR (sec1.tope = MAX);

    IF (sec1.tope = MAX) AND (sec1.valores [Indice] <= sec2.valores [Posicion]) THEN
        REPEAT
            WHILE (Indice <= sec1.tope) AND (sec1.valores [Indice] < sec2.valores [Posicion]) DO
                Indice := Indice + 1;
                                
            IF (Indice <= sec1.tope) AND (sec1.valores [Indice] = sec2.valores [Posicion]) THEN
                Posicion := Posicion + 1

        UNTIL (Posicion > sec2.tope) OR (Indice > sec1.tope) OR (sec1.valores [Indice] > sec2.valores [Posicion]);

    IF Posicion <= sec2.tope THEN
        res.quePaso := Fallo
    
    ELSE
        SecuenciaFusionada (sec1, sec3, res)
END;

{-----------------------------------------------------------------------------------------------------------------------}

PROCEDURE FusionarSecuencias (sec1, sec2 : Secuencia; VAR sec3 : Secuencia; VAR res: Resultado);

{Análogo al procedimiento "AgregarValoresRigurosamente", con la
excepción de que este procedimiento no tiene precondiciones.}

BEGIN
    IF sec1.tope = 0 THEN
        SecuenciaFusionada (sec2, sec3, res)
       
    ELSE
        IF sec2.tope = 0 THEN
            SecuenciaFusionada (sec1, sec3, res)

        ELSE
            IF (sec1.tope = MAX) AND (sec2.tope = MAX) THEN
                IF SecuenciasIguales (sec1, sec2) THEN
                    SecuenciaFusionada (sec1, sec3, res)

                ELSE
                    res.quePaso := Fallo

            ELSE
                IF sec1.tope + sec2.tope <= MAX THEN
                    IF sec1.tope >= sec2.tope THEN
                        AgregarValores (sec1, sec2, sec3, res)                         
                    
                    ELSE
                        AgregarValores (sec2, sec1, sec3, res)                            
                        
                ELSE
                    IF sec1.tope >= sec2.tope THEN
                       AgregarValoresRigurosamente (sec1, sec2, sec3, res)

                    ELSE
                        AgregarValoresRigurosamente (sec2, sec1, sec3, res)
END;

{-----------------------------------------------------------------------------------------------------------------------}

PROCEDURE CrearColeccion (VAR col : Coleccion);

{Crea una lista encadenada vacía.}

BEGIN
    New (col);
    col := NIL
END;

{-----------------------------------------------------------------------------------------------------------------------}

PROCEDURE AgregarSecuencia (VAR col : Coleccion; sec : Secuencia; VAR pos : Natural);

{Inserta el parámetro "sec" al final del parámetro "col", y devuelve
en el parámetro "pos" la posición en el cual fue insertado.}

VAR Posicion : 1..MAXINT;
    Final, Temp : Coleccion;

BEGIN
    New (Final);
    Final^.sec := sec;
    Final^.sig := NIL;

    IF col = NIL THEN
        BEGIN
            Posicion := 1;
            col := Final
        END
    
    ELSE
        BEGIN
            Posicion := 2;
            Temp := col;

            WHILE Temp^.sig <> NIL DO
                BEGIN
                    Temp := Temp^.sig;
                    Posicion := Posicion + 1
                END;

            Temp^.sig := Final
        END;
    
    pos := Posicion
END;

{-----------------------------------------------------------------------------------------------------------------------}

FUNCTION todasIguales (col : Coleccion) : Boolean;

{Determina si todas las secuencias del parámetro
"col" son iguales. Si el parámetro "col" está vacío
o tiene una sola secuencia, también devuelve true.}

VAR sec : Secuencia;

BEGIN
    IF (col = NIL) OR (col^.sig = NIL) THEN
        todasIguales := TRUE

    ELSE
        BEGIN
            REPEAT
                sec := col^.sec;
                col := col^.sig;
            UNTIL (col^.sig = NIL) OR NOT SecuenciasIguales (sec, col^.sec);

            todasIguales := SecuenciasIguales (sec, col^.sec)
        END
END;

{-----------------------------------------------------------------------------------------------------------------------}

PROCEDURE SecuenciaFusionadaEnColeccion (sec : Secuencia; VAR secF : Secuencia; VAR res: Resultado);

{Análogo al procedimiento "SecuenciaFusionada", con la excepción de que devuelve "Agregado"
en vez de "Creado" en "res.quePaso" (campo correspondiente del parámetro "res").}

BEGIN
    secF := sec;
    res.quePaso := Agregado
END;

{-----------------------------------------------------------------------------------------------------------------------}

PROCEDURE AgregarValoresEnColeccion (sec1, sec2 : Secuencia; VAR sec3 : Secuencia; VAR res : Resultado);

{Análogo al procedimiento "AgregarValores", con la excepción de que utiliza el
procedimiento "SecuenciaFusionadaEnColeccion" en lugar de "SecuenciaFusionada".}

VAR i : 2..MAX + 1;
    Indice, Posicion: 1..MAX + 1;    

BEGIN
    Indice   := 1;
    Posicion := 1;
    
    REPEAT                               
        WHILE (Indice <= sec1.tope) AND (sec1.valores [Indice] < sec2.valores [Posicion]) DO
            Indice := Indice + 1;
                            
        IF (Indice <= sec1.tope) AND (sec1.valores [Indice] = sec2.valores [Posicion]) THEN
            Posicion := Posicion + 1

        ELSE
            BEGIN
                sec1.tope := sec1.tope + 1;

                FOR i := sec1.tope DOWNTO Indice + 1 DO
                    sec1.valores [i] := sec1.valores [i - 1];

                sec1.valores [Indice] := sec2.valores [Posicion];
                Posicion := Posicion + 1;
                Indice := Indice + 1           
            END;

    UNTIL Posicion > sec2.tope;

    SecuenciaFusionadaEnColeccion (sec1, sec3, res)
END;

{-----------------------------------------------------------------------------------------------------------------------}

PROCEDURE AgregarValoresRigurosamenteEnColeccion (sec1, sec2 : Secuencia; VAR sec3 : Secuencia; VAR res : Resultado);

{Análogo al procedimiento "AgregarValoresRigurosamente", con la excepción de que utiliza el
procedimiento "SecuenciaFusionadaEnColeccion" en lugar de "SecuenciaFusionada".}

VAR i : 2..MAX + 1;
    Indice, Posicion: 1..MAX + 1;    

BEGIN
    Indice   := 1;
    Posicion := 1;
    
    REPEAT
        WHILE (Indice <= sec1.tope) AND (sec1.valores [Indice] < sec2.valores [Posicion]) DO
            Indice := Indice + 1;
                            
        IF (Indice <= sec1.tope) AND (sec1.valores [Indice] = sec2.valores [Posicion]) THEN
            Posicion := Posicion + 1

        ELSE
            IF sec1.tope < MAX THEN
                BEGIN
                    sec1.tope := sec1.tope + 1;                    

                    FOR i := sec1.tope DOWNTO Indice + 1 DO                        
                        sec1.valores [i] := sec1.valores [i - 1];                    

                    sec1.valores [Indice] := sec2.valores [Posicion];                    
                    Posicion := Posicion + 1;
                    Indice := Indice + 1;
                END

    UNTIL (Posicion > sec2.tope) OR (Indice > sec1.tope) OR (sec1.tope = MAX);

    IF (sec1.tope = MAX) AND (sec1.valores [Indice] <= sec2.valores [Posicion]) THEN
        REPEAT
            WHILE (Indice <= sec1.tope) AND (sec1.valores [Indice] < sec2.valores [Posicion]) DO
                Indice := Indice + 1;
                                
            IF (Indice <= sec1.tope) AND (sec1.valores [Indice] = sec2.valores [Posicion]) THEN
                Posicion := Posicion + 1

        UNTIL (Posicion > sec2.tope) OR (Indice > sec1.tope) OR (sec1.valores [Indice] > sec2.valores [Posicion]);
            
    IF Posicion <= sec2.tope THEN
        res.quePaso := Fallo      
    
    ELSE        
        SecuenciaFusionadaEnColeccion (sec1, sec3, res)
        
END;

{-----------------------------------------------------------------------------------------------------------------------}

PROCEDURE FusionarSecuenciasEnColeccion (sec1, sec2 : Secuencia; VAR sec3 : Secuencia; VAR res: Resultado);

{Análogo al procedimiento "FusionarSecuencias", con la excepción de que utiliza el procedimiento
"SecuenciaFusionadaEnColeccion" en lugar de "SecuenciaFusionada", el procedimiento
"AgregarValoresEnColeccion" en lugar de "AgregarValores", y el procedimiento
"AgregarValoresRigurosamenteEnColeccion" en lugar de "AgregarValoresRigurosamente".}

BEGIN
    IF sec1.tope = 0 THEN
        SecuenciaFusionadaEnColeccion (sec2, sec3, res)
       
    ELSE
        IF sec2.tope = 0 THEN
            SecuenciaFusionadaEnColeccion (sec1, sec3, res)

        ELSE
            IF (sec1.tope = MAX) AND (sec2.tope = MAX) THEN
                IF SecuenciasIguales (sec1, sec2) THEN
                    SecuenciaFusionadaEnColeccion (sec1, sec3, res)

                ELSE
                    res.quePaso := Fallo

            ELSE
                IF sec1.tope + sec2.tope <= MAX THEN
                    IF sec1.tope >= sec2.tope THEN
                        AgregarValoresEnColeccion (sec1, sec2, sec3, res)                         
                    
                    ELSE
                        AgregarValoresEnColeccion (sec2, sec1, sec3, res)                            
                        
                ELSE
                    IF sec1.tope >= sec2.tope THEN
                       AgregarValoresRigurosamenteEnColeccion (sec1, sec2, sec3, res)

                    ELSE
                        AgregarValoresRigurosamenteEnColeccion (sec2, sec1, sec3, res)
END;

{-----------------------------------------------------------------------------------------------------------------------}

PROCEDURE PosicionesDiferentes (VAR col : Coleccion; pos1, pos2 : Natural; VAR res : Resultado);

{Dadas las posiciones de dos secuencias del parámetro "col" (parámetros "pos1" y "pos2"), crea
una nueva secuencia conteniendo la "unión" (entre comillas porque técnicamente no se está
trabajando con conjuntos) de ambas en orden creciente, y la inserta al final del parámetro
"col", y devuelve "Agregado" en "res.quePaso" (campo correspondiente del parámetro "res") y la
posición en la cual fue insertado en "res.posicion" (campo correspondiente del parámetro
"res"). En caso de que los parámetros "pos1" o "pos2" sean posiciones inválidas, o que la nueva
secuencia no tenga espacio suficiente para almacenar todos los valores, devuelve "Fallo" en
"res.quePaso" (campo correspondiente del parámetro "res").}

{Precondición: (col <> NIL) AND (pos1 <> 0) AND (pos2 <> 0) AND (pos1 <> pos2)}

VAR 
    Posicion : Natural;
    sec1, sec2 : Secuencia;
    Temp, Final : Coleccion;

BEGIN
    Posicion := 1;
    Temp := col;

    WHILE (Temp^.sig <> NIL) AND (Posicion < pos1) DO
        BEGIN
            Posicion := Posicion + 1;
            Temp := Temp^.sig;           
        END;

    IF Posicion = pos1 THEN
        BEGIN            
            sec1 := Temp^.sec;

            WHILE (Temp^.sig <> NIL) AND (Posicion < pos2) DO
                BEGIN
                    Posicion := Posicion + 1;
                    Temp := Temp^.sig;           
                END;
            
            IF Posicion = pos2 THEN
                BEGIN
                    sec2 := Temp^.sec;

                    FusionarSecuenciasEnColeccion (sec1, sec2, sec1, res);

                    IF res.quePaso = Agregado THEN
                        BEGIN
                            WHILE Temp^.sig <> NIL DO
                                BEGIN
                                    Posicion := Posicion + 1;
                                    Temp := Temp^.sig
                                END;

                            New (Final);
                            Final^.sec := sec1;
                            Final^.sig := NIL;

                            Temp^.sig := Final;

                            res.posicion := Posicion + 1
                        END
                END

            ELSE
                res.quePaso := Fallo
        END

    ELSE
        res.quePaso := Fallo
END;

{-----------------------------------------------------------------------------------------------------------------------}

PROCEDURE PosicionesIguales (VAR col : Coleccion; pos : Natural; VAR res : Resultado);

{Análogo al procedimiento "PosicionesDiferentes", pero con una precondición diferente.}

{Precondición: (col <> NIL) AND (pos1 <> 0) AND (pos2 <> 0) AND (pos1 = pos2)}

VAR Temp, Final : Coleccion;
    Posicion : Natural;
    sec : Secuencia;

BEGIN
    Posicion := 1;
    Temp := col;

    WHILE (Temp^.sig <> NIL) AND (Posicion < pos) DO
        BEGIN
            Posicion := Posicion + 1;
            Temp := Temp^.sig
        END;
                    
    IF Posicion = pos THEN
        BEGIN
            sec := Temp^.sec;
            
            WHILE (Temp^.sig <> NIL) DO
                BEGIN
                    Posicion := Posicion + 1;
                    Temp := Temp^.sig
                END;

            New (Final);
            Final^.sec := sec;
            Final^.sig := NIL;

            Temp^.sig := Final;

            res.quePaso := Agregado;
            res.posicion := Posicion + 1
        END

    ELSE
        res.quePaso := Fallo
END;

{-----------------------------------------------------------------------------------------------------------------------}

PROCEDURE FusionarEnColeccion (VAR col : Coleccion; pos1, pos2 : Natural; VAR res : Resultado);

{Análogo al procedimiento "PosicionesDiferentes", con la
excepción de que este procedimiento no tiene precondiciones.}

BEGIN
    IF (col = NIL) OR (pos1 = 0) OR (pos2 = 0) THEN
        res.quePaso := Fallo

    ELSE
        IF pos1 = pos2 THEN
            PosicionesIguales (col, pos1, res)

        ELSE
            IF pos1 < pos2 THEN
                PosicionesDiferentes (col, pos1, pos2, res)
                    
            ELSE
                PosicionesDiferentes (col, pos2, pos1, res)
END;

{-----------------------------------------------------------------------------------------------------------------------}

PROCEDURE imprimirResultado (res: Resultado);

BEGIN
  CASE res.quepaso OF
    Fallo: writeln('Fallo');
    Creado: writeln('Creado: ');
    Agregado: writeln('Agregado en posición ', res.posicion:1)
  END
END;

{-----------------------------------------------------------------------------------------------------------------------}

PROCEDURE imprimirSecuencia (sec: Secuencia);

VAR i: 0..MAX;

BEGIN
  Write ('tope: ', sec.tope:1, '; Valores: ');

  FOR i := 1 TO sec.tope DO 
    Write (sec.valores[i]:1, ' ');

  Writeln (); 
END;

{***********************************************************************************************************************}

{Programa principal}

BEGIN
  Writeln ('s ---> Crear secuencia con el siguiente ID.');
  Writeln ('v ---> Agregar en la siguiente secuencia el siguiente valor.');
  Writeln ('i ---> ¿Son iguales las siguientes dos secuencias?');
  Writeln ('f ---> Fusionar las siguientes dos secuencias en la siguiente secuencia.');
  Writeln ('c ---> Crear colección.');
  Writeln ('a ---> Agregar en la siguiente colección la siguiente secuencia.');
  Writeln ('t ---> ¿Son iguales las secuncias dentro de la siguiente colección?');
  Writeln ('e ---> Fusionar en la siguiente colección las dos siguientes secuencias.');
  Writeln ('q ---> Salir.');

  Writeln ();

  REPEAT
    Read (opcion);
     
    CASE opcion OF
      's' : BEGIN {CrearSecuencia}
              Read (idSec);
              CrearSecuencia (secs [idSec]);
              Writeln ('Se creó la secuencia')
            END;
           
      'v' : BEGIN {AgregarValor}
              Read (idSec, valor);
              AgregarValor (secs [idSec], valor, res);
              imprimirResultado (res)
            END;
           
      'i' : BEGIN {SecuenciasIguales}
              Read (idSec1, idSec2);
              IF SecuenciasIguales (secs [idSec1], secs [idSec2]) THEN
                Writeln ('Son iguales')
              ELSE
                Writeln ('No son iguales')
            END;    
           
      'f' : BEGIN {FusionarSecuencias}
              Read (idSec1, idSec2, idSec);
              FusionarSecuencias (secs [idSec1], secs [idSec2], secs [idSec], res);
              imprimirResultado (res);
              IF (res.quepaso = Creado) THEN
                imprimirSecuencia (secs [idSec])
            END;

      'c' : BEGIN {CrearColeccion}
              CrearColeccion (col);
              Writeln ('Se creó la colección')  
            END;

      'a' : BEGIN {AgregarSecuencia}
              Read (idSec);
              AgregarSecuencia (col, secs [idSec], pos);
              Writeln ('Se agregó la secuencia ', idSec:1, ' en la posición ', pos:1);
            END;

      't' : BEGIN {TodasIguales}
              IF TodasIguales (col) THEN
                Writeln ('Todas son iguales')
              ELSE 
                Writeln ('NO todas son iguales')
            END;

      'e' : BEGIN {FusionarEnColeccion}
              Read (pos1, pos2);
              FusionarEnColeccion (col, pos1, pos2, res);
              imprimirResultado (res)
            END
    END      
  UNTIL opcion = 'q'
END.