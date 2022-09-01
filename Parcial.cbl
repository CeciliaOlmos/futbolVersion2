      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARCHIVO-NOVEDADES
           ASSIGN TO "../novedades.dat"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT ARCHIVO-JUGADORES
           ASSIGN TO "../jugadores.dat"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT ARCHIVO-EQUIPOS
           ASSIGN TO "../equipo.dat"
           ORGANIZATION LINE SEQUENTIAL.

           SELECT ARCHIVO-LISTADO
           ASSIGN TO "../listado.dat".

           SELECT ARCH-SORT
           ASSIGN TO DISK "SORTWORK".

      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD  ARCHIVO-NOVEDADES.
       01  REG-NOV.
           03 NOV-EQU PIC 99.
           03 NOV-CANT-JUG PIC 99.
           03 NOV-VEC-GOLEAD OCCURS 25 DEPENDING NOV-CANT-JUG.
               05 NOV-JUG PIC X(6).
               05 NOV-GOLES PIC 99.

       FD  ARCHIVO-JUGADORES.
       01  JUG-CAB-REG.
           03 JUG-CAB-TIPOREG PIC 9.
           03 JUG-CAB-EQU PIC 99.
       01  JUG-DET-REG.
           03 JUG-DET-TIPOREG PIC 9.
           03 JUG-DET-JUG PIC X(6).
           03 JUG-DET-GOLES PIC 9(8).

       FD  ARCHIVO-EQUIPOS.
       01  EQU-REG.
           03 EQU-COD PIC 99.
           03 EQU-NOMBRE PIC X(15).

       FD  ARCHIVO-LISTADO.
       01  IMPRIMIR-LINEA PIC X(40).

       SD  ARCH-SORT.
       01  REG-SORT.
           03 SORT-EQU PIC 99.
           03 SORT-JUG PIC X(6).
           03 SORT-GOLES PIC 99.

      *-----------------------
       WORKING-STORAGE SECTION.
       01  W-FLAG PIC 9.
       01  W-CANT-JUG PIC 99.
       01  W-CANT-LINEAS PIC 9(8).
       01  ENCABEZADO.
           03 FILLER PIC X(7) VALUE "LISTADO".
       01  SUB-ENCABEZADO.
           03 FILLER PIC X(8) VALUE "CODIGO: ".
           03 ENC-COD-EQU PIC 99.
           03 FILLER PIC X(10) VALUE SPACE.
           03 FILLER PIC X(8) VALUE "NOMBRE: ".
           03 ENC-NOM-EQU PIC X(15).
       01  TITULO.
           03 FILLER PIC X(12) VALUE "COD. JUGADOR".
           03 FILLER PIC X(10) VALUE SPACES.
           03 FILLER PIC X(5) VALUE "GOLES".
       01  JUGADORES.
           03 FILLER PIC X(3) VALUE SPACE.
           03 LIST-COD-JUG PIC X(6).
           03 FILLER PIC X(11) VALUE SPACES.
           03 LIST-GOLES PIC 9(8).
       01  PIE.
           03 FILLER PIC X(26) VALUE "GOLES TOTALES DEL EQUIPO: ".
           03 LIST-EQU-GOLES PIC 9(13).
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      ************************************
      ************ARCHIVOS****************
      ************************************
            PERFORM 1000-INICIO.
            PERFORM 2000-PROCESO.
            PERFORM 4000-FIN.
      *************************************
      *********PROGRAMA PRINCIPAL**********
      *************************************
       1000-INICIO.
           OPEN INPUT ARCHIVO-NOVEDADES.
           OPEN INPUT ARCHIVO-JUGADORES.
           OPEN INPUT ARCHIVO-EQUIPOS.
           OPEN OUTPUT ARCHIVO-LISTADO.
           MOVE 1 TO W-FLAG.
           MOVE ZERO TO LIST-EQU-GOLES.

       2000-PROCESO.
           SORT ARCH-SORT
              ON ASCENDING SORT-EQU
              ON ASCENDING SORT-JUG
              INPUT PROCEDURE 2100-PROCESO-ENTRADA
              OUTPUT PROCEDURE 2200-PROCESO-SALIDA.

       2100-PROCESO-ENTRADA.
           PERFORM 2100-PROCESAR-NOVEDADES.
           PERFORM 2150-PROCESAR-JUGADORES.

       2100-PROCESAR-NOVEDADES.
           PERFORM 2110-LEER-ARCHIVO-NOVEDADES.
           PERFORM UNTIL W-FLAG = 0
               PERFORM 2120-MOVER-EQUIPO-NOVEDADES
               PERFORM VARYING W-CANT-JUG FROM 1 BY 1
                   UNTIL W-CANT-JUG > NOV-CANT-JUG
                   PERFORM 2130-MOVER-RESTO-NOVEDADES
                   RELEASE REG-SORT
               END-PERFORM
               PERFORM 2110-LEER-ARCHIVO-NOVEDADES
           END-PERFORM.

       2110-LEER-ARCHIVO-NOVEDADES.
           READ ARCHIVO-NOVEDADES AT END MOVE 0 TO W-FLAG.

       2120-MOVER-EQUIPO-NOVEDADES.
           MOVE NOV-EQU TO SORT-EQU.

       2130-MOVER-RESTO-NOVEDADES.
           MOVE NOV-JUG(W-CANT-JUG) TO SORT-JUG.
           MOVE NOV-GOLES(W-CANT-JUG) TO SORT-GOLES.

       2150-PROCESAR-JUGADORES.
           MOVE 1 TO W-FLAG.
           PERFORM 2160-LEER-ARCHIVO-JUGADORES.
           PERFORM UNTIL W-FLAG = 0
               PERFORM 2170-MOVER-EQUIPO-JUGADORES
               PERFORM 2160-LEER-ARCHIVO-JUGADORES
               PERFORM UNTIL JUG-DET-TIPOREG = 1 OR W-FLAG = 0
                   PERFORM 2180-MOVER-RESTO-JUGADORES
                   RELEASE REG-SORT
                   PERFORM 2160-LEER-ARCHIVO-JUGADORES
               END-PERFORM
           END-PERFORM.


       2160-LEER-ARCHIVO-JUGADORES.
           READ ARCHIVO-JUGADORES AT END MOVE 0 TO W-FLAG.

       2170-MOVER-EQUIPO-JUGADORES.
           MOVE JUG-CAB-EQU TO SORT-EQU.

       2180-MOVER-RESTO-JUGADORES.
           MOVE JUG-DET-JUG TO SORT-JUG.
           MOVE JUG-DET-GOLES TO SORT-GOLES.

       2200-PROCESO-SALIDA.
           MOVE 1 TO W-FLAG.
           PERFORM 2205-GRABAR-ENCABEZADO.
           PERFORM 2210-LEER-ARCHIVO-SORT.
           PERFORM UNTIL W-FLAG = 0
               PERFORM 2220-MOVER-EQUIPO-SORT
               PERFORM 2230-BUSCAR-EQUIPO
               PERFORM 2240-MOVER-NOM-EQU
               PERFORM 2250-GRABAR-SUB-ENCABEZADO
               PERFORM 2260-GRABAR-TITULO
               PERFORM UNTIL ENC-COD-EQU NOT = SORT-EQU OR W-FLAG = 0
                   PERFORM 2270-MOVER-RESTO-SORT
                   PERFORM 2210-LEER-ARCHIVO-SORT
                   PERFORM 2280-COMPARAR-ANTERIOR
                   PERFORM 2300-SUMAR-TOTAL-EQUIPO
                   PERFORM 2290-GRABAR-JUGADORES
               END-PERFORM
               PERFORM 2310-GRABAR-PIE
           END-PERFORM.

       2205-GRABAR-ENCABEZADO.
           WRITE IMPRIMIR-LINEA FROM ENCABEZADO AFTER ADVANCING PAGE.
           ADD 1 TO W-CANT-LINEAS.

       2210-LEER-ARCHIVO-SORT.
           RETURN ARCH-SORT INTO REG-SORT AT END MOVE 0 TO W-FLAG.

       2220-MOVER-EQUIPO-SORT.
           MOVE SORT-EQU TO ENC-COD-EQU.

       2230-BUSCAR-EQUIPO.
           PERFORM UNTIL W-FLAG = 0 OR SORT-EQU = EQU-COD
               PERFORM 2235-LEER-EQUIPOS
           END-PERFORM.

       2235-LEER-EQUIPOS.
           READ ARCHIVO-EQUIPOS AT END MOVE 0 TO W-FLAG.

       2240-MOVER-NOM-EQU.
           MOVE EQU-NOMBRE TO ENC-NOM-EQU.

       2250-GRABAR-SUB-ENCABEZADO.
           WRITE IMPRIMIR-LINEA FROM SUB-ENCABEZADO
               AFTER ADVANCING 2 LINE.

       2260-GRABAR-TITULO.
           WRITE IMPRIMIR-LINEA FROM TITULO
               AFTER ADVANCING 1 LINE.

       2270-MOVER-RESTO-SORT.
           MOVE SORT-JUG TO LIST-COD-JUG.
           MOVE SORT-GOLES TO LIST-GOLES.

       2280-COMPARAR-ANTERIOR.
           IF SORT-EQU = ENC-COD-EQU AND SORT-JUG = LIST-COD-JUG
               ADD SORT-GOLES TO LIST-GOLES
               PERFORM 2210-LEER-ARCHIVO-SORT
           END-IF.

       2290-GRABAR-JUGADORES.
           WRITE IMPRIMIR-LINEA FROM JUGADORES AFTER ADVANCING 1 LINE.

       2300-SUMAR-TOTAL-EQUIPO.
           DISPLAY LIST-GOLES.
           ADD LIST-GOLES TO LIST-EQU-GOLES.

       2310-GRABAR-PIE.
           WRITE IMPRIMIR-LINEA FROM PIE AFTER ADVANCING 1 LINE.
           MOVE ZERO TO LIST-EQU-GOLES.

       4000-FIN.
           CLOSE ARCHIVO-NOVEDADES.
           CLOSE ARCHIVO-JUGADORES.
           CLOSE ARCHIVO-EQUIPOS.
           CLOSE ARCHIVO-LISTADO.
           STOP RUN.

       END PROGRAM YOUR-PROGRAM-NAME.
