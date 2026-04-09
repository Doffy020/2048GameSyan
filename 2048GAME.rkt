#lang racket
(require 2htdp/image)
(require 2htdp/universe)
; prueba git
;; ============================================
;; CE2048 - Juego 2048 en Racket
;; ============================================

;; lista-ceros: número -> lista
(define (lista-ceros n)
  (if (= n 0)
      '()
      (cons 0 (lista-ceros (- n 1)))))

;; tablero-vacio-aux: número número -> lista
(define (tablero-vacio-aux n m)
  (if (= m 0)
      '()
      (cons (lista-ceros n) (tablero-vacio-aux n (- m 1)))))

;; tablero-vacio: número número -> lista
;; Crea un tablero de m x n con puros ceros
(define (tablero-vacio m n)
  (if (= m 0)
      '()
      (cons (lista-ceros n) (tablero-vacio (- m 1) n))))

;; reemplazar-en-fila: lista número número -> lista
(define (reemplazar-en-fila fila columna valor)
  (if (= columna 0)
      (cons valor (cdr fila))
      (cons (car fila) (reemplazar-en-fila (cdr fila) (- columna 1) valor))))

;; poner-valor: lista número número número -> lista
(define (poner-valor tablero fila columna valor)
  (if (= fila 0)
      (cons (reemplazar-en-fila (car tablero) columna valor) (cdr tablero))
      (cons (car tablero) (poner-valor (cdr tablero) (- fila 1) columna valor))))

;; obtener-filas: tablero -> número
(define (obtener-filas tablero)
  (if (empty? tablero)
      0
      (+ 1 (obtener-filas (cdr tablero)))))

;; obtener-columnas: tablero -> número
(define (obtener-columnas tablero)
  (if (empty? tablero)
      0
      (longitud-lista (car tablero))))

;; obtener-valor: tablero fila columna -> número
(define (obtener-valor tablero fila columna)
  (cond [(= fila 0) (cond [(= columna 0) (car (car tablero))]
                          [else (obtener-valor (cons (cdr (car tablero)) (cdr tablero)) 0 (- columna 1))])]
        [else (obtener-valor (cdr tablero) (- fila 1) columna)]))

;; ============================================
;; FUNCIONES ALEATORIAS
;; ============================================

;; aleatorio: número -> número
(define (aleatorio n)
  (random n))

;; longitud-lista: lista -> número
(define (longitud-lista lst)
  (if (empty? lst)
      0
      (+ 1 (longitud-lista (cdr lst)))))

;; obtener-elemento: lista número -> elemento
(define (obtener-elemento lst n)
  (if (= n 0)
      (car lst)
      (obtener-elemento (cdr lst) (- n 1))))

;; posiciones-vacias: tablero -> lista
(define (posiciones-vacias tablero)
  (define (recorrer-fila fila num-fila num-col)
    (cond [(empty? fila) '()]
          [(= (car fila) 0) (cons (cons num-fila num-col) (recorrer-fila (cdr fila) num-fila (+ num-col 1)))]
          [else (recorrer-fila (cdr fila) num-fila (+ num-col 1))]))
  (define (recorrer-tablero t num-fila)
    (cond [(empty? t) '()]
          [else (append (recorrer-fila (car t) num-fila 0)
                        (recorrer-tablero (cdr t) (+ num-fila 1)))]))
  (recorrer-tablero tablero 0))

;; insertar-ficha-aleatoria: tablero -> tablero
;; Inserta un 2 (90% probabilidad) o un 4 (10% probabilidad)
(define (insertar-ficha-aleatoria tablero)
  (define vacias (posiciones-vacias tablero))
  (define total (longitud-lista vacias))
  (define pos (obtener-elemento vacias (aleatorio total)))
  (define valor (if (< (aleatorio 10) 9) 2 4))
  (poner-valor tablero (car pos) (cdr pos) valor))

;; insertar-2-aleatorio: tablero -> tablero
;; Mantiene la función original por compatibilidad (siempre 2)
(define (insertar-2-aleatorio tablero)
  (insertar-ficha-aleatoria tablero))

;; insertar-dos-2s-aleatorio: tablero -> tablero
(define (insertar-dos-2s-aleatorio tablero)
  (define con-primero (insertar-2-aleatorio tablero))
  (insertar-2-aleatorio con-primero))

;; aleatorio-entre: número número -> número
(define (aleatorio-entre min max)
  (+ min (aleatorio (+ (- max min) 1))))

;; tablero-inicial: -> tablero
;; Crea un tablero de tamaño aleatorio M x N (M y N entre 4 y 10)
(define (tablero-inicial)
  (define filas (aleatorio-entre 4 10))
  (define columnas (aleatorio-entre 4 10))
  (define vacio (tablero-vacio filas columnas))
  (insertar-dos-2s-aleatorio vacio))

;; ============================================
;; MOVIMIENTOS - FILA IZQUIERDA
;; ============================================

;; eliminar-ceros: lista -> lista
(define (eliminar-ceros fila)
  (cond [(empty? fila) '()]
        [(= (car fila) 0) (eliminar-ceros (cdr fila))]
        [else (cons (car fila) (eliminar-ceros (cdr fila)))]))

;; combinar-iguales: lista -> lista
(define (combinar-iguales fila)
  (cond [(empty? fila) '()]
        [(empty? (cdr fila)) (cons (car fila) '())]
        [(= (car fila) (cadr fila)) 
         (cons (* 2 (car fila)) (combinar-iguales (cddr fila)))]
        [else (cons (car fila) (combinar-iguales (cdr fila)))]))

;; completar-con-ceros: lista número -> lista
(define (completar-con-ceros fila n)
  (if (>= (length fila) n)
      fila
      (completar-con-ceros (append fila '(0)) n)))

;; mover-fila-izquierda: lista número -> lista
(define (mover-fila-izquierda fila n)
  (completar-con-ceros (combinar-iguales (eliminar-ceros fila)) n))

;; mover-tablero-izquierda: tablero -> tablero
(define (mover-tablero-izquierda tablero)
  (define n (obtener-columnas tablero))
  (define (mover-filas t)
    (cond [(empty? t) '()]
          [else (cons (mover-fila-izquierda (car t) n)
                      (mover-filas (cdr t)))]))
  (mover-filas tablero))

;; ============================================
;; MOVIMIENTOS - DERECHA
;; ============================================

;; invertir-fila: lista -> lista
(define (invertir-fila fila)
  (cond [(empty? fila) '()]
        [else (append (invertir-fila (cdr fila)) (list (car fila)))]))

;; mover-fila-derecha: lista número -> lista
(define (mover-fila-derecha fila n)
  (invertir-fila (mover-fila-izquierda (invertir-fila fila) n)))

;; mover-tablero-derecha: tablero -> tablero
(define (mover-tablero-derecha tablero)
  (define n (obtener-columnas tablero))
  (define (mover-filas t)
    (cond [(empty? t) '()]
          [else (cons (mover-fila-derecha (car t) n)
                      (mover-filas (cdr t)))]))
  (mover-filas tablero))

;; ============================================
;; MOVIMIENTOS - ARRIBA Y ABAJO (TRANSPONER)
;; ============================================

;; obtener-columna: tablero número -> lista
(define (obtener-columna tablero n)
  (cond [(empty? tablero) '()]
        [else (cons (obtener-valor tablero 0 n)
                    (obtener-columna (cdr tablero) n))]))

;; eliminar-primera-columna: tablero -> tablero
(define (eliminar-primera-columna tablero)
  (cond [(empty? tablero) '()]
        [else (cons (cdr (car tablero))
                    (eliminar-primera-columna (cdr tablero)))]))

;; transponer: tablero -> tablero
(define (transponer tablero)
  (cond [(empty? (car tablero)) '()]
        [else (cons (obtener-columna tablero 0)
                    (transponer (eliminar-primera-columna tablero)))]))

;; mover-tablero-arriba: tablero -> tablero
(define (mover-tablero-arriba tablero)
  (transponer (mover-tablero-izquierda (transponer tablero))))

;; mover-tablero-abajo: tablero -> tablero
(define (mover-tablero-abajo tablero)
  (transponer (mover-tablero-derecha (transponer tablero))))


;; ============================================
;; DETECCIÓN DE VICTORIA Y DERROTA
;; ============================================

;; contiene-2048: tablero -> booleano
;; Devuelve #t si alguna casilla tiene el valor 2048, #f en caso contrario
(define (contiene-2048 tablero)
  (cond [(empty? tablero) #f]
        [(contiene-2048-en-fila (car tablero)) #t]
        [else (contiene-2048 (cdr tablero))]))

;; contiene-2048-en-fila: lista -> booleano
;; Devuelve #t si la fila contiene un 2048
(define (contiene-2048-en-fila fila)
  (cond [(empty? fila) #f]
        [(= (car fila) 2048) #t]
        [else (contiene-2048-en-fila (cdr fila))]))

;; tablero-lleno?: tablero -> booleano
;; Devuelve #t si no hay casillas vacías (ceros)
(define (tablero-lleno? tablero)
  (cond [(empty? tablero) #t]
        [(fila-llena? (car tablero)) (tablero-lleno? (cdr tablero))]
        [else #f]))

;; fila-llena?: lista -> booleano
;; Devuelve #t si la fila no tiene ceros
(define (fila-llena? fila)
  (cond [(empty? fila) #t]
        [(= (car fila) 0) #f]
        [else (fila-llena? (cdr fila))]))

;; hay-movimiento-posible?: tablero -> booleano
;; Devuelve #t si se puede hacer al menos un movimiento
(define (hay-movimiento-posible? tablero)
  (or (movimiento-izquierda-cambia? tablero)
      (movimiento-derecha-cambia? tablero)
      (movimiento-arriba-cambia? tablero)
      (movimiento-abajo-cambia? tablero)))

;; movimiento-izquierda-cambia?: tablero -> booleano
(define (movimiento-izquierda-cambia? tablero)
  (not (equal? tablero (mover-tablero-izquierda tablero))))

;; movimiento-derecha-cambia?: tablero -> booleano
(define (movimiento-derecha-cambia? tablero)
  (not (equal? tablero (mover-tablero-derecha tablero))))

;; movimiento-arriba-cambia?: tablero -> booleano
(define (movimiento-arriba-cambia? tablero)
  (not (equal? tablero (mover-tablero-arriba tablero))))

;; movimiento-abajo-cambia?: tablero -> booleano
(define (movimiento-abajo-cambia? tablero)
  (not (equal? tablero (mover-tablero-abajo tablero))))

;; juego-terminado?: tablero -> booleano
;; Devuelve #t si el juego terminó (sin movimientos posibles)
(define (juego-terminado? tablero)
  (not (hay-movimiento-posible? tablero)))

;; ============================================
;; CONTAR DOS's
;; ============================================

;; contar-dos-en-fila: lista -> número
(define (contar-dos-en-fila fila)
  (cond [(empty? fila) 0]
        [(= (car fila) 2) (+ 1 (contar-dos-en-fila (cdr fila)))]
        [else (contar-dos-en-fila (cdr fila))]))

;; contar-dos: tablero -> número
(define (contar-dos tablero)
  (cond [(empty? tablero) 0]
        [else (+ (contar-dos-en-fila (car tablero))
                 (contar-dos (cdr tablero)))]))

;; ============================================
;; INTERFAZ GRÁFICA
;; ============================================

;; color-segun-valor: número -> string
;; Devuelve el color según el valor de la baldosa
(define (color-segun-valor n)
  (cond [(= n 0) "light gray"]
        [(= n 2) "yellow"]
        [(= n 4) "orange"]
        [(= n 8) "red"]
        [(= n 16) "purple"]
        [(= n 32) "blue"]
        [(= n 64) "green"]
        [(= n 128) "cyan"]
        [(= n 256) "magenta"]
        [(= n 512) "pink"]
        [(= n 1024) "brown"]
        [(= n 2048) "gold"]
        [else "black"]))

;; dibujar-baldosa: número -> image
;; Dibuja una baldosa individual con borde negro (tamaño 80x80)
(define (dibujar-baldosa n)
  (cond [(= n 0) (overlay (square 80 "outline" "black")
                          (square 80 "solid" "light gray"))]
        [else (overlay (text (number->string n) 32 "white")
                       (square 80 "solid" (color-segun-valor n))
                       (square 80 "outline" "black"))]))

;; dibujar-fila: lista -> image
;; Dibuja una fila completa del tablero
(define (dibujar-fila fila)
  (cond [(empty? fila) (square 0 "solid" "white")]
        [else (beside (dibujar-baldosa (car fila))
                      (dibujar-fila (cdr fila)))]))

;; dibujar-tablero: tablero -> image
;; Dibuja el tablero completo
(define (dibujar-tablero tablero)
  (cond [(empty? tablero) (square 0 "solid" "white")]
        [else (above (dibujar-fila (car tablero))
                     (dibujar-tablero (cdr tablero)))]))

;; dibujar-puntaje: número -> image
;; Dibuja el texto del puntaje
(define (dibujar-puntaje puntaje)
  (text (string-append "Puntaje: " (number->string puntaje)) 36 "black"))

;; dibujar-mensaje: string -> image
;; Dibuja un mensaje (ganaste o game over)
(define (dibujar-mensaje msg)
  (text msg 48 "red"))

;; dibujar-todo: estado -> image
(define (dibujar-todo estado)
  (define tablero (car estado))
  (define puntaje (cdr estado))
  (define filas (obtener-filas tablero))
  (define columnas (obtener-columnas tablero))
  (define texto-tamaño (text (string-append "Tablero " (number->string filas) "x" (number->string columnas)) 24 "black"))
  (define texto-salir (text "Cierra la ventana para salir" 18 "gray"))
  (define espacio (rectangle 1 20 "solid" "white"))
  (define victoria (contiene-2048 tablero))
  (define terminado (juego-terminado? tablero))
  (cond [victoria (above (dibujar-puntaje puntaje)
                         texto-tamaño
                         (dibujar-tablero tablero)
                         (dibujar-mensaje "¡GANASTE!")
                         texto-salir
                         espacio)]
        [terminado (above (dibujar-puntaje puntaje)
                          texto-tamaño
                          (dibujar-tablero tablero)
                          (dibujar-mensaje "GAME OVER")
                          texto-salir
                          espacio)]
        [else (above (dibujar-puntaje puntaje)
                     texto-tamaño
                     (dibujar-tablero tablero)
                     texto-salir
                     espacio)]))

;; mover-con-tecla: estado tecla -> estado
;; Maneja las teclas de flecha
(define (mover-con-tecla estado tecla)
  (define tablero (car estado))
  (define puntaje (cdr estado))
  (define nuevo-tablero
    (cond [(key=? tecla "left") (mover-tablero-izquierda tablero)]
          [(key=? tecla "right") (mover-tablero-derecha tablero)]
          [(key=? tecla "up") (mover-tablero-arriba tablero)]
          [(key=? tecla "down") (mover-tablero-abajo tablero)]
          [else tablero]))
  (if (equal? tablero nuevo-tablero)
      estado
      (cons nuevo-tablero (+ puntaje (calcular-puntaje-ganado tablero nuevo-tablero)))))

;; calcular-puntaje-ganado: tablero tablero -> número
;; Calcula cuántos puntos se ganaron en el movimiento
(define (calcular-puntaje-ganado antes despues)
  (sumar-diferencias antes despues))

;; sumar-diferencias: tablero tablero -> número
;; Suma las diferencias de valores entre dos tableros
(define (sumar-diferencias t1 t2)
  (cond [(empty? t1) 0]
        [else (+ (sumar-diferencias-fila (car t1) (car t2))
                 (sumar-diferencias (cdr t1) (cdr t2)))]))

;; sumar-diferencias-fila: lista lista -> número
(define (sumar-diferencias-fila f1 f2)
  (cond [(empty? f1) 0]
        [else (+ (max 0 (- (car f2) (car f1)))
                 (sumar-diferencias-fila (cdr f1) (cdr f2)))]))

;; insertar-nueva-ficha: estado -> estado
;; Después de mover, inserta un 2 o 4 en una posición vacía
(define (insertar-nueva-ficha estado)
  (define tablero (car estado))
  (define puntaje (cdr estado))
  (define vacias (posiciones-vacias tablero))
  (cond [(empty? vacias) estado]
        [else (define nuevo-tablero (insertar-ficha-aleatoria tablero))
              (cons nuevo-tablero puntaje)]))

;; actualizar-juego: estado tecla -> estado
;; Función principal que actualiza el juego con cada tecla
(define (actualizar-juego estado tecla)
  (define tablero (car estado))
  (define puntaje (cdr estado))
  (define victoria (contiene-2048 tablero))
  (define terminado (juego-terminado? tablero))
  (cond [victoria estado]
        [terminado estado]
        [else (define nuevo-estado (mover-con-tecla estado tecla))
              (define nuevo-tablero (car nuevo-estado))
              (if (equal? tablero nuevo-tablero)
                  nuevo-estado
                  (insertar-nueva-ficha nuevo-estado))]))

;; crear-estado-inicial: -> estado
;; Crea el estado inicial del juego (tablero + puntaje 0)
(define (crear-estado-inicial)
  (cons (tablero-inicial) 0))

;; iniciar-juego: -> void
;; Lanza la ventana del juego
(define (iniciar-juego)
  (big-bang (crear-estado-inicial)
            (to-draw dibujar-todo)
            (on-key actualizar-juego)))


;; ============================================
;; INICIAR JUEGO
;; ============================================

(iniciar-juego)
