#lang racket

;Se lee el archivo donde se almacenan las ventas a realizar
;El formato es el siguiente: (lista de monedas a ingresar (codigo del producto))

(define archVentas (open-input-file "Venta.txt")) ;Abre el archivo creado previamente
(define Ventas (read archVentas)) ;lee la primera lista y la asocia con x


;Se lee el archivo donde se almacena la base de datos con todos los productos de la maquina
;El formato es el siguiente: (nombre (precio) (codigo) (cantidad) )

(define archProductos (open-input-file "Productos.txt")) ;Abre el archivo creado previamente
(define Productos (read archProductos)) ;lee la primera lista y la asocia con x

;Se crea el out file para la actualizacion de la base de datos de los productos despues de una venta
;(define updateProductos (open-output-file "Productos.txt" #:exists 'replace)) ;Si existe el archivo, lo reemplaza

(define (registroVentas Productos Ventas)
  ;Si la cantidad de productos es cero, la operacion se aborta
 (cond ([= 0 (car (cadddr (car Productos)))] "Sin productos")
       ;Si el dinero ingresado es mejor al costo, la operacion se aborta
       ([ <= (- [apply + (caar Ventas)] [caadr (car Productos)]) 0] "Fondos insuficientes")
       ;Si la operacion es exitosa, te devuelve el producto junto con el camboio y hace la llam
       (else (cons (list (caar Productos) (list (- [apply + (caar Ventas)] [caadr (car Productos)]))) [vending-machine Productos (cdr Ventas)])
             ;(write(inventory-update (caar (cddr (car Productos))) Productos) updateProductos)
                   )))
        
(define (vending-machine Productos Ventas)
  ;Revisa si la lista de Productos leida en el txt no esta vacia
  (cond ((and (null? Productos) (null? Ventas)) null)
        ; Si el codigo es igual al del producto se realiza la transacion
         ((equal? (get-codigo Ventas) (caar (cddr (car Productos)))) [registroVentas Productos Ventas])
         ;Si el codigo es diferente se realiza la llamada recursiva al resto de la lista
         (else (vending-machine (cdr Productos) Ventas))))
  ;(close-output-port updateProductos))


(define (get-codigo Ventas)
  (cond ((not (null? Ventas)) (car (map caadr Ventas)))))


(define (inventory-update code Productos)
 (replace (append (list(caar Productos))
                  (list(cadr (car Productos)))
                  (list(caddr (car Productos)))
                  (list(map (lambda (number)(- number 1 ))(cadddr (car Productos)))))code Productos '())) 

(define replace
  (lambda (s pos lst fin-lst)
    (cond ((null? lst) 0)
      ((zero? pos)(append (reverse (cons s fin-lst)) (rest lst)))
      (else(replace s (- pos 1) (rest lst) (cons (first lst) fin-lst))))))

;(close-output-port updateProductos)