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
;(write (append (cons (list (caar Productos) (cadr (car Productos)) (caddr (car Productos))) (list(map (lambda (lista) (- lista 1)) (cadddr (car Productos))))) (cdr Productos)) updateProductos)
;(close-output-port updateProductos)


(define (registroVentas Productos Ventas)
 (cond ([= 0 (car (cadddr (car Productos)))] "Sin productos")
       (([<= 0 (- [caadr (car Productos)][apply +(car Ventas)])]) (caar Productos))))
        
;(write (append (append  (list (caar Productos) (cadr (car Productos)) (caddr (car Productos))) (list(map (lambda (lista) (- lista 1)) (cadddr (car Productos))))) (cdr Productos)) updateProductos)
 
(define (vending-machine Productos Ventas)
  ;Revisa si la lista de Productos leida en el txt no esta vacia
  (cond ((and (null? Productos) (null? Ventas)) 0)
        ; Si el codigo es igual al del producto se realiza la transacion
         ((equal? (caadr Ventas) (caar (cddr (car Productos)))) (registroVentas Productos Ventas))
         ;Si el codigo es diferente se realiza la llamada recursiva al resto de la lista
         (else (vending-machine (cdr Productos) Ventas))))
         