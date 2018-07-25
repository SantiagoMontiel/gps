(setq grafo '(	(a (b f))
					(b (a c))
					(c (b d))
					(d (c n e))
					(e (d))
					(f (g ))
					(g (h))
					(h (i l))
					(i (m j))
					(j (k))
					(k (o))
					(l (b f)) 
					(m (l c))
					(n (j m))
					(o (e n))))

(setq diccionario '(	(a (PaseoColon Independencia))
							(b (PaseoColon Chile))
							(c (PaseoColon Mexico))
							(d (PaseoColon Venezuela))
							(e (PaseoColon Belgrano))
							(f (Independencia Balcarce))
							(g (Independencia Defensa))
							(h (Defensa Chile))
							(i (Defensa Mexico))
							(j (Defensa Venezuela))
							(k (Defensa Belgrano))
							(l (Balcarce Chile))
							(m (Balcarce Mexico))
							(n (Balcarce Venezuela))
							(o (Balcarce Belgrano))))

							
;Programa principal
(defun gps ()
	(print '(Introduce esquina origen))
	(setf  origen (read))
	(print '(Introduce esquina destino))
	(setf  destino (read))
	(print '(Desea la ruta mas rapida? '(S/N)))
	(setf  respuesta (read))
	(if (or (eq respuesta 'S) (eq respuesta 's))	(gpsMinimo origen destino diccionario grafo)
		(gpsTodos origen destino diccionario grafo))
)

(print (gps))

;Obtengo el camino mas corto entre dos calles
(defun gpsMinimo (i f dicc grafo)
	(mostrarRutas
		(list (caminoMinimo (mapearNodo i dicc) (mapearNodo f dicc) grafo)) dicc )
)

;Obtengo todos los caminos posibles entre dos calles dadas
(defun gpsTodos (i f dicc grafo)
	(mostrarRutas (obtenerCaminos (mapearNodo i dicc) (mapearNodo f dicc) grafo) dicc)
)

;Obtiene el valor del nodo dada una esquina
(defun mapearNodo (esquina dicc)
	(if (null dicc)	'CALLE_INEXISTENTE
		(if (equal (car(cdr (car dicc))) esquina)	(car (car dicc))
			(mapearNodo esquina (cdr dicc))))
)

;Obtiene el nombre de las intersecciones dado un nodo
(defun mapearEsquina (nodo &optional (dicc))
	(if (null dicc)	'EL_NODO_NO_EXISTE_EN_EL_MAPA
		(if (equal (car(car dicc)) nodo)	(car (cdr (car dicc)))
			(mapearEsquina nodo (cdr dicc))))
)
			
; Si el elemento x pertenece a la lista devuelve True sino devuelve False
(defun pertenece (x lista) 
	(cond
		( (eq x (car lista))	T)
		( (not (atom lista))	(pertenece x (cdr lista)))
		(T	nil)
	)
)		

;Devuelve una lista con todos los elementos de l1 que no estan en l2
(defun elementosDistintos(l1 l2)
	(if (null l1)	nil
		(if (pertenece (car l1)	l2 )	(elementosDistintos (cdr l1) l2)
			(cons (car l1) (elementosDistintos (cdr l1) l2))))
)

;Devuelve la lista de menor longitud entre dos listas dadas
(defun listaMenor (l1 l2)
	(if (< (length l1) (length l2))	l2
		l2)
)

;Obtiene la calle que tienen en comun dos esquinas
(defun callesEnComun (esquina1 esquina2)
	(if (null esquina1) 'ERROR_NO_TIENEN_CALLES_EN_COMUN
		(if (pertenece (car esquina1) esquina2)	(car esquina1)
			(callesEnComun (cdr esquina1) esquina2)))
)

;Obtiene todas las adyacencias del nodo
(defun obtenerAdyacencias (nodo grafo)
	(if (null grafo) 	'ERROR_EL_NODO_NO_EXISTE
		(if (eq nodo (car (car grafo)))	(car (cdr (car grafo)))
			(obtenerAdyacencias nodo (cdr grafo))))
)

;Armo una lista con todos los caminos que llegan al destino
(defun obtenerCaminos (i f grafo &optional (result nil) (tray (list (list i))))
	(if (null tray)	result
		;Si el camino llega a destino lo agrego a resultado y procesos los siguientes caminos
		(if (eq (car (car tray)) f)	(obtenerCaminos i f grafo (cons (reverse (car tray)) result) (cdr tray))
			;Si llegue a un bucle lo descarto y proceso los siguientes.
			(if (null (puedoAvanzar tray grafo))	(obtenerCaminos i f grafo result (cdr tray))
				;Si puedo ir a un nodo que nunca fui lo agrego al tray y sigo procesando.
				(obtenerCaminos i f grafo result (agregarCaminos tray)))))
)

;Agrego todas las adyacencias por las que no pase al camino que estoy recorriendo
(defun agregarCaminos (caminos)
	(append (mapcar (lambda (nodo) (cons nodo (car caminos))) 
													  (elementosDistintos (obtenerAdyacencias (car (car caminos)) grafo) (car caminos))) (cdr caminos))
)			

;Devuelve true si puedo recorrer un nodo que nunca recorri o nil si ya pase por todos los nodos 
(defun puedoAvanzar (caminos grafo)
	(if (null (elementosDistintos (obtenerAdyacencias (car (car caminos)) grafo) (car caminos)))	nil  T)
)

;Obtiene la informacion que debe recorrer para llegar a destino para todos los caminos posibles
(defun mostrarRutas (recorridos dicc &optional (rutas nil))
	(if (null recorridos) rutas
		(mostrarRutas (cdr recorridos) dicc (append rutas (list (obtenerDescripcion (relacionarIntersecciones (car recorridos) dicc))))))
)

;Devuelve en una lista el nombre de las calles por las que tiene que pasar tantas veces como cuadras tiene que recorrer
(defun relacionarIntersecciones (recorrido dicc &optional (calles nil)  )
	(if (or (null recorrido) (null (cdr recorrido))) calles
		(relacionarIntersecciones (cdr recorrido) 
									    dicc  
									   (append calles (list (callesEnComun (mapearEsquina (car recorrido) dicc )
																						    (mapearEsquina (car (cdr recorrido)) dicc))))))
)

;Procesa las intersecciones y devuelve la descripcion de la ruta
(defun obtenerDescripcion (intersecciones)		
	(procesar intersecciones '() 0 (car intersecciones))
)

(defun procesar (camino descripcion contador calleActual)
	(if (null camino)	(append descripcion (list 'RECORRER contador 'CUADRAS 'POR  calleActual 'HASTA 'LLEGAR 'A 'DESTINO))
		(if (equal (car camino) calleActual)	(procesar (cdr camino) descripcion (+ contador 1) calleActual)	
			(procesar (cdr camino) (append descripcion (list 'RECORRER contador 'CUADRAS 'POR  calleActual 'Y 'DOBLAR 'EN (car camino))) 1 (car camino))))
)

;Devuelve el camino mas corto entre dos nodos
(defun caminoMinimo (i f grafoP &optional (result nil) (tray (list (list i))))
	(obtenerMinimo (obtenerCaminos i f grafoP result tray))
)

;Devuelve la lista mas pequeña dada una lista de listas
(defun obtenerMinimo (lista)
	(if (null lista) nil
		(if (null (cdr lista))	(car lista)
			(listaMenor (car lista) (obtenerMinimo (cdr lista)))))
)