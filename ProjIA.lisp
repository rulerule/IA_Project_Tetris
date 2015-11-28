;GRUPO 46        ANDRE GONCALVES 65956

;LOADS
;(load (compile-file "utils.lisp"))


;STRUCTS
;struct tipo estado
(defstruct estado pontos pecas-por-colocar pecas-colocadas tabuleiro)

;struct tipo problema
(defstruct problema estado-inicial solucao accoes resultado custo-caminho)

;**************************************************FUNCOES AUXILIARES*************************************************

;FUNCAO AUXILIAR PARA COPIA DE ARRAYS
(defun copy-array (array &key (undisplace nil))
  (declare (type array array))
  (let ((copy (%make-array-with-same-properties array undisplace)))
    (unless (array-displacement copy)
      (dotimes (n (array-total-size copy))
        (setf (row-major-aref copy n) (row-major-aref array n))))
    copy))

(defun %make-array-with-same-properties (array undisplace)
  (apply #'make-array
	 (list* (array-dimensions array)
		:element-type (array-element-type array)
		:adjustable (adjustable-array-p array)
		:fill-pointer (when (array-has-fill-pointer-p array)
				(fill-pointer array))
		(multiple-value-bind (displacement offset)
		    (array-displacement array)
		  (when (and displacement (not undisplace))
		    (list :displaced-to displacement
			  :displaced-index-offset offset))))))


;FUNCAO PARA VERIFICACAO DE ALTURA DE UMA COLUNA
(defun verifica-altura(tabuleiro inteiro)
	(let ((linhas 17) (aux))
		(loop
			(setq aux (aref tabuleiro linhas inteiro))
			(if (eq linhas 0) (return 0))
			(setq linhas (- linhas 1))
			(when (not (eq aux nil)) (return  (+ linhas 2))))))


;FUNCAO PARA VERIFICAR SE ALGUMA DAS COLUNAS TOCA NO TOPO
(defun verifica-altura-tabuleiro(tabuleiro)
		(let ((colunas 0) (aux))
			(loop
				(setq aux (verifica-altura tabuleiro colunas))
				(if (> aux 17) (return T))
				(setq colunas (+ colunas 1))
				(when (eq colunas 9) (return nil)))))

;FUNCAO PARA VERIFICAR SE UMA LINHA ESTA COMPLETA
(defun verifica-linha-completa(tabuleiro inteiro)
	(let ((colunas 9) (aux))
		(loop
		(setq aux (aref tabuleiro inteiro colunas))
			(if (eq aux nil) (return nil))
		(setq colunas (- colunas 1))
			(when (< colunas 0) (return T)))))

;FUNCAO PARA VERIFICAR OS LIMITES
(defun verifica-limites (linha coluna)
	(if (or (or (< linha 0) (> linha 17)) (or (< coluna 0) (> coluna 9))) nil T)
	)

;FUNCAO PARA APAGAR UMA LINHA DO TABULEIRO (COLOCAR TODA A LINHA A NIL)
(defun apaga-linha(tabuleiro linha)
	(let ((colunas 9))
		(loop
			(setf (aref tabuleiro linha colunas) nil)
				(setq colunas (- colunas 1))
					(when (eq colunas 0) (progn
					(setf(aref tabuleiro linha colunas) nil)
					(return tabuleiro))))))

;ARRAY SLICER (USAR UMA LINHA DO ARRAY BIDIMENSIONAL)
(defun array-slice (arrayTeste row)
	(make-array (array-dimension arrayTeste 1)
		:displaced-to arrayTeste
			:displaced-index-offset (* row (array-dimension arrayTeste 1))))

;ESCREVER UMA LINHA EM CIMA DE OUTRA ( DAR DROPDOWN DO ARRAY , ESCRITA DE TODAS AS LINHAS SUPERIORES NA INFERIOR) *****SINGLE LINE*****
(defun Overwrite (linhaInicial linhaFinal)
	(let ((colunas 9))
	(loop
		(setf (aref linhaInicial colunas) (aref linhaFinal colunas))
			(setq colunas (- colunas 1))
			(when (eq colunas 0) (progn
				(setf (aref linhaInicial colunas) (aref linhaFinal colunas))
				(return linhaFinal))))))

;REPLACEMENT DA LINHA DO TABULEIRO ATE AO TOPO!! (APLICACAO DO OVERWRITE ATE AO TOPO DO TABULEIRO)
(defun Replacement (tabuleiro linha)
	(let ((topo 17) (inicial) (final) (linhaCounter linha))
		(loop
			(setq inicial (array-slice tabuleiro linhaCounter))
			(setq final (array-slice tabuleiro (+ linhaCounter 1)))
			(Overwrite inicial final)
			(setq linhaCounter ( + linhaCounter 1))
			(when (eq linhaCounter topo) (progn
				(apaga-linha tabuleiro 17)
				(return tabuleiro))))))

;VERIFICAR SE O TABULEIRO TEM ALGUMA LINHA COMPLETA (INEFECIENTE SO DEVIA VERIFICAR 4 LINHAS)
(defun LinhasCompletas (tabuleiro)
	(let ((numeroLinhasCompletas 0) (incremento 0))
			(loop
				(if  (tabuleiro-linha-completa-p tabuleiro incremento) (setf numeroLinhasCompletas (+ numeroLinhasCompletas 1)))
				(setq incremento (+ incremento 1))
				(when (equal incremento 17)
				(return numeroLinhasCompletas)))))

; QUAL A LINHA COMPLETA
(defun NumeroLinhaCompleta (tabuleiro)
	(let ((incremento 0))
			(loop
				(if  (tabuleiro-linha-completa-p tabuleiro incremento) (return incremento))
				(setq incremento (+ incremento 1))
				(when (equal incremento 17)
				(return nil)))))

;NUMERO DE PONTOS POR LINHAS FEITAS
(defun calculoDePontos (inteiro)
	(let ((valor))
	(cond ((equal inteiro 1) (setf valor 100))
		  ((equal inteiro 2) (setf valor 300))
		  ((equal inteiro 3) (setf valor 500))
		  ((equal inteiro 4) (setf valor 800))
			(T (setf valor 0)))
			valor))



;TABULEIRO DO ESTADO
(defun getTabuleiro(estado)
	(estado-tabuleiro estado))

; LISTA DE PECAS POR COLOCAR
(defun getPecasPorColocar (estado)
	(estado-pecas-por-colocar estado))

; PONTOS
(defun getPontos (estado)
	(estado-pontos estado))

;PECAS COLOCADAS
(defun getPecasColocadas (estado)
	(estado-pecas-colocadas estado))


;VERIFICA LISTA DE PECAS POR COLOCAR VAZIA (devolve T caso vazia , n caso ainda exista pecas)
(defun VerificaListaPorColocarVazia (estado)
	(if (eq  (estado-pecas-por-colocar estado)  nil) T nil))

;VERIFICAR SE O ESTADO E FINAL
(defun verifica-estado-final (estado)
		(if (or (tabuleiro-topo-preenchido-p (estado-tabuleiro estado)) (VerificaListaPorColocarVazia estado)) T nil))

;AUXILIAR DE CUSTO-OPORTUNIDADE
(defun contaPontos (lst)
	(let ((result 0))
		(dolist (element lst)
			(cond ((equal element 'i) (setf result (+ result 800)))
				  ((equal element 'l) (setf result (+ result 500)))
				  ((equal element 'j) (setf result (+ result 500)))
				  ((equal element 'o) (setf result (+ result 300)))
				  ((equal element 's) (setf result (+ result 300)))
				  ((equal element 'z) (setf result (+ result 300)))
				  ((equal element 't) (setf result (+ result 300)))
				  (T t))
		)
	 result))



;ACTUALIZACAO DE LISTAS APOS ACCAO
(defun actualizaListas (estado)
	(setf (estado-pecas-colocadas estado) (append (list (first (estado-pecas-por-colocar estado))) (estado-pecas-colocadas estado)))
	(setf (estado-pecas-por-colocar estado) (rest (estado-pecas-por-colocar estado)))
	)

;VERIFICACAO DE COLUNA MAIS ALTA EM QUE PODE FICAR A PECA
(defun verifica-altura-peca (tabuleiro coluna peca)
(let ((numeroColunas) (DimensaoPeca) (colunaMaisAlta 0) (aux 0) (colunaFinal 0))
  (setq DimensaoPeca (array-dimensions peca))
  (setq numeroColunas (first (rest DimensaoPeca)))
  (setq aux  (- numeroColunas 1))
    (loop
      (if ( > (tabuleiro-altura-coluna tabuleiro (+ aux coluna)) colunaMaisAlta)
        (setq colunaMaisAlta (tabuleiro-altura-coluna tabuleiro (+ aux coluna))))
      (if ( equal (aref peca 0 aux) nil)  (setq colunaMaisAlta (- colunaMaisAlta 1)) (setq colunaFinal colunaMaisAlta))
      (if (> colunaFinal colunaMaisAlta) (setq colunaMaisAlta colunaFinal))
      (setq aux (- aux 1))
      (if (< colunaMaisAlta 0) (setq colunaMaisAlta 0))
      (when ( < aux 0) (return colunaMaisAlta))
    )
)
)


;COLOCAR PECA NO TABULEIRO
(defun colocaPeca (tabuleiro coluna peca)
	(let ((linhaParaColocar) (DimensaoPeca) (linhasPeca) (colunasPeca) (linhas 0) (colunas 0))
		(setq DimensaoPeca (array-dimensions peca))
		(setq linhasPeca (first DimensaoPeca))
		(setq colunasPeca (first (rest DimensaoPeca)))
		(setq linhaParaColocar (verifica-altura-peca tabuleiro coluna peca))
    (if (> (+ linhasPeca LinhaParaColocar) 17) nil (progn

		(loop

			(loop
        (if (equal (aref tabuleiro (+ linhaParaColocar linhas) (+ coluna colunas)) nil)
				(setf (aref tabuleiro (+ linhaParaColocar linhas) ( + coluna colunas)) (aref peca linhas colunas)))
				(setq colunas (+ colunas 1))
				(when (equal  colunas  colunasPeca)
				(return)))

			(setq linhas (+ linhas 1))
			(setq colunas 0)
			(when (equal linhas linhasPeca) (return T)))
      ))
	)
)

;APAGAR VARIAS LINHAS DO TABULEIRO
(defun ReplaceLinhas (tabuleiro)
	(let ((controle 0))
		(loop
			(setq controle (numeroLinhaCompleta tabuleiro))
			(if (not (equal controle nil)) (replacement tabuleiro controle))
			(when (equal controle nil) (return nil))))
)


;************VALIDACAO BASICA DE PECAS*********************

(defun validoI0 (inteiro)
	(if (and (>= inteiro 0) (<= inteiro 9)) T nil))

(defun validoI1 (inteiro)
	(if (and (>= inteiro 0) (<= inteiro 6)) T nil))

(defun validoL0 (inteiro)
	(if (and (>= inteiro 0) (<= inteiro 8)) T nil))

(defun validoL1 (inteiro)
	(if (and (>= inteiro 0) (<= inteiro 7)) T nil))

(defun validoL2 (inteiro)
	(if (and (>= inteiro 0) (<= inteiro 8)) T nil))

(defun validoL3 (inteiro)
	(if (and (>= inteiro 0) (<= inteiro 7)) T nil))

(defun validoJ0 (inteiro)
	(if (and (>= inteiro 0) (<= inteiro 8)) T nil))

(defun validoJ1 (inteiro)
	(if (and (>= inteiro 0) (<= inteiro 7)) T nil))

(defun validoJ2 (inteiro)
	(if (and (>= inteiro 0) (<= inteiro 8)) T nil))

(defun validoJ3 (inteiro)
	(if (and (>= inteiro 0) (<= inteiro 7)) T nil))

(defun validoO0 (inteiro)
	(if (and (>= inteiro 0) (<= inteiro 8)) T nil))

(defun validoS0 (inteiro)
	(if (and (>= inteiro 0) (<= inteiro 7)) T nil))

(defun validoS1 (inteiro)
	(if (and (>= inteiro 0) (<= inteiro 8)) T nil))

(defun validoZ0 (inteiro)
	(if (and (>= inteiro 0) (<= inteiro 7)) T nil))

(defun validoZ1 (inteiro)
	(if (and (>= inteiro 0) (<= inteiro 8)) T nil))

(defun validoT0 (inteiro)
	(if (and (>= inteiro 0) (<= inteiro 7)) T nil))

(defun validoT1 (inteiro)
	(if (and (>= inteiro 0) (<= inteiro 8)) T nil))

(defun validoT2 (inteiro)
	(if (and (>= inteiro 0) (<= inteiro 7)) T nil))

(defun validoT3 (inteiro)
	(if (and (>= inteiro 0) (<= inteiro 8)) T nil))


;*******LISTA DE ACCOES PECAS***************
(defun Accoes-I (estado)
	(let ((colunas 0) (listaAccoes ()) (aux))
		(loop
			(if (null estado) (return nil))
			(setq aux (cria-accao colunas peca-i0))
			(setf listaAccoes (append  listaAccoes (list aux)))
			(setq colunas (+ colunas 1))
			(when (equal (validoi0 colunas) nil)(progn (setq colunas 0)
			(return))))
		(loop
			(setq aux (cria-accao colunas peca-i1))
			(setf listaAccoes (append  listaAccoes (list aux)))
			(setq colunas (+ colunas 1))
			(when (equal (validoi1 colunas) nil)
			(return listaAccoes)))))


(defun Accoes-L (estado)
	(let ((colunas 0) (listaAccoes ()) (aux))
		(loop
			(if (null estado) (return nil))
			(setq aux (cria-accao colunas peca-l0))
			(setf listaAccoes (append  listaAccoes (list aux)))
			(setq colunas (+ colunas 1))
			(when (equal (validol0 colunas) nil)(progn (setq colunas 0)
			(return))))
		(loop
			(setq aux (cria-accao colunas peca-l1))
			(setf listaAccoes (append  listaAccoes (list aux)))
			(setq colunas (+ colunas 1))
			(when (equal (validol1 colunas) nil)(progn (setq colunas 0)
			(return))))
		(loop
			(setq aux (cria-accao colunas peca-l2))
			(setf listaAccoes (append  listaAccoes (list aux)))
			(setq colunas (+ colunas 1))
			(when (equal (validol2 colunas) nil)(progn (setq colunas 0)
			(return))))
		(loop
			(setq aux (cria-accao colunas peca-l3))
			(setf listaAccoes (append  listaAccoes (list aux)))
			(setq colunas (+ colunas 1))
			(when (equal (validol3 colunas) nil)
			(return listaAccoes)))))


(defun Accoes-J (estado)
	(let ((colunas 0) (listaAccoes ()) (aux))
		(loop
			(if (null estado) (return nil))
			(setq aux (cria-accao colunas peca-j0))
			(setf listaAccoes (append  listaAccoes (list aux)))
			(setq colunas (+ colunas 1))
			(when (equal (validoj0 colunas) nil)(progn (setq colunas 0)
			(return))))
		(loop
			(setq aux (cria-accao colunas peca-j1))
			(setf listaAccoes (append  listaAccoes (list aux)))
			(setq colunas (+ colunas 1))
			(when (equal (validoj1 colunas) nil)(progn (setq colunas 0)
			(return))))
		(loop
			(setq aux (cria-accao colunas peca-j2))
			(setf listaAccoes (append  listaAccoes (list aux)))
			(setq colunas (+ colunas 1))
			(when (equal (validoj2 colunas) nil)(progn (setq colunas 0)
			(return))))
		(loop
			(setq aux (cria-accao colunas peca-j3))
			(setf listaAccoes (append  listaAccoes (list aux)))
			(setq colunas (+ colunas 1))
			(when (equal (validoj3 colunas) nil)
			(return listaAccoes)))))


(defun Accoes-O (estado)
	(let ((colunas 0) (listaAccoes ()) (aux))
		(loop
			(if (null estado) (return nil))
			(setq aux (cria-accao colunas peca-o0))
			(setf listaAccoes (append  listaAccoes (list aux)))
			(setq colunas (+ colunas 1))
			(when (equal (validoo0 colunas) nil)
			(return listaAccoes)))))


(defun Accoes-S (estado)
	(let ((colunas 0) (listaAccoes ()) (aux))
		(loop
			(if (null estado) (return nil))
			(setq aux (cria-accao colunas peca-s0))
			(setf listaAccoes (append  listaAccoes (list aux)))
			(setq colunas (+ colunas 1))
			(when (equal (validos0 colunas) nil)(progn (setq colunas 0)
			(return))))
		(loop
			(setq aux (cria-accao colunas peca-s1))
			(setf listaAccoes (append  listaAccoes (list aux)))
			(setq colunas (+ colunas 1))
			(when (equal (validos1 colunas) nil)
			(return listaAccoes)))))


(defun Accoes-Z (estado)
	(let ((colunas 0) (listaAccoes ()) (aux))
		(loop
			(if (null estado) (return nil))
			(setq aux (cria-accao colunas peca-z0))
			(setf listaAccoes (append  listaAccoes (list aux)))
			(setq colunas (+ colunas 1))
			(when (equal (validoz0 colunas) nil)(progn (setq colunas 0)
			(return))))
		(loop
			(setq aux (cria-accao colunas peca-z1))
			(setf listaAccoes (append  listaAccoes (list aux)))
			(setq colunas (+ colunas 1))
			(when (equal (validoz1 colunas) nil)
			(return listaAccoes)))))


(defun Accoes-T (estado)
	(let ((colunas 0) (listaAccoes ()) (aux))
		(loop
			(if (null estado) (return nil))
			(setq aux (cria-accao colunas peca-t0))
			(setf listaAccoes (append  listaAccoes (list aux)))
			(setq colunas (+ colunas 1))
			(when (equal (validot0 colunas) nil)(progn (setq colunas 0)
			(return))))
		(loop
			(setq aux (cria-accao colunas peca-t1))
			(setf listaAccoes (append  listaAccoes (list aux)))
			(setq colunas (+ colunas 1))
			(when (equal (validot1 colunas) nil)(progn (setq colunas 0)
			(return))))
		(loop
			(setq aux (cria-accao colunas peca-t2))
			(setf listaAccoes (append  listaAccoes (list aux)))
			(setq colunas (+ colunas 1))
			(when (equal (validot2 colunas) nil)(progn (setq colunas 0)
			(return))))
		(loop
			(setq aux (cria-accao colunas peca-t3))
			(setf listaAccoes (append  listaAccoes (list aux)))
			(setq colunas (+ colunas 1))
			(when (equal (validot3 colunas) nil)
			(return listaAccoes)))))

;**************************************************************TIPOS************************************************************

;****************************************TIPO ACCAO****************************************

(defun cria-accao (inteiro biarray)
	(cons inteiro biarray))

(defun accao-coluna (accao)
	(first accao))

(defun accao-peca (accao)
	(rest accao))


;***********************************TIPO TABULEIRO***************************************

(defun cria-tabuleiro ()
	(make-array '(18 10) :initial-element nil))

(defun copia-tabuleiro (tabuleiro)
	(copy-array tabuleiro))

(defun tabuleiro-preenchido-p (tabuleiro linha coluna)
	(if (eq (aref tabuleiro linha coluna) nil)nil T))

(defun tabuleiro-altura-coluna (tabuleiro coluna)
	(verifica-altura tabuleiro coluna))

(defun tabuleiro-linha-completa-p (tabuleiro linha)
	(verifica-linha-completa tabuleiro linha))

(defun tabuleiro-preenche! (tabuleiro linha coluna)
	(if (verifica-limites linha coluna)
	(setf (aref tabuleiro linha coluna) T)))

(defun tabuleiro-remove-linha! (tabuleiro linha)
	(replacement tabuleiro linha))

(defun tabuleiro-topo-preenchido-p (tabuleiro)
	(verifica-altura-tabuleiro tabuleiro))

(defun tabuleiros-iguais-p (tabuleiro1 tabuleiro2)
	(equalp tabuleiro1 tabuleiro2))

(defun tabuleiro->array (tabuleiro)
	(copia-tabuleiro tabuleiro))

(defun array->tabuleiro (arrayT)
	(copia-tabuleiro arrayT))


;****************************************TIPO ESTADO****************************************


(defun copia-estado (estado)
	(let ((pontosAux) (pecasPorColocarAux) (pecasColocadasAux) (tabuleiroAux) (estadoFinal))
		(setq pontosAux (getPontos estado))
		(setq pecasPorColocarAux  (copy-list (getPecasPorColocar estado)))
		(setq pecasColocadasAux (copy-list (getPecasColocadas estado)))
		(setq tabuleiroAux (copy-array (getTabuleiro estado)))
	(setq estadoFinal (make-estado :pontos pontosAux :pecas-por-colocar pecasPorColocarAux :pecas-colocadas pecasColocadasAux :tabuleiro tabuleiroAux))
	estadoFinal))

(defun estados-iguais-p (estado1 estado2)
	(equalp estado1 estado2))

(defun estado-final-p (estado)
	(verifica-estado-final estado))


;******************************************************************************FUNCOES****************************************************************************

	;VERIFICA SE O ESTADO E SOLUCAO
(defun solucao (estado)
	(if (and ( not (tabuleiro-topo-preenchido-p (estado-tabuleiro estado))) (VerificaListaPorColocarVazia estado)) T nil))

	;ACCOES POSSIVEIS
(defun accoes (estado)
	(let ((pecas) (proximapeca))
      (if (equal (estado-final-p estado) nil) (progn
			(setq pecas (getPecasPorColocar estado))
			(setq proximapeca (first pecas))
			(cond ((equal proximapeca 'i) (Accoes-I estado))
				  ((equal proximapeca 'l) (Accoes-L estado))
				  ((equal proximapeca 'j) (Accoes-J estado))
				  ((equal proximapeca 'o) (Accoes-O estado))
				  ((equal proximapeca 's) (Accoes-S estado))
				  ((equal proximapeca 'z) (Accoes-Z estado))
				  (T (Accoes-T estado))))
          nil)
  )
)


;APLICAR UMA ACCAO A UM ESTADO
(defun resultado (estado accao)
	(let ((estado2) (aux))
	(setf estado2 (copia-estado estado))
	(progn
	(actualizaListas estado2)
	(setq aux(colocaPeca (getTabuleiro estado2) (accao-coluna accao) (accao-peca accao)))
  (if (equal aux T) (progn
	(if (> (LinhasCompletas (getTabuleiro estado2)) 0) (progn  (setf (estado-pontos estado2) (+ (getPontos estado2) (calculoDePontos (LinhasCompletas (getTabuleiro estado2)))))
	(replaceLinhas (getTabuleiro estado2))))estado2)
	estado2))))


;PONTOS NEGATIVOS (E ISTO O OBJECTIVO)
(defun qualidade (estado)
	(- (getPontos estado)))

;CUSTO-OPORTUNIDADE
(defun custo-oportunidade (estado)
	( - (contaPontos (getPecasColocadas estado)) (getPontos estado)))


;******************************************************************************HEURISTICAS****************************************************************************








;******************************************************************************PROCURAS****************************************************************************


;PROCURA PROFUNDIDADE PRIMEIRO (FALTA ABSTRACAO) MISS TESTE 16
(defun procura-pp (problema)
  (let ((listaAccoesPeca) (accaoUtilizar) (listaFinal nil) (estadoCicloPrincipal)(estadoATestar)(estadoAnterior)(accaoFinal) (funcResultado) (funcSolucao) (funcAccoes))
      (setq estadoCicloPrincipal (problema-estado-inicial problema))
      (setq funcResultado (problema-resultado problema))
      (setq funcSolucao (problema-solucao problema))
      (setq funcAccoes (problema-accoes problema))
    (loop
      (setq listaAccoesPeca (funcall funcAccoes estadoCicloPrincipal))
      (if (null listaAccoesPeca) (return nil))
      (setq listaAccoesPeca (reverse listaAccoesPeca))
      (setq accaoUtilizar (first listaAccoesPeca))
      (setq estadoATestar estadoCicloPrincipal)

      (loop
        (setq estadoAnterior estadoATestar)
        (setq estadoATestar (funcall funcResultado estadoATestar accaoUtilizar))
        (setq listaAccoesPeca (rest listaAccoesPeca))
        (setq accaoFinal accaoUtilizar)
        (setq accaoUtilizar (first listaAccoesPeca))
        (if (equal accaoUtilizar nil)(return nil))
        (when (not (equalp (estado-tabuleiro estadoATestar) (estado-tabuleiro estadoAnterior)))
        (return)))

        (if (equalp (estado-tabuleiro estadoATestar) (estado-tabuleiro estadoAnterior)) (return nil))


      (setq estadoCicloPrincipal (funcall funcResultado estadoCicloPrincipal accaoFinal))
      (setq listaFinal (append listaFinal (list accaoFinal)))
      (when (equal (funcall funcSolucao estadoCicloPrincipal) t) (return listaFinal)))))


;PROCURA A*
(defun procura-A* (problema heuristica)
  (let ((listaAccoesPeca) (accaoUtilizar) (listaFinal nil) (estadoCicloPrincipal) (valorCaminho)
    (estadoATestar) (funcResultado) (funcSolucao) (funcAccoes) (funcCusto) (valorCaminhoAux) (pecasAux)
    (AccaoUtilizarAux)(estadoATestarAux)(valorCaminho2) (accaoBest) (contadorLast) (estadoTeste) (contador1Peca))
    (setq estadoCicloPrincipal (problema-estado-inicial problema))
    (setq funcResultado (problema-resultado problema))
    (setq funcSolucao (problema-solucao problema))
    (setq funcAccoes (problema-accoes problema))
    (setq funcCusto (problema-custo-caminho problema))
    (setq valorCaminho (funcall heuristica estadoCicloPrincipal))
    (setq valorCaminho ( + valorCaminho 2))
    (setq valorCaminho 100000)
    (setq valorCaminhoAux valorCaminho)
    (setq contador1Peca valorCaminhoAux)


    (loop
     (setq listaAccoesPeca (funcall funcAccoes estadoCicloPrincipal))
     (if (null listaAccoesPeca) (return nil))
     ;(setq listaAccoesPeca (reverse listaAccoesPeca))
     (setq accaoUtilizar (first listaAccoesPeca))
     (setq estadoATestar estadoCicloPrincipal)
     (setq accaoBest accaoUtilizar)

       (loop
     (setq estadoATestar (funcall funcResultado estadoATestar accaoUtilizar))
     (setq pecasAux (funcall funcAccoes estadoATestar))
     (setq contadorLast (funcall funcCusto estadoATestar))
              (loop
                  (if (null pecasAux) (return))
                  ;(setq pecasAux (reverse pecasAux))
                  (setq AccaoUtilizarAux (first pecasAux))
                  (setq estadoATestarAux estadoATestar)
                  (setq estadoATestarAux (funcall funcResultado estadoATestarAux accaoUtilizarAux))
                  (setq valorCaminho2 (funcall funcCusto estadoATestarAux))
                  (if ( < valorCaminho2 valorCaminhoAux) (setq valorCaminhoAux valorCaminho2))
                  (setq pecasAux (rest pecasAux))
                  (setq accaoUtilizarAux (first pecasAux))
                  (setq estadoATestarAux estadoATestar)
                  (when (equal accaoUtilizarAux nil) (return)))

      (if (< contadorLast contador1Peca) (progn (setq contador1Peca contadorLast) (setq accaoBest accaoUtilizar)))
      (if (equal contadorLast valorCaminhoAux) (setq accaoBest accaoUtilizar))

     (if (< valorCaminhoAux valorCaminho) (progn
         (setq valorCaminho valorCaminhoAux)
         (setq accaoBest accaoUtilizar)))


     (setq listaAccoesPeca (rest listaAccoesPeca))
     (setq accaoUtilizar (first listaAccoesPeca))
     (setq estadoTeste estadoATestar)
     (setq estadoATestar estadoCicloPrincipal)
     (when (equal accaoUtilizar nil) (return)))

      (if (equalp (estado-tabuleiro (funcall funcResultado estadoTeste accaoBest)) (estado-tabuleiro estadoCicloPrincipal)) (return nil))

         (setq estadoCicloPrincipal (funcall funcResultado estadoCicloPrincipal accaoBest))
         (setq listaFinal (append listaFinal (list accaoBest)))
         (when (equal (funcall funcSolucao estadoCicloPrincipal) t) (return listaFinal)))))


;PROCURA-BEST
(defun heuristicaAuxiliar (estado)
  (qualidade estado))


(defun procura-best (array1 listaPecas)
  (let ((estadoAuxiliar) (problemaAuxiliar))
      (setq estadoAuxiliar (make-estado :pontos 0 :pecas-por-colocar listaPecas :pecas-colocadas() :tabuleiro (array->tabuleiro array1)))
      (setq problemaAuxiliar (make-problema :estado-inicial estadoAuxiliar :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'qualidade))
    (procura-A* problemaAuxiliar #'heuristicaAuxiliar)))
    ;  (procura-pp problemaAuxiliar)))


;(load "utils.fas")
