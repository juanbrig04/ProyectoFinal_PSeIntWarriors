Proceso Proyecto_Unificado
	
	Definir OPCION_MENU como entero 
	Definir flagInicio Como Entero
	flagInicio <- 0
	//------------------------------------------------VARIABLES RULETA-----------------------------------------------------------------------------
	Definir dineroDisponibleRuleta, montoApostar, opcionApuesta, numRuleta, gananciaRuleta Como Entero
	Definir apuestaRuleta, colorRuleta, continuarJugandoRuleta, montoTexto, opcionTexto Como Cadena
	Dimension vRojo[18]
	//------------------------------------------------VARIABLES RULETA-----------------------------------------------------------------------------
	
	//----------------------------------------------VARIABLES JUEGO INTERACTIVO-----------------------------------------------------------------------------
	definir i, vida, energia, TAM, opc_inicio_historia, amuleto, llave, eleccionVal1, Eleccion_1Comunidad Como Entero
	definir Eleccion_1, Eleccion_1Valor, Eleccion_2Valor, Eleccion_1Puente, EleccionFinal, Elec_secreto Como Entero
	definir nombre_jugadorHistoria, AgregarObjeto como caracter 
	definir inventario Como Entero
	
	vida <- 100
	energia <- 100
	amuleto <- 0
	llave <- 0
	TAM <- 4
	
	Dimension inventario[TAM]
	inventario[0] <- 0//"Fruto Místico"
	inventario[1] <- 0//"Piedra de Luz"
	inventario[2] <- 0//"Amuleto del Cometa"
	inventario[3] <- 0//"Llave Antigua"
	//----------------------------------------------------FIN DE VARIABLES JUEGO INTERACTIVO-------------------------------------------------------------
	
	//tragamonedas
	Definir opcionMenu Como Entero
    Definir menuActivo Como Logico
    Definir Volver_al_menu Como Cadena // Declarar variable dummy para instrucciones
    
    menuActivo <- Verdadero
	
	//----------------------------------------------------------COMIENZO DEL MAIN--------------------------------------------------------------------------
	
	//pantalla de inicio
	Repetir
		Limpiar Pantalla
		OPCION_MENU <- 0
		Repetir
			Escribir "========================================================================================================================"
			Escribir "                                                 MENÚ PRINCIPAL"
			Escribir "========================================================================================================================"
			Escribir "Ingrese 1 para jugar al black jack" 
			Escribir "Ingrese 2 para jugar a la ruleta" 
			Escribir "Ingrese 3 para jugar al tragamonedas" 
			Escribir "Ingrese 4 para jugar a la historia interactiva" 
			Escribir "Ingrese 5 para salir del programa " 
			Escribir " " 		
			Escribir " " 
			Leer OPCION_MENU
		Hasta Que OPCION_MENU >= 1 O OPCION_MENU <= 5
		
		Esperar Tecla
		Limpiar Pantalla
		
		Segun OPCION_MENU hacer
			1:
				
				
				Repetir
					Si flagInicio = 0 Entonces
						Definir cantidadMazos, j, p, v, indice, totalCartas Como Entero
						Definir tempMazoFinal, tempPalos, tempNombreCarta Como Cadena
						Definir mazoFinal Como Cadena
						Definir palos, valores Como Cadena
						Definir op, opRepe, opPosibleBlackJackDealer, opDoblar Como Caracter
						Definir valoresCartas, flagCantMazos, tempValores, flag, flagCartas, flagRepetir, flagSaldo, flagApuesta, blackjackJugador, blackjackDealer, flagDoblar, flagPosibleBlackJackDealer Como Entero
						Definir Saldobj, apuesta Como Real
						flag <- 0
						flagApuesta <- 0
						flagRepetir <- 0
						flagCantMazos <- 0
					FinSi
					// Solicitar cantidad de mazos (mínimo 1)
					Si flag = 0 Entonces
						Repetir
							Si flagCantMazos = 1 Entonces
								Escribir "Por favor, ingrese una cantidad válida"
								Escribir ""
							FinSi
							Escribir "Ingrese la cantidad de mazos a utilizar (mínimo 1): "
							Leer cantidadMazos
							flagCantMazos <- 1
						Hasta Que cantidadMazos >= 1
						flag <- 1
					FinSi
					
					// ===============================
					// DEFINICIÓN DE PALOS Y VALORES
					// ===============================
					Si flagInicio = 0 Entonces
						Dimension palos[4]
						palos[0] <- "Corazones"
						palos[1] <- "Diamantes"
						palos[2] <- "Tréboles"
						palos[3] <- "Picas"
					FinSi
					Si flagInicio = 0 Entonces
						Dimension valores[13]
						valores[0] <- "A"
						valores[1] <- "2"
						valores[2] <- "3"
						valores[3] <- "4"
						valores[4] <- "5"
						valores[5] <- "6"
						valores[6] <- "7"
						valores[7] <- "8"
						valores[8] <- "9"
						valores[9] <- "10"
						valores[10] <- "J"
						valores[11] <- "Q"
						valores[12] <- "K"
					FinSi
					
					Si flagInicio = 0 Entonces
						Dimension valoresCartas[13]
						valoresCartas[0] <- 11
						valoresCartas[1] <- 2
						valoresCartas[2] <- 3
						valoresCartas[3] <- 4
						valoresCartas[4] <- 5
						valoresCartas[5] <- 6
						valoresCartas[6] <- 7
						valoresCartas[7] <- 8
						valoresCartas[8] <- 9
						valoresCartas[9] <- 10
						valoresCartas[10] <- 10
						valoresCartas[11] <- 10
						valoresCartas[12] <- 10
					FinSi
					
					// ===============================
					// GENERAR EL MAZO COMPLETO
					// ===============================
					Si flagInicio = 0 Entonces
						totalCartas <- 52 * cantidadMazos
						Dimension mazoFinal[totalCartas]
						Dimension palosMazo[totalCartas]
						Dimension nombreCarta[totalCartas]
						Dimension valoresMazo[totalCartas]
					FinSi
					
					Si flagInicio = 0 Entonces
						indice <- 0
						Para i <- 0 Hasta cantidadMazos - 1 Con Paso 1
							Para p <- 0 Hasta 3 Con Paso 1
								Para v <- 0 Hasta 12 Con Paso 1
									mazoFinal[indice] <- valores[v] + " de " + palos[p]
									nombreCarta[indice] <- valores[v]
									valoresMazo[indice] <- valoresCartas[v]
									palosMazo[indice] <- palos[p]
									indice <- indice + 1
								FinPara
							FinPara
						FinPara
					FinSi
					
					// ===============================
					// BARAJADO (Fisher-Yates)
					// ===============================
					Para i <- totalCartas - 1 Hasta 1 Con Paso -1
						j <- Aleatorio(0, i)
						temp <- mazoFinal[i]
						tempValores <- valoresMazo[i]
						tempPalos <- palosMazo[i]
						tempNombreCarta <- nombreCarta[i]
						mazoFinal[i] <- mazoFinal[j]
						valoresMazo[i] <- valoresMazo[j]
						palosMazo[i] <- palosMazo[j]
						nombreCarta[i] <- nombreCarta[j]
						mazoFinal[j] <- temp
						valoresMazo[j] <- tempValores
						palosMazo[j] <- tempPalos
						nombreCarta[j] <- tempNombreCarta 
					FinPara
					
					// ===============================
					// REPARTO INICIAL
					// ===============================
					Si flagInicio = 0 Entonces
						Definir jugadorCartas, dealerCartas Como Cadena
						Definir cantJugador, cantDealer, indiceMazo, sumaJugador, sumaDealer Como Entero
						Dimension jugadorCartas[10]
						Dimension dealerCartas[10]
						indiceMazo <- 0
					FinSi
					Si flagInicio = 0 O Saldobj <= 0 Entonces
						opRepe <- "S"
						Saldobj <- 0
						flagSaldo <- 0
						Repetir
							Si flagSaldo = 1 Entonces
								Escribir "Por favor, ingrese un número válido (Saldo > 0)"
							FinSi
							Escribir "Ingrese el saldo con el que quiere jugar:"
							Leer Saldobj
							flagSaldo <- 1
						Hasta Que Saldobj > 0
					FinSi
					
					
					Repetir
						
						Escribir ""
						Escribir "Saldo actual:"
						Escribir "$" Saldobj
						Esperar 1 Segundos
						Escribir "Ingrese la apuesta que quiere realizar"
						Escribir "Por favor, apueste con conciencia"
						Repetir
							Si flagApuesta = 1 Entonces
								Escribir "Por favor, ingrese una apuesta válida, (apuesta <= saldo y apuesta > 0) "
							FinSi
							Leer apuesta
							flagApuesta <- 1
						Hasta Que (apuesta <= Saldobj) Y apuesta > 0
						
						flagApuesta <- 0
						cantJugador <- 0
						cantDealer <- 0
						sumaJugador <- 0
						sumaDealer <- 0
						indiceSegundaCartaDealer <- 0
						indicePrimeraCartaDealer <- 0
						indiceSegundaCartaJugador <- 0
						indicePrimeraCartaJugador <- 0
						flagPosibleBlackJackDealer <- 0
						
						// Reparto alternado
						jugadorCartas[cantJugador] <- mazoFinal[indiceMazo]
						indicePrimeraCartaJugador <- indiceMazo
						cantJugador <- cantJugador + 1
						indiceMazo <- indiceMazo + 1
						
						dealerCartas[cantDealer] <- mazoFinal[indiceMazo]
						indicePrimeraCartaDealer <- indiceMazo
						cantDealer <- cantDealer + 1
						indiceMazo <- indiceMazo + 1
						
						jugadorCartas[cantJugador] <- mazoFinal[indiceMazo]
						indiceSegundaCartaJugador <- indiceMazo
						cantJugador <- cantJugador + 1
						indiceMazo <- indiceMazo + 1
						
						dealerCartas[cantDealer] <- mazoFinal[indiceMazo]
						indiceSegundaCartaDealer <- indiceMazo
						cantDealer <- cantDealer + 1
						indiceMazo <- indiceMazo + 1
						
						// ===============================
						// AJUSTE DE AS (por si hay 2 As)
						// ===============================
						indiceAsJugador <- 0
						Si nombreCarta[indicePrimeraCartaJugador] = "A" Y nombreCarta[indiceSegundaCartaJugador] = "A" Entonces
							sumaJugador <- valoresMazo[indiceMazo - 4] + valoresMazo[indiceSegundaCartaJugador] - 10
							indiceAsJugador <- 1
						SiNo
							Si nombreCarta[indicePrimeraCartaJugador] = "A" O nombreCarta[indiceSegundaCartaJugador] = "A" Entonces
								indiceAsJugador <- 1
								sumaJugador <- valoresMazo[indicePrimeraCartaJugador] + valoresMazo[indiceSegundaCartaJugador]
							SiNo
								sumaJugador <- valoresMazo[indicePrimeraCartaJugador] + valoresMazo[indiceSegundaCartaJugador]
							FinSi
						FinSi
						
						indiceAsDealer <- 0
						Si nombreCarta[indicePrimeraCartaDealer] = "A" Y nombreCarta[indiceSegundaCartaDealer] = "A" Entonces
							sumaDealer <- valoresMazo[indicePrimeraCartaDealer] + valoresMazo[indiceSegundaCartaDealer] - 10
							indiceAsDealer <- 1
						SiNo
							Si nombreCarta[indicePrimeraCartaDealer] = "A" O nombreCarta[indiceSegundaCartaDealer] = "A" Entonces
								indiceAsDealer <- 1
								sumaDealer <- valoresMazo[indicePrimeraCartaDealer] + valoresMazo[indiceSegundaCartaDealer]
							SiNo
								sumaDealer <- valoresMazo[indicePrimeraCartaDealer] + valoresMazo[indiceSegundaCartaDealer]
							FinSi
						FinSi
						Esperar 1 Segundos
						// ===============================
						// MOSTRAR CARTAS
						// ===============================
						Escribir "------------------------------------"
						Escribir "Cartas del Jugador:"
						Para i <- cantJugador - 1 Hasta cantJugador - 1 Con Paso 1 Hacer
							Escribir "- ", jugadorCartas[cantJugador - 2]
							Escribir "- ", jugadorCartas[cantJugador - 1]
						FinPara
						Escribir "------------------------------------"
						blackjackJugador <- 0
						blackjackDealer <- 0
						blackjackJugador <- valoresMazo[indicePrimeraCartaJugador] + valoresMazo[indiceSegundaCartaJugador]
						blackjackDealer <- valoresMazo[indicePrimeraCartaDealer] + valoresMazo[indiceSegundaCartaDealer]
						Esperar 1 Segundos
						// ===============================
						// MOSTRAR CARTAS DEL DEALER
						// ===============================
						Para i <- cantDealer - 1  Hasta cantDealer - 1 Con Paso 1 Hacer
							Escribir "Carta visible del Dealer:"
							Escribir "- ", dealerCartas[cantDealer - 2]
							//Escribir "- ", dealerCartas[cantDealer - 1]
						FinPara
						Escribir "Carta oculta del Dealer: ???"
						Escribir "------------------------------------"
						
						Escribir "------------------------------------"
						Escribir "Suma inicial del Jugador: ", sumaJugador
						Si blackjackJugador = 21 Entonces
							Escribir "¡BLACK JACK!"
						FinSi
						Escribir "Suma del Dealer: ", valoresMazo[indicePrimeraCartaDealer]
						Escribir "------------------------------------"
						
						// ===============================
						// TURNO DEL JUGADOR
						// ===============================
						opPosibleBlackJackDealer <- "N"
						Si valoresMazo[indicePrimeraCartaDealer] = 10 O valoresMazo[indicePrimeraCartaDealer] = 11 Entonces
							Repetir
								Si flagPosibleBlackJackDealer = 1 Entonces
									Escribir "Por favor, ingrese una opción válida (S/N)"
								FinSi
								Escribir "Posible Black Jack del dealer"
								Escribir "¿Quiere retirarse con el 50% de su apuesta? (S/N)"
								Leer opPosibleBlackJackDealer
								opPosibleBlackJackDealer <- Mayusculas(opPosibleBlackJackDealer)
							Hasta Que opPosibleBlackJackDealer = "S" O opPosibleBlackJackDealer = "N"
							
						FinSi
						Si opPosibleBlackJackDealer = "N" Y sumaJugador < 21 Entonces
							flagCartas <- 0
							
							Si (apuesta * 2) <= Saldobj Entonces
								Repetir
									Si flagDoblar = 1 Entonces
										Escribir "Por favor, ingrese una opción válida (S/N)"
									FinSi
									Escribir "Quiere doblar su apuesta? (S/N)"
									Leer opDoblar
									opDoblar <- Mayusculas(opDoblar)
								Hasta Que opDoblar <> "S" O opDoblar <> "N"
								Si opDoblar = "S" Entonces
									apuesta <- apuesta * 2
									Escribir "Usted acaba de doblar su apuesta"
									Escribir "Apuesta actualizada :$", apuesta
									Esperar 1 Segundos
								SiNo
									Escribir "Usted eligio no doblar su apuesta"
									Escribir "Apuesta acutal: $", apuesta
									Esperar 1 Segundos
								FinSi
							SiNo
								Si (apuesta * 2) > Saldobj Entonces
									Escribir "No puede doblar, saldo insuficiente"
									opDoblar <- "N"
								FinSi
							FinSi
							
							
							Repetir
								Si opDoblar = "N" Entonces
									Repetir
										Escribir "¿Desea pedir otra carta? (S/N)"
										Leer op
										op <- Mayusculas(op)
									Hasta Que op = "S" O op = "N"
								FinSi
								Si op = "S" O opDoblar = "S" Entonces
									jugadorCartas[cantJugador] <- mazoFinal[indiceMazo]
									Escribir "Obtuviste: ", jugadorCartas[cantJugador]
									cantJugador <- cantJugador + 1
									indiceMazo <- indiceMazo + 1
									
									Si nombreCarta[indiceMazo - 1] = "A" O sumaJugador > 21 Entonces
										sumaJugador <- sumaJugador + valoresMazo[indiceMazo - 1] - 10
									SiNo
										sumaJugador <- sumaJugador + valoresMazo[indiceMazo - 1]
									FinSi
									Escribir "- ", mazoFinal[indicePrimeraCartaJugador]
									Escribir "- ", mazoFinal[indiceSegundaCartaJugador]
								FinSi
								Si flagCartas = 0 Y op = "S" O opDoblar = "S" Entonces
									Escribir "- ", mazoFinal[indiceMazo - 1]
								SiNo
									Si flagCartas = 1 Y op = "S" Entonces
										Escribir "- ", mazoFinal[indiceMazo - 2]
										Escribir "- ", mazoFinal[indiceMazo - 1]
									SiNo
										Si flagCartas = 2 Y op = "S" Entonces
											Escribir "- ", mazoFinal[indiceMazo - 3]
											Escribir "- ", mazoFinal[indiceMazo - 2]
											Escribir "- ", mazoFinal[indiceMazo - 1]
										FinSi
									FinSi
								FinSi
								Si op = "S" O opDoblar = "S" Entonces
									Escribir "Suma actual: ", sumaJugador
								FinSi
								flagCartas <- flagCartas + 1
								Esperar 2 Segundos
							Hasta Que op <> "S" O opDoblar = "S" O sumaJugador >= 21
						FinSi
						
						
						// ===============================
						// TURNO DEL DEALER
						// ===============================
						Escribir "------------------------------------"
						Escribir "Cartas del Dealer:"
						Escribir "- ", mazoFinal[indicePrimeraCartaDealer]
						Escribir "- ", mazoFinal[indiceSegundaCartaDealer]
						Escribir "Suma inicial del Dealer: ", sumaDealer
						Esperar 2 Segundos
						Si blackjackDealer = 21 Entonces
							Escribir "¡BLACK JACK!"
						FinSi
						Escribir "------------------------------------"
						
						Si sumaJugador <= 21 Entonces
							flagCartas <- 0
							Mientras sumaDealer < 17 Hacer
								dealerCartas[cantDealer] <- mazoFinal[indiceMazo]
								Escribir "Dealer pide: ", dealerCartas[cantDealer]
								cantDealer <- cantDealer + 1
								indiceMazo <- indiceMazo + 1
								
								// Ajuste de As si se pasa
								Si nombreCarta[indiceMazo - 1] = "A" O sumaDealer > 21 Entonces
									sumaDealer <- sumaDealer + valoresMazo[indiceMazo - 1] - 10
								SiNo
									sumaDealer <- sumaDealer + valoresMazo[indiceMazo - 1]
								FinSi
								Escribir "- ", mazoFinal[indicePrimeraCartaDealer]
								Escribir "- ", mazoFinal[indiceSegundaCartaDealer]
								Si flagCartas = 0 Entonces
									Escribir "- ", mazoFinal[indiceMazo - 1]
								SiNo
									Si flagCartas = 1 Entonces
										Escribir "- ", mazoFinal[indiceMazo - 2]
										Escribir "- ", mazoFinal[indiceMazo - 1]
									SiNo
										Si flagCartas = 2 Entonces
											Escribir "- ", mazoFinal[indiceMazo - 3]
											Escribir "- ", mazoFinal[indiceMazo - 2]
											Escribir "- ", mazoFinal[indiceMazo - 1]
										FinSi
									FinSi
								FinSi
								Escribir "Suma del Dealer: ", sumaDealer
								flagCartas <- flagCartas + 1
								Esperar 2 Segundos
							FinMientras
						FinSi
						
						// ===============================
						// RESULTADO FINAL
						// ===============================
						Escribir "------------------------------------"
						Escribir "Suma final del Jugador: ", sumaJugador
						Escribir "Suma final del Dealer: ", sumaDealer
						Si opPosibleBlackJackDealer = "S" Entonces
							Escribir "Usted se a retirado por posible Black Jack del dealer"
							Escribir "Su apuesta fue de: $", apuesta
							Escribir "Devolución del 50% de su apuesta: $", (apuesta * 0.5)
							Saldobj <- Saldobj - (apuesta * 0.5)
						SiNo
							Si blackjackDealer = 21 Y blackjackJugador = 21 Entonces
								Escribir "Dealer y jugador sacaron Black Jack"
								Escribir "Push (Empate)"
								Saldobj <- Saldobj
							SiNo
								Si blackjackJugador = 21 Entonces
									Escribir "¡Ganaste! Sacaste Black Jack, pago 3:2"
									apuesta <- (apuesta * 1.5)
									Escribir "Ganas $", apuesta
									Saldobj <- Saldobj + apuesta
								SiNo
									Si sumaJugador > 21 Y opPosibleBlackJackDealer = "N" Entonces
										Escribir "Dealer gana (te pasaste)."
										Escribir "Pierdes $", apuesta
										Saldobj <- Saldobj - apuesta
									Sino
										Si sumaDealer > 21 Y blackjackJugador <> 21 Entonces
											Escribir "¡Ganaste! (Dealer se pasó)"
											Escribir "Ganas $", apuesta
											Saldobj <- Saldobj + apuesta
										Sino Si sumaJugador > sumaDealer Entonces
												Escribir "¡Ganaste!"
												Escribir "Ganas $", apuesta
												Saldobj <- Saldobj + apuesta
											Sino Si sumaJugador < sumaDealer Y sumaDealer <= 21 Y opPosibleBlackJackDealer = "N" Entonces
													Escribir "Dealer gana."
													Escribir "Pierdes $", apuesta
													Saldobj <- Saldobj - apuesta
												Sino
													Escribir "Push (Empate)."
													Saldobj <- Saldobj
												FinSi
											FinSi
										FinSi
									FinSi
								FinSi
							FinSi
						FinSi
						
						flagInicio <- 1
						Si flagInicio = 1 Y Saldobj > 0 Entonces
							Repetir
								Si flagRepetir = 1 Entonces
									Escribir "Por favor, ingrese (S) para seguir jugando o (N) para salir del juego"
								FinSi
								Escribir "¿Desea jugar otra mano? (S/N)"
								Leer opRepe
								opRepe <- Mayusculas(opRepe)
								flagRepetir <- 1
							Hasta Que Mayusculas(opRepe) = "S" O Mayusculas(opRepe) = "N"
							flagRepetir <- 0
						FinSi
					Hasta Que indiceMazo >= (totalCartas - 4) O opRepe = "N" O Saldobj <= 0
				Hasta Que indiceMazo >= (totalCartas - 4) O opRepe = "N" O Saldobj <= 0
				Si indiceMazo >= (totalCartas - 4) Entonces
					Escribir "No hay cartas suficientes en el mazo para seguir jugando"
					Escribir "Saldo final: $", Saldobj
					Escribir "Gracias por jugar al Black Jack! (:"
					Esperar 3 Segundos
				FinSi
				Si Saldobj <= 0 Entonces
					Escribir "Te quedaste sin saldo"
					Escribir "Gracias por jugar al Black Jack! (:"
					Esperar 3 Segundos
				FinSi
				Si opRepe = "N" Entonces
					Escribir "Saldo final: $", Saldobj
					Escribir "Saliendo del programa..."
					Esperar 3 Segundos
				FinSi
				
			2:
				//CARGO NUMEROS ROJOS DE LA RULETA
				vRojo[0] <- 1; vRojo[1] <- 3; vRojo[2] <- 5; vRojo[3] <- 7
				vRojo[4] <- 9; vRojo[5] <- 12; vRojo[6] <- 14; vRojo[7] <- 16
				vRojo[8] <- 18; vRojo[9] <- 19; vRojo[10] <- 21; vRojo[11] <- 23
				vRojo[12] <- 25; vRojo[13] <- 27; vRojo[14] <- 30; vRojo[15] <- 32
				vRojo[16] <- 34; vRojo[17] <- 36
				
				PantallaCarga
				
				dineroDisponibleRuleta <- 1000
				continuarJugandoRuleta <- "S"
				
				Mientras continuarJugandoRuleta = "S" O continuarJugandoRuleta = "s" Hacer
					
					MostrarMenuRuleta(dineroDisponibleRuleta)
					
					//VALIDA OPCIONES DEL MENÚ
					Repetir
						Escribir Sin Saltar "Seleccione una opción (1-4): "
						Leer opcionTexto
						Si EsNumeroValido(opcionTexto) Entonces
							opcionApuesta <- ConvertirANumero(opcionTexto)
						Sino
							opcionApuesta <- 0
						FinSi
						Si opcionApuesta < 1 O opcionApuesta > 4 Entonces
							Escribir "Debe ingresar un número válido (1-4)."
						FinSi
					Hasta Que opcionApuesta >= 1 Y opcionApuesta <= 4
					
					//SALIDA DEL JUEGO
					Si opcionApuesta = 4 Entonces
						Escribir ""
						Escribir "Gracias por jugar. Dinero final: $", dineroDisponibleRuleta
						continuarJugandoRuleta <- "N"
					Sino
						
						//MONTO A APOSTAR VALIDACIONES
						Repetir
							Escribir "Ingrese monto a apostar:"
							Leer montoTexto
							Si EsNumeroValido(montoTexto) Entonces
								montoApostar <- ConvertirANumero(montoTexto)
								Si montoApostar <= 0 Entonces
									Escribir "El monto debe ser mayor a 0."
								FinSi
								Si montoApostar > dineroDisponibleRuleta Entonces
									Escribir "No tiene suficiente dinero. Saldo: $", dineroDisponibleRuleta
								FinSi
							Sino
								Escribir "Debe ingresar un número válido."
								montoApostar <- 0
							FinSi
						Hasta Que montoApostar > 0 Y montoApostar <= dineroDisponibleRuleta
						
						//OPCION DE APUESTA
						Segun opcionApuesta Hacer
							1:
								Repetir
									Escribir "Ingrese número (0 - 36):"
									Leer apuestaRuleta
									Si No(EsNumeroValido(apuestaRuleta)) Entonces
										Escribir "Debe ingresar un número válido."
									Sino
										Si ConvertirANumero(apuestaRuleta) < 0 O ConvertirANumero(apuestaRuleta) > 36 Entonces
											Escribir "Número fuera de rango (0-36)."
											apuestaRuleta <- ""
										FinSi
									FinSi
								Hasta Que EsNumeroValido(apuestaRuleta) Y ConvertirANumero(apuestaRuleta) >= 0 Y ConvertirANumero(apuestaRuleta) <= 36
								
							2:
								Repetir
									Escribir "Ingrese color (rojo/negro/verde):"
									Leer apuestaRuleta
									apuestaRuleta <- Minusculas(apuestaRuleta)
									Si apuestaRuleta <> "rojo" Y apuestaRuleta <> "negro" Y apuestaRuleta <> "verde" Entonces
										Escribir "Color inválido. Escriba rojo, negro o verde."
									FinSi
								Hasta Que apuestaRuleta = "rojo" O apuestaRuleta = "negro" O apuestaRuleta = "verde"
								
							3:
								Repetir
									Escribir "Ingrese (par/impar):"
									Leer apuestaRuleta
									apuestaRuleta <- Minusculas(apuestaRuleta)
									Si apuestaRuleta <> "par" Y apuestaRuleta <> "impar" Entonces
										Escribir "Opción inválida. Escriba par o impar."
									FinSi
								Hasta Que apuestaRuleta = "par" O apuestaRuleta = "impar"
						FinSegun
						
						//GIRAR RULETA
						numRuleta <- GirarRuleta
						colorRuleta <- ObtenerColor(numRuleta, vRojo)
						ruletaAnimacion(numRuleta, colorRuleta)
						
						//EVALUAR GANANCIA APUESTA
						gananciaRuleta <- EvaluarApuesta(opcionApuesta, apuestaRuleta, montoApostar, numRuleta, colorRuleta)
						dineroDisponibleRuleta <- dineroDisponibleRuleta + gananciaRuleta
						
						Escribir ""
						Escribir "Dinero actual: $", dineroDisponibleRuleta
						Esperar Tecla
						
						//VERIFICAR SALDO
						Si dineroDisponibleRuleta <= 0 Entonces
							Escribir ""
							Escribir "¡Te quedaste sin dinero! Fin del juego."
							continuarJugandoRuleta <- "N"
						Sino
							Repetir
								Escribir ""
								Escribir "¿Desea seguir jugando? (S/N)"
								Leer continuarJugandoRuleta
								continuarJugandoRuleta <- Mayusculas(continuarJugandoRuleta)
								Si continuarJugandoRuleta <> "S" Y continuarJugandoRuleta <> "N" Entonces
									Escribir "Opción inválida. Escriba S o N."
								FinSi
							Hasta Que continuarJugandoRuleta = "S" O continuarJugandoRuleta = "N"
						FinSi
					FinSi
				FinMientras
				
				Escribir ""
				Escribir "Gracias por jugar. Dinero final: $", dineroDisponibleRuleta
			3:    Mientras menuActivo Hacer
					Limpiar Pantalla
					Escribir"+-----------------------------------------------------------------------------------------------------------+"
					Escribir "|                                                                                                           |"
					Escribir "|               _____ ____      _    ____    _    __  __  ___  _   _ _____ ____    _    ____                |"
					Escribir "|              |_   _|  _ \    / \  / ___|  / \  |  \/  |/ _ \| \ | | ____|  _ \  / \  / ___|               |"
					Escribir "|                | | | |_) |  / _ \| |  _  / _ \ | |\/| | | | |  \| |  _| | | | |/ _ \ \___ \               |"
					Escribir "|                | | |  _ <  / ___ \ |_| |/ ___ \| |  | | |_| | |\  | |___| |_| / ___ \ ___) |              |"
					Escribir "|                |_| |_| \_\/_/   \_\____/_/   \_\_|  |_|\___/|_| \_|_____|____/_/   \_\____/               |"
					Escribir "|                                                                                                           |"
					Escribir"+-----------------------------------------------------------------------------------------------------------+"
					Escribir "|                                                                                                           |"
					Escribir "|                                           [1] Iniciar Juego                                               |"
					Escribir "|                                           [2] Instrucciones                                               |"
					Escribir "|                                           [3] Salir                                                       |"
					Escribir "|                                                                                                           |"
					Escribir"+-----------------------------------------------------------------------------------------------------------+"
					Escribir Sin Saltar "Seleccione una opción (1-3): "
					Leer opcionMenu
					
					Segun opcionMenu Hacer
						1:
							Limpiar Pantalla
							JuegoTragamonedas()
						2:
							Limpiar Pantalla
							Escribir "+---------------- INSTRUCCIONES ----------------+"
							Escribir "| - Pulsa 1 para girar los rodillos.            |"
							Escribir "| - Si salen tres frutas iguales, ganas.        |"
							Escribir "| - Cada fruta tiene un valor distinto.         |"
							Escribir "| - Jackpot se acumula con cada tirada.         |"
							Escribir "| - ¡Prueba tu suerte y diviértete!             |"
							Escribir "| - Jackpot se gana con 3 cerezas.              |"
							Escribir "+-----------------------------------------------+"
							Escribir "Presione ENTER para volver al menú..."
							Leer Volver_al_menu
						3:
							Limpiar Pantalla
							Escribir "Gracias por jugar. ¡Hasta luego!"
							menuActivo <- Falso
						De Otro Modo:
							Escribir "Opción no válida. Intente nuevamente."
							Esperar 1 Segundos
					FinSegun
				FinMientras
				
				
				
			4:	
				Escribir "========================================================================================================================"
				Escribir "                                                  CAMINO DEL COMETA  "
				Escribir "========================================================================================================================"
				Escribir ""
				Escribir "                                   Ingresa tu nombre, viajero del destino:"
				Leer nombre_jugadorHistoria
				
				repetir 
					LimpiarPantalla
					
					Escribir "========================================================================================================================"
					Escribir "                                                 MENÚ PRINCIPAL"
					Escribir "========================================================================================================================"
					Escribir "1) Iniciar historia"
					Escribir "2) Instrucciones y controles"
					Escribir "3) Salir"
					Leer opc_inicio_historia
					
					segun opc_inicio_historia hacer 
						1:
							Capitulo_1()
							MostrarEstado(vida, energia, amuleto, llave) 
							Eleccion_1 <- decision_1(vida, energia, amuleto, llave, inventario, TAM)
							segun Eleccion_1 Hacer
								1: 
									// CAMINO DEL VALOR 
									
									energia <- energia - 10
									LimpiarPantalla
									Escribir "========================================================================================================================"
									Escribir "                                                     CAMINO  DEL  VALOR "
									Escribir "========================================================================================================================"
									MostrarEstado(vida, energia, amuleto, llave) 
									Eleccion_1Valor <- desicio_2(vida, energia, amuleto, llave, inventario, TAM) 
									Si Eleccion_1Valor = 1 Entonces 
										Escribir "Abres el cofre: dentro hay una Piedra de Luz."
										inventario[1] <- 1//obtenes piedra de luz 
									Sino
										Escribir "El sabio asiente. - La desconfianza también es lección, dice y se va."
									FinSi
									Esperar Tecla
									Eleccion_2Valor <- decision_3(vida, energia, amuleto, llave, inventario, TAM)
									segun Eleccion_2Valor Hacer 
										1:
											energia <- energia - 20
											Escribir "Cruzas. Al otro lado, un brillo azulado te cautiva."
											inventario[2] <- 1
										2:
											energia <- energia - 10
											Escribir "Bajas río abajo y hallas una cueva. Dentro, una Llave Antigua."
											inventario[3] <- 1
											llave <- 1
									FinSegun
									Esperar Tecla
									LimpiarPantalla
									Escribir "El camino te lleva hacia una colina donde el cometa parece rozar el horizonte."
									MostrarEstado(vida, energia, amuleto, llave) 
									Esperar Tecla
									EtapaFinal(vida, energia, amuleto, llave, inventario, TAM) 
									
									
								2:
									//   CAMINO DE LA COMUNIDA 
									
									energia <- energia - 5
									vida <- vida + 10
									LimpiarPantalla
									Escribir "========================================================================================================================"
									Escribir "                                                 CAMINO DE LA COMUNIDAD "
									Escribir "========================================================================================================================"
									MostrarEstado(vida, energia, amuleto, llave) 
									Eleccion_1Comunidad <- desicion_1Com(vida, energia, amuleto, llave, inventario, TAM) 
									Si Eleccion_1Comunidad = 1 Entonces
										energia <- energia - 20
										vida <- vida - 10
										Escribir "Liderás la expedición. Encuentran huellas que llevan a una antigua puerta sellada."
										Escribir "Dentro, entre escombros, hallás una Llave Antigua."
										inventario[3] <- 1
										llave <- 1 
									Sino
										energia <- energia + 5
										Escribir "Quedás organizando los recursos. La comunidad no te olvida."
									FinSi
									Esperar Tecla
									Escribir "La aldea propone una vigilia en honor al cometa."
									MostrarEstado(vida, energia, amuleto, llave) 
									esperar Tecla
									EtapaFinal(vida, energia, amuleto, llave, inventario, TAM) 
									
									
								3:
									// CAMINO DEL SECRETO / VALOR
									
									energia <- energia - 8
									Escribir "Encuentras restos con símbolos del cometa. Hallás un Fruto Místico."
									inventario[0] <- 1//obtienes Fruto Místico"
									Esperar Tecla
									energia <- energia - 10
									LimpiarPantalla
									Escribir "========================================================================================================================"
									Escribir "                                                    CAMINO  DEL  VALOR "
									Escribir "========================================================================================================================"
									MostrarEstado(vida, energia, amuleto, llave) 
									Eleccion_1Valor <- desicio_2(vida, energia, amuleto, llave, inventario, TAM) 
									Si Eleccion_1Valor = 1 Entonces 
										Escribir "Abres el cofre: dentro hay una Piedra de Luz."
										inventario[1] <- 1//obtenes piedra de luz 
									Sino
										Escribir "El sabio asiente. - La desconfianza también es lección, dice y se va."
									FinSi
									Esperar Tecla
									Eleccion_2Valor <- decision_3(vida, energia, amuleto, llave, inventario, TAM) 
									segun Eleccion_2Valor Hacer 
										1:
											energia <- energia - 20
											Escribir "Cruzas. Al otro lado, un brillo azulado te cautiva."
											inventario[2] <- 1// obtienes amuleto del cometa 
										2:
											energia <- energia - 10
											Escribir "Bajas río abajo y hallas una cueva. Dentro, una Llave Antigua."
											inventario[3] <- 1
											llave <- 1 
									FinSegun
									Esperar Tecla
									LimpiarPantalla
									Escribir "El camino te lleva hacia una colina donde el cometa parece rozar el horizonte."
									MostrarEstado(vida, energia, amuleto, llave)
									Esperar Tecla
									EtapaFinal(vida, energia, amuleto, llave, inventario, TAM) 
									
									
							FinSegun
						2:
							MostrarInstrucciones()
						De Otro Modo:
							escribir "Opcion Invalida."
					FinSegun
				Hasta Que opc_inicio_historia = 3 
				
				LimpiarPantalla
				Escribir "Gracias por jugar, ", nombre_jugadorHistoria, "."
				Escribir "Tu historia quedará escrita entre las estrellas."
			5:
				Escribir "¡Gracias por jugar!"
				Escribir "Saliendo del programa..."
				
		FinSegun
		
	Hasta Que OPCION_MENU = 5
	
FinAlgoritmo






//----------------------------------------------- FUNCIONES	RULETA---------------------------------------------------------------------

//VALIDAR NUMEROS
Funcion esValido <- EsNumeroValido(texto)
	Definir esValido Como Logico
	Definir i, tam Como Entero
	Definir caracter Como Cadena
	
	esValido <- Verdadero
	tam <- Longitud(texto)
	
	Si tam = 0 Entonces
		esValido <- Falso
	Sino
		Para i <- 0 Hasta tam - 1 Con Paso 1 Hacer
			caracter <- Subcadena(texto, i, i)
			Si caracter < "0" O caracter > "9" Entonces
				esValido <- Falso
			FinSi
		FinPara
	FinSi
FinFuncion

//PANTALLA DE CARGA
SubProceso PantallaCarga
	Definir i Como Entero
	Definir barra Como Cadena
	barra <- ""
	
	Limpiar Pantalla
	Escribir ""
	Escribir "-----------------------------------------"
	Escribir "|            C A S I N O                |"
	Escribir "-----------------------------------------"
	Escribir "          Cargando Ruleta..."
	Escribir ""
	Escribir Sin Saltar "["
	Para i <- 1 Hasta 25 Con Paso 1 Hacer
		barra <- barra + "#"
		Escribir Sin Saltar "#"
		Esperar 120 Milisegundos
	FinPara
	Escribir "]"
	Escribir ""
	Escribir "Carga completa. ¡Listo para jugar!"
	Esperar 1000 Milisegundos
	Limpiar Pantalla
FinSubProceso

//MENU RULETA
SubProceso MostrarMenuRuleta(dineroActual)
	Limpiar Pantalla
	Escribir "========================================"
	Escribir "|             C A S I N O              |"
	Escribir "========================================"
	Escribir "|             R U L E T A              |"
	Escribir "----------------------------------------"
	Escribir " Dinero disponible: $", dineroActual
	Escribir "----------------------------------------"
	Escribir " 1) Apostar a un número  (paga 35 a 1)"
	Escribir " 2) Apostar a un color   (rojo/negro/verde)"
	Escribir " 3) Apostar a par/impar  (paga 1 a 1)"
	Escribir " 4) Salir"
	Escribir "----------------------------------------"
FinSubProceso

//GIRAR RULETA
Funcion numRuleta <- GirarRuleta
	numRuleta <- Aleatorio(0,36)
FinFuncion

//COLOR DE LA RULETA
Funcion colorRuleta <- ObtenerColor(numRuleta, vRojo)
	Definir colorRuleta Como Cadena
	Definir i Como Entero
	
	Si numRuleta = 0 Entonces
		colorRuleta <- "verde"
	Sino
		colorRuleta <- "negro"
		Para i <- 0 Hasta 17 Hacer
			Si numRuleta = vRojo[i] Entonces
				colorRuleta <- "rojo"
			FinSi
		FinPara
	FinSi
FinFuncion

//ANIMACION RULETA
SubProceso ruletaAnimacion(numeroFinal, colorFinal)
	Definir i, numMostrar Como Entero
	Para i <- 1 Hasta 10 Con Paso 1 Hacer
		Limpiar Pantalla
		numMostrar <- Aleatorio(0, 36)
		Escribir ""
		Escribir "       RULETA GIRANDO..."
		Escribir ""
		Escribir "       [ ", numMostrar, " ]"
		Esperar 150 Milisegundos
	FinPara
	
	Limpiar Pantalla
	Escribir "----------------------------------------"
	Escribir "        ¡RESULTADO FINAL!"
	Escribir " Número: ", numeroFinal
	Escribir " Color: ", colorFinal
	Si colorFinal = "verde" Entonces
		Escribir "(El 0 siempre es verde y gana la casa)"
	FinSi
	Escribir "----------------------------------------"
FinSubProceso

//GANANCIA RULETA
Funcion gananciaRuleta <- EvaluarApuesta(opcionApuesta, apuestaRuleta, montoApostar, numRuleta, colorRuleta)
	Definir gananciaRuleta, numApuesta Como Entero
	gananciaRuleta <- 0
	
	Segun opcionApuesta Hacer
		1:
			numApuesta <- ConvertirANumero(apuestaRuleta)
			Si numRuleta = numApuesta Entonces
				Escribir "¡Ganó! Pago 35 a 1"
				gananciaRuleta <- montoApostar * 35
			Sino
				Escribir "Perdió la apuesta."
				gananciaRuleta <- -montoApostar
			FinSi
		2:
			Si colorRuleta = apuestaRuleta Entonces
				Si colorRuleta = "verde" Entonces
					Escribir "¡Ganó! Apostó al verde (paga 35 a 1)"
					gananciaRuleta <- montoApostar * 35
				Sino
					Escribir "¡Ganó! Pago 1 a 1"
					gananciaRuleta <- montoApostar
				FinSi
			Sino
				Escribir "Perdió la apuesta."
				gananciaRuleta <- -montoApostar
			FinSi
		3:
			Si numRuleta = 0 Entonces
				Escribir "Salió 0 (verde): la casa gana."
				gananciaRuleta <- -montoApostar
			Sino
				Si (apuestaRuleta = "par" Y numRuleta MOD 2 = 0) O (apuestaRuleta = "impar" Y numRuleta MOD 2 <> 0) Entonces
					Escribir "¡Ganó! Pago 1 a 1"
					gananciaRuleta <- montoApostar
				Sino
					Escribir "Perdió la apuesta."
					gananciaRuleta <- -montoApostar
				FinSi
			FinSi
	FinSegun
FinFuncion

//-----------------------------------------------FIN FUNCIONES	RULETA---------------------------------------------------------------------




//-------------------------------------------------------FUNCIONES HISTORIa-----------------------------------------------------------------





//                                       FUNCIONES DE INVENTARIOS 

subproceso VerInventario(inventario, TAM)
	definir i Como Entero
	Escribir "========== INVENTARIO =========="
	para i <- 0 Hasta TAM-1 Hacer
		si i=0 Entonces
			si inventario[i]= 1 Entonces
				escribir i+1, ")  Fruto Místico"
			sino 
				escribir i+1, ")  Objeto no encontrado"
			FinSi 
		FinSi
		si i=1 Entonces
			si inventario[i]= 1 Entonces
				escribir i+1, ")  Piedra de Luz"
			sino 
				escribir i+1, ")  Objeto no encontrado"
			FinSi 
		FinSi
		si i=2 Entonces
			si inventario[i]= 1 Entonces
				escribir i+1, ")  Amuleto del Cometa"
			sino 
				escribir i+1, ")  Objeto no encontrado"
			FinSi 
		FinSi
		si i=3 Entonces
			si inventario[i]= 1 Entonces
				escribir i+1, ")  Llave Antigua"
			sino 
				escribir i+1, ")  Objeto no encontrado"
			FinSi 
		FinSi
	FinPara
	Escribir "================================="
FinSubProceso

subproceso UsarObjeto(inventario, TAM, energia Por Referencia, vida Por Referencia, amuleto Por Referencia)
	definir seleccion como entero 
	
	Escribir "¿Qué objeto querés usar? (ingresa el número, 0 para cancelar)"
	escribir""
	escribir "1)  Fruto Místico"
	escribir "2)  Piedra de Luz"
	escribir "3)  Amuleto del Cometa"
	escribir "4)  Llave Antigua"
	repetir 
		leer seleccion
		segun seleccion Hacer
			0:
				escribir "Cancelado "
			1:
				Si inventario[seleccion-1] == 1 Entonces
					Escribir "Comés el Fruto Místico. Recuperás 30 de energía."
					energia <- energia + 30
					Si energia > 100 Entonces 
						energia <- 100 
					FinSi
					inventario[seleccion-1] <- 0
				Sino
					Escribir "Ese slot no contiene un Fruto Místico."
					seleccion<- -1
				FinSi
				Esperar Tecla
			2:
				Si inventario[seleccion-1] = 1 Entonces
					Escribir "Activás la Piedra de Luz. Tu vida aumenta 20 y energía 10."
					vida <- vida + 20
					energia <- energia + 10
					Si vida > 100 Entonces 
						vida <- 100 
					FinSi
					Si energia > 100 Entonces 
						energia <- 100 
					FinSi
					inventario[seleccion-1] <- 0 
				Sino
					Escribir "Ese slot no contiene una Piedra de Luz."
					seleccion<- -1
				FinSi
				Esperar Tecla
			3:
				Si inventario[seleccion-1] = 1 Entonces
					Escribir "El Amuleto ya está equipado. Protege contra una muerte segura."
					amuleto <- 1
				Sino
					Escribir "Ese slot no contiene un Amuleto."
					seleccion<- -1
				FinSi
				Esperar Tecla
			4:
				Si inventario[seleccion-1] = 1 Entonces
					Escribir "La Llave Antigua brilla; la guardás para un momento decisivo."
					llave <- 1
				Sino
					Escribir "Ese slot no contiene una Llave Antigua."
					seleccion<- -1
				FinSi
				Esperar Tecla
			De Otro Modo:
				Escribir "No ocurre nada."
				seleccion<- -1
				Esperar Tecla
		FinSegun
	Hasta Que seleccion > 0 O seleccion <= TAM
	Si vida <= 0 Entonces
		EjecutarFinalPorVida(amuleto, vida) 
	FinSi
	Si energia <= 0 Entonces
		EjecutarFinalPorEnergia(amuleto, energia) 
	FinSi
FinSubProceso

SubProceso  EjecutarFinalPorVida(amuleto Por Referencia  ,vida Por Referencia  )
	LimpiarPantalla
	Escribir "Tu vida llegó a 0..."
	Si amuleto = 1 Entonces
		Escribir "El Amuleto del Cometa te protege: te sostienes con energía residual."
		vida <- 20
		amuleto <- 0 
		Esperar Tecla
	Sino
		Escribir "Has caído. Final prematuro por heridas."
		Escribir "FINAL TRÁGICO: Caída del Viajero."
	FinSi
FinSubProceso

SubProceso  EjecutarFinalPorEnergia(amuleto Por Referencia  ,vida Por Referencia)
	LimpiarPantalla
	Escribir "Tu energía se agotó completamente..."
	Si amuleto = 1 Entonces
		Escribir "El Amuleto te mantiene consciente un momento más."
		energia <- 10
		amuleto <- 0
		Esperar Tecla
	Sino
		Escribir "Caes exhausto bajo el cielo. Final por agotamiento."
		Escribir "FINAL: Agotamiento del Caminante."
	FinSi
FinSubProceso

SubProceso MostrarEstado(vida, energia, amuleto, llave)
	Escribir "================================================================================"
	Escribir "      VIDA: ", vida, " / 100          ENERGÍA: ", energia, " / 100"
	Escribir "================================================================================"
	Si amuleto = 1 Entonces
		Escribir "  Amuleto del Cometa: EQUIPADO"
	FinSi
	Si llave = 1 Entonces
		Escribir "  Llave Antigua: EN POSESIÓN"
	FinSi
	esperar 2 segundos 
FinSubProceso

SubProceso MostrarMenuJugador(vida Por Referencia,  energia Por Referencia, amuleto Por Referencia, llave Por Referencia , inventario Por Referencia , TAM)
	definir opc_menu Como Entero	
	Repetir
		LimpiarPantalla
		Escribir "================= MENÚ DEL JUGADOR ================="
		Escribir "1) Ver estado (vida / energía)"
		Escribir "2) Ver inventario"
		Escribir "3) Usar objeto"
		Escribir "4) Volver a la historia"
		Leer opc_menu
		Segun opc_menu Hacer
			1:
				MostrarEstado(vida, energia, amuleto, llave) 
				Esperar Tecla
			2:
				VerInventario(inventario, TAM)
				Esperar Tecla
			3:
				UsarObjeto(inventario, TAM, energia, vida, amuleto) 
				Esperar Tecla
				
			4: 
				Escribir "Volviendo a la historia..."
				Esperar 2 Segundos
			De Otro Modo:
				Escribir "Opción inválida."
				Esperar Tecla
		FinSegun
	Hasta Que opc_menu = 4
FinSubProceso
//_____________________________________________________________________________________________________________________________________


//_____________________________________________________________________________________________________________________________________

//                                              PRIMER CAPITULO - CAMINO DE VALOR 
subproceso Capitulo_1
	LimpiarPantalla
	Escribir "========================================================================================================================"
	Escribir "                                         CAPÍTULO 1 -  EL LLAMADO DEL COMETA"
	Escribir "========================================================================================================================"
	Escribir "                               La noche se abre en tu aldea y un cometa surca el cielo."
	escribir""
	Escribir "                         .                                  .     :^                                                        .    .    " 
    Escribir "     .    .                           ..          ..              .     :~                 ...                    ..                   .    " 
    Escribir "          .       :~^..                                            .            ..     .:^:::...:.                                .         " 
    Escribir "   :          .~?JJ?J~.::         ..                          .:   .                  :^^!!^:.:..:.          .    .      .                   " 
    Escribir "            :~YY55YY7^  .^...                  ..      :^                             7~^~!^:.:^..^                      !^                 " 
    Escribir "        .:~?PP5Y?7?:  :. .~J^::.                       ^~  ..                 .       ~!^^^::.... ^   .       .          .           .      " 
    Escribir "    .::^~7YPYJYPPY^   :~.  ....::....                   .. ..   .                .    .^^^^:^^: .:.   .                              ..     " 
    Escribir "  .:^^~!?J55YYPG7:.^.  :^.:.     ....:::.               ..      .                       .::.~~:.:.                             .            " 
    Escribir ".:^^^~7???Y5?^:.   .:::  ..!!      ::  .::                                                 ..                                  .            " 
    Escribir ":^^^:~?Y7JP!     ..  :!7~  .~^.   ..7:   .^^:.                   .:.                .                          .^:.             .           " 
    Escribir "^^^^!J5YY?.      :!^. :JJY^:..    .::~~.   .::::^.             :!J~::.              .                        .!JJ..:::..                    " 
    Escribir "^^^77JYJJ~~^. .:.  :~: .^~!!!^.      .:~^.     ^?^::.       .^7JJJ~ .::.        .:                        .:~YPP7!:. ...::..                " 
    Escribir "~~~~!JYPG5BGJ~:.~^^:..:.     . .!?!~:  .^~~:.     ..::...:::!7?777...::::::.                            .:^^!75Y?JJ7~..!:.:::...            " 
    Escribir "^^~~777?J5GP5P~  .~!!~~~..     ..::!7!!!^^::^::.  .^:..^^..:^!7!!7?577~:...::..                     :~::: .^~JYY5P?^:^..:::::.....:..       " 
    Escribir "::^^~^^^^!PP55!~~~...::^::::..     .::^^^^^:....~!::^:...::. .:^^7YGJ^^:^^~: .:::.               .^?7^  .::^~?JJYJ~  .^. ..:~^:::...::....  " 
    Escribir ".:::::::~?PPJY5PPGJ~.        ...       .:::::.  .^!?!:... ..::.  .:^.:~::.:^:.  .::...    .::::::^7!.. .::^~7PP5Y!::..^^     ..::!^^:....:::" 
    Escribir "   .:.:^!!~~~!!?55PG5J7^..   .7?77^~^~^..:^::^:     ..  .:^. ..:.   . .::   .     ...:::^!?^      :.  .:^~7?P5^. .:?~:~^.::.    ^JY?~.      " 
    Escribir "          ::^^:~77?5JPBJ::::. ..~!~!7??J!!^^:^~^!:      .:^!!~:..            .::^^.....::77^        .:^~^~7P?.  .^?77J7^..::^^....^^!~^.    " 
    Escribir "          .. .:..^^.:^!!!..:::..   ...:^!77?!!. :^^:        ...:.::.    ...::~!!7~..:^.   :      .  .:..::!~.    :.:!~!!:     .:::.   .     " 
    Escribir "    ^^ .  !!       ^.  ..    ...   :.       .:     .   ..    .7  .. ..::.?^..      ^:.   ~7 :    7.      ..       :    ..    ^    :.   ::   " 
    Escribir ". . 7^ J:.P? . ~! .5^ .  ^. 7! .   Y: ^.  . ^7   .  :  ?^ ^  ^5.^! ::.. .P! ^7 .. .P! ^:.Y7^J...:5. ~.. .?  ^. :..J    ?^:^ :P: ^ ?! : ?! :." 
    Escribir "?^J.P~~G:!B~!? 55 ~B^ ? ^Y..P!:J ~7P^.J~^.J^YJ~^~J :P::P~:Y.JJG!YY ?! ?.!GJ.7P.?!.!G?^5~:PJ7P.?!?5::J.?:~G..5^:?.^5..7:P!?Y 7P::5~G?^J.5Y.Y7" 
    Escribir "P?Y7G!5G!5P5PP!B5^JB!~P^?P^~#J?G!5GG7?G?GJP55YY?PG~J#~?B?JG?PPBJGY~G5~G7YPP!5B?G5!JBJ?B7?PGPG?PJ5B??P7G?YG^JGJJ5~JP~7G?BPPP!PG7?G5#5YP?BY?55" 
    Escribir "YPPPPJJJ??7~^77JY?YP5YPYY5P5B55PY5PJJ5YY555P5777J555PY7J?JYJY5JJJYJPPJYYYP5~JJ77J?7??J??G5P5Y5P5Y5??J?J?557PBP5#PJJ7?YJ?Y?!?YY7???YJ!~~~~~^^" 
    Escribir ":::~:.::^~!!^.^^^^::^^^^.:~^:::.::^^^^~^:^^~^....:^^^::.:^^~~:...:^^::.^:::..^:..^:::..:..:..::::::::^:. ...:::^~...  ::^^..:..:......      " 
    Escribir "J77!7!!~!7!??77?J!J?7!?Y7!J???J?YY??7?J5?77YJ?J?Y555PY?Y!J?JYJ??YYYYY5??PG?5J?J?!JGPYGGYYJY?7PPY5JJP5JYJJJJJ77YJJY??J~JGYY7?JJY5J5?7J???!J5?" 
    Escribir "7757YY~YPYYYJY??5Y5Y??!!Y!75YJP?YGP55P??P7:J5~55?G!^5!:?^5~?J5!~Y~YP:!J!5P7JY:?J^?BG~~575Y!P7YB?GJ7#J!G!5BBP~?P^JB?~P7Y57PJY#5?Y!5J!5!?P?JBG" 
    Escribir "::!.:^^^7P!:~!^!7?P5^!^:7^:55!J::JG?:7^.!~.7~:~~.~:::::..^^:.7^.~^^~^:^.^P^.^..:::?Y^::.?Y:J^:!:P?:J!^!:5JJ5 ^~.~#J.!^~?.~^^P~:^:!!^5^7G~:?J" 
    Escribir ".::::..:.::^:::..:5J:..:...~^::.::7:::::.....      .:.....:..:....  .....:::::. ...:.:::!7.~:...J! .....!7.::::.:7~::..:.:::^^::^::^~::7^:.." 
    Escribir "   ... ...   .....~:.    .  .:....        .                                          .........  .       .:       .........      ........... " 
    Escribir "                                        :^ ................:.     ...::......                                                               "
	esperar Tecla	
FinSubProceso

Funcion deci_camino <- decision_1(vida, energia, amuleto, llave, inventario, TAM) 
	Definir deci_camino Como Entero
	deci_camino <- -1
	Mientras deci_camino < 1 O deci_camino > 3
		LimpiarPantalla
		Escribir "¿Qué haces? (0 = abrir menú jugador)"
		Escribir "1) Seguir el cometa hacia el bosque."
		Escribir "2) Permanecer en la aldea y ayudar a la gente."
		Escribir "3) Explorar las ruinas cercanas en busca de pistas."
		Leer deci_camino
		Si deci_camino = 0 Entonces 
			MostrarMenuJugador(vida, energia, amuleto, llave, inventario, TAM) 
		FinSi
	FinMientras
FinFuncion

Funcion deci_valor <- desicio_2(vida, energia, amuleto, llave, inventario, TAM)
	Definir deci_valor Como Entero
	deci_valor <- -1
	Mientras deci_valor < 1 O deci_valor > 2
		limpiar pantalla
		Escribir "Sigues la luz entre árboles. Un anciano sabio te detiene."
		Escribir "Te ofrece un pequeño cofre sellado."
		Escribir "1) Aceptar el cofre."
		Escribir "2) Rechazar por desconfianza."
		Escribir "(0 = menú jugador) 1 o 2: "
		Leer deci_valor
		Si deci_valor = 0 Entonces 
			MostrarMenuJugador(vida, energia, amuleto, llave, inventario, TAM) 
		FinSi
	FinMientras
FinFuncion

Funcion opci_rio <- decision_3(vida, energia, amuleto, llave, inventario, TAM)
	definir opci_rio Como Entero
	opci_rio <- -1
	Mientras opci_rio < 1 O opci_rio > 2
		LimpiarPantalla
		Escribir "Llegás a un río caudaloso. El unico puente colgante parece frágil."
		Escribir "¿Qué hacés?"
		Escribir "1) Cruzar el puente con cuidado."
		Escribir "2) Buscar un paso alternativo río abajo."
		Escribir "(0 = menú jugador eleguir 1 o 2): "
		Leer opci_rio
		Si opci_rio = 0 Entonces 
			MostrarMenuJugador(vida, energia, amuleto, llave, inventario, TAM) 
		FinSi
	FinMientras
FinFuncion

Funcion opc_puente <- desicion_1Puente(vida, energia, amuleto, llave, inventario, TAM)
	Definir opc_puente Como Entero
	Esperar Tecla
	
	opc_puente <- -1
	Mientras opc_puente < 1 O opc_puente > 3
		Limpiar Pantalla 
		Escribir "Antes de subir al lugar del rito, podés preparar algo más."
		Escribir "1) Buscar provisiones (puede aparecer un Fruto Místico)."
		Escribir "2) Hablar con los ancianos (puede aportar vida/energía)."
		Escribir "3) Partir al sitio del Rito ahora."
		Escribir "(0 = menú jugador 1 a 3): "
		Leer opc_puente
		Si opc_puente = 0 Entonces 
			MostrarMenuJugador(vida, energia, amuleto, llave, inventario, TAM) 
		FinSi
	FinMientras
FinFuncion

Funcion dec_fin <- desicionFin(vida, energia, amuleto, llave, inventario, TAM) 
	Definir dec_fin Como Entero
	LimpiarPantalla
	Escribir "========================================================================================================================"
	Escribir "                                       CAPÍTULO FINAL - EL RITO DEL COMETA"
	Escribir "========================================================================================================================"
	ESCRIBIR""
	Escribir "@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&####BBBBGGGGGGPPPPPPPPPPGGGGBBBBB#####&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@"
	Escribir "@@@ FIN @@@@@@@@@@@&@&&&&&&&&&&&&&&&&###BBGGP555YJJJ??77!!!!!!!!!77??JJY55PPPGGGB###&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@"
	Escribir "@@@@@@@@@@@@@&&&&&&&&##&&&&&&&&&###BBGPP5YJ7!~^::....:::..           ..:^~!7?JY5PPGBB##&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@"
	Escribir "@@@@@@@@@@@@@&&&&&&&&&&&&&&&###BBGPP5YJ7~:..... .::.::::....               ..:^~7?Y5PGGBB##&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"
	Escribir "@@@@@@@@&&&&&&&&&&&&&&&&&###BGP55YJ?!^:....:.....:....    ..                   . .:~7JY5PGGB###&&&&&&&&&&&&&&&&&&&&&&&&&"
	Escribir "@&&&&&&&&&&&&&&&&&&&&&###BGP5YJ?7~^...:^~^:::... ...                                 .:!?Y5PPGB###&&&&&&&&&&&&&&&&&&&&&&"
	Escribir "&&&&&&&&&&&&&&&&&&&###BGG5YJ?7!~:..::..::.::..                                           :!?Y5PGBB###&&&&&&&&&&&&&&&&&&&"
	Escribir "&&&&&&&&&&&&&&&&####BGP5YJ7!~^:::.::.  ...::::::....                                  ...  .^!JY5PGB######&&&&&&&&&&&&&&"
	Escribir "&&&&&&&&&&&&&###PPBGP5YJ?77!^::.:..   .:::........            .. . ...                        :!JY5PGB###&&&&&&&&&&&&&&"
	Escribir "&&&&&&&&#######BGPP5YJ?77!~^.....  ..:::::.    .    ..          ..^^:^^...              ..      :!JY5PGB####&&&&&&&&&&&&&"
	Escribir "&&&&########BBGGP5YJ??!^^.    ...:....:..... .              .^:^..^~:::^:               ...     .:^?Y5PGB####&&&&&&&&&&&"
	Escribir "#####B##BBBBBGGP5Y??J7:..:^^^^^^~!7???!~~!^^:..     ..   .:..  ...:::::::::...               .    :7?:~JY5GGBB#######&&&&&&"
	Escribir "######BBBBBBGGP5YJ?J?:..:^^^^^^~!7???!~~!^^:..     ..   .:..  ...:::::::::...               .    :7?:~JY5GGBB#######&&&&"
	Escribir "#######BBBBGGP5YJJJ7~~::^^::!7????JJJJJ7!^^^.......:^^^:::.. ..:::..::^:::.....    ...            .^^.:?Y5PGBBB######&&&"
	Escribir "##BB#BBBBPPPP5YJJJ7!77^. ..^???JJJJJJJJ?!:....^^^::^7?7!~:^:..::::....:::.:....                     .. :?Y5PGBBBB######&"
	Escribir "##B#B#BBBGPP5YJJJJ7?J!^:  :!?JJJ???J?!~!~:...:^!~~~~7???7!77!!~~^:... ..:::...                        . :JY5PGBBBB######"
	Escribir "#####BBBBGG5YJ?JJJJJ?~^^  :7???????7~.::^:..::..::^!!!!777777!!~^:::.  ..........                 .  ..  ^JY5PGBBBB#####"
	Escribir "###BBBBBGGP5J??JJJJJJ?!^. .:^!7!~~~^::^^:^^:..:..:^^:~!!~~~^^:^^^^:... ..    ..:                 ........ !JYPGGBBBB####"
	Escribir "###BBBBGGP5Y??JJJJJJ?77!^:..:::::^~~^~77!!~^:......::^~~~^^:...:^^::.    ..    ...                  ..... .?Y5PGGBBBB###"
	Escribir "##BBBBGGP5YJ7JJJJJJ??7!77!~:::..:::^~7~!~^::::... .:::^~!!~^~^:^^::^.    .       .                         !JYPPGBBBB###"
	Escribir "##BBBBGGP5Y?7JJJJ???77!7???7!^~^^~:~!~~^^:^~^::::.:^~~~777777!^^:...:.....        .                         .?Y5PGGBBB###"
	Escribir "#B###BGPPYJ77JJ?7??77??????????7!!~77~~^^:^^~^^::^^~!7!77??77!^:...:.....        .                         .?Y5PGGBBBB#&"
	Escribir "##@@@&GP5YJ7!JJ!^!!!????????777!7??~^^^^^^:^^::::^^!!!!7777!!!~::..::.:.....   ....                         7J5PPGGBBB&@"
	Escribir "#@@@@@#P5YJ!:!?~^^~!???????Y5?J5#&#Y::^:^:::::^~^:^77!!!!~^^^^^::..:. ...:....~^.....                       ?5Y5PGGB#&@@"
	Escribir "@@@@@@&P5YJ! ~7~7J!~7?????G&&BB@@@@&7:::^:::.?BBY~5BBG7~!~^^::..::....:::...^5BG? ^7J7:              :~!~..J&@B5PPG&@@@@"
	Escribir "@@@@@@@&PYJ7.~7YGBG~!777?G&#&@&@@@@@5...:...7BP##PPBB&Y:^^:... !PGY^ ..:^~::Y55BB7J#@@B7.           ~P#&&Y?5@@@BPP#@@@@@"
	Escribir "@@@@@@@@BYJ?..?&#&@!7PY77#@&@@@@@@@@&?. ..  5&&@@&GG@@&^ ...  ~&@@&5:.7P#BPP#GB@&BB&@@@&?         7PBB&&@@#G@@@&PB@@@@@@"
	Escribir "@@@@@@@@@#GJ~ ?@@@@#PG#J5&@@@@@@@@@@@&Y:  .?#@@@@@@@@@@B^    !#@@@@#~7B&@&##&@@@@@@@@@@@#^ ~7^  :P&@@@@@@@@@@@@@##@@@@@@"
	Escribir "@@@@@@@@@@@&#G#@@@@@&#&&@@@@@@@@@@@@@@&J^:Y@@@@@@@@@@@@@5~~ ~B@@@@@&B@@@@@@@&@@@@@@@@@@@@575GG^.P@@@@@@@@@@@@@@@@@@@@@@@"
	Escribir "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&B@@@@@@@@@@@@@@@@&PB&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##@GP@@@@@@@@@@@@@@@@@@@@@@@@"
	Escribir "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"	
	
	Esperar Tecla
	
	dec_fin <- -1
	Mientras dec_fin < 1 O dec_fin > 4
		Limpiar Pantalla
		Escribir "El cometa brilla con fuerza. Una voz resuena: "
		escribir""
		Escribir "  - Escoge tu acción final."
		escribir""
		Escribir "1) Entregar tu energía por la salvación."
		Escribir "2) Oponerte al cometa con voluntad."
		Escribir "3) Buscar un equilibrio y negociar su poder."
		Escribir "4) Intentar usar la Llave Antigua en el corazón del Rito (si la tenés)."
		Escribir "(0 = menú jugador Elige 1..4): "
		Leer dec_fin
		Si dec_fin = 0 Entonces 
			MostrarMenuJugador(vida, energia, amuleto, llave, inventario, TAM) 
		FinSi
	FinMientras
FinFuncion

Funcion opc_llave <- secreto(vida, energia, amuleto, llave, inventario, TAM)
	Definir opc_llave como Entero
	LimpiarPantalla
	Escribir "Usás la Llave Antigua en un altar oculto. Un portal se abre, revelando un núcleo del cometa."
	
	Escribir "@@@&&&&&##BGP5YYJYYPGBBBBBBBBBBGGGGP5J?777?Y5PPPPPPPPPPPPPPPPPP55J???JY555555555YY7::::~Y55555PPPPPP5YJJ??J5PGGGGBBBBBBBBBBBB###############"
	Escribir "&&&&&&&&########BBGP5YJ???JY5PPPPPPPPP555Y?7!!!7?JYYYYYYYYYYYYYJJJ?7777?JJJJJJ??7:....!JJJJYYYYYJ?7!!7?J5555PPPPPPPPGGGGGGGGGGGGGPPP55YYYYYY"
	Escribir "&&&&&&&########BBBBBGP5YJ??77?JY55P555555YY?7!!!!7?JJJJJJJJJJJJJJ??7!!77???????7:....:???JJJJJJ?!!!!7?JYY555555PPPPPPPPPPPPPPPP55YYJJJJJYY5P"
	Escribir "&&&&&&########BBBBBBBGGGPYJ?77!77?JY555YYYYJJ7!!~~!7???JJJJ???????7!!!!!7777777~.....~77??????!~~~!!?JJYYYYY555555555555555YYYJJ????JJY5PPGG"
	Escribir "&&&&#########BBBBBBBGGGGGPP5J?7!!!!77?JYYJJJJ?7!~~~~~!7??????777777!~~~~!!!!!!!:. ..:!777777!~~^^~!7?JJJJJYYYYYYYYYYYYYYJJ??7777??JY55PPGGBB"
	Escribir "&##########BBBBBBBBGGGGGPPPPP5Y?7!!~~~!!7??????7!~^^^^~!!77777!!!!!~^^^^~~~~~~^.   .^!!!!!!~^^^^~!7?????JJJJJJJJJJJJ??77!!!!77?JJY5PPPGGGGBB"
	Escribir "####BBBBBBBBBGGGGGGGGPPPPPP5555YYYYJ?!~^::::^~!!!!!~^::..:^^~~~~^^^^::::::^::.    .:^^^^::.:::~~!!!!!7777777!!!~~^^^^~~!7?JJYY5555PPPPGGGGGG"
	Escribir "GGBBBBBBBBGGGGGGGGPPPPPPP55555YYYYJJJJ?7~^:::.::^~~~~^:....::^^^^::::....::::.    .::::....::^~~~~~!!!!!~~~^^^:::^^~~!7?JJYYY5555PPPPPPGGGGG"
	Escribir "!77?JJY55PPGGGGPPPPPP5555555YYYYYJJJJ????7!^:......:^^^:......::::::.........     .:.......:^^^^^^~~^^^:::..:::^^~!7???JJJYYY55555PPPPPPPGGG"
	Escribir "BGGGPP5YJ?7!~~^^::::::::::::^^~~!!!7777!!!!~~~~~^^::..                                  .....       ...:^^~~~!!!777????JJJJJJJJJ??777!!~^^^^"
	Escribir "BBBBGGGGGPPP5YYJ?77!~^:::..... ......::^^^^^^^^^^^^::..                                           ..::^^^~~~!!!!!777!!!!~~^^^:::..........::"
	Escribir "BBBBGGGGGGGPPPP5555YYJ??7!~^^::..           .....:::.....                                      ...:::::^^^^^^^^:::......      .....::^^~~!77"
	Escribir "BBBBBBGGGGGGPPPP5555YYYYJJJ???7!~~^^:...                                                     .............            ....:::^~~!!7??JJYY555"
	Escribir "BBBBBBBGGGGGPPPPP5555YYYJJJJ????777!!!~~^^::....                                                             .....::^^~~!!77???JJYYYY5555PPP"
	Escribir "BBBBBGGGGGGPPPPPP5555YYYYJJJ????7777!!!~~~^^^:::.....                                               ......::^^^~~!!!777????JJJYYYY55555PPPPP"
	Escribir "BBBBGGGGGGGPPPPP55555YYYJJJJ????777!!!~~~~^^^::::.....                                        ......:::^^^^~~~!!!!777????JJJJJYYY55555PPPPPP"
	Escribir "BBBGGGGGGGPPPPP55555YYYJJJJ????777!!!~~~^^^:::.....                                           ....:::::^^^~~~!!!!7777???JJJJJYYYY55555PPPPPG"
	Escribir "BBGGGGGGPPPPP55555YYYJJJJ???777!!~~^^:::.....                                                     ....:::^^~~~!!!!777????JJJJJYYY55555PPPPPG"
	Escribir "GGGGGPPPPPP5555YYYJJ??77!!~^^^::...                                                                      ...::^^~~!!77????JJJJYYYY5555PPPPPP"
	Escribir "55YYJ77!~~^^::....          .....:::^^^^^^^^:::::...                                         ...:::::::::......       ....:^^~!77?JJYY5555PP"
	Escribir "~^^^::::........::::^^^~~!!!!777777!!!~~~~^^^::...                                             ..:::^^^^~~~~~~~^^^:::............::^~~!7?JYY"
	Escribir "^^^^^^^~~!!777??JJJJJJJJJJJ?????7777!!!!~~^^:...       ......                                    ..:^^~~~~!!!!!7777777!!!~~^^:::.......:::^~"
	Escribir "JJJYY5555PPP5555555YYYYYJJJJJ????777!!!~^^:.... .......:......    .             .....     ....      .:^~~~!!!77777??????JJJJJJ??77!~~^^:::::"
	Escribir "GGGGGGPPPPPPPP5555555YYYYYJJJJ???77!!~^::......:::::::::::..........    ...     .......    ..::..     ..:~!!!777?????JJJJJYYYYYYYYY5YYYYJJ?7"
	Escribir "GGGGGGGGGGPPPPPPP555555YYYJJJJ??7!!~^:::::::^^^~~~~^^^^^^:.....:::..   .....    .:::::::.   ..:^^:..     .:^!!77????JJJJYYYYYY55555555PPPPPP"
	Escribir "BBBBBGGGGGGGPPPPPP5555YYYYJJ??7!~~^^^^^^~~~!!!!!!!!~~~~^:::::::^^:..  ..:::..  ..:^^^^^^^:.....:^~~^^:.     .:~!7???JJJJYYYY555555PPPPPPPPPP"
	Escribir "BBBBBBGGGGGGGPPPPP5555YYYJ?77!!~~~~~!!7777777777!!!!!~~^^^^^^^^^^:.....:^^^:.....^^^^~~~~~^:....:^!!!!~^:..   ..:~7?JJJJYYY55555PPPPPPPGGGGG"
	Escribir "BBBBBBBGGGGGGPPPPP555YJJ?77!!!!777????????????777777!~~^^^~~~~~~~:....:~~~~~:....^~~~~!!!!!~^:::::^!!7777!~^:.  ...^!?JYYYY5555PPPPPPGGGGGGG"
	Escribir "BBBBBBBGGGGGGPPPP55YJJ??7777??JJJJJJJJJJJJJJJJ????77!~~~~!!!!!!!~:::::~~~~!~^....^!!!!!!!7777!~^:::^~7??????7!^......:^7JYY5555PPPPPGGGGGGGB"
	Escribir "BBBBBBBGGGGGPP55YYJJJJ?JJJJYYYYYYYYYYYYYYYJJJJJJ??7!!!!!7777777!~^::^~!!!!!!~:..::!7777777????7!~^^^^~7???JJJJJ?!^:.....:~7Y555PPPPPGGGGGBBB"
	Escribir "BBBBBBGGGGPP55YYYJJJYYY55555555555555555YYYYYJJJ?77777????????7!~^^^~!777777~^:::^!777?????JJJ???7~~^~~!?JJJJYYJYJ?7~:....:^!?YPPPPPGGGGGBBB"
	Escribir "BGGPPPPPPPPPGGGGGGGGGGGGGGGGGGGGGPPPPPPPP555YJJJJYYYYYYYYYYYJ?!~~~!?JJJJJJJJJ?!~~~!?JYYYYYYYYYYYYYYYYYJ?7777?Y5555555PPPPP5Y?!~^^^^~!J5GGBBB"
	Escribir "GGGGGGGGGGGBBBBBBBBBBBBGGGGGGGGGGGGGPPPPP55YYYYYY5555555YYYYY7!~~!7JJYYYYYYYYJ7!~~!?YYYYYYYY55555555555YJ??7??Y555PPPPPPPPPPPP5J7!~^~~!7YPGB"
	Escribir "GGGBBBBBBBBBBBBBBBBBBBBBBBBBBBBGGGGGGGPPP5YYYY55555555555555J!!~!7JYYYYYYYYYYYJ7!!!?YY5555555555555555PP55YJ??JY55PPPPPPPGGGGGGGP5Y?!~~~!7?Y"
	Escribir "BBBBB###BBB##BBBBBBBBBBBBBBBBBBBBGGGGGPP55555PPPPPPPPPPP555Y?!!!!JYYYYY555555YY?7!7?Y555555555PPPPPPPPPPPPP5YYJYY5PPPPPGGGGGGGGGGGGGPY?7!!!!"
	Escribir "########################BBBBBBBBBBGGGPPPPPPGGGGGGGGGPPPPPP5J7!!7Y55555555555555Y?77?J55PPPPPPPPPPPPPPPPPPPGPPPPP5555PPGGGGGGBBBBBBBBBBBBBGG5"
	Escribir "&&&&&&&&&&&&#################BBBBGGGPGGGGBBBBBBGGGGGGGGPPPJ777?5PPPPPPPPPPPPPPP5Y???J5PPPPPPPPPPGGGGGGGGGGGGGGGGGGGPPPPPGGBBBBBBBBBB########"	
	Esperar Tecla
	
	opc_llave <- -1
	Mientras opc_llave < 1 O opc_llave > 2
		Limpiar Pantalla
		Escribir "Dentro del núcleo hay dos opciones:"
		Escribir "1) Sellarlo para siempre, cerrando la conexión del cometa con la tierra."
		Escribir "2) Canalizar su energía para curar y transformar la vida en el mundo."
		Escribir "(0 = menú jugador 1 o 2): "
		Leer opc_llave
		Si opc_llave = 0 Entonces 
			MostrarMenuJugador(vida, energia, amuleto, llave, inventario, TAM) 
		FinSi
	FinMientras
	
FinFuncion
//_________________________________________________________________________________________________________________________________________



//_________________________________________________________________________________________________________________________________________

//                                                              CAMINO -  COMUNION 

Funcion deci_comu <- desicion_1Com( vida Por Referencia, energia Por Referencia , amuleto Por Referencia , llave Por Referencia , inventario Por Referencia , TAM)
	Definir deci_comu Como Entero
	Escribir "Ayudás a la gente: reparás techos y repartís agua."
	Escribir "Una anciana te da un Amuleto del Cometa como gratitud."
	inventario[2]<- 1
	amuleto <- 1 
	Esperar Tecla
	Escribir ""
	Escribir "Se organiza una expedición para investigar el origen del bloqueo del río."
	Escribir "¿Qué proponés?"
	Escribir "1) Liderar la expedición tú mismo."
	Escribir "2) Quedarte en la aldea organizando recursos."
	
	deci_comu <- -1
	Mientras deci_comu < 1 O deci_comu > 2
		Escribir "(0 = menú jugador 1 o 2): "
		Leer deci_comu
		Si deci_comu = 0 Entonces 
			MostrarMenuJugador(vida, energia, amuleto, llave, inventario, TAM) 
		FinSi
	FinMientras
FinFuncion

SubProceso EtapaFinal( vida Por Referencia, energia Por Referencia , amuleto Por Referencia , llave Por Referencia , inventario Por Referencia , TAM)
	LimpiarPantalla
	Escribir "========================================================================================================================"
	Escribir "                                                    CAMINO - EL PUENTE "
	Escribir "========================================================================================================================"
	ESCRIBIR""
	Escribir "                                    ?7JB5~~^^^^^^^^^^^^^^^YG?7!                                     "
	Escribir "                                 .^!7JGBP^               .YBG?7~:.                                  "
	Escribir "                              .^!!7JP!5GGJ               !GBY?P?7!~:                                "
	Escribir "                            :~!~!JPBP:5JYG^              5PYJ^BG5?!!!~.                             "
	Escribir "                         .^!!~!Y5!?55:5JJBJ             ^BYJY^5P!75?~~!!:                           "
	Escribir "                       .~!~^!YGP: Y?5^YJJ5G^            J#YJY~YJ? !BP?~^!!^.                        "
	Escribir "                     .~!~^!YPGPY.^Y!5~YJY?BJ           :GGYJY~Y!Y ^5GGPJ~^~!~.                      "
	Escribir "                   :!7~^~J5?5Y?Y.7J^5~JJYYGB^          ?#BYJJ!5^5^^575YJP?~^~7~.                    "
	Escribir "                 :!7~^~JPJ.7P~7Y:Y!^57JYYPPY!^^^^~~^^^^?PGPYJ75.J7^5~7P^:YP?~^~7!:                  "
	Escribir "               :!7~^~?PBG.:5J ?Y!5:~5JYY?!~^^^^^^^^^^^^^~!7J5Y5:!Y!5!.5Y ~BG5?^^!7!:                "
	Escribir "       .::::..!7~^~?P5~YP:?5^ ?YJ5JJJ7!^::::::::^^^::::::::^~!?JJ5J5! !5!~PJ~557^^!7!: :::::.       "
	Escribir "    :!????Y557!^~755!  Y57YY~?55Y?7!~^^^^^^^^^^^~:~^^^^^^^^^^^^!7?Y55Y7!5J?5?  !557^^!775YJ???7!.    "
	Escribir "   ^YJJJJYGBJ^^755!  .:Y5YYYYJ?7!~^^^^^^^^^^^^^:~:^^^^^^^^^^^^^^^~~!?JY5555J:.  7557^^!5BPJJJJJJ.   "
	Escribir "   .J55555GP7!YP?^^!7JY55YJ7!!^:...:::::::::::::~.^^:::::::::::::...^~!7?Y55YJ7!^^?PY!~JPG5555P7    "
	Escribir "    !Y???JP55PPJ?JY5P5Y?7!!!~^^^~~~^^^^^^^^^^^^^!.:~^^^^^^^^^^^^^~~~^^~~!!7?J5P5YJ?JPPJ555JJJJY^    "
	Escribir "    7YJJJJPPBGPPP55J?!!~^:..:^^^^^^^^^^^^^^^^^^^!.:!~~~~~~~~~~^^::::::::..:^~!7JY5PPPGBB55JJJJY~    "
	Escribir "    7Y?J?JGGBGPY?7!!~^.. .:^^^^^^^^^^^^^^^^^^^^~~ .!~~~~~~~~~~~^^^^^^^^^::. .:^~!7?J5PGBGP?JJJY~    "
	Escribir "  .^J55555G5J7!~~~^:. .::^^^^^^^^^^^^^^^^^^^~~~!~ .!~^~~^~~~~^^^^^^^^^^^^^^:.  .:^~~~!?YPG5555P?:.  "
	Escribir ":?JJJYYYYYJ????????????????????????????????????JJ??Y?????????????????????????????????????JYYYYYJJJ?."
	Escribir ""	
	Escribir "La energía en el aire es densa. Todo converge al rito."
	MostrarEstado(vida, energia, amuleto, llave)
	
	Repetir 
		
		Eleccion_1Puente <- desicion_1Puente(vida, energia, amuleto, llave, inventario, TAM) 
		Segun Eleccion_1Puente Hacer
			1:
				energia <- energia - 5
				Escribir "Buscás provisiones y encuentras un Fruto Místico escondido."
				inventario[0] <- 1// obtuviste Fruto Místico
				Esperar Tecla
				
			2:
				energia <- energia - 5
				vida <- vida + 10
				Escribir "Los ancianos te bendicen, recuperás algo de vida."
				Esperar Tecla
				
			3:
				Escribir "Partís hacia el lugar donde el cometa toca el horizonte..."
				Esperar Tecla
				Eleccion_1Puente <- -1
		FinSegun
	Hasta Que Eleccion_1Puente = -1
	//_________________________________________________________________________________________________________________________________________
	
	//                                             CAMINO FINAL- CAMINO DEL VALOR 
	//_________________________________________________________________________________________________________________________________________
	
	EleccionFinal <- desicionFin(vida, energia, amuleto, llave, inventario, TAM)
	segun EleccionFinal Hacer
		1:
			LimpiarPantalla
			Escribir "Decidís entregar tu energía por el bien mayor."
			escribir""
			Escribir "                                                .?GP!.                                              "
			Escribir "                                               .Y5P5?7                                              "
			Escribir "                                              .!Y5GG5?~:                                            "
			Escribir "                                         .:^!?YPPGGPGGG5J!:.:                                       "
			Escribir "                                       .!??YGGGGGGGGBBBGP?~^!~.                                     "
			Escribir "                                       !JJ7?5PPGGBBBBBGPJ7:~7!^                                     "
			Escribir "                                      :JGB7?55PGBBBBBBPJ?~:YBP7.                                    "
			Escribir "                                      ~P#G~?YYPBBBBBB#G?!^.Y#G?.                                    "
			Escribir "                                     :Y#BP!?YPPGBBBBBBGG!:^GBBB~                                    "
			Escribir "                                    :YB5Y5!7YGPGBBBB#GGB!::JBGBP^                                   "
			Escribir "                                   ^JBG?!:^?5GGGGBBB###B?: .!YP#?.                                  "
			Escribir "                                  ?PPBJ^  ~JPGGGGBBBBB##Y~   ~YB5Y~                                 "
			Escribir "                                 ?BBGG~   !5PGGGGBBBB##B5~.   7B#&G^                                "
			Escribir "                                ^GBBG7.  ^B&&##BBBB###&&BP?   :P###!                                "
			Escribir "                                JBBG7    ?@@@@@@@@@@@@@@@@G.   ^P##J                                "
			Escribir "                               !BBP!    .G@@@@@@@@@@@@@@@&&~    ~P#G:                               "
			Escribir "                              7BG?:     7@@@@@@@@@@@@@@@@&&?     ^P#P:                              "
			Escribir "                             ^GY^       G@@@@@@@@@@@@@@@@@&Y      ^P#7                              "
			Escribir "                             JG^       .B&@@@@@@@@@@@@@@@@&5       :P5.                             "
			Escribir "                            7GB?       .B@@@@@@@@@@@@@@@@@&Y       !#B~                             "
			Escribir "                          :?Y^7?        G@@@@@@@@@@@@@@@@@&J      .5J?P!                            "
			Escribir "                         ^7Y~ .~        P@@@@@@@@@@@@@@@@@&!      !7  JP:                           "
			Escribir "                         . ^.           7@@@@@@@@#&@@@@@@@B:         :7!^                           "
			Escribir "                                        .P@&@@@@@5G@@@@@@&7          .  .                           "
			Escribir "                                         .P&@@@@@JP@@@@@&7                                          "
			Escribir "                                          :#@@@@@5B@@@@&J                                           "
			Escribir "                                           G@@@@@P#@&@@#:                                           "
			Escribir "                                           G@@@@@!Y&&&@P.                                           "
			Escribir "                                           ?#####!75GGG~                                            "
			Escribir "                                           ~5GGG5!5PPPY~                                            "
			Escribir "                                            7PBGG?J55J.                                             "
			Escribir "                                             ?PGGJJ5J.                                              "
			Escribir "                                             :5GG?JY:                                               "
			Escribir "                                              !GP77J.                                               "
			Escribir "                                              :5P!~?.                                               "
			Escribir "                                              :5BY!~.                                               "
			Escribir "                                               .7Y~                                                 "
			Escribir "                                                 .                                                  "
			energia <- energia - 80
			vida <- vida - 60
			MostrarEstado(vida, energia, amuleto, llave)
			Esperar Tecla
			Si vida <= 0 O energia <= 0 Entonces
				Si amuleto = 1 Entonces
					Escribir "El Amuleto del Cometa se enciende y evita tu muerte, consumiéndose en el proceso."
					vida <- 30
					energia <- 10
					amuleto <- 0 
					inventario[2] <- 0 
					Esperar Tecla
					Escribir "Tu sacrificio fue grande, pero tu vida fue salvada por el amuleto. El mundo se reequilibra."
					
					Escribir "                                                    :?5PPPP5J77JYY5?                                "
					Escribir "                                                  .!5PPPPPPPP5YJJJJY^                               "
					Escribir "                                                 :JPPPPPPPPPPGGP55YY^                               "
					Escribir "                                                :5PPPPPPPPPP5Y?~!YP?.                               "
					Escribir "                                                ?GPPPPPPPPYJ77!~^!~                                 "
					Escribir "                                  :~:.      ..^~Y5PPPPPPP5Y5JJYYYJ!                                 "
					Escribir "                                ^7?7~~^^..^!7?YYYY5PPPPPP5YP5YY5PPY~                                "
					Escribir "                                7?7?77!!!!???Y5PPPPPP55J!^?55YYYJ?J?~.                              "
					Escribir "                                ^!!7???777!!!7?Y5P5J?7!~~^?PP5JJ??JY?!:                             "
					Escribir "                                  :~7?JJJJ??77!!!!!77???77Y5YJ?7!!?J?!!                             "
					Escribir "                              .^~!7??JY5PP55YJ??77!!~~~!!7??55J7!~7YJ77:                            "
					Escribir "                           .^!7???JJY55PPPPPPPP55YJ??77!!~~^~~~~~^~JYJ?:                            "
					Escribir "                        .^!777??JYY55PPPPPPPPPPPPPP55J?????7!~^^^~77YPP^                            "
					Escribir "                     .^!777??JJYY55PPPPPPPPPPPPPPPPPPPP5J????!!!~~7!~!7~:.                          "
					Escribir "                  .^!777??JJYY55PPPPPPPPPPPPPPPPPPPPPPPPPPY?7!!!!!!!!!~!!~^:.                       "
					Escribir "               .^!7?????JYY55PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP?^^~!!!!!!7?777!~~^:.                   "
					Escribir "            .^!7?????JYY555PPPPPPPYYPPPPPPPPPPPPPPPPPPPPPPPPY:::^~^!7?Y??????7!!!~^:..              "
					Escribir "           ~7??????JY555PPPPPPPPP7.JPPPPPPPPPPPPPPPPPPPPPPPP5^::^~^~!5P5?^:^!7???777!!~^^:..        "
					Escribir "         :7J????JY55PPPPPPPPPPP?: ^PPPPPPPPPPPPPPPPPPPPPPPPPY~::^~^~~JPP5?^.  .:~77???7!!!!!:       "
					Escribir "        ^YY??JY55PPPPPPPPPPP5?^.:7PPPPPPPPPPPPPPPPPPPPPPPP5!.~:^:^^^~?PPPPJ!:     .:~!?????7^       "
					Escribir "       ^55YY5PPPPPPPPPPPPP5?!~!7JPPPPPPPPPPPPPPPPPPPPPPPP7.  ^^::^^^~!PPPPP57^.       .^!!^.        "
					Escribir "       JPPPPPPPPPPPPPPP5?!!!!7?YPPPPPPPPPPPPPPPPPPPPPP57:    :^::^~^~~YPPPPP5?~.                    "
					Escribir "      ^5PPPPPPPPPPPPPP5J!!7J??J5PPPPPPPPPPPPPPPPPPP5J~.      .~:^^~^~~JPPPPPP5?~^                   "
					Escribir "     :5PPPPPPPPPPPPPPPPY^^!??JY5PPPPPPPPPPPPPPPP5Y7~^:::......~^^^^^^~?PPPPPPP5J!:                  "
					Escribir "     !PPPPPPPPPPPPPPPPP?^^^!!?JY5PPPPPPPPPPPPPPY!!~^^:::::::::~^:^^^^~!PPPPPPPP5?~:                 "
					Escribir "     .5PPPPPPPPPPPPPP57^~!~^^~7JY55PPPPPPPPPPP5!^~^::::::::^~^~^:^^~^~!5PPPPPPP5Y?~:                "
					Escribir "      ^5GPPPPPPPPPPPPJ~~!777!^^!?Y55PPPPPPPPP5?^::~::^^^^7YYJJJ~:^^~^~~JPPPPPPPP5Y?~:               "
					Escribir "       .?PPPPPPPPPPPP5?7!7??J?!~!7JY5PPP5PPP5J!^::~~~~^^!PPPPPP7:^^^^~~7PPPPPPPPP5Y?^^              "
					Escribir "         ^?5PPPPPPPPPP5J???JYYJ?7!7?JY55PPPYJ!~^^~!!~~^^?PPPPPPY:^^^^^~!5PPPPPPPPP5Y?^^             "
					Escribir "           !GPPPPPPPPPP5YJJJYY555YJ?77?YP5J77!77???7!~~!5PPPPPP5^:^^~^~~5PPPPPPPPPP557~.            "
					Escribir "           !PPPPPPPPPPPP5YYY555PPPPP5YJY5YYYYYYYJJ??!!!?5PGPPPPP~:^^~^~~JPPPPPPPPPPPP57:            "
					Escribir "           ?GPPPPPPPPPPPP55555PPPPPPPPPPPPPPPP5YYJJ?77?J::~!7YPG7:^^~^~~7PPPPPPPPPPPPP5^            "
					Escribir "          .5PPPPPPPPPPPPPPPP5PPPPPPPPPPPPPPPPP55YYYJ??J?      :~!^^^^^^~!PGGGGGPPPPP5?^             "
					Escribir "          ?PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP55YYYJY5~        :^^^^~^~~~~!7777!~^:.               "
					Escribir "         .PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP55555P7         :~:^^~^~!:                          "
					Escribir "         ^PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP7          .~^^^~^~!^                          "
					Escribir "         ~PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP5~            ^^^^^^^~~                          "
					Escribir "         ~PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP?.             :^^^^~^~~.                         "
					Escribir "         ^PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPJ^               :^^^^~^~~:                         "					
					Escribir "FINAL ÉPICO-MODERADO: El Sacrificio Bendecido."
					
				Sino
					Escribir "Tu energía y vida se extinguen. El mundo queda a salvo por tu precio."
					
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&###GB#######BBBBBBBBBGGGGGGPPPPPPPPY7.    .JPGGGGGGBBBBBBBBB###############GP#BBB##############&&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&&&&######GG###BBBBBBBBBBBBGGGGGGPPPPPPPPP57.    .JPPGGGGGBBBBBBBBBB##############GP#BBB##############&&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&&########B##BBBBBBBBBBBBBGGGGGGPPPPPP5PP5Y7     .?5PPGGGGGGBBBBBBBBBB###############BBBB##############&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&###########BBBBBBBBBBBBBGGGGGGGPPPPP555P5Y7     .?55PPGGGGGGBBBBBBBBBBB##########&&#BBBBBBB############&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&############GBBBBBBBBBBBBGGGGGGGGGPPP5555Y5YY7     .7Y55PPPGGGGBBBBBBBBBBB##########BG#BBBBBBB############&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&############PGBBBBBBBBBBGGGGGGGGGPPPPP55YYYJJ!      !JY55PPPGGGGGBBBBBBBBB##########G5#BBBBBBBB###########&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&###############PGBBBBBBBBBGGGGGGGPPPPPPPP55YYJ??!      ^7JY55PPPPGGGGBBBBBBBBB#########G5#BBBBBBBBB##########&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&################PPBBBBBBBGGGGGGGGGGPPPP55555YJJ7~:      .!?J55555PPGGGGGBBBBBBBB########G5#BBBBBBBBBB##########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&###########BB5PBBBBGBGGGGGGGGGGPPPPP555YYJJ?!^       .~?JYY5555PPGGGGBBBBBBBB########G5#BBGBBBBBBB##########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&###########BBB5PBBBBGBGGGGGGGPPPPPPP5555YJ?77~:        ^7?JJYYY5PPGGGGBBBBBBBBB#B#####G5#BBGBBBBBBB##########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&###########BBB5PBGGGGGGGGPPPPPPPP5555YYJJ?7!~:.        .^!7?JYY55PPGGGGBBBBBBBB#B#####G5#BBGBBBBBBB##########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&###########BB5PBBBGGGGGGGPPPPP555YYYJJ?7~^::.          :^!7?JYY5PPGGGGBBBBBBBB#B#####G5#BBGBBBBBBB##########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&###########BB5PBBBBGBGGGGGGGGPPPPP555YJ?7!^:           .:^~7?JJYY5PPPGGGGBBBBBBB#####G5#BBGBBBBBBB##########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&#############5PBBBBGBGGGGGGGPPPPP55YYJJ?!~^^.         .^!7?JYY55PPPGGGGGGGBBBBBBBBB##G5#GGGBBBBBBB##########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&#############5PBBBBGBGGGGGGGPPPP55YYYJJJ?7!~:.        .^!?JYY55PPPGGGGBBBBBBBBBB#####G5#BBGBBBBBBBB#########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&#############PPBBBBGBGGGGGGPPPPP55555YYJ?777!^.       :~7?JYY55PPPGGGGBBBBBBBBBB#####G5#BBGBBBBBBB##########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&#############PGBBBBBBGGGGGGGGPPPPP55YYYJJJ??7~.      .~7?JJY55PPPPGGGGBBBBBBBB#B#####G5#BBBBBBBBBB##########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&############PGBBBBBBBGGGGGGGGPPPP555YYYYYJJ?!:      :7??JYY55PPPGGGGGBBBBBBBB#######G5#BBBBBBBBBB##########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&############PGBBBBBBBBGGGGGGGPPPPP5555YYYJJ?7^      ^7JYYY55PPPGGGGGGBBBBBBB########G5#BBBBBBBBB###########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&#&#########PGBBBBBBBBBGGGGGGPPPPPPP5555YJJ??~      ^?JY555PPPPGGGGGBBBBBBBB########G5#BBBBBBBB###########&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&##&#########PG#BBBBBBBBBGGGGGGGPPPPPP5555YY?~^      ~?Y555PPPGGGGGBBBBBBBBBB########G5#BBBBBBB############&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&############PG#BBBBBBBBBBBGGGGGGGPPPPPP55YJ7::.     ~JY55PPPPGGGGBBBBBBBBBB#########G5#BBBBBBB############&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&###########PG##BBBBBBBBBBBBGGGGGGGPPPP55Y5?.~.     !J55PPPGGGGBBBBBBBBBBB##########G5#BBBBBB############&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&#########PG####BBBBBBBBBBBGGGGGGGPPPP555?:~.     !Y5PPPGGGGGBBBBBBBBBB##########&G5#BBBBBB############&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&##&######PG#######BBBBBBBBBGGGGGGGPPPPP5Y?7.     !Y5PPGGGGBBBBBBBBB#############&GP#BBBBBB############&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&#########PG#########BBBBBBBBBBGGGGGPPPPP55?.     7Y5PGGGGGBBBBBBB###############&GP#BBBBBB############&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&&&#&&&###GG##########BBBBBBBBBBGGGGPPPPPP5?.    .7YPPGGBBBBBBBBB################&GP#BBBB##############&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&###GG############BBBBBBBBGGGGGGGGPPP?.     75PGGGBBBBBBB##################&GP#BBB###############&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&#&GB#############BBBBBBBBGGGGGGGGPP?.     7YPGGBBBBBBB##################&&GP#BBB###############&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&GB&##############BBBBBBBGGGGGGGGP?.     75PGBBBBBBBB###############&&&&&GP#BBB###############&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&GB&###############BBBBBBBBBGGGGGP?.     ?5PGBBBBB##################&&&&&GP###B##############&&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&GB&###############BBBBBBBBBBGGBGPJ.    .?PPGBBBB###############&&&#&&&&&BP&##B############&&&&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&GB&###############BBBBBBBBBBBBBGPJ.    .?PGGBBBB##############&&&&#&&&&&BP&###############&&&&&&&&&&&&&&&&&"					
					Escribir "FINAL TRÁGICO: Héroe Consumido."
				FinSi
				Esperar Tecla
				Limpiar Pantalla
			Sino
				Escribir "Tu ofrenda calma al cometa. La tierra respira y prospera."
				Si llave = 1 Entonces
					Escribir "La Llave Antigua reacciona con tu sacrificio y desbloquea un portal..."
					
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&###GB#######BBBBBBBBBGGGGGGPPPPPPPPY7.    .JPGGGGGGBBBBBBBBB###############GP#BBB##############&&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&&&&######GG###BBBBBBBBBBBBGGGGGGPPPPPPPPP57.    .JPPGGGGGBBBBBBBBBB##############GP#BBB##############&&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&&########B##BBBBBBBBBBBBBGGGGGGPPPPPP5PP5Y7     .?5PPGGGGGGBBBBBBBBBB###############BBBB##############&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&###########BBBBBBBBBBBBBGGGGGGGPPPPP555P5Y7     .?55PPGGGGGGBBBBBBBBBBB##########&&#BBBBBBB############&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&############GBBBBBBBBBBBBGGGGGGGGGPPP5555Y5YY7     .7Y55PPPGGGGBBBBBBBBBBB##########BG#BBBBBBB############&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&############PGBBBBBBBBBBGGGGGGGGGPPPPP55YYYJJ!      !JY55PPPGGGGGBBBBBBBBB##########G5#BBBBBBBB###########&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&###############PGBBBBBBBBBGGGGGGGPPPPPPPP55YYJ??!      ^7JY55PPPPGGGGBBBBBBBBB#########G5#BBBBBBBBB##########&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&################PPBBBBBBBGGGGGGGGGGPPPP55555YJJ7~:      .!?J55555PPGGGGGBBBBBBBB########G5#BBBBBBBBBB##########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&###########BB5PBBBBGBGGGGGGGGGGPPPPP555YYJJ?!^       .~?JYY5555PPGGGGBBBBBBBB########G5#BBGBBBBBBB##########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&###########BBB5PBBBBGBGGGGGGGPPPPPPP5555YJ?77~:        ^7?JJYYY5PPGGGGBBBBBBBBB#B#####G5#BBGBBBBBBB##########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&###########BBB5PBGGGGGGGGPPPPPPPP5555YYJJ?7!~:.        .^!7?JYY55PPGGGGBBBBBBBB#B#####G5#BBGBBBBBBB##########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&###########BB5PBBBGGGGGGGPPPPP555YYYJJ?7~^::.          :^!7?JYY5PPGGGGBBBBBBBB#B#####G5#BBGBBBBBBB##########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&###########BB5PBBBBGBGGGGGGGGPPPPP555YJ?7!^:           .:^~7?JJYY5PPPGGGGBBBBBBB#####G5#BBGBBBBBBB##########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&#############5PBBBBGBGGGGGGGPPPPP55YYJJ?!~^^.         .^!7?JYY55PPPGGGGGGGBBBBBBBBB##G5#GGGBBBBBBB##########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&#############5PBBBBGBGGGGGGGPPPP55YYYJJJ?7!~:.        .^!?JYY55PPPGGGGBBBBBBBBBB#####G5#BBGBBBBBBBB#########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&#############PPBBBBGBGGGGGGPPPPP55555YYJ?777!^.       :~7?JYY55PPPGGGGBBBBBBBBBB#####G5#BBGBBBBBBB##########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&#############PGBBBBBBGGGGGGGGPPPPP55YYYJJJ??7~.      .~7?JJY55PPPPGGGGBBBBBBBB#B#####G5#BBBBBBBBBB##########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&############PGBBBBBBBGGGGGGGGPPPP555YYYYYJJ?!:      :7??JYY55PPPGGGGGBBBBBBBB#######G5#BBBBBBBBBB##########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&############PGBBBBBBBBGGGGGGGPPPPP5555YYYJJ?7^      ^7JYYY55PPPGGGGGGBBBBBBB########G5#BBBBBBBBB###########&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&#&#########PGBBBBBBBBBGGGGGGPPPPPPP5555YJJ??~      ^?JY555PPPPGGGGGBBBBBBBB########G5#BBBBBBBB###########&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&##&#########PG#BBBBBBBBBGGGGGGGPPPPPP5555YY?~^      ~?Y555PPPGGGGGBBBBBBBBBB########G5#BBBBBBB############&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&############PG#BBBBBBBBBBBGGGGGGGPPPPPP55YJ7::.     ~JY55PPPPGGGGBBBBBBBBBB#########G5#BBBBBBB############&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&###########PG##BBBBBBBBBBBBGGGGGGGPPPP55Y5?.~.     !J55PPPGGGGBBBBBBBBBBB##########G5#BBBBBB############&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&#########PG####BBBBBBBBBBBGGGGGGGPPPP555?:~.     !Y5PPPGGGGGBBBBBBBBBB##########&G5#BBBBBB############&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&##&######PG#######BBBBBBBBBGGGGGGGPPPPP5Y?7.     !Y5PPGGGGBBBBBBBBB#############&GP#BBBBBB############&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&#########PG#########BBBBBBBBBBGGGGGPPPPP55?.     7Y5PGGGGGBBBBBBB###############&GP#BBBBBB############&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&&&#&&&###GG##########BBBBBBBBBBGGGGPPPPPP5?.    .7YPPGGBBBBBBBBB################&GP#BBBB##############&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&###GG############BBBBBBBBGGGGGGGGPPP?.     75PGGGBBBBBBB##################&GP#BBB###############&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&#&GB#############BBBBBBBBGGGGGGGGPP?.     7YPGGBBBBBBB##################&&GP#BBB###############&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&GB&##############BBBBBBBGGGGGGGGP?.     75PGBBBBBBBB###############&&&&&GP#BBB###############&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&GB&###############BBBBBBBBBGGGGGP?.     ?5PGBBBBB##################&&&&&GP###B##############&&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&GB&###############BBBBBBBBBBGGBGPJ.    .?PPGBBBB###############&&&#&&&&&BP&##B############&&&&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&GB&###############BBBBBBBBBBBBBGPJ.    .?PGGBBBB##############&&&&#&&&&&BP&###############&&&&&&&&&&&&&&&&&"					
					Escribir "FINAL SECRETO: Guardián Estelar (con Llave)."
				Sino
					
					Escribir "                                                                             .^!!~.                 "
					Escribir "                                                                           .:~7?!.                  "
					Escribir "                                                                         .~?557:                    "
					Escribir "                                                                     .:^7YP5J^                      "
					Escribir "                                                                   :7YPPGPJ^                        "
					Escribir "                                                                :!JPGGGP?:                          "
					Escribir "                                                             .!YGBGGG57:                            "
					Escribir "                                         ...              .~JPGGGGGY!.                              "
					Escribir "                                       .7P5YYJJ?!^^.   :!JPGGGGG5?^.                                "
					Escribir "                    .....            :7Y5J?J??JYYYYYY?JPGGGGGPY!.                                   "
					Escribir "             ..:^^~~!!!!~~~~^^^^^^..^^:.^!JG?~^^~JPP55GGGP557:                                      "
					Escribir "         .^~!!!!!!~~~~~~~~!!!!!!!!~!!!PPP55GPPP55PGGPYPBBB5^                                        "
					Escribir "     .:~!!!!!!!!~!!~~~~~~~~~~!!!!~~~!7YP##BBBBGGGPP555Y?PBGJ^                                       "
					Escribir " .:~!777!!!!!!!!~~~!!!~~~~!~!7!!~~~~~~~7YPBBBBBGGPG5PPG!YPPY7~ . ..                                "
					Escribir "!7777777!!!!!!!!~~~!!!!!!77????!!~~~~~~~~~!?5GBBBGGBBPJ~P5J~^^                                     "
					Escribir "?7777777777!!!!!!!~!!!77?JJJYYJ7!~~~^~~~~~^^^!PB##BYJJJYYYY~                                      "
					Escribir "777777!!!!!!!!!!!!!!!7?JJYYYYJJ7!~^^^^~~~~~~^~75B&&#&&#GPP!                                        "
					Escribir "777777!!!7!!!!77!!!77?J?JYYYYJ7!~~^^^^~!~!!!777!!J555YYYJ!.                                        "
					Escribir "7777!!!!!777!7777777???J55JJYYJ?77!!!7777??JJJ77!!^                                                "
					Escribir "7!!77!!!!7777777777??JY5PPPGBBGP55YY55PPPJ?GYJ???!.                                                "
					Escribir "77!!!!!!7777777777?JPGB#&&&&&&&&#&&&&&#5^ ^55YYJ~                                                  "
					Escribir "7777!!!7777777777?JB&&&&&&&###&&&&&#BY?7.  .~7?!.                                                  "
					Escribir "7777!!777777777??JB@&&&&&&&&&@&&#PY?!!!:                                                           "
					Escribir "77777777777777???5&&&&&&&@@@&BP5YY?7!!^                                                            "
					Escribir "777777777??7????Y#&&@@@&#G?!!~^~:.....                                                             "
					Escribir "7777???????????Y&@@@&P?^.                                                                          "
					Escribir "77??????????J?5&&B55~                                                                              "					
					Escribir "FINAL: Salvador del Horizonte."
				FinSi
			FinSi
			
			
		2:
			LimpiarPantalla
			Escribir "Te oponés al cometa con toda tu fuerza y voluntad."
			energia <- energia - 50
			vida <- vida - 30
			MostrarEstado(vida, energia, amuleto, llave)
			esperar Tecla
			
			Si vida <= 0 Entonces
				Si amuleto = 1 Entonces
					Escribir "El amuleto evita tu muerte, pero se rompe. Sobrevives, herido."
					vida <- 20
					energia <- 0
					amuleto <- 0
					Esperar Tecla
					Escribir "Aun agotado, tu espíritu brilla y el cometa pierde su sombra."
					
					Escribir "@@@&&&&&##BGP5YYJYYPGBBBBBBBBBBGGGGP5J?777?Y5PPPPPPPPPPPPPPPPPP55J???JY555555555YY7::::~Y55555PPPPPP5YJJ??J5PGGGGBBBBBBBBBBBB###############"
					Escribir "&&&&&&&&########BBGP5YJ???JY5PPPPPPPPP555Y?7!!!7?JYYYYYYYYYYYYYJJJ?7777?JJJJJJ??7:....!JJJJYYYYYJ?7!!7?J5555PPPPPPPPGGGGGGGGGGGGGPPP55YYYYYY"
					Escribir "&&&&&&&########BBBBBGP5YJ??77?JY55P555555YY?7!!!!7?JJJJJJJJJJJJJJ??7!!77???????7:....:???JJJJJJ?!!!!7?JYY555555PPPPPPPPPPPPPPPP55YYJJJJJYY5P"
					Escribir "&&&&&&########BBBBBBBGGGPYJ?77!77?JY555YYYYJJ7!!~~!7???JJJJ???????7!!!!!7777777~.....~77??????!~~~!!?JJYYYYY555555555555555YYYJJ????JJY5PPGG"
					Escribir "&&&&#########BBBBBBBGGGGGPP5J?7!!!!77?JYYJJJJ?7!~~~~~!7??????777777!~~~~!!!!!!!:. ..:!777777!~~^^~!7?JJJJJYYYYYYYYYYYYYYJJ??7777??JY55PPGGBB"
					Escribir "&##########BBBBBBBBGGGGGPPPPP5Y?7!!~~~!!7??????7!~^^^^~!!77777!!!!!~^^^^~~~~~~^.   .^!!!!!!~^^^^~!7?????JJJJJJJJJJJJ??77!!!!77?JJY5PPPGGGGBB"
					Escribir "####BBBBBBBBBGGGGGGGGPPPPPP5555YYYYJ?!~^::::^~!!!!!~^::..:^^~~~~^^^^::::::^::.    .:^^^^::.:::~~!!!!!7777777!!!~~^^^^~~!7?JJYY5555PPPPGGGGGG"
					Escribir "GGBBBBBBBBGGGGGGGGPPPPPPP55555YYYYJJJJ?7~^:::.::^~~~~^:....::^^^^::::....::::.    .::::....::^~~~~~!!!!!~~~^^^:::^^~~!7?JJYYY5555PPPPPPGGGGG"
					Escribir "!77?JJY55PPGGGGPPPPPP5555555YYYYYJJJJ????7!^:......:^^^:......::::::.........     .:.......:^^^^^^~~^^^:::..:::^^~!7???JJJYYY55555PPPPPPPGGG"
					Escribir "BGGGPP5YJ?7!~~^^::::::::::::^^~~!!!7777!!!!~~~~~^^::..                                  .....       ...:^^~~~!!!777????JJJJJJJJJ??777!!~^^^^"
					Escribir "BBBBGGGGGPPP5YYJ?77!~^:::..... ......::^^^^^^^^^^^^::..                                           ..::^^^~~~!!!!!777!!!!~~^^^:::..........::"
					Escribir "BBBBGGGGGGGPPPP5555YYJ??7!~^^::..           .....:::.....                                      ...:::::^^^^^^^^:::......      .....::^^~~!77"
					Escribir "BBBBBBGGGGGGPPPP5555YYYYJJJ???7!~~^^:...                                                     .............            ....:::^~~!!7??JJYY555"
					Escribir "BBBBBBBGGGGGPPPPP5555YYYJJJJ????777!!!~~^^::....                                                             .....::^^~~!!77???JJYYYY5555PPP"
					Escribir "BBBBBGGGGGGPPPPPP5555YYYYJJJ????7777!!!~~~^^^:::.....                                               ......::^^^~~!!!777????JJJYYYY55555PPPPP"
					Escribir "BBBBGGGGGGGPPPPP55555YYYJJJJ????777!!!~~~~^^^::::.....                                        ......:::^^^^~~~!!!!777????JJJJJYYY55555PPPPPP"
					Escribir "BBBGGGGGGGPPPPP55555YYYJJJJ????777!!!~~~^^^:::.....                                           ....:::::^^^~~~!!!!7777???JJJJJYYYY55555PPPPPG"
					Escribir "BBGGGGGGPPPPP55555YYYJJJJ???777!!~~^^:::.....                                                     ....:::^^~~~!!!!777????JJJJJYYY55555PPPPPG"
					Escribir "GGGGGPPPPPP5555YYYJJ??77!!~^^^::...                                                                      ...::^^~~!!77????JJJJYYYY5555PPPPPP"
					Escribir "55YYJ77!~~^^::....          .....:::^^^^^^^^:::::...                                         ...:::::::::......       ....:^^~!77?JJYY5555PP"
					Escribir "~^^^::::........::::^^^~~!!!!777777!!!~~~~^^^::...                                             ..:::^^^^~~~~~~~^^^:::............::^~~!7?JYY"
					Escribir "^^^^^^^~~!!777??JJJJJJJJJJJ?????7777!!!!~~^^:...       ......                                    ..:^^~~~~!!!!!7777777!!!~~^^:::.......:::^~"
					Escribir "JJJYY5555PPP5555555YYYYYJJJJJ????777!!!~^^:.... .......:......    .             .....     ....      .:^~~~!!!77777??????JJJJJJ??77!~~^^:::::"
					Escribir "GGGGGGPPPPPPPP5555555YYYYYJJJJ???77!!~^::......:::::::::::..........    ...     .......    ..::..     ..:~!!!777?????JJJJJYYYYYYYYY5YYYYJJ?7"
					Escribir "GGGGGGGGGGPPPPPPP555555YYYJJJJ??7!!~^:::::::^^^~~~~^^^^^^:.....:::..   .....    .:::::::.   ..:^^:..     .:^!!77????JJJJYYYYYY55555555PPPPPP"
					Escribir "BBBBBGGGGGGGPPPPPP5555YYYYJJ??7!~~^^^^^^~~~!!!!!!!!~~~~^:::::::^^:..  ..:::..  ..:^^^^^^^:.....:^~~^^:.     .:~!7???JJJJYYYY555555PPPPPPPPPP"
					Escribir "BBBBBBGGGGGGGPPPPP5555YYYJ?77!!~~~~~!!7777777777!!!!!~~^^^^^^^^^^:.....:^^^:.....^^^^~~~~~^:....:^!!!!~^:..   ..:~7?JJJJYYY55555PPPPPPPGGGGG"
					Escribir "BBBBBBBGGGGGGPPPPP555YJJ?77!!!!777????????????777777!~~^^^~~~~~~~:....:~~~~~:....^~~~~!!!!!~^:::::^!!7777!~^:.  ...^!?JYYYY5555PPPPPPGGGGGGG"
					Escribir "BBBBBBBGGGGGGPPPP55YJJ??7777??JJJJJJJJJJJJJJJJ????77!~~~~!!!!!!!~:::::~~~~!~^....^!!!!!!!7777!~^:::^~7??????7!^......:^7JYY5555PPPPPGGGGGGGB"
					Escribir "BBBBBBBGGGGGPP55YYJJJJ?JJJJYYYYYYYYYYYYYYYJJJJJJ??7!!!!!7777777!~^::^~!!!!!!~:..::!7777777????7!~^^^^~7???JJJJJ?!^:.....:~7Y555PPPPPGGGGGBBB"
					Escribir "BBBBBBGGGGPP55YYYJJJYYY55555555555555555YYYYYJJJ?77777????????7!~^^^~!777777~^:::^!777?????JJJ???7~~^~~!?JJJJYYJYJ?7~:....:^!?YPPPPPGGGGGBBB"
					Escribir "BGGPPPPPPPPPGGGGGGGGGGGGGGGGGGGGGPPPPPPPP555YJJJJYYYYYYYYYYYJ?!~~~!?JJJJJJJJJ?!~~~!?JYYYYYYYYYYYYYYYYYJ?7777?Y5555555PPPPP5Y?!~^^^^~!J5GGBBB"
					Escribir "GGGGGGGGGGGBBBBBBBBBBBBGGGGGGGGGGGGGPPPPP55YYYYYY5555555YYYYY7!~~!7JJYYYYYYYYJ7!~~!?YYYYYYYY55555555555YJ??7??Y555PPPPPPPPPPPP5J7!~^~~!7YPGB"					
					Escribir "FINAL: El Rebelde que Perdura."
					esperar Tecla
				Sino
					Escribir "El cometa te aplasta con su poder; el sacrificio impide una catástrofe mayor."
					
					Escribir "";
					Escribir "                         .                                  .     :^                                                        .    .    " 
					Escribir "     .    .                           ..          ..              .     :~                 ...                    ..                   .    " 
					Escribir "          .       :~^..                                            .            ..     .:^:::...:.                                .         " 
					Escribir "   :          .~?JJ?J~.::         ..                          .:   .                  :^^!!^:.:..:.          .    .      .                   " 
					Escribir "            :~YY55YY7^  .^...                  ..      :^                             7~^~!^:.:^..^                      !^                 " 
					Escribir "        .:~?PP5Y?7?:  :. .~J^::.                       ^~  ..                 .       ~!^^^::.... ^   .       .          .           .      " 
					Escribir "    .::^~7YPYJYPPY^   :~.  ....::....                   .. ..   .                .    .^^^^:^^: .:.   .                              ..     " 
					Escribir "  .:^^~!?J55YYPG7:.^.  :^.:.     ....:::.               ..      .                       .::.~~:.:.                             .            " 
					Escribir ".:^^^~7???Y5?^:.   .:::  ..!!      ::  .::                                                 ..                                  .            " 
					Escribir ":^^^:~?Y7JP!     ..  :!7~  .~^.   ..7:   .^^:.                   .:.                .                          .^:.             .           " 
					Escribir "^^^^!J5YY?.      :!^. :JJY^:..    .::~~.   .::::^.             :!J~::.              .                        .!JJ..:::..                    " 
					Escribir "^^^77JYJJ~~^. .:.  :~: .^~!!!^.      .:~^.     ^?^::.       .^7JJJ~ .::.        .:                        .:~YPP7!:. ...::..                " 
					Escribir "~~~~!JYPG5BGJ~:.~^^:..:.     . .!?!~:  .^~~:.     ..::...:::!7?777...::::::.                            .:^^!75Y?JJ7~..!:.:::...            " 
					Escribir "^^~~777?J5GP5P~  .~!!~~~..     ..::!7!!!^^::^::.  .^:..^^..:^!7!!7?577~:...::..                     :~::: .^~JYY5P?^:^..:::::.....:..       " 
					Escribir "::^^~^^^^!PP55!~~~...::^::::..     .::^^^^^:....~!::^:...::. .:^^7YGJ^^:^^~: .:::.               .^?7^  .::^~?JJYJ~  .^. ..:~^:::...::....  " 
					Escribir ".:::::::~?PPJY5PPGJ~.        ...       .:::::.  .^!?!:... ..::.  .:^.:~::.:^:.  .::...    .::::::^7!.. .::^~7PP5Y!::..^^     ..::!^^:....:::" 
					Escribir "   .:.:^!!~~~!!?55PG5J7^..   .7?77^~^~^..:^::^:     ..  .:^. ..:.   . .::   .     ...:::^!?^      :.  .:^~7?P5^. .:?~:~^.::.    ^JY?~.      " 
					Escribir "          ::^^:~77?5JPBJ::::. ..~!~!7??J!!^^:^~^!:      .:^!!~:..            .::^^.....::77^        .:^~^~7P?.  .^?77J7^..::^^....^^!~^.    " 
					Escribir "          .. .:..^^.:^!!!..:::..   ...:^!77?!!. :^^:        ...:.::.    ...::~!!7~..:^.   :      .  .:..::!~.    :.:!~!!:     .:::.   .     " 
					Escribir "    ^^ .  !!       ^.  ..    ...   :.       .:     .   ..    .7  .. ..::.?^..      ^:.   ~7 :    7.      ..       :    ..    ^    :.   ::   " 
					Escribir ". . 7^ J:.P? . ~! .5^ .  ^. 7! .   Y: ^.  . ^7   .  :  ?^ ^  ^5.^! ::.. .P! ^7 .. .P! ^:.Y7^J...:5. ~.. .?  ^. :..J    ?^:^ :P: ^ ?! : ?! :." 
					Escribir "?^J.P~~G:!B~!? 55 ~B^ ? ^Y..P!:J ~7P^.J~^.J^YJ~^~J :P::P~:Y.JJG!YY ?! ?.!GJ.7P.?!.!G?^5~:PJ7P.?!?5::J.?:~G..5^:?.^5..7:P!?Y 7P::5~G?^J.5Y.Y7" 
					Escribir "P?Y7G!5G!5P5PP!B5^JB!~P^?P^~#J?G!5GG7?G?GJP55YY?PG~J#~?B?JG?PPBJGY~G5~G7YPP!5B?G5!JBJ?B7?PGPG?PJ5B??P7G?YG^JGJJ5~JP~7G?BPPP!PG7?G5#5YP?BY?55" 
					Escribir "YPPPPJJJ??7~^77JY?YP5YPYY5P5B55PY5PJJ5YY555P5777J555PY7J?JYJY5JJJYJPPJYYYP5~JJ77J?7??J??G5P5Y5P5Y5??J?J?557PBP5#PJJ7?YJ?Y?!?YY7???YJ!~~~~~^^" 
					Escribir ":::~:.::^~!!^.^^^^::^^^^.:~^:::.::^^^^~^:^^~^....:^^^::.:^^~~:...:^^::.^:::..^:..^:::..:..:..::::::::^:. ...:::^~...  ::^^..:..:......      " 
					Escribir "J77!7!!~!7!??77?J!J?7!?Y7!J???J?YY??7?J5?77YJ?J?Y555PY?Y!J?JYJ??YYYYY5??PG?5J?J?!JGPYGGYYJY?7PPY5JJP5JYJJJJJ77YJJY??J~JGYY7?JJY5J5?7J???!J5?" 
					Escribir "7757YY~YPYYYJY??5Y5Y??!!Y!75YJP?YGP55P??P7:J5~55?G!^5!:?^5~?J5!~Y~YP:!J!5P7JY:?J^?BG~~575Y!P7YB?GJ7#J!G!5BBP~?P^JB?~P7Y57PJY#5?Y!5J!5!?P?JBG" 
					Escribir "::!.:^^^7P!:~!^!7?P5^!^:7^:55!J::JG?:7^.!~.7~:~~.~:::::..^^:.7^.~^^~^:^.^P^.^..:::?Y^::.?Y:J^:!:P?:J!^!:5JJ5 ^~.~#J.!^~?.~^^P~:^:!!^5^7G~:?J" 
					Escribir ".::::..:.::^:::..:5J:..:...~^::.::7:::::.....      .:.....:..:....  .....:::::. ...:.:::!7.~:...J! .....!7.::::.:7~::..:.:::^^::^::^~::7^:.." 
					Escribir "   ... ...   .....~:.    .  .:....        .                                          .........  .       .:       .........      ........... " 
					Escribir "                                        :^ ................:.     ...::......                                                               "					
					Escribir "FINAL: Mártir de la Rebelión."
					Esperar Tecla
					
				FinSi
			Sino
				Escribir "Tu rebelión fractura el cometa. Parte de su energía cae en la tierra."
				Si llave = 1 Entonces
					Escribir "La Llave Antigua se fusiona con los fragmentos, y te convierte en un guardián."
					
					Escribir "                                                    :?5PPPP5J77JYY5?                                "
					Escribir "                                                  .!5PPPPPPPP5YJJJJY^                               "
					Escribir "                                                 :JPPPPPPPPPPGGP55YY^                               "
					Escribir "                                                :5PPPPPPPPPP5Y?~!YP?.                               "
					Escribir "                                                ?GPPPPPPPPYJ77!~^!~                                 "
					Escribir "                                  :~:.      ..^~Y5PPPPPPP5Y5JJYYYJ!                                 "
					Escribir "                                ^7?7~~^^..^!7?YYYY5PPPPPP5YP5YY5PPY~                                "
					Escribir "                                7?7?77!!!!???Y5PPPPPP55J!^?55YYYJ?J?~.                              "
					Escribir "                                ^!!7???777!!!7?Y5P5J?7!~~^?PP5JJ??JY?!:                             "
					Escribir "                                  :~7?JJJJ??77!!!!!77???77Y5YJ?7!!?J?!!                             "
					Escribir "                              .^~!7??JY5PP55YJ??77!!~~~!!7??55J7!~7YJ77:                            "
					Escribir "                           .^!7???JJY55PPPPPPPP55YJ??77!!~~^~~~~~^~JYJ?:                            "
					Escribir "                        .^!777??JYY55PPPPPPPPPPPPPP55J?????7!~^^^~77YPP^                            "
					Escribir "                     .^!777??JJYY55PPPPPPPPPPPPPPPPPPPP5J????!!!~~7!~!7~:.                          "
					Escribir "                  .^!777??JJYY55PPPPPPPPPPPPPPPPPPPPPPPPPPY?7!!!!!!!!!~!!~^:.                       "
					Escribir "               .^!7?????JYY55PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP?^^~!!!!!!7?777!~~^:.                   "
					Escribir "            .^!7?????JYY555PPPPPPPYYPPPPPPPPPPPPPPPPPPPPPPPPY:::^~^!7?Y??????7!!!~^:..              "
					Escribir "           ~7??????JY555PPPPPPPPP7.JPPPPPPPPPPPPPPPPPPPPPPPP5^::^~^~!5P5?^:^!7???777!!~^^:..        "
					Escribir "         :7J????JY55PPPPPPPPPPP?: ^PPPPPPPPPPPPPPPPPPPPPPPPPY~::^~^~~JPP5?^.  .:~77???7!!!!!:       "
					Escribir "        ^YY??JY55PPPPPPPPPPP5?^.:7PPPPPPPPPPPPPPPPPPPPPPPP5!.~:^:^^^~?PPPPJ!:     .:~!?????7^       "
					Escribir "       ^55YY5PPPPPPPPPPPPP5?!~!7JPPPPPPPPPPPPPPPPPPPPPPPP7.  ^^::^^^~!PPPPP57^.       .^!!^.        "
					Escribir "       JPPPPPPPPPPPPPPP5?!!!!7?YPPPPPPPPPPPPPPPPPPPPPP57:    :^::^~^~~YPPPPP5?~.                    "
					Escribir "      ^5PPPPPPPPPPPPPP5J!!7J??J5PPPPPPPPPPPPPPPPPPP5J~.      .~:^^~^~~JPPPPPP5?~^                   "
					Escribir "     :5PPPPPPPPPPPPPPPPY^^!??JY5PPPPPPPPPPPPPPPP5Y7~^:::......~^^^^^^~?PPPPPPP5J!:                  "
					Escribir "     !PPPPPPPPPPPPPPPPP?^^^!!?JY5PPPPPPPPPPPPPPY!!~^^:::::::::~^:^^^^~!PPPPPPPP5?~:                 "
					Escribir "     .5PPPPPPPPPPPPPP57^~!~^^~7JY55PPPPPPPPPPP5!^~^::::::::^~^~^:^^~^~!5PPPPPPP5Y?~:                "
					Escribir "      ^5GPPPPPPPPPPPPJ~~!777!^^!?Y55PPPPPPPPP5?^::~::^^^^7YYJJJ~:^^~^~~JPPPPPPPP5Y?~:               "
					Escribir "       .?PPPPPPPPPPPP5?7!7??J?!~!7JY5PPP5PPP5J!^::~~~~^^!PPPPPP7:^^^^~~7PPPPPPPPP5Y?^^              "
					Escribir "         ^?5PPPPPPPPPP5J???JYYJ?7!7?JY55PPPYJ!~^^~!!~~^^?PPPPPPY:^^^^^~!5PPPPPPPPP5Y?^^             "
					Escribir "           !GPPPPPPPPPP5YJJJYY555YJ?77?YP5J77!77???7!~~!5PPPPPP5^:^^~^~~5PPPPPPPPPP557~.            "
					Escribir "           !PPPPPPPPPPPP5YYY555PPPPP5YJY5YYYYYYYJJ??!!!?5PGPPPPP~:^^~^~~JPPPPPPPPPPPP57:            "
					Escribir "           ?GPPPPPPPPPPPP55555PPPPPPPPPPPPPPPP5YYJJ?77?J::~!7YPG7:^^~^~~7PPPPPPPPPPPPP5^            "
					Escribir "          .5PPPPPPPPPPPPPPPP5PPPPPPPPPPPPPPPPP55YYYJ??J?      :~!^^^^^^~!PGGGGGPPPPP5?^             "
					Escribir "          ?PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP55YYYJY5~        :^^^^~^~~~~!7777!~^:.               "
					Escribir "         .PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP55555P7         :~:^^~^~!:                          "
					Escribir "         ^PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP7          .~^^^~^~!^                          "
					Escribir "         ~PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP5~            ^^^^^^^~~                          "
					Escribir "         ~PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP?.             :^^^^~^~~.                         "
					Escribir "         ^PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPJ^               :^^^^~^~~:                         "					
					Escribir "FINAL SECRETO: El Guardián Forjado (con Llave)."
				Sino
					
					
					LimpiarPantalla
					
					
					Escribir "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
					Escribir "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&#BB&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
					Escribir "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&BPG&@@@@@@@@@@@&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
					Escribir "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&###&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
					Escribir "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&#&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
					Escribir "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&BB&@@&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@@@"
					Escribir "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&######G5G&@@@&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@"
					Escribir "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&#BGPPGGGGPYYG&@@@@@&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@"
					Escribir "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&#P5Y5GBBG5YY?Y#&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@"
					Escribir "@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&@@&&GYYYPBGP5YJJ?JB&&&&&&&&&&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@"
					Escribir "@@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&#PPPGP5PP5J??JG&&&&&&&&&&&&&&##&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@"
					Escribir "@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&#GP5YPPPY??JP&&&&&&&&&&&&&#BB###&&&&&&&&&&@@@@@@@@@@@@@@@@@@@"
					Escribir "@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&BPY555555YJJYG&&&&&&&&&#BBB#########&&&&&&&&@@@@@@@@@@@@@@@@@@"
					Escribir "@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&GYJ?JY5555YYYP#&&&&&&&&B5YYY5PB#B#####&&&&&&&&&@@@@@@@@@@@@@@@@"
					Escribir "@@@@@@&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&BYJ?77??????J5B#&&&&&&&#GYJJJYY5GB#####&&&&&&&&&&&@@@@@@@@@@@@@@"
					Escribir "@@@@@@&&&&&&&&&&&#&&&&&&&&&&&&&&&&&&P5YYJ?JJ?????JB&&&&&&&&#PJJYYYY5PBB###&&&&&&&&&&&&@@@@@@@@@@@@@@"
					Escribir "@@@&&&&&&&&&###&#BB#&&&&&&&&&&&&&&&#GBBBBGB#GJJ??YB#&&###&##5JY5PPPGGBB####&&&&&&&&&&&&&@@@@@@@@@@@@"
					Escribir "&&&&&&&&&#BBGPPGBBGB#&&&&&&&&&&&&&&###BBBB###G???YB##BGB###G5JYY555PPPPG###&&&&&&&&&&&&&&&@@@@@@@@@@"
					Escribir "&&&&&&&&BB##GP55PPPPGB#&&&&&&&&&&&&&&&&&###&##Y7?J5GPPPG##BPYJJ????77?YPBB###&&&&&&&&&&&&&@@@@@@@@@@"
					Escribir "&&&&&&&BG&&&#BGGGGBBB#BGGB#&&&#####BB##&&&####J~7?JYYY5PP5YJJ????777?JY5PGB##&&&&&&&&&&&&&@@@@@@@@@@"
					Escribir "&&&&&&&#B&&&&&&&#BBBB###BGPPB##&&#GPGBB##&####P!~77JJYY5PP5YYJJYJ????JYPGB##&&&&&&&&&&&&&&&@@@@@@@@@"
					Escribir "&&&&&&&&#&&&&&&&#G5Y555PB&#G55GB#GY5G##BB######G7~~!777JY5PGBGPPY??77J5GB###&&&&&&&&&&&&&&&&@@@@@@@@"
					Escribir "&&&&&&&&&&&&#BBBGYJYPPPPPB&&#P5YYJY5PGB#BGGG###BJ~^!7~^^~!JPGBB5?!!!!?YPB######&&&&&&&&&&&&&&&@@@@@@"
					Escribir "&&&&&&&&&&&#PPYJ777YGGGGGB##BG5YYYYJJJPBBGGB###5~~^!7~^~~7J5GBGJ!!~~!7?YPB######&&&&&&&&&&&&&&&@@@@@"
					Escribir "&&&&&&&&&#BJJ5?777?G##########PYYYYJYPB###B###G!^^~!~~7JYYYPBGJ!!!77!!7JYPG####&&&&&&&&&&&&&&&&@@@@@"
					Escribir "&&&&&&&&#BY^?J??7JG#&&&&&&&&&#5YYYJ5B#&&#####BJ^~~!~^?PGPY5GB5!~!!??!!J5PPGBB###&&&&&&&&&&&&&&&@@@@@"
					Escribir "@&&&&&&##P~^????JB&&&&&&&&&&#PYYYYYB#&&&&##BGJ~!77!^~5BBP5PBBJ~~!7?7!75PGGGBBB##&&&&&&&&&&&&&&&&&@@@"
					Escribir "@@@&&&&#B?^!?J?JB&&&&&&&&&&B5YYJYJG#&##&&#G?~~7??7~^7PBG55PGP?^~7??!!55PPGGBBB#&&&&&&&&&&&&&&&&&&&&&"
					Escribir "@@@&&&#BJ^7JJYJY#&&&&&&##&#5YYJJYJG#&B####BYJJ???~:^?55YY5PPJ~~7??7!5BGPPPGGBB#&&&&&&&&&&&&&&&&&&&&&"
					Escribir "@@@@&&B?~?JJYYJ5B#&&&&&5YBGYY??JYYB&&B#######BP?7^.:~7??JYJ7~~7??7!YB##BPPPGBB#&&&&&&&&&&&&&&&&&&&&&"
					Escribir "@@@@&#5~?JYJ55YYPGGB#&#YJYYJ?7J5PG#&&##&&#####BY7~....:::^^~!7??7~JB####BGPGGB##&&&&&&&&&&&&&&&&&&&&"
					Escribir "@@@@&#J7JYYY555YPPPPPGPYJJJ775G##&&&&&&&&&#####P7~....:^:^!???7!~7JP##BGGGPPGGGB#&&&&&&&&&&&&&&&&&&&"
					Escribir "@@@@&BJJYYYY5PPPPGGGGGP5YJ!!5#&&&&&&&&#######BB57!^::~!7!^^!!~^^7??JG##BB#BBBGGPB#&&&&&&&&&&&&&&&&&&"
					Escribir "@@@@&PYY55Y5PGGGGGGBBBG5?7!JB&&&&&&##########BGJ777~!7JYYJ!:.:^!77??YB####&#BGGPG#&&&&&&&&&&&&&&&&&&"
					Escribir "@@@@#G5555YPGBBBBBBBBB5J??7?G#&&&&########BBBBYJ7?J7J5PGPP57^~~~~!???YG#&&#BGGGPP#&&&&&&&&&&&&&&&&&&"
					Escribir "@@&#BBGPP5PGB#BBGGGGG5JJJ?7!?5GBBBGBBBBBGGBPPG5Y??YJ5PPPP5YJ!^^^~!7?77JG#BBBBBGPB&@@@&&BB#######&&&@"
					Escribir "&&#BB#BBGGB###GGGGGP5JJJ??7!^~7JYYYYPGBBBGBGPGG5J??JY555PP55J?77!7??77JB#B#BBB##&@@@@&BPGB######&@@@"
					Escribir "&#BGG###BB###BGPPPPPJJJ????7!~~~7????YPGPPGGPGBP5YJ????JY5PPP5J??JJ?77PBB##GB&&&&&&&@&#55PGGBB#&&@@@"
					Escribir "&&#BB##B5G##GPP55PPPYJJJJJJJYY5GB###GP5YJPPPGB#BGGP5YYJ???JYYJJYYYYJ??PGBBBPG####BBB##B5JJY5PB#&@@@@"
					Escribir "@@@&&##G5GBPP5555PGGP5555555PPGB##BGGGGPGGPG&###BBBBGGGP5YYYYYY555YYY?J5G##GP#&&#####BBPJ??J5B&@@@@@"
					Escribir "@@@@@&#BGGP555Y55GGGGGGGGG5GPPGGGGGGGGBBGPP#&&&&####BBBGGGPP55YJY55555YYYG##GG&&&###&&&BY77?5#&&@@@@"
					Escribir "@@@@@@@@&#BGGP55PBBGBB#BBBBBGGGGBGGGBBBGGPB##&&##B#######BBPPPP5YJ5555555YPB#GG#&&&&&&##BJ~7P####&&&"
					Escribir "@@@@@@@@@@@&&#BBBBB######BGGGBBB#BGBBBBGGB####BB#BBBB##&&&&#GGGGPYJY55PPPPPPGGPPB&&##BGPPJ^7G#&###BG"
					Escribir "@@@@@@@@@@@@@@@&&&####BBBBBGG#BBBGBBBBBBB##########BB&&&@@&&&###BG5YY55PGGGBBGPPPGP55YYJ?!!YB&&&##B#"
					Escribir "@@@@@@@@@@@@@@@&B#&&&&##BBBGGBBBGB#####BGGB######BB#BBB#&&&&&&&&##BGPPPPPGB##BPPGGGP55YJJJPB#&&&&&@@"
					Escribir "@@@@@@@@@@@@@@@&#BB&@@@&&#BBGPGGGB####&&#BBBB#######BGGBBB####&&&&&&##BBBGBBBGP55PPPP5Y5GB&&&&&&&&@@"
					Escribir "@@@@@@@@@@@@@@@@@&&@@@@@@@@&&#BGGGB#&&&@@@@&&&#####BBGGPGGGGGGBB##&&&&&#BB##G5Y5PPPPG##&&@@@@@@@@&#&@@"
					Escribir "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&#BGBB#&&&#&@@&&####BBGGGBBBGGGGGBB###BBBBGP5PB#&&&@@@@@@@@@@@@@@@@@@@"
					Escribir "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&#BGGGB##&&##BBBBGGB####&&#BGGGPGGBBP55PG#&@@@@@@@@@@@@@@@@@@@@@@@@"
					Escribir "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&&BGPGGB#BBBB#BB&##B#&#BBGPGBB#&&#&&@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
					
					Escribir "FINAL: La Llama de la Rebelión."
				FinSi
				esperar Tecla
			FinSi
			
			
			
			
			
			
			
		3:
			
			LimpiarPantalla
			Escribir "Buscás negociar con la energía del cometa, sin destruir ni entregarlo todo."
			energia <- energia - 25
			vida <- vida + 20
			MostrarEstado(vida, energia, amuleto, llave)
			Esperar Tecla
			
			Si energia <= 0 Entonces
				Si amuleto = 1 Entonces
					Escribir "El amuleto evita el colapso. Te sostiene y luego se rompe."
					energia <- 15
					amuleto <- 0
					Esperar Tecla
					Escribir "Logras un equilibrio frágil. La tierra respira."
					
					Limpiar Pantalla
					Escribir "@@@@@@@@@@@@@@@@@@@@@@@@@&&&&&&&&&&&&&&&####BBBBGGGGGGPPPPPPPPPPGGGGBBBBB#####&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@@@@@"
					Escribir "@@@ FIN @@@@@@@@@@@&@&&&&&&&&&&&&&&&&###BBGGP555YJJJ??77!!!!!!!!!77??JJY55PPPGGGB###&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@"
					Escribir "@@@@@@@@@@@@@&&&&&&&&##&&&&&&&&&###BBGPP5YJ7!~^::....:::..           ..:^~!7?JY5PPGBB##&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@"
					Escribir "@@@@@@@@@@@@@&&&&&&&&&&&&&&&###BBGPP5YJ7~:..... .::.::::....               ..:^~7?Y5PGGBB##&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"
					Escribir "@@@@@@@@&&&&&&&&&&&&&&&&&###BGP55YJ?!^:....:.....:....    ..                   . .:~7JY5PGGB###&&&&&&&&&&&&&&&&&&&&&&&&&"
					Escribir "@&&&&&&&&&&&&&&&&&&&&&###BGP5YJ?7~^...:^~^:::... ...                                 .:!?Y5PPGB###&&&&&&&&&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&&&&###BGG5YJ?7!~:..::..::.::..                                           :!?Y5PGBB###&&&&&&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&&&&####BGP5YJ7!~^:::.::.  ...::::::....                                  ...  .^!JY5PGB######&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&&&&&&###PPBGP5YJ?77!^::.:..   .:::........            .. . ...                        :!JY5PGB###&&&&&&&&&&&&&&"
					Escribir "&&&&&&&&#######BGPP5YJ?77!~^.....  ..:::::.    .    ..          ..^^:^^...              ..      :!JY5PGB####&&&&&&&&&&&&&"
					Escribir "&&&&########BBGGP5YJ??!^^.    ...:....:..... .              .^:^..^~:::^:               ...     .:^?Y5PGB####&&&&&&&&&&&"
					Escribir "#####B##BBBBBGGP5Y??J7:..:^^^^^^~!7???!~~!^^:..     ..   .:..  ...:::::::::...               .    :7?:~JY5GGBB#######&&&&&&"
					Escribir "######BBBBBBGGP5YJ?J?:..:^^^^^^~!7???!~~!^^:..     ..   .:..  ...:::::::::...               .    :7?:~JY5GGBB#######&&&&"
					Escribir "#######BBBBGGP5YJJJ7~~::^^::!7????JJJJJ7!^^^.......:^^^:::.. ..:::..::^:::.....    ...            .^^.:?Y5PGBBB######&&&"
					Escribir "##BB#BBBBPPPP5YJJJ7!77^. ..^???JJJJJJJJ?!:....^^^::^7?7!~:^:..::::....:::.:....                     .. :?Y5PGBBBB######&"
					Escribir "##B#B#BBBGPP5YJJJJ7?J!^:  :!?JJJ???J?!~!~:...:^!~~~~7???7!77!!~~^:... ..:::...                        . :JY5PGBBBB######"
					Escribir "#####BBBBGG5YJ?JJJJJ?~^^  :7???????7~.::^:..::..::^!!!!777777!!~^:::.  ..........                 .  ..  ^JY5PGBBBB#####"
					Escribir "###BBBBBGGP5J??JJJJJJ?!^. .:^!7!~~~^::^^:^^:..:..:^^:~!!~~~^^:^^^^:... ..    ..:                 ........ !JYPGGBBBB####"
					Escribir "###BBBBGGP5Y??JJJJJJ?77!^:..:::::^~~^~77!!~^:......::^~~~^^:...:^^::.    ..    ...                  ..... .?Y5PGGBBBB###"
					Escribir "##BBBBGGP5YJ7JJJJJJ??7!77!~:::..:::^~7~!~^::::... .:::^~!!~^~^:^^::^.    .       .                         !JYPPGBBBB###"
					Escribir "##BBBBGGP5Y?7JJJJ???77!7???7!^~^^~:~!~~^^:^~^::::.:^~~~777777!^^:...:.....        .                         .?Y5PGGBBB###"
					Escribir "#B###BGPPYJ77JJ?7??77??????????7!!~77~~^^:^^~^^::^^~!7!77??77!^:...:.....        .                         .?Y5PGGBBBB#&"
					Escribir "##@@@&GP5YJ7!JJ!^!!!????????777!7??~^^^^^^:^^::::^^!!!!7777!!!~::..::.:.....   ....                         7J5PPGGBBB&@"
					Escribir "#@@@@@#P5YJ!:!?~^^~!???????Y5?J5#&#Y::^:^:::::^~^:^77!!!!~^^^^^::..:. ...:....~^.....                       ?5Y5PGGB#&@@"
					Escribir "@@@@@@&P5YJ! ~7~7J!~7?????G&&BB@@@@&7:::^:::.?BBY~5BBG7~!~^^::..::....:::...^5BG? ^7J7:              :~!~..J&@B5PPG&@@@@"
					Escribir "@@@@@@@&PYJ7.~7YGBG~!777?G&#&@&@@@@@5...:...7BP##PPBB&Y:^^:... !PGY^ ..:^~::Y55BB7J#@@B7.           ~P#&&Y?5@@@BPP#@@@@@"
					Escribir "@@@@@@@@BYJ?..?&#&@!7PY77#@&@@@@@@@@&?. ..  5&&@@&GG@@&^ ...  ~&@@&5:.7P#BPP#GB@&BB&@@@&?         7PBB&&@@#G@@@&PB@@@@@@"
					Escribir "@@@@@@@@@#GJ~ ?@@@@#PG#J5&@@@@@@@@@@@&Y:  .?#@@@@@@@@@@B^    !#@@@@#~7B&@&##&@@@@@@@@@@@#^ ~7^  :P&@@@@@@@@@@@@@##@@@@@@"
					Escribir "@@@@@@@@@@@&#G#@@@@@&#&&@@@@@@@@@@@@@@&J^:Y@@@@@@@@@@@@@5~~ ~B@@@@@&B@@@@@@@&@@@@@@@@@@@@575GG^.P@@@@@@@@@@@@@@@@@@@@@@@"
					Escribir "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&B@@@@@@@@@@@@@@@@&PB&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##@GP@@@@@@@@@@@@@@@@@@@@@@@@"
					Escribir "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"					
					Escribir "FINAL: El Equilibrio Salvado."
					Esperar Tecla
				Sino
					Escribir "Tu energía se extingue antes de concluir, pero el cometa se calma parcialmente."
					
					
					Escribir "               .::^^^^^^^^::.                                                                      "
					Escribir "             .:^^^^^^^^^^^^^^:                                                                     "
					Escribir "            .^^^^^^^^^^^^^^^^^:                          .:~77!^.                                  "
					Escribir "            :^^^^^^^^^^^^^^^^^^:                     .^~~^:^^!?Y5Y!:                               "
					Escribir "            :^^^^^^^^^^^^^^^^^^:                   .7!.        .~JPPY~                             "
					Escribir "            :^^^^^^^^^^^^^^^^^^.                   77J7^      ^5PPPPPGJ.                          "
					Escribir "             :^^^^^^^^^^^^^^^^.                .^^^^7J:~:  .?YPPPPPPPPG7         "
					Escribir "              .::^^^^^^^^^^::.                .7^.. ~!:    !PPGPPPPPPPG7                          "
					Escribir "                 .:::::::..                    ^55J:      !55YY5PPPPPP5.                          "
					Escribir "                                               :PY5J  .:.??7J^!?PPPPGGY7^.                        "
					Escribir "                                              ^YPYJPY!YY5P!77~JGPPP5!5PGP!:                       "
					Escribir "                                             ^GP5PPPPPPPPG5^::!PPPPG?7PGG?^                        "
					Escribir "                                            .JJY5555PP55!:   ?5PPPPYJ?7:                          "
					Escribir "                                                ... :Y^    :!J?!~!7JPJ!^.                         "
					Escribir "                                                     ^P: :?JY!^~~^^.757!Y^                        "
					Escribir "                                                   .!?Y5!??7~!7!^::^^Y5~!5?.                      "
					Escribir "                                                 .?P?~?P7Y7~?^       ~PJ:?5J!                     "
					Escribir "                                                :Y5P.J5Y7J!J::        J5~^Y7!Y:                   "
					Escribir "                                               .5?JY ?YYY:Y~!.      . 75!:?Y:Y5.                  "
					Escribir "                                               J5J5? Y!Y.YY~        7 75!:7Y^?5Y                  "
					Escribir "                                              .GYYP7.PJ^7?. :.     7~ ?5~:?Y^JJP7                 "
					Escribir "                                              !55Y?!~P77:.^^.     !? .5Y^^Y?7?JJG~                "
					Escribir "                                             ^P^J5^!YYJ^!~.      ~J  ?P7:!Y?7^YJPP:               "
					Escribir "                                            ~PY!7Y:?P5!^        ~Y^^7PJ^^YY7::JJ5?Y.              "
					Escribir "                                           ~Y7YY5^JY~.    .    75!^~PY~^?Y!:^:JJP~!?              "
					Escribir "                                         :JP7:?PP~P::^^^^:.  :J!  !PJ^~JJ^:^:~YJP^ 5:             "
					Escribir "                                        :Y7?Y!!PP!77:       !5~:!Y57~7J!^:^^^:?JJ5?  ~J             "
					Escribir "                                       :57^!JJ~:.~~!!~:   .YJ~7YY?!?J7^:^^^:?JJ5?  ~J             "
					Escribir "                                  ^~!!!JPPJ!^:..   .^..  ^5PJJJ??J?!^:^^^^:!YJYP.  !?             "
					Escribir "                                !?!:.   ^P~~7Y5Y7~^!7~:^J5JJJ??7!^::::^^^:!YJJP~   J~             "
					Escribir "                              ^J!        !Y   :!JJ!~??77YYYJ?7!!~~~~!!!~^!YJJP!    5:             "
					Escribir "                             ^Y: ~        7Y.    ^J?. ^^..~?J!~7???7!~:^?JJJ5!    :5              "
					Escribir "                          :~75:  ~7 ^.     !Y^     ^J~  ::  ~YY7^^^^^~7JJJYY^     7Y?^            "					
					Escribir "FINAL: El Equilibrio Incompleto."
				FinSi
				Esperar Tecla
			Sino
				Escribir "Lográs un pacto: la tierra y el cometa comparten energía sin aniquilarse."
				Si llave = 1 Entonces
					Escribir "La Llave Antigua abre un santuario donde se custodia ese balance."
					
					Escribir "... ... ... ... ...  ..  ... ... ... ..   .   ...^~!!7JJJYBB5YYJJ?7!^:::.  .   .. ... ...  ..  ..  ... ... ... ... ..."
					Escribir "... ... ... ... ... ...  ... ... ... ..  ^7??J5PGGB##BB###GB&#GGP5PPGB###BGP5J??!: .. ... ... .... ... ... ... ... ... ."
					Escribir ". ... ... ... ... ........ ... ...  .:~JB#J77!~~~!!?Y5P5J7!&7~?5PPY?7~^^^~~!!77?&GY7^.  ... ........ ... ... ... ... ..."
					Escribir "... ... ... ... ...............  .~?YY5#P: .:~7JYYJ7~^..  7#.    :~7JYYJ7~:.    5P^7Y5J!:  ........... ... ... ... ... ."
					Escribir "............................  :!J5Y!^JBJ!?Y5JJ!~:.  ......P5......   .:^!?Y5Y?!:~&~  :!Y5Y7:  .........................."
					Escribir ".......................   .^7Y5J7~!7G@&GY7~:      .. ... :#7 .... ..      .^!JP#B&G?!^:..~?YY7^.  ......................"
					Escribir ".....................  :!YB#B5YJY###B~77?JYYYJJ?7!^::..  7#. ...:^~!7?JJJYYYJ?77~GBB&5JYYJJ7?5PPJ~.  ..................."
					Escribir ".................... 7PGG5J7~:~JP?________________________________________________ ? !55!::~!7?J5B#G5! ................."
					Escribir ".................. :5@?.    ~YP7. |                                              |&~  .~5P7.    ...:Y@5: ..............."
					Escribir "................. !B@J . .75P!. . |                                              | : ..  ^YPJ^ .... ~&5G7 .............."
					Escribir "............... :5GBP. :?PY^  ... |                                              |....... :?PY~. ..:#7^G5: ............."
					Escribir ".............  !GJ?B::JPJ: ...... |______________________________________________| ........ .7P5!.  PP .?G7  ..........."
					Escribir "... ... .... :YG^~#?YP?. ....... .BY  .:!J5Y!^.  ...... ?#:...... ...^?55?^.  .:#7 ..........  !5P7.?#.. ^P5^  ... ....."
					Escribir ". ... ... ..!GJ.:#@P7. ..  ..    :#7:7Y5Y!. .. ... ...  5G  ... ... ..  ^?55?^ ^&~   .   .   ..  ^YPY&^   .?B7.  ... ..."
					Escribir "... ... ...Y#Y7Y#&PJYYYYYJJJ????7J&BGY~.  .  ... ... ...P5..  ... ... ..   ^75PP#~~!777????????JJJJP#@G??7~^!G5:.. ... ."
					Escribir ". ... ...:G#57?#&G ....:^::^~^^!G&&GJYYYY?7!~:...   ..  BJ  ..  ....:^~!7?YYJYP@&#?!777!!!!^^~^::^^.~&BG!7?JYY##! .. ..."
					Escribir "... ...  PG  ~B7JB.  ..  ..   ~P5:JB .:::~7?JYYYJJ?!~^::#!.^^~7?JJYYYJ??!^^^. YG:?G7   ..  ..  ..   5G.5P: ... P&^ ... ."
					Escribir ". ... ..~&~ 7#! ?#...  ... .:YG!. .BJ  ..   .. .^~~7JJYG@BYJJ?!~~^....   .   ?#: .^PP~  ...  ..  ...#?  JB~  ..?@B:. ..."
					Escribir "... ... 5P ?B~  7#:  ..  ..7GJ:  ..7#^.. ... ...  .  :5P7G5:  ..  ... ... ..~#!..  .7GJ:. ...  ..  7&: ..7#7.. !&#5 .. ."
					Escribir ". ...  ~&~JB^ ..7#:..  ..^PP~  ... .G5 ... ... ... .?G?. :YG!.  ... ... .. .BJ  ...  :YG!..  ..  ..P5...  ~BJ..:#?B? ..."
					Escribir "... ...5P5G: .. ~#^  ...JG7  ... .. ~&~  ... ...  ~G5:..  .!GY:.  ... ... .PP ... ...  ~P5:..  .. :&7  ... ^G5. G?~&~  ."
					Escribir ". ... ^&#5... ..~#^.. !GY: ... ... ..5G... ...  :YG!.. ...  :YG~ .. ... ..J#:.. ... ... .?G?...  .?B.... .. .5G:YG JB:.."
					Escribir "... ..P@J.  ... :#~ :5P~ ... ... ... :#7 ...  .7GJ:  ... ...  !GJ.... .. ~#~  ... ... ... :5G~ ...GY.. ... ...JB5#. G5 ."
					Escribir ". .. ^@G!^^:.   :#!?G?.... ... ... .. ?#:. ..^5P~  ... ....... .YG~ ... :#? ... ... ... ... ~G5: ^&^ ..  ..    7#&?YB#:."
					Escribir ".... !@Y7JYYYYJ77##P^..   .  ..  ......BJ ..?G7. ............... !BY. ..GP........... ...  . .?G7YG....:^^~!????G@G~YB.."
					Escribir ".... ~@G   .::^~Y@@YYYYYYJ777~^^^:...  !#^~PY:  ..  ..  ..  ..  . :5G~ JB.   .   .. ..:::^^^~!!J#@GJYYYYYJ?77!~:5@5 5P.."
					Escribir ".... ~@@~ .... .GYP5...:^^~!7??JYYYYJJJ7B##J~~!!~~!~~~~~~~~~^^~~^^^^JB5&7^!!77?JJJJYYYYYJJ??7#&#~:....         !#5B.PY ."
					Escribir ".... !#GP .... YB..GY ...       ...:^^~!G&##77?????????JJ??JJJJJJJJJJ?B@@57?7!!~~^^::.....     .P5.5G..........:#?^&^G? ."
					Escribir ".... !#7&^ .. ~&~  ^BJ .............. 7B7.5P.                       JB~7G!      ............ :GY. .PP........ 5G..#YB7 ."
					Escribir ".... !#:GY ...BJ .. ^#? .............JB~ ..YB^ .................. :PP:  ~GY. .............. :BY... :BY ..... !#^. 5#&! ."
					Escribir ".... ~#:7#:. 5G..... ~#7 ......... .5P: ... ?#! ................ ^BY.... :5G^ ............ ^BY..... ^#? ....:#J . 7@@^.."
					Escribir ".... !&..BJ !&~ ..... !#~ ....... ^GY....... ~B? .............. 7B7...... .?B7 .......... ^BJ .......~#7 .. 5G... ^&&: ."
					Escribir ".....:GY:?B^B? ....... ?#^ ..... !B?......... :G5. ............YB~  ....... ~GY. ....... ~#? ........ 7#~ .!&^..  .BB..."
					Escribir "... ...7P5&#P.. ... ... ?B^  .. ?#!  ... ... ...5G:  ... ... :PP: ... ... .. .5G^ ... ..~#7.. ...  ... ?#:.#J  .^7JGY  ."
					Escribir ". ... ...7G@Y!^^.  ..  ..YB:. .YB^ ... ... ...  .YB~.. ...  ~BJ ... ... ... .. 7B7  .. !#7  ...  ..  .. JGPG^7Y5Y?!: ..."
					Escribir "... ... ...:!?JYYYJ7!^:.  5G.:PP.... ... ... ...  7B! ..  .7B7... ... ... ... ..^G5:  !#!  ..  .. .:^^~!?#&PJ7^..  ... ."
					Escribir ". ... ... ...  .::^7?JYYJ?7#GBY..   .   .  ... ... ~BJ  ..YB~.  ... ... ... ..   .YG^7B~ :^^~7?JJYYYJJ?7~~^.  .. ... ..."					
					Escribir "FINAL SECRETO: El Santuario del Cometa (con Llave)."
				Sino
					
					Escribir "           YYYJP. :?5G?!.7JBY7^.JJBGJ?Y75B?Y!57GJ5~ .:~!777!^:...:^~!777!~:..   .:^~!!!?557^^!?~.     :?7.~!!7?!!!!!~~~~~^       "
					Escribir "      :~!!!J5GB57!~~JP?!!~!Y??7!!7YY!!?J5G?!^7JG5????7~:.  .^!777!^:.   .:^~!!7!!~^::.7J!!J!!?~?7    !J!?J! ~???:  :^ .::.       "
					Escribir "      :^::^!?7~..:!??~.  .~??~:.   ^?Y?^::::!?YJ!7?7!~~7?J5J7~^...:^~!77!~^::::^~!!77!J?5P55P!5JP7^~~5?5P?Y~P?YJP:?JYY7~~^       "
					Escribir "       .~??~.  ~?7~.   ^7?~.    .~?7^     ^77^.      ~?J7:::^^^~!?PGJ7~^^~7??77!~^:::::!YP?77?BY7!!~~^~YP7^:7PB?!:55G57          "
					Escribir "      ~?!.  :7?!.   .!?7:     ^7?~.    .!?!:      .7J7:        ^7?!^.:^^!?JYP5J?7JPJ!~~~~!?YJ7!~^~~?Y5YJ7!~~~!77YYJYJ?777~       "
					Escribir "      .   ~?7^    ^??~.    .!?7:     .7J~       :?J~.       ^7?!:       .^!77~:^~?J7!~:     ^!77!^.  .:~!77~:   .^!?7^           "
					Escribir "       .!J!.    ~?7:     :7?~.       !~       .?J~       .!?!:       .~7?!:         .^75Y!~:   .^!??!:    :~7?7^    :!?7:        "
					Escribir "      !J~    .!J!.     ~??^       ^7.       .7J~       :7?^^.      :7?~:   :!.     .~77~:.^!77!^.  .^7?7:     ^7?7:    ~?~       "
					Escribir "      .    .!J!.     ~J7:     :~.??J7      ~Y!       :??~.!JY..  :??^ ^J?77!G:  .!??~.        ^Y577~:  ^7?!.     ^??^            "
					Escribir "          :J!.     ~J7.     ^.?Y?5.Y!^:  :J?.     ^.~?: J55755? ~?^  ~G^^P~?G^ .?!. ^.      ^??~. .~75?~::~?7:     :7^           "
					Escribir "       ^Y!..     ~J7.     ~?J5YY:P??Y5: !Y^    . !5J   .57Y5J57 .~  7J~J757PY^.   ~JYJ    ~J?!!  !Y. !G!!?7~~J?.      ^J^        "
					Escribir "       Y~7?:~. :J?:     ~J7~?5 7BY7Y7  ?J     :P55:57! JY!PY!P!^Y! JYY.JY~~YJ!7?77Y ~P   .!:.Y?YJ!P:  ^:  JY  ~::. ~^!??J        "
					Escribir "     .!5J~P!P! ~^     :J?. P!JJJ?~7G~ !5!^   .JG755YG7^P7?P77P:?7:JP ~GY!!55   P~JY^JJY?  .:5J P?~5J!   ~Y7?Y!!5Y.!GJP^JJ!:      "
					Escribir "     .JY7YY7JY!      !Y^ :JP^ PP!?PJP?JPB?   !P~?P7!G!JP~75~75. .Y~?J75~^?Y^ :J57 P?~?J:  55~Y757!JJ: 7Y?Y!JY!YJ ^5!JP?~JY:      "
					Escribir "      :G?!P?^Y5:    ??.  Y7:JY?:^755JJ5YP~  :P?7Y5~75~P^~PJ!YJ :YP! YP7~!G~ ~55 ?5J!7P!  JPJ JP!~5Y  7PG.:BY!!YY!YY.Y5!7B^       "
					Escribir "       !Y!~P7 ?5:   :   ?P? !G?!JGJ5!YJ~5! .JY7~G?!5?Y??75!!P! ?J 75Y7~7G! .P^!??5!!5J.:JY ?YY!!5?..?5 ~P7~~5P:.P7.PP~!57        "
					Escribir "       :57~?J?!~5      :G.~5P~:!J:?Y^Y5~JJ :G!^55~!P!?? YY^~Y:?YY~:P!^:75:.YY? J5~^7P!.P!J!?J^^?Y..5!Y~7Y~!G?  5!J?J~^7P:        "
					Escribir "        ~G!!GJ:75?    !YY7.5Y!!P? ?Y~YG7J5 !P~7PY!~57 ~YPY!7. P: YPJ!7YG. 7G.~5J7~7G7~YP: PY~~Y5 !Y5 ^BY~7Y7..?G^.G?~7B^         "
					Escribir "        .J?^^5J::G:   ?7 !P?^^57  ?P!JP~J5^57?!G?!J5  :5Y~    !Y:JJ..^Y7 !5~Y^P?!7G7 J?.JY5!~YY.:P.~JJ!^~YJ  J?:J5!^~Y?          "
					Escribir "         ^G!~J?!7JJ   ^Y^^5~^?5.  !5~!G7^P^5^ 5J::J7  ?YY      :PPJ!77: .YP.^B7^~JY.?YY:75~^75: ^5.:57^!G7   ?J 75!~JG.          "
					Escribir "          ~?~~G~ 7?    ~5PY!7!    ~P~!P7J~ :5J57!?~   5Y?      ~Y5^     ?J^Y?Y~^?B^ P^.Y5!^7G:   ~PP!^!?!     7PP~~??:           "
					Escribir "           .:^JYY7     JJY..       ^J75Y7  ^5Y^..    :5Y~   ^^ YJ5      7Y ^G!~7G~  ^J7P!^7?^    ?Y5^^:       7JP~^:             "
					Escribir "              .5Y~    ^55~    ::    J!?YJ  ?YJ       ~YY:  77 .5J7       ?Y5J^~J?    7YP~^:     !YY~    :7   :YJ?                "
					Escribir "               !Y5.  .5Y7    ^Y    75 JY? :5Y: :.    .7^~G!   ?JY:?      JJ5      .~?YY^~:     ?J5.    !J   .5JJ                 "
					Escribir "        ::     ^Y5:  ~55:    5Y...^P: Y5~ ~YY. .!7. .~~7BJ    J?Y Y:     5J?    .7?:YY7 ^7?~  .5J7    !7    ^YY~      ^:         "
					Escribir "         !J^   .5Y~  ?Y5    YJ:Y?~?! :55: 7YY    ~PJY!755^7   Y?Y 7Y    .5Y!   7P! :5Y^   YY5^7J5:  ^Y!     7J5.    !Y~          "
					Escribir "          ~GJ.  YY7  YYJ  .5P  ^#Y!7J!7~  JJJ   ^JGGY:JJ? Y?  Y?Y .P7   :5Y~ ^PG:  !Y5.  7P JPP?J :YB~ :~:  J?5   :5G^           "
					Escribir "           ~GJ^ YJ?  5Y7 ^YB:  ?B^J7:Y5~  JJJ .?J5~.5P.J~ :BJ J?5  ~#?  ^5Y~?5P.   ?YY  ?#? .5!G~^YG~  ~~JY^5?Y  !YG^            "
					Escribir "            ~?J!JJJ .55~~JY7  75Y  YY 7GJ:J?J^Y!Y~  !GP^   !BJJ?5.  ?GJ.:5JG7Y^    JJY JJP:  J!75J7J   :J ?GPJJ ??J7             "
					Escribir "            .5:JGJJ :5YJJ^5  :5J!  ^BY ~5JP7P5.J7   ?75P.   5!PYY^   YJJ^?Y7!J     JJJJ7!J   ?7!G.Y^    5^ Y7G?7J.5.             "
					Escribir "             Y! GJY :55P 7?  7YY:  :5P^ 7JYYP:.P.   ?7^PY.  ?7:5J7   :5~Y7Y 5~     ??GY 5^   Y7G! P.    Y! ~J^G5 !Y              "
					Escribir "             ?? ?YY ^55! Y~  ?YY   :YY~  Y!GJ ~Y    7J?!G7  :5 Y?Y    J!!B^ 5:     ??G~.P.   57P.:P.    J7 :P P7 ??              "
					Escribir "             ?? !55 ^5P^ Y!  ?G^   .PY?  !JJ? ~P    ~5P:5P:  Y!?JP:   ~5:G  P^     7YP: P:  .P?P .P.    !5 .G:P~ ?J              "					
					Escribir "FINAL: Armonía Renovada."
				FinSi
			FinSi
			Esperar Tecla
			
			
		4: 
			
			Si llave = 1 Entonces 
				Elec_secreto <- secreto(vida, energia, amuleto, llave, inventario, TAM) 
				segun Elec_secreto Hacer
					1:
						vida <- vida - 10
						energia <- energia - 20
						MostrarEstado(vida, energia, amuleto, llave)
						Esperar Tecla
						Limpiar Pantalla
						Escribir "Sellas el núcleo. El cometa pierde su influencia. La tierra continúa, pero sin su brillo misterioso."
						
						
						Escribir "                                                  .!5PPPPPPPP5YJJJJY^                               "
						Escribir "                                                 :JPPPPPPPPPPGGP55YY^                               "
						Escribir "                                                :5PPPPPPPPPP5Y?~!YP?.                               "
						Escribir "                                                ?GPPPPPPPPYJ77!~^!~                                 "
						Escribir "                                  :~:.      ..^~Y5PPPPPPP5Y5JJYYYJ!                                 "
						Escribir "                                ^7?7~~^^..^!7?YYYY5PPPPPP5YP5YY5PPY~                                "
						Escribir "                                7?7?77!!!!???Y5PPPPPP55J!^?55YYYJ?J?~.                              "
						Escribir "                                ^!!7???777!!!7?Y5P5J?7!~~^?PP5JJ??JY?!:                             "
						Escribir "                                  :~7?JJJJ??77!!!!!77???77Y5YJ?7!!?J?!!                             "
						Escribir "                              .^~!7??JY5PP55YJ??77!!~~~!!7??55J7!~7YJ77:                            "
						Escribir "                           .^!7???JJY55PPPPPPPP55YJ??77!!~~^~~~~~^~JYJ?:                            "
						Escribir "                        .^!777??JYY55PPPPPPPPPPPPPP55J?????7!~^^^~77YPP^                            "
						Escribir "                     .^!777??JJYY55PPPPPPPPPPPPPPPPPPPP5J????!!!~~7!~!7~:.                          "
						Escribir "                  .^!777??JJYY55PPPPPPPPPPPPPPPPPPPPPPPPPPY?7!!!!!!!!!~!!~^:.                       "
						Escribir "               .^!7?????JYY55PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP?^^~!!!!!!7?777!~~^:.                   "
						Escribir "            .^!7?????JYY555PPPPPPPYYPPPPPPPPPPPPPPPPPPPPPPPPY:::^~^!7?Y??????7!!!~^:..              "
						Escribir "           ~7??????JY555PPPPPPPPP7.JPPPPPPPPPPPPPPPPPPPPPPPP5^::^~^~!5P5?^:^!7???777!!~^^:..        "
						Escribir "         :7J????JY55PPPPPPPPPPP?: ^PPPPPPPPPPPPPPPPPPPPPPPPPY~::^~^~~JPP5?^.  .:~77???7!!!!!:       "
						Escribir "        ^YY??JY55PPPPPPPPPPP5?^.:7PPPPPPPPPPPPPPPPPPPPPPPP5!.~:^:^^^~?PPPPJ!:     .:~!?????7^       "
						Escribir "       ^55YY5PPPPPPPPPPPPP5?!~!7JPPPPPPPPPPPPPPPPPPPPPPPP7.  ^^::^^^~!PPPPP57^.       .^!!^.        "
						Escribir "       JPPPPPPPPPPPPPPP5?!!!!7?YPPPPPPPPPPPPPPPPPPPPPP57:    :^::^~^~~YPPPPP5?~.                    "
						Escribir "      ^5PPPPPPPPPPPPPP5J!!7J??J5PPPPPPPPPPPPPPPPPPP5J~.      .~:^^~^~~JPPPPPP5?~^                   "
						Escribir "     :5PPPPPPPPPPPPPPPPY^^!??JY5PPPPPPPPPPPPPPPP5Y7~^:::......~^^^^^^~?PPPPPPP5J!:                  "
						Escribir "     !PPPPPPPPPPPPPPPPP?^^^!!?JY5PPPPPPPPPPPPPPY!!~^^:::::::::~^:^^^^~!PPPPPPPP5?~:                 "
						Escribir "     .5PPPPPPPPPPPPPP57^~!~^^~7JY55PPPPPPPPPPP5!^~^::::::::^~^~^:^^~^~!5PPPPPPP5Y?~:                "
						Escribir "      ^5GPPPPPPPPPPPPJ~~!777!^^!?Y55PPPPPPPPP5?^::~::^^^^7YYJJJ~:^^~^~~JPPPPPPPP5Y?~:               "
						Escribir "       .?PPPPPPPPPPPP5?7!7??J?!~!7JY5PPP5PPP5J!^::~~~~^^!PPPPPP7:^^^^~~7PPPPPPPPP5Y?^^              "
						Escribir "         ^?5PPPPPPPPPP5J???JYYJ?7!7?JY55PPPYJ!~^^~!!~~^^?PPPPPPY:^^^^^~!5PPPPPPPPP5Y?^^             "
						Escribir "           !GPPPPPPPPPP5YJJJYY555YJ?77?YP5J77!77???7!~~!5PPPPPP5^:^^~^~~5PPPPPPPPPP557~.            "
						Escribir "           !PPPPPPPPPPPP5YYY555PPPPP5YJY5YYYYYYYJJ??!!!?5PGPPPPP~:^^~^~~JPPPPPPPPPPPP57:            "
						Escribir "           ?GPPPPPPPPPPPP55555PPPPPPPPPPPPPPPP5YYJJ?77?J::~!7YPG7:^^~^~~7PPPPPPPPPPPPP5^            "
						Escribir "          .5PPPPPPPPPPPPPPPP5PPPPPPPPPPPPPPPPP55YYYJ??J?      :~!^^^^^^~!PGGGGGPPPPP5?^             "
						Escribir "          ?PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP55YYYJY5~        :^^^^~^~~~~!7777!~^:.               "
						Escribir "         .PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP55555P7         :~:^^~^~!:                          "
						Escribir "         ^PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP7          .~^^^~^~!^                          "
						Escribir "         ~PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP5~            ^^^^^^^~~                          "
						Escribir "         ~PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPP?.             :^^^^~^~~.                         "
						Escribir "         ^PPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPPJ^               :^^^^~^~~:                         "						
						Escribir "FINAL SECRETO: El Guardián Estelar"
					2:
						vida <- vida - 5
						energia <- energia - 10
						MostrarEstado(vida, energia, amuleto, llave)
						Esperar Tecla
						Limpiar Pantalla
						Escribir "Canalizás la energía para nutrir la tierra. Los bosques renacen y la gente prospera."
						
						
						Escribir "@@@@@@@@@@@@@@@@@&@&&&&&&&&&&&&&&&&###BBGGP555YJJJ??77!!!!!!!!!77??JJY55PPPGGGB###&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@@@@@@&"
						Escribir "@@@@@@@@@@@@@&&&&&&&&##&&&&&&&&&###BBGPP5YJ7!~^::....:::..           ..:^~!7?JY5PPGBB##&&&&&&&&&&&&&&&&&&@@@@@@@@@@@@@@@"
						Escribir "@@@@@@@@@@@@@&&&&&&&&&&&&&&&###BBGPP5YJ7~:..... .::.::::....               ..:^~7?Y5PGGBB##&&&&&&&&&&&&&&&&&&&&&&&&&&&&&"
						Escribir "@@@@@@@@&&&&&&&&&&&&&&&&&###BGP55YJ?!^:....:.....:....    ..                   . .:~7JY5PGGB###&&&&&&&&&&&&&&&&&&&&&&&&&"
						Escribir "@&&&&&&&&&&&&&&&&&&&&&###BGP5YJ?7~^...:^~^:::... ...                                 .:!?Y5PPGB###&&&&&&&&&&&&&&&&&&&&&&"
						Escribir "&&&&&&&&&&&&&&&&&&&###BGG5YJ?7!~:..::..::.::..                                           :!?Y5PGBB###&&&&&&&&&&&&&&&&&&&"
						Escribir "&&&&&&&&&&&&&&&&####BGP5YJ7!~^:::.::.  ...::::::....                                  ...  .^!JY5PGB######&&&&&&&&&&&&&&"
						Escribir "&&&&&&&&&&&&&###PPBGP5YJ?77!^::.:..   .:::........            .. . ...                        :!JY5PGB###&&&&&&&&&&&&&&&"
						Escribir "&&&&&&&&#######BGPP5YJ?77!~^.....  ..:::::.    .    ..          ..^^:^^...              ..      :!JY5PGB####&&&&&&&&&&&&"
						Escribir "&&&&########BBGGP5YJ??!^^.    ...:....:..... .              .^:^..^~:::^:               ...     .:^?Y5PGB####&&&&&&&&&&&"
						Escribir "######BBBBBBGGP5YJ?J?:..:^^^^^^~!7???!~~!^^:..     ..   .:..  ...:::::::::...               .    :7?:~JY5GGBB#######&&&&"
						Escribir "#######BBBBGGP5YJJJ7~~::^^::!7????JJJJJ7!^^^.......:^^^:::.. ..:::..::^:::.....    ...            .^^.:?Y5PGBBB######&&&"
						Escribir "##BB#BBBBPPPP5YJJJ7!77^. ..^???JJJJJJJJ?!:....^^^::^7?7!~:^:..::::....:::.:....                     .. :?Y5PGBBBB######&"
						Escribir "##B#B#BBBGPP5YJJJJ7?J!^:  :!?JJJ???J?!~!~:...:^!~~~~7???7!77!!~~^:... ..:::...                        . :JY5PGBBBB######"
						Escribir "#####BBBBGG5YJ?JJJJJ?~^^  :7???????7~.::^:..::..::^!!!!777777!!~^:::.  ..........                 .  ..  ^JY5PGBBBB#####"
						Escribir "###BBBBBGGP5J??JJJJJJ?!^. .:^!7!~~~^::^^:^^:..:..:^^:~!!~~~^^:^^^^:... ..    ..:                 ........ !JYPGGBBBB####"
						Escribir "###BBBBGGP5Y??JJJJJJ?77!^:..:::::^~~^~77!!~^:......::^~~~^^:...:^^::.    ..    ...                  ..... .?Y5PGGBBBB###"
						Escribir "##BBBBGGP5YJ7JJJJJJ??7!77!~:::..:::^~7~!~^::::... .:::^~!!~^~^:^^::^.    .       .                         !JYPPGBBBB###"
						Escribir "#B###BGPPYJ77JJ?7??77??????????7!!~77~~^^:^^~^^::^^~!7!77??77!^:...:.....        .                         .?Y5PGGBBBB#&"
						Escribir "##@@@&GP5YJ7!JJ!^!!!????????777!7??~^^^^^^:^^::::^^!!!!7777!!!~::..::.:.....   ....                         7J5PPGGBBB&@"
						Escribir "#@@@@@#P5YJ!:!?~^^~!???????Y5?J5#&#Y::^:^:::::^~^:^77!!!!~^^^^^::..:. ...:....~^.....                       ?5Y5PGGB#&@@"
						Escribir "@@@@@@&P5YJ! ~7~7J!~7?????G&&BB@@@@&7:::^:::.?BBY~5BBG7~!~^^::..::....:::...^5BG? ^7J7:              :~!~..J&@B5PPG&@@@@"
						Escribir "@@@@@@@&PYJ7.~7YGBG~!777?G&#&@&@@@@@5...:...7BP##PPBB&Y:^^:... !PGY^ ..:^~::Y55BB7J#@@B7.           ~P#&&Y?5@@@BPP#@@@@@"
						Escribir "@@@@@@@@BYJ?..?&#&@!7PY77#@&@@@@@@@@&?. ..  5&&@@&GG@@&^ ...  ~&@@&5:.7P#BPP#GB@&BB&@@@&?         7PBB&&@@#G@@@&PB@@@@@@"
						Escribir "@@@@@@@@@#GJ~ ?@@@@#PG#J5&@@@@@@@@@@@&Y:  .?#@@@@@@@@@@B^    !#@@@@#~7B&@&##&@@@@@@@@@@@#^ ~7^  :P&@@@@@@@@@@@@@##@@@@@@"
						Escribir "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&B@@@@@@@@@@@@@@@@&PB&@@@@@@@@@@@@@@@@@@@@@@@@@@@@@##@GP@@@@@@@@@@@@@@@@@@@@@@@@"
						
						Escribir "FINAL SECRETO: El Don del Cometa (Llave usada para transformar)."
				FinSegun
				
			SiNo
				Escribir "No posees la Llave Antigua. La opción falla."
				Esperar Tecla
				
			FinSi
	FinSegun
FinSubProceso
//____________________________________________________________________________________________________________________________

//                                                            Mostrar instrucciones 
//____________________________________________________________________________________________________________________________

subproceso  MostrarInstrucciones
	LimpiarPantalla
	Escribir "================================================================================"
	Escribir "                             INSTRUCCIONES DEL JUEGO"
	Escribir "================================================================================"
	Escribir ""
	Escribir "- Navegación: en los menús ingresa el número de la opción."
	Escribir "- En muchos prompts verás (0 = menú jugador);"
	Escribir "  eso te permite abrir el menú del jugador en cualquier momento."
	Escribir ""
	Escribir "- Inventario: podés recoger hasta 4 objetos."
	Escribir "  - Fruto Místico: recupera energía (consumible)."
	Escribir "  - Piedra de Luz: recupera vida y energía (consumible)."
	Escribir "  - Amuleto del Cometa: protección única (no consumible hasta usarla)."
	Escribir "  - Llave Antigua: desbloquea finales secretos (no consumible hasta usarla)."
	Escribir ""
	Escribir "- Menú del jugador: Ver estado / Ver inventario / Usar objeto."
	Escribir "- Vida y energía: si cualquiera llega a 0, puede terminar el juego;"
	Escribir "  el Amuleto puede evitar una muerte una vez."
	Escribir ""
	Escribir "- Guardá decisiones en mente: algunos objetos permiten finales secretos."
	Escribir ""
	Escribir "Consejo: abrí el menú con 0 cuando veas dudas o quieras usar un objeto."
	Escribir ""
	Escribir "¡Buena suerte, viajero del cometa!"
	Escribir "--------------------------------------------------------------------------------"
	Esperar Tecla
FinsubProceso

//-----------------------------------------FIN FUNCIONES HISTORIA-----------------------------------------------------------


//funciones tragamonedas

// ====================== JUEGO TRAGAMONEDAS ======================
SubProceso JuegoTragamonedas
    Definir saldo_inicial, saldo, valor_tiradas, opcion_juego, jackpot,Opcionsaldo_inicial Como Entero
    Definir a, b, c, i Como Entero // Declarar i para el bucle
    Definir simbolos Como Cadena
    Dimension simbolos[3]
    
    // Asignar valores a índices 0, 1, 2
    simbolos[0] <- "CEREZA"
    simbolos[1] <- "LIMON"
    simbolos[2] <- "SANDIA"
    
	
	// Saldo inicial
	Escribir"+-----------------------------------------------------------------------------------------------------------+"
	Escribir"|                                 |                                     |                                   |"       
	Escribir"|                                 |                                     |                                   |"           
	Escribir"|          PPPGPGPGPGPP           |            PGPGPGPGPPPP             |           PGPGPGPPPPPP            |"       
	Escribir"|          PPPPPGGGPPP:           |            PPPGGGPPBPP:             |           PPPPPGGPPBP:            |"        
	Escribir"|          PPPPGGPPPG?            |            PPGGPPPGPP?              |           PPYPPGGPPP?             |"        
	Escribir"|               PPPG?             |                  PPPG?              |                 PPPG?             |"        
	Escribir"|              PPPG5              |                 PG5G5               |               PPPG5               |"        
	Escribir"|             P555P               |                P555P                |              P555P                |"        
	Escribir"|                                 |                                     |                                   |"       
	Escribir"|                                 |                                     |                                   |"         
	Escribir"+===========================================================================================================+"
	Escribir"|                                                                                                           |" 
	Escribir"|                                        ELIGE TU SALDO INICIAL (ARS)                                       |"
	Escribir"|                                                                                                           |" 
	Escribir"+===========================================================================================================+"
	Escribir"|                 [1] 5.000 ARS (principiante)                 [4] 100.000 ARS (high roller)                |"
	Escribir"|                 [2] 20.000 ARS (recomendado)                 [5] 150.000 ARS (nivel avanzado)             |" 
	Escribir"|                 [3] 50.000 ARS (intermedio)                  [6] 200.000 ARS (experto)                    |"
	Escribir"+-----------------------------------------------------------------------------------------------------------+"
	Escribir "Seleccione una opción (1-6): "
	
	Leer Opcionsaldo_inicial
    Mientras Opcionsaldo_inicial < 1 o Opcionsaldo_inicial > 6 Hacer
		
		Limpiar Pantalla
		Escribir"+-----------------------------------------------------------------------------------------------------------+"
		Escribir"|                                 |                                     |                                   |"       
		Escribir"|                                 |                                     |                                   |"           
		Escribir"|          PPPGPGPGPGPP           |            PGPGPGPGPPPP             |           PGPGPGPPPPPP            |"       
		Escribir"|          PPPPPGGGPPP:           |            PPPGGGPPBPP:             |           PPPPPGGPPBP:            |"        
		Escribir"|          PPPPGGPPPG?            |            PPGGPPPGPP?              |           PPYPPGGPPP?             |"        
		Escribir"|               PPPG?             |                  PPPG?              |                 PPPG?             |"        
		Escribir"|              PPPG5              |                 PG5G5               |               PPPG5               |"        
		Escribir"|             P555P               |                P555P                |              P555P                |"        
		Escribir"|                                 |                                     |                                   |"       
		Escribir"|                                 |                                     |                                   |"         
		Escribir"+===========================================================================================================+"
		Escribir"|                                                                                                           |" 
		Escribir"|                                        ELIGE TU SALDO INICIAL (ARS)                                       |"
		Escribir"|                                                                                                           |" 
		Escribir"+===========================================================================================================+"
		Escribir"|                 [1] 5.000 ARS (principiante)                 [4] 100.000 ARS (high roller)                |"
		Escribir"|                 [2] 20.000 ARS (recomendado)                 [5] 150.000 ARS (nivel avanzado)             |" 
		Escribir"|                 [3] 50.000 ARS (intermedio)                  [6] 200.000 ARS (experto)                    |"
		Escribir"+-----------------------------------------------------------------------------------------------------------+"
        Escribir "Ingrese un opcion de saldo valido (1-6)"
		Leer Opcionsaldo_inicial
		Limpiar Pantalla
    FinMientras
	
	Limpiar Pantalla
	
	
	
	
	
	//valor de las tiradas
    
	// === ASIGNAR SALDO Y APUESTA SEGÚN NIVEL ===
    Segun Opcionsaldo_inicial Hacer
        1:
		    saldo_inicial <-5000
            valor_tiradas <- 500
			saldo<-saldo_inicial
			Escribir"+-----------------------------------------------------------------------------------------------------------+"
			Escribir"|                                  |                                     |                                  |"       
			Escribir"|                                  |                                     |                                  |"           
			Escribir"|           PPPGPGPGPGPP           |            PGPGPGPGPPPP             |           PGPGPGPPPPPP           |"       
			Escribir"|           PPPPPGGGPPP:           |            PPPGGGPPBPP:             |           PPPPPGGPPBP:           |"        
			Escribir"|           PPPPGGPPPG?            |            PPGGPPPGPP?              |           PPYPPGGPPP?            |"        
			Escribir"|                PPPG?             |                  PPPG?              |                 PPPG?            |"        
			Escribir"|               PPPG5              |                 PG5G5               |               PPPG5              |"        
			Escribir"|              P555P               |                P555P                |              P555P               |"        
			Escribir"|                                  |                                     |                                  |"       
			Escribir"|                                  |                                     |                                  |"         
			Escribir"+===========================================================================================================+"
			Escribir"|                                                                                                           |" 
			Escribir"|                                            TAMAÑO DE APUESTA                                              |"
			Escribir"|                                                                                                           |" 
			Escribir"+===========================================================================================================+"
			Escribir"|                                                                                                           |"
			Escribir"|                                                  500 ARS                                                  |" 
			Escribir"|                                                                                                           |"
			Escribir"+-----------------------------------------------------------------------------------------------------------+"
			Escribir" Presione una tecla para continuar."
			Esperar Tecla
        2:
			saldo_inicial <-20000
            valor_tiradas <- 1000
			saldo<-saldo_inicial
			Escribir"+-----------------------------------------------------------------------------------------------------------+"
			Escribir"|                                  |                                     |                                  |"       
			Escribir"|                                  |                                     |                                  |"           
			Escribir"|           PPPGPGPGPGPP           |            PGPGPGPGPPPP             |           PGPGPGPPPPPP           |"       
			Escribir"|           PPPPPGGGPPP:           |            PPPGGGPPBPP:             |           PPPPPGGPPBP:           |"        
			Escribir"|           PPPPGGPPPG?            |            PPGGPPPGPP?              |           PPYPPGGPPP?            |"        
			Escribir"|                PPPG?             |                  PPPG?              |                 PPPG?            |"        
			Escribir"|               PPPG5              |                 PG5G5               |               PPPG5              |"        
			Escribir"|              P555P               |                P555P                |              P555P               |"        
			Escribir"|                                  |                                     |                                  |"       
			Escribir"|                                  |                                     |                                  |"         
			Escribir"+===========================================================================================================+"
			Escribir"|                                                                                                           |" 
			Escribir"|                                            TAMAÑO DE APUESTA                                              |"
			Escribir"|                                                                                                           |" 
			Escribir"+===========================================================================================================+"
			Escribir"|                                                                                                           |"
			Escribir"|                                                 1000 ARS                                                  |" 
			Escribir"|                                                                                                           |"
			Escribir"+-----------------------------------------------------------------------------------------------------------+"
			Escribir" Presione una tecla para continuar."
			Esperar Tecla
        3:
			saldo_inical <-50000
            valor_tiradas <- 2000
			saldo<-saldo_inicial
			Escribir"+-----------------------------------------------------------------------------------------------------------+"
			Escribir"|                                  |                                     |                                  |"       
			Escribir"|                                  |                                     |                                  |"           
			Escribir"|           PPPGPGPGPGPP           |            PGPGPGPGPPPP             |           PGPGPGPPPPPP           |"       
			Escribir"|           PPPPPGGGPPP:           |            PPPGGGPPBPP:             |           PPPPPGGPPBP:           |"        
			Escribir"|           PPPPGGPPPG?            |            PPGGPPPGPP?              |           PPYPPGGPPP?            |"        
			Escribir"|                PPPG?             |                  PPPG?              |                 PPPG?            |"        
			Escribir"|               PPPG5              |                 PG5G5               |               PPPG5              |"        
			Escribir"|              P555P               |                P555P                |              P555P               |"        
			Escribir"|                                  |                                     |                                  |"       
			Escribir"|                                  |                                     |                                  |"         
			Escribir"+===========================================================================================================+"
			Escribir"|                                                                                                           |" 
			Escribir"|                                            TAMAÑO DE APUESTA                                              |"
			Escribir"|                                                                                                           |" 
			Escribir"+===========================================================================================================+"
			Escribir"|                                                                                                           |"
			Escribir"|                                                 2000 ARS                                                  |" 
			Escribir"|                                                                                                           |"
			Escribir"+-----------------------------------------------------------------------------------------------------------+"
			Escribir" Presione una tecla para continuar."
			Esperar Tecla
        4:
			saldo_inicial <-100000
            valor_tiradas <- 5000
			saldo<-saldo_inicial
			Escribir"+-----------------------------------------------------------------------------------------------------------+"
			Escribir"|                                  |                                     |                                  |"       
			Escribir"|                                  |                                     |                                  |"           
			Escribir"|           PPPGPGPGPGPP           |            PGPGPGPGPPPP             |           PGPGPGPPPPPP           |"       
			Escribir"|           PPPPPGGGPPP:           |            PPPGGGPPBPP:             |           PPPPPGGPPBP:           |"        
			Escribir"|           PPPPGGPPPG?            |            PPGGPPPGPP?              |           PPYPPGGPPP?            |"        
			Escribir"|                PPPG?             |                  PPPG?              |                 PPPG?            |"        
			Escribir"|               PPPG5              |                 PG5G5               |               PPPG5              |"        
			Escribir"|              P555P               |                P555P                |              P555P               |"        
			Escribir"|                                  |                                     |                                  |"       
			Escribir"|                                  |                                     |                                  |"         
			Escribir"+===========================================================================================================+"
			Escribir"|                                                                                                           |" 
			Escribir"|                                            TAMAÑO DE APUESTA                                              |"
			Escribir"|                                                                                                           |" 
			Escribir"+===========================================================================================================+"
			Escribir"|                                                                                                           |"
			Escribir"|                                                 5000 ARS                                                  |" 
			Escribir"|                                                                                                           |"
			Escribir"+-----------------------------------------------------------------------------------------------------------+"
			Escribir" Presione una tecla para continuar."
			Esperar Tecla
        5:
			saldo_inicial <-150000
            valor_tiradas <- 7500
			Escribir"+-----------------------------------------------------------------------------------------------------------+"
			Escribir"|                                  |                                     |                                  |"       
			Escribir"|                                  |                                     |                                  |"           
			Escribir"|           PPPGPGPGPGPP           |            PGPGPGPGPPPP             |           PGPGPGPPPPPP           |"       
			Escribir"|           PPPPPGGGPPP:           |            PPPGGGPPBPP:             |           PPPPPGGPPBP:           |"        
			Escribir"|           PPPPGGPPPG?            |            PPGGPPPGPP?              |           PPYPPGGPPP?            |"        
			Escribir"|                PPPG?             |                  PPPG?              |                 PPPG?            |"        
			Escribir"|               PPPG5              |                 PG5G5               |               PPPG5              |"        
			Escribir"|              P555P               |                P555P                |              P555P               |"        
			Escribir"|                                  |                                     |                                  |"       
			Escribir"|                                  |                                     |                                  |"         
			Escribir"+===========================================================================================================+"
			Escribir"|                                                                                                           |" 
			Escribir"|                                            TAMAÑO DE APUESTA                                              |"
			Escribir"|                                                                                                           |" 
			Escribir"+===========================================================================================================+"
			Escribir"|                                                                                                           |"
			Escribir"|                                                7500  ARS                                                  |" 
			Escribir"|                                                                                                           |"
			Escribir"+-----------------------------------------------------------------------------------------------------------+"
			Escribir" Presione una tecla para continuar."
			Esperar Tecla
        6:
			saldo_inicial <-200000
            valor_tiradas <- 10000
			saldo<-saldo_inicial
			Escribir"+-----------------------------------------------------------------------------------------------------------+"
			Escribir"|                                  |                                     |                                  |"           
			Escribir"|                                  |                                     |                                  |"       
			Escribir"|           PPPGPGPGPGPP           |            PGPGPGPGPPPP             |           PGPGPGPPPPPP           |"       
			Escribir"|           PPPPPGGGPPP:           |            PPPGGGPPBPP:             |           PPPPPGGPPBP:           |"        
			Escribir"|           PPPPGGPPPG?            |            PPGGPPPGPP?              |           PPYPPGGPPP?            |"        
			Escribir"|                PPPG?             |                  PPPG?              |                 PPPG?            |"        
			Escribir"|               PPPG5              |                 PG5G5               |               PPPG5              |"        
			Escribir"|              P555P               |                P555P                |              P555P               |"        
			Escribir"|                                  |                                     |                                  |"       
			Escribir"|                                  |                                     |                                  |"         
			Escribir"+===========================================================================================================+"
			Escribir"|                                                                                                           |"       
			Escribir"|                                            TAMAÑO DE APUESTA                                              |"
			Escribir"|                                                                                                           |"       
			Escribir"+===========================================================================================================+"
			Escribir"|                                                                                                           |"
			Escribir"|                                                10000 ARS                                                  |" 
			Escribir"|                                                                                                           |"
			Escribir"+-----------------------------------------------------------------------------------------------------------+"
			Escribir" Presione una tecla para continuar."
			Esperar Tecla
    FinSegun
	
	
	
	Limpiar Pantalla
	
	
	
	
	//inicializacion de juego
    jackpot <- 0
    opcion_juego <- 1
	
    Mientras opcion_juego <> 0 Hacer
		//INSERTAR UNA IMAGEN
		
        Escribir"+==========================================================================================================+"
		Escribir"|                                                                                                          |"
		Escribir"|                                                                                                          |"
        Escribir"|                              Saldo: ", saldo, "   Apuesta: ", valor_tiradas, "   Jackpot: ", jackpot,"                                    |"
		Escribir"|                                                                                                          |"
		Escribir"|                                                                                                          |"
		Escribir"+==========================================================================================================+"
        
        Si saldo < valor_tiradas Entonces
            Escribir "No tienes saldo suficiente. Fin del juego."
            opcion_juego <- 0
        SiNo
            Escribir "(1) Tirar - (0) Salir"
            Leer opcion_juego
            Mientras opcion_juego <> 1 Y opcion_juego <> 0 Hacer
                Escribir "Opción inválida."
                Leer opcion_juego
            FinMientras
            
            Si opcion_juego = 1 Entonces
                saldo <- saldo - valor_tiradas
                jackpot <- jackpot + (valor_tiradas*0.02)
                
                // Resultado final
                a <- Aleatorio(0,2)
                b <- Aleatorio(0,2)
                c <- Aleatorio(0,2)
                MostrarSimbolos(simbolos[a], simbolos[b], simbolos[c])
                
                // Premios
                Si a = b Y b = c Entonces
                    Si simbolos[a] = "CEREZA" Entonces
						Escribir"+----------------------------------------------------------------------------------------------------------+"
                        Escribir "** JACKPOT! Te llevaste ", jackpot, " créditos! **"
                        saldo <- saldo + jackpot
                        jackpot <- 0
                    SiNo
						Escribir"+----------------------------------------------------------------------------------------------------------+"
						Escribir"|                                                                                                          |"
                        Escribir"|                                          PREMIO MAYOR!                                                   |"
						Escribir"|                                                                                                          |"
						Escribir"+----------------------------------------------------------------------------------------------------------+"
                        saldo <- saldo + (valor_tiradas * 4)
                    FinSi
                SiNo
                    Si a = b O a = c O b = c Entonces
						Escribir"+----------------------------------------------------------------------------------------------------------+"
						Escribir"|                                                                                                          |"
                        Escribir"|                                          PREMIO MEDIANO!                                                 |"
						Escribir"|                                                                                                          |"
						Escribir"+----------------------------------------------------------------------------------------------------------+"
                        saldo <- saldo + Trunc(valor_tiradas / 2)
                    SiNo
						Escribir"+----------------------------------------------------------------------------------------------------------+"
						Escribir"|                                                                                                          |"
						Escribir"|                                          SIGUE JUGANDO...                                                |"
						Escribir"|                                                                                                          |"
						Escribir"+----------------------------------------------------------------------------------------------------------+"
                    FinSi
                FinSi
            FinSi
        FinSi
        Esperar Tecla// Reemplazar Esperar Tecla para mejor fluidez
        Limpiar Pantalla
    FinMientras
    Escribir "Juego terminado. Saldo final: ", saldo
    Escribir "Presione ENTER para volver al menú..."
    Leer Volver_al_menu // Permitir volver al menú
FinSubProceso












// ====================== SUBPROCESOS DE SIMBOLOS ======================
SubProceso MostrarSimbolos(simbolo1, simbolo2, simbolo3)
    Escribir"+----------------------------------------------------------------------------------------------------------+"
    Escribir "| ", ObtenerLinea1(simbolo1), "|", ObtenerLinea1(simbolo2), "|", ObtenerLinea1(simbolo3), " |"
    Escribir "| ", ObtenerLinea2(simbolo1), "|", ObtenerLinea2(simbolo2), "|", ObtenerLinea2(simbolo3), " |"
	Escribir "| ", ObtenerLinea3(simbolo1), "|", ObtenerLinea3(simbolo2), "|", ObtenerLinea3(simbolo3), " |"
	Escribir "| ", ObtenerLinea4(simbolo1), "|", ObtenerLinea4(simbolo2), "|", ObtenerLinea4(simbolo3), " |"
	Escribir "| ", ObtenerLinea5(simbolo1), "|", ObtenerLinea5(simbolo2), "|", ObtenerLinea5(simbolo3), " |"
	Escribir "| ", ObtenerLinea6(simbolo1), "|", ObtenerLinea6(simbolo2), "|", ObtenerLinea6(simbolo3), " |"
	Escribir "| ", ObtenerLinea7(simbolo1), "|", ObtenerLinea7(simbolo2), "|", ObtenerLinea7(simbolo3), " |"
	Escribir "| ", ObtenerLinea8(simbolo1), "|", ObtenerLinea8(simbolo2), "|", ObtenerLinea8(simbolo3), " |"
	Escribir "| ", ObtenerLinea9(simbolo1), "|", ObtenerLinea9(simbolo2), "|", ObtenerLinea9(simbolo3), " |"
	Escribir "| ", ObtenerLinea10(simbolo1), "|", ObtenerLinea10(simbolo2), "|", ObtenerLinea10(simbolo3), " |"
	Escribir "| ", ObtenerLinea11(simbolo1), "|", ObtenerLinea11(simbolo2), "|", ObtenerLinea11(simbolo3), " |"
	Escribir "| ", ObtenerLinea12(simbolo1), "|", ObtenerLinea12(simbolo2), "|", ObtenerLinea12(simbolo3), " |"
	
FinSubProceso

Funcion texto <- ObtenerLinea1(simbolo)
    Segun simbolo Hacer
        "CEREZA": texto <- "                    :??^:         "
        "SANDIA": texto <- "        ~7^.                      "
        "LIMON": texto <- "                                  "
    FinSegun
FinFuncion

Funcion texto <- ObtenerLinea2(simbolo)
    Segun simbolo Hacer
        "CEREZA": texto <- "                 :~?57^.          "
        "SANDIA": texto <- "     ~YPGP?!^.                    "
        "LIMON": texto <- "                                  "
    FinSegun
FinFuncion

Funcion texto <- ObtenerLinea3(simbolo)
    Segun simbolo Hacer
        "CEREZA": texto <- "               ^7?J?^.            "
        "SANDIA": texto <- "   ^JP5PPPP5J7:                   "
        "LIMON": texto <- "                                  "
    FinSegun
FinFuncion

Funcion texto <- ObtenerLinea4(simbolo)
    Segun simbolo Hacer 
        "CEREZA": texto <- "            :77~: Y!:....         "
        "SANDIA": texto <- "    ~Y5PBP555PP5YJ!^.             "
        "LIMON": texto <- "              ...::::::.. .. ..   "
    FinSegun
FinFuncion

Funcion texto <- ObtenerLinea5(simbolo)
    Segun simbolo Hacer
        "CEREZA": texto <- "            !?:  .J5Y5PPPY?.      "
        "SANDIA": texto <- "     ?P5PGP555555PP5Y7:           "
        "LIMON": texto <- "          .:::::::::::::::.:      "
    FinSegun
FinFuncion

Funcion texto <- ObtenerLinea6(simbolo)
    Segun simbolo Hacer
        "CEREZA": texto <- "      ..:::7?^. .?5PGGGPPPPG5.    "
        "SANDIA": texto <- "    ^!Y5PGPP555555555P55J!^.      "
        "LIMON": texto <- "         .::::::::::::::::^7~     "
    FinSegun
FinFuncion

Funcion texto <- ObtenerLinea7(simbolo)
    Segun simbolo Hacer
        "CEREZA": texto <- "   .?JY5PGG5Y!:?P5PPP555555GP^.   "
        "SANDIA": texto <- "       !Y55GBP555555555555PP5?:   "
        "LIMON": texto <- "        :::::::::::::::::::^.     "
    FinSegun
FinFuncion

Funcion texto <- ObtenerLinea8(simbolo)
    Segun simbolo Hacer
        "CEREZA": texto <- " .^J5P5555PPPPGP7555555555555PG~: "
        "SANDIA": texto <- "      ~!?5P55GB55GP5PP5PGPPBG5?:  "
        "LIMON": texto <- "       .^::::::::::::::::::.      "
    FinSegun 
FinFuncion

Funcion texto <- ObtenerLinea9(simbolo)
    Segun simbolo Hacer 
        "CEREZA": texto <- " ?P5555555555PGGP5555555555PGJ.   "
        "SANDIA": texto <- "       :!?Y5PP5PBP5BG5PBP5Y?:     "
        "LIMON": texto <- "       :::::::::::::::::::.       "
    FinSegun
FinFuncion

Funcion texto <- ObtenerLinea10(simbolo)
    Segun simbolo Hacer 
        "CEREZA": texto <- " !JP55555555555GGPPP55555PPGPJ~:  "
        "SANDIA": texto <- "           :^!7?JYY555YYJ?7^      "
        "LIMON": texto <- "       :::::::::::::::::.         "
    FinSegun
FinFuncion

Funcion texto <- ObtenerLinea11(simbolo)
    Segun simbolo Hacer 
        "CEREZA": texto <- " ^J55555555555PGY~7Y5PPPP5Y?:.    "
        "SANDIA": texto <- "               .::^^~^^::         "
        "LIMON": texto <- "        ::..::::::::::..          "
    FinSegun
FinFuncion

Funcion texto <- ObtenerLinea12(simbolo)
    Segun simbolo Hacer
        "CEREZA": texto <- "  :?5PPPPPPPPG5?~:  ::^^^:..      "
        "SANDIA": texto <- "                 ¨¨´¨¨¨           "
        "LIMON": texto <- "          : ; ... :.              "
    FinSegun
FinFuncion
