% Aquí va el código.
mago(harry,sangre(mestiza),caracter([corajudo,amistoso,orgulloso,inteligente]),odiaria(slytherin)).
mago(draco,sangre(pura),caracter([orgulloso,inteligente]),odiaria(hufflepuff)).
mago(hermione,sangre(impura),caracter([responsable,orgulloso,inteligente]),odiaria(ninguna)).

mago(pepe,sangre(mestiza),caracter([corajudo,amistoso,orgulloso,inteligente]),odiaria(slytherin)).

casa(gryffindor).
casa(slytherin).
casa(hufflepuff).
casa(ravenclaw).

caracteristicasParaElegir(gryffindor,caracter([corajudo])).
caracteristicasParaElegir(slytherin,caracter([orgulloso,inteligente])).
caracteristicasParaElegir(ravenclaw,caracter([inteligente,responsable])).
caracteristicasParaElegir(hufflepuff,caracter([amistoso])).


sePermiteEntrarEn(Mago,slytherin):-
    mago(Mago,_,_,_),
    not(mago(Mago,sangre(impura),_,_)).

sePermiteEntrarEn(Mago,Casa):-
    mago(Mago,_,_,_),
    casa(Casa),
    Casa \= slytherin.

tieneElCaracterPara(Mago,Casa):-
    mago(Mago,_,caracter(Caracter),_),
    casa(Casa),
    caracteristicasParaElegir(Casa,caracter(CaracterNecesario)),
    perteneceAlaLista(CaracterNecesario,Caracter).

perteneceAlaLista([X|XS],Lista):-
    member(X,Lista),
    perteneceAlaLista(XS,Lista).
perteneceAlaLista([],_).
    

seleccionDeCasa(Mago,Casa):-
    mago(Mago,_,_,_),
    casa(Casa),
    not(mago(Mago,_,_,odiaria(Casa))),
    sePermiteEntrarEn(Mago,Casa),
    tieneElCaracterPara(Mago,Casa).

seleccionDeCasa(hermione, gryffindor).


%[harry,hermione,draco]
%[harry,pepe]
%[harry,draco,hermione]


cadenaDeAmistades([X|XS]):-
    mago(X,_,_,_),
    nth1(1,XS,Mago),
    mago(Mago,_,_,_),
    tieneElCaracterPara(X,hufflepuff),
    tieneElCaracterPara(Mago,hufflepuff),
    seleccionDeCasa(X,Casa),
    seleccionDeCasa(Mago,Casa),
    cadenaDeAmistades(XS).
cadenaDeAmistades([_|[]]).

esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

accion(harry, fueraDeCama, -50).
accion(harry, fue(bosque), -50).
accion(harry, fue(tercerPiso),-75).
accion(harry, buenaAccion(ganarAVoldemort), 60).

accion(hermione, fueraDeCama, -50).
accion(hermione, fue(tercerPiso),-75).
accion(hermione, fue(seccion_restringida_de_la_biblioteca),-10).
accion(hermione, buenaAccion(salvarASusAmigos),50).

accion(draco, fue(mazmorras), 0).
accion(harry, buenaAccion(ganarPartidaDeAjedrezMagico), 50).
accion(ron, buenaAccion(ganarPartidaDeAjedrezMagico),50).


buenAlumno(Mago):-
    accion(Mago,_,_),
    not(hizoMalaAccion(Mago)).

hizoMalaAccion(Mago):-
    accion(Mago,_,Puntos),
    Puntos<0.


accionRecurrente(Accion):-
    accion(Mago1,Accion,_),
    accion(Mago2,Accion,_),
    Mago1\=Mago2.

puntajeTotalDeCasa(Casa,Puntaje):-
    casa(Casa),
    findall(Puntaje, (esDe(Mago,Casa), sumarPuntosDeMago(Mago,Puntaje)), Lista),
    sumlist(Lista,Puntaje).

sumarPuntosDeMago(X, Puntaje):-
    findall(Puntos, accion(X,_,Puntos),Lista),
    sumlist(Lista,Puntaje).

casaGanadora(Casa):-
    casa(Casa),
    not(noLeGanoAEstaCasaEnPuntos(Casa,_)).

noLeGanoAEstaCasaEnPuntos(Casa1,Casa2):-
    puntajeTotalDeCasa(Casa1,Puntaje1),
    puntajeTotalDeCasa(Casa2,Puntaje2),
    Puntaje1<Puntaje2.