%declaracion de librerias

:-use_module(library(pce)).
:-use_module(library(pce_style_item)).
:-pce_image_directory('./imagenesvegetal').

% metodo principal para iniciar la interfaz grafica, declaracion de
% botones, labels, y la pocicion en pantalla.
inicio:-
	new(Menu, dialog('Grupo 9 INF-320', size(500,200))),
	new(L,label(nombre,'Sistema basado en reglas de producción para la clasificación de vegetales')),
	new(@texto,label(nombre,'Responder las siguientes preguntas, así determinaremos la especie del vegetal')),
	new(@respl,label(nombre,'')),
	new(Salir,button('Salir',and(message(Menu, destroy),message(Menu,free)))),
	new(@boton,button('Iniciar Preguntas',message(@prolog,botones))),

	% Interfaz para hallar el boton
	send(Menu,append(L)),new(@btncarrera,button('Diagnostico?')),
	send(Menu,display,L,point(30,20)),
	send(Menu,display,L,point(40,100)),
	send(Menu,display,@boton,point(140,45)),
	send(Menu,display,@texto,point(10,10)),
	send(Menu,display,Salir,point(270,45)),
	send(Menu,display,@respl,point(10,0)),
	send(Menu,open_centered).

% motor de inferencia
:-dynamic si/1,no/1.
preguntar(Problema):- new(Di,dialog('Identificación de especie')),
     new(L2,label(texto,'Responde las siguientes preguntas')),
     new(La,label(prob,Problema)),
     new(B1,button(si,and(message(Di,return,si)))),
     new(B2,button(no,and(message(Di,return,no)))),
         send(Di,append(L2)),
	 send(Di,append(La)),
	 send(Di,append(B1)),
	 send(Di,append(B2)),
	 send(Di,default_button,si),
	 send(Di,open_centered),get(Di,confirm,Answer),
	 write(Answer),send(Di,destroy),
	 ((Answer==si)->assert(si(Problema));
	 assert(no(Problema)),fail).

%limpiar para preguntar en arbol

pregunta(S):-(si(S)->true; (no(S)->false; preguntar(S))).
limpiar :- retract(si(_)),fail.
limpiar :- retract(no(_)),fail.
limpiar.

% eleccion de reglas de producción

botones :- lim,
	send(@boton,free),
	send(@btncarrera,free),
	fallas(Falla),
	send(@texto,selection(' ')),
	send(@respl,selection(Falla)),
        new(@boton,button('realizar test',message(@prolog,botones))),
	send(Menu,display,@boton,point(40,350)),
	send(Menu,display,@boton,point(240,350)),
	send(Menu,display,@btncarrera,point(20,50)),
limpiar.
lim :- send(@respl, selection('')).

muestra_imagen(Filename) :-
    new(I ,image(Filename)),
    !,
    new(B, bitmap(I)),
    new(P, picture),
    send(P, display, B),
    send(P, open).

%base de conocimiento reglas de produccion
%salidas para la respuesta final
fallas('El vegetal es Tipo hoja: El repollo, la lechuga y la espinaca son vegetales tipo hoja'):-planta,!,muestra_imagen('hoja.jpg').

fallas('El vegetal es Tipo Vástago: El espárrago, el apio y la coliflor son de tipo vástago'):-vastago,!,muestra_imagen('vastago.jpg').

fallas('El vegetal es Tipo Raíz: Como El Rábano, el nabo y la zanahoria'):-raiz,!,muestra_imagen('raiz.jpg').

fallas('El vegetal es Tipo Semilla: Los guisantes y las habas son vegetales de tipo semilla'):-semilla,!,muestra_imagen('semilla.jpg').

fallas('El vegetal es Tipo Flor: Ejemplos como El brócoli y la alcachofa.'):-flores,!,muestra_imagen('flor.jpg').

fallas('El vegetal es Tipo bulbo o bombillas: La cebolla, el ajo y el puerro.'):-bulbo,!,muestra_imagen('bombilla.jpg').
fallas('El vegetal es Tipo tubérculo: Las papas, el camote, la mandioca y la yuca son ejemplos de tubérculos.'):-tuberculo,!,muestra_imagen('tuberculo.jpg').

fallas('No existe un vegetal de ese tipo porfavor intente nuevamente').

% identificador de fallas para hallar el final

planta:-vegetal_planta,
	pregunta('tiene láminas o parece una lámina foliar(hoja de papel delgado)'),
	pregunta('debido a su forma considera que fue tomada del tallo de una planta, es decir forma estipular'),
	pregunta('considera que un peciolo o estípula es ligero al levantarlo como una hoja de papel'),
       (pregunta('el vegetal se compone de un tallo con varias laminas o hojas?');pregunta('solo es una única lamina o hoja la que esta observando')),
	pregunta('al observar el vegetal observa pequeños "hilos en su lámina" que forman estructura con un patrón de venación'),
	pregunta('no cuenta con un tallo diferenciado alto').


vastago:-vegetal_vastago,
	pregunta('parece una planta joven recién brotada con varias hojas'),
	pregunta('tiene un tallo referenciado delgado y debil (fácil de manipular/doblar)'),
(pregunta('su tallo es largo y sostiene varias hojas');pregunta('su tallo es corto pero del mismo tamaño de las hojas o superior')).

raiz:-vegetal_raiz,
	pregunta('en su forma no parece una hoja o un tallo de planta'),
	pregunta('al abrir el vegetal nota semillas o pequeños retículos formados en el interior del vegetal'),
	pregunta('tiene una cubierta o cáscara y un interior comestible'),
	pregunta('tiene un agujero o varios agujeros en la cascara').
semilla:-vegetal_semilla,
	pregunta('puede dar lugar a una nueva planta'),
	pregunta('tiene una cubierta seminal gruesa'),
	pregunta('parece legumbre').

flores:-vegetal_flores,
	pregunta('esta recubierta por un fruto que las protege'),
	pregunta('tiene estructura de flor'),
	pregunta('tiene pequeños hilos con punta redonda').
bulbo:-vegetal_bulbo,
	pregunta('cree justo debajo de la superficie del suelo'),
	pregunta('tiene un brote carnoso y frondoso por encima del suelo'),
	pregunta('sus hojas son carnosas y frondosas').

tuberculo:-vegetal_tuberculo,
	pregunta('contiene almidón que forma agua en su interior es decir es húmedo'),
	pregunta('parece un tipo de tallo engrosado'),
	pregunta('crecen bajo el suelo es decir es subterráneo'),
	pregunta('creció en contra de la gravedad es decir es de largo tamaño'),
	pregunta('los nudos que se observan fueron antes donde crecieron ramas u hojas').



%identificador de falla que dirige a las preguntas correspondientes


vegetal_vastago:-pregunta('¿Como vegetal parece tener un tallo con pequeñas hojas unidas?'),!.
vegetal_planta:-pregunta('¿Como vegetal observa laminas (hojas de papel) del mismo tipo?'),!.
vegetal_raiz:-pregunta('¿Como vegetal parece una fruta?').
vegetal_semilla:-pregunta('¿Como vegetal es dura al interior y exterior/cascara?').
vegetal_flores:-pregunta('¿Es llamativo y tiene una forma de pequeñas plantas en su interior?').
vegetal_bulbo:-pregunta('¿Tiene una estructura en forma de bombilla?').
vegetal_tuberculo:-pregunta('¿es notorio que tiene forma de tallo corto y con grosor?').






