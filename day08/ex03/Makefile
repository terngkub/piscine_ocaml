NAME = reaction

SOURCES = atom.ml molecule.ml alkane.ml reaction.ml main.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep
CAMLFIND = ocamlfind

LIBS = 
WITHGRAPHICS = -package Graphics

all: .depend $(NAME)

$(NAME): $(NAME).opt $(NAME).byt
	ln -fs $(NAME).opt $(NAME)

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

$(NAME).byt: $(OBJS)
	ocamlfind $(CAMLC) -o $(NAME).byt -linkpkg $(LIBS) $(OBJS)

$(NAME).opt: $(OPTOBJS)
	ocamlfind $(CAMLOPT) -o $(NAME).opt -linkpkg $(LIBS) $(OPTOBJS)

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLFIND) $(CAMLC) -c $< $(LIBS)

.mli.cmi:
	$(CAMLFIND) $(CAMLC) -c $< $(LIBS)

.ml.cmx:
	$(CAMLFIND) $(CAMLOPT) -c $< $(LIBS)

clean:
	rm -f *.cm[iox] *.o .depend

fclean: clean
	rm -f $(NAME) $(NAME).opt $(NAME).byt

re: fclean all

.depend:
	$(CAMLDEP) $(SOURCES) > .depend

.PHONY: all re fclean clean

include .depend
