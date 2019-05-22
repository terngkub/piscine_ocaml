NAME = ft_graphics.native
CC = ocamlbuild
LIB = -package graphics

all:
	$(CC) $(LIB) $(NAME)

clean:
	ocamlbuild -clean

fclean: clean
	rm -f $(NAME)

re: all

PHONY: all clean fclean re