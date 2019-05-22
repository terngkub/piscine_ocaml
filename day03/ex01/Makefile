NAME = gardening.native
CC = ocamlbuild
LIB = -package graphics

all: $(NAME)

$(NAME): gardening.ml
	$(CC) $(LIB) $(NAME)

clean:
	ocamlbuild -clean

fclean: clean
	rm -f $(NAME)

re: fclean all

PHONY: all clean fclean re