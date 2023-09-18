##
## EPITECH PROJECT, 2023
## glados
## File description:
## Makefile
##

STACK	:=	stack

SRC		:=	app/Main.hs	\

NAME	:=	glados

all:		$(NAME)

$(NAME):	$(SRC)
	$(STACK) build
	cp -f "`$(STACK) path --local-install-root`/bin/$(NAME)-exe" $(NAME)

clean:
	$(STACK) clean

fclean: clean
	$(STACK) clean --full
	$(STACK) purge
	$(RM) "$(NAME)"

re:		fclean
	$(MAKE)

test:
	$(STACK) test

norm:
	banana.sh .

.PHONY:		all clean fclean re test norm
