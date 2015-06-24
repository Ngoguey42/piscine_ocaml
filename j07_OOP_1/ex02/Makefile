# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: jaguillo <jaguillo@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2015/06/20 11:16:05 by jaguillo          #+#    #+#              #
#    Updated: 2015/06/24 16:17:33 by ngoguey          ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

NAME := a.out

OBJS_DIR := bin/
SRCS_DIR := ./

OBJS_DIR_E := $(shell echo $(OBJS_DIR) | sed 's/[\.\/]/\\&/g')
REPLACE_PATTERN := "s/[^[:space:]:]+/$(OBJS_DIR_E)&/g"

FLAGS := -I $(OBJS_DIR)
LIBS :=

SRCS := people.ml dalek.ml doctor.ml main.ml

BYT_OBJS := $(addprefix $(OBJS_DIR),$(SRCS:.ml=.cmo))
OPT_OBJS := $(addprefix $(OBJS_DIR),$(SRCS:.ml=.cmx))
INC_OBJS := $(addprefix $(OBJS_DIR),$(SRCS:.ml=.cmi))

all: $(NAME)
$(NAME): $(OBJS_DIR)$(NAME).byt $(OBJS_DIR)$(NAME).opt
	@ln -sf $(OBJS_DIR)$(NAME).opt $@
	@echo "\033[33m$@\033[0m"


byt: $(OBJS_DIR)$(NAME).byt
$(OBJS_DIR)$(NAME).byt: $(OBJS_DIR) $(BYT_OBJS)
	@ocamlc $(LIBS) -g -o $@ $(BYT_OBJS)
	@ln -sf $(OBJS_DIR)$(NAME).byt $(NAME)
	@echo "\033[33m$@\033[0m"
$(OBJS_DIR)%.cmo: $(SRCS_DIR)%.ml
	@ocamlc -g $(FLAGS) -o $@ -c $<
	@echo "\033[35m$<(byt)\033[0m"


opt: $(OBJS_DIR)$(NAME).opt
$(OBJS_DIR)$(NAME).opt: $(OBJS_DIR) $(OPT_OBJS)
	@ocamlopt $(LIBS) -o $@ $(OPT_OBJS)
	@ln -sf $(OBJS_DIR)$(NAME).opt $(NAME)
	@echo "\033[33m$@\033[0m"
$(OBJS_DIR)%.cmx: $(SRCS_DIR)%.ml
	@ocamlopt $(FLAGS) -o $@ -c $<
	@echo "\033[36m$<(opt)\033[0m"


.depend: Makefile $(SRCS_DIR)
	@ocamldep -I $(SRCS_DIR) $(SRCS) | \
		sed -E $(REPLACE_PATTERN) > .depend
	@echo "\033[33m.depend\033[0m"

$(OBJS_DIR)%.cmi: $(SRCS_DIR)%.mli
	@ocamlopt -I $(OBJS_DIR) -o $@ -c $<
	@echo "$<"

$(SRCS_DIR) $(OBJS_DIR):
	@mkdir -p $@

clean:
	@rm -f $(BYT_OBJS) $(OPT_OBJS) $(INC_OBJS)
fclean: clean
	@rm -f .depend $(NAME) $(OBJS_DIR)$(NAME).opt $(OBJS_DIR)$(NAME).byt
re: fclean all
.PHONY: all clean fclean re opt byt

-include .depend
