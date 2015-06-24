# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: jaguillo <jaguillo@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2015/06/20 11:16:05 by jaguillo          #+#    #+#              #
#    Updated: 2015/06/24 19:07:24 by ngoguey          ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

NAME := a.out

OCB := ocamlc.opt -g
OCC := ocamlopt.opt
OCD := ocamldep

OBJS_DIR := bin/
SRCS_DIR := ./
OBJS_DIR_E := $(shell echo $(OBJS_DIR) | sed 's/[\.\/]/\\&/g')
REPLACE_PATTERN := "s/[^[:space:]:]+/$(OBJS_DIR_E)&/g"

FLAGS := -I $(OBJS_DIR)
LIBS :=

SRCS := army.ml people.ml dalek.ml doctor.ml galifrey.ml main.ml

BYT_OBJS := $(addprefix $(OBJS_DIR),$(SRCS:.ml=.cmo))
OPT_OBJS := $(addprefix $(OBJS_DIR),$(SRCS:.ml=.cmx))
INC_OBJS := $(addprefix $(OBJS_DIR),$(SRCS:.ml=.cmi))

all: $(NAME)
$(NAME): $(OBJS_DIR)$(NAME).byt $(OBJS_DIR)$(NAME).opt
	@ln -sf $(OBJS_DIR)$(NAME).opt $@
	@echo "\033[33m$@\033[0m"

# OCAML BYTECODE
byt: $(OBJS_DIR)$(NAME).byt
$(OBJS_DIR)$(NAME).byt: $(OBJS_DIR) $(BYT_OBJS)
	@$(OCB) $(LIBS) -o $@ $(BYT_OBJS)
	@ln -sf $(OBJS_DIR)$(NAME).byt $(NAME)
	@echo "\033[33m$@\033[0m"
$(OBJS_DIR)%.cmo: $(SRCS_DIR)%.ml
	@$(OCB) $(FLAGS) -o $@ -c $<
	@echo "\033[35m$<(byt)\033[0m"

# OCAML NATIVE
opt: $(OBJS_DIR)$(NAME).opt
$(OBJS_DIR)$(NAME).opt: $(OBJS_DIR) $(OPT_OBJS)
	@$(OCC) $(LIBS) -o $@ $(OPT_OBJS)
	@ln -sf $(OBJS_DIR)$(NAME).opt $(NAME)
	@echo "\033[33m$@\033[0m"
$(OBJS_DIR)%.cmx: $(SRCS_DIR)%.ml
	@$(OCC) $(FLAGS) -o $@ -c $<
	@echo "\033[36m$<(opt)\033[0m"

# OCAML INTERFACES
$(OBJS_DIR)%.cmi: $(SRCS_DIR)%.mli
	@$(OCC) -I $(OBJS_DIR) -o $@ -c $<
	@echo "$<"

i:
	@for i in $(SRCS); do						\
		echo "\033[33m$$i\033[0m";				\
		$(OCC) $(FLAGS) -i $$i 2>/dev/null ||	\
			(printf "\033[31m";					\
			$(OCC) $(FLAGS) -i $$i;				\
			printf "\033[0m");					\
	done


$(SRCS_DIR) $(OBJS_DIR):
	@mkdir -p $@

# MAKEFILE CLEANING
clean:
	@rm -f $(BYT_OBJS) $(OPT_OBJS) $(INC_OBJS)
fclean: clean
	@rm -f .depend $(NAME) $(OBJS_DIR)$(NAME).opt $(OBJS_DIR)$(NAME).byt
re: fclean all
.PHONY: all clean fclean re opt byt

# OCAML DEPENDANCY
.depend: Makefile $(SRCS_DIR)
	@$(OCD) -I $(SRCS_DIR) $(SRCS) | \
		sed -E $(REPLACE_PATTERN) > .depend
	@echo "\033[33m.depend\033[0m"
-include .depend
