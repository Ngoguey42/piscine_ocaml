#******************************************************************************#
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2015/06/24 13:50:49 by ngoguey           #+#    #+#              #
#    Updated: 2015/06/24 13:50:50 by ngoguey          ###   ########.fr        #
#                                                                              #
#******************************************************************************#

NAME := a.out

OBJS_DIR := bin/
SRCS_DIR := ./
OBJS_DIR_E := $(shell echo $(OBJS_DIR) | sed 's/[\.\/]/\\&/g')
SRCS_DIR_E := $(shell echo $(SRCS_DIR) | sed 's/[\.\/]/\\&/g')

FLAGS := -I $(OBJS_DIR)
LIBS := 

SRCS := people.ml main.ml

BYT_OBJS := $(addprefix $(OBJS_DIR),$(SRCS:.ml=.cmo))
OPT_OBJS := $(addprefix $(OBJS_DIR),$(SRCS:.ml=.cmx))
INC_OBJS := $(addprefix $(OBJS_DIR),$(SRCS:.ml=.cmi))

all: $(OBJS_DIR) $(NAME)

$(NAME): $(OBJS_DIR)$(NAME).byt $(OBJS_DIR)$(NAME).opt
	@ln -sf $(OBJS_DIR)$(NAME).opt $@
	@echo "\033[33m$@\033[0m"

# BYT COMPILATION
byt: $(OBJS_DIR)$(NAME).byt
$(OBJS_DIR)$(NAME).byt: $(BYT_OBJS)
	@ocamlc $(LIBS) -g -o $@ $(BYT_OBJS)
	@ln -sf $(OBJS_DIR)$(NAME).byt $(NAME)
	@echo "\033[33m$@\033[0m"
$(OBJS_DIR)%.cmo: $(SRCS_DIR)%.ml
	@ocamlc -g $(FLAGS) -o $@ -c $<
	@echo "\033[35m$<(byt)\033[0m"

# OPT COMPILATION
opt: $(OBJS_DIR)$(NAME).opt
$(OBJS_DIR)$(NAME).opt: $(OPT_OBJS)
	@ocamlopt $(LIBS) -o $@ $(OPT_OBJS)
	@ln -sf $(OBJS_DIR)$(NAME).opt $(NAME)
	@echo "\033[33m$@\033[0m"
$(OBJS_DIR)%.cmx: $(SRCS_DIR)%.ml
	@ocamlopt $(FLAGS) -o $@ -c $<
	@echo "\033[36m$<(opt)\033[0m"

# CMI COMPILATION
$(OBJS_DIR)%.cmi: $(SRCS_DIR)%.mli
	@ocamlopt -I $(OBJS_DIR) -o $@ -c $<
	@echo "$<"

$(OBJS_DIR):
	@mkdir -p $@

clean:
	@rm -f $(BYT_OBJS) $(OPT_OBJS) $(INC_OBJS)
fclean: clean
	@rm -f .depend
	@rm -f $(NAME) $(OBJS_DIR)$(NAME).opt $(OBJS_DIR)$(NAME).byt
re: fclean all
.PHONY: all clean fclean re byt opt

# DEP CREATION
.depend: Makefile
	@ocamldep -I $(SRCS_DIR) $(addprefix $(SRCS_DIR),$(SRCS)) | \
		sed -E 's/$(SRCS_DIR_E)/$(OBJS_DIR_E)/g' > .depend

-include .depend
