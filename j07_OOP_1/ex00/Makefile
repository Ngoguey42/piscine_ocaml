# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: jaguillo <jaguillo@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2015/06/20 11:16:05 by jaguillo          #+#    #+#              #
#    Updated: 2015/06/24 12:27:15 by ngoguey          ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

NAME := a.out

OBJS_DIR := bin
SRCS_DIR := .
SRCS_DIR_E := $(shell echo $SRCS_DIR | sed 's/[\.\/]/\\&/g')

FLAGS := -I $(OBJS_DIR)
LIBS := 

SRCS := people.ml main.ml

BYT_OBJS := $(addprefix $(OBJS_DIR)/,$(SRCS:.ml=.cmo))
OPT_OBJS := $(addprefix $(OBJS_DIR)/,$(SRCS:.ml=.cmx))
INC_OBJS := $(addprefix $(OBJS_DIR)/,$(SRCS:.ml=.cmi))

all: $(OBJS_DIR) $(NAME)

$(NAME): $(OBJS_DIR)/$(NAME).byt $(OBJS_DIR)/$(NAME).opt
	@ln -sf $(OBJS_DIR)/$(NAME).opt $@
	@echo "Done"

byt: $(OBJS_DIR)/$(NAME).byt
# $(OBJS_DIR)/$(NAME).byt: $(INC_OBJS) $(BYT_OBJS)
$(OBJS_DIR)/$(NAME).byt: $(BYT_OBJS)
	@ocamlc $(LIBS) -g -o $@ $(BYT_OBJS)
	@ln -sf $(OBJS_DIR)/$(NAME).byt $(NAME)
	@echo "$@"

opt: $(OBJS_DIR)/$(NAME).opt
# $(OBJS_DIR)/$(NAME).opt: $(INC_OBJS) $(OPT_OBJS)
$(OBJS_DIR)/$(NAME).opt: $(OPT_OBJS)
	@ocamlopt $(LIBS) -o $@ $(OPT_OBJS)
	@ln -sf $(OBJS_DIR)/$(NAME).opt $(NAME)
	@echo "$@"

.depend: Makefile
	@ocamldep -I $(SRCS_DIR) $(addprefix $(SRCS_DIR)/,$(SRCS)) | \
		sed -E 's/$(SRCS_DIR_E)/$(OBJS_DIR)/g' > .depend

$(OBJS_DIR)/%.cmo: $(SRCS_DIR)/%.ml $(OBJS_DIR)/%.cmi
	@ocamlc -g $(FLAGS) -o $@ -c $<
	@echo "$<"

$(OBJS_DIR)/%.cmx: $(SRCS_DIR)/%.ml $(OBJS_DIR)/%.cmi
	@ocamlopt $(FLAGS) -o $@ -c $<
	@echo "$<"

 $(OBJS_DIR)/%.cmo: $(SRCS_DIR)/%.ml
	@ocamlc -g $(FLAGS) -o $@ -c $<
	@echo "$<"

$(OBJS_DIR)/%.cmx: $(SRCS_DIR)/%.ml
	@ocamlopt $(FLAGS) -o $@ -c $<
	@echo "$<"

$(OBJS_DIR)/%.cmi: $(SRCS_DIR)/%.mli
	@ocamlopt -I $(OBJS_DIR) -o $@ -c $<
	@echo "$<"

$(OBJS_DIR):
	@mkdir -p $@

clean:
# @rm -f $(BYT_OBJS) $(OPT_OBJS)
	@rm -f $(BYT_OBJS) $(OPT_OBJS) $(INC_OBJS)

fclean: clean
	@rm -f $(NAME) $(OBJS_DIR)/$(NAME).opt $(OBJS_DIR)/$(NAME).byt

re: fclean all

.PHONY: all clean fclean re

-include .depend
