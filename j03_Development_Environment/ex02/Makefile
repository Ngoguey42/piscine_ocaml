#******************************************************************************#
#                                                                              #
#                                                         :::      ::::::::    #
#    Makefile                                           :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: ngoguey <ngoguey@student.42.fr>            +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2015/06/18 13:44:39 by ngoguey           #+#    #+#              #
#    Updated: 2015/06/18 16:50:48 by ngoguey          ###   ########.fr        #
#                                                                              #
#******************************************************************************#

NAME = cipher

SOURCES = uncipher.ml cipher.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt
CAMLDEP = ocamldep

LIBS = graphics.cma -cclib -lGraphics
INCLUDE = -I $(OBJPATH)

OBJPATH = obj
SRCPATH = .

all: depend $(NAME)

$(NAME): opt byt
	ln -fs $(OBJPATH)/$(NAME).opt $(NAME)

opt: $(OBJPATH)/$(NAME).opt
byt: $(OBJPATH)/$(NAME).byt

SRC = $(addprefix $(SRCPATH)/,$(SOURCES))

OBJS = $(SRC:$(SRCPATH)/%.ml=$(OBJPATH)/%.cmo)
OPTOBJS = $(SRC:$(SRCPATH)/%.ml=$(OBJPATH)/%.cmx)

$(OBJPATH)/$(NAME).byt: $(OBJS)
	$(CAMLC) -o $(OBJPATH)/$(NAME).byt $(LIBS) $(OBJS)

$(OBJPATH)/$(NAME).opt: $(OPTOBJS)
	$(CAMLOPT) -o $(OBJPATH)/$(NAME).opt $(LIBS:.cma=.cmxa) $(OPTOBJS)

$(OBJS): $(OBJPATH)/%.cmo : $(SRCPATH)/%.ml
	$(CAMLC) -o $@ $(INCLUDE) -c $<

$(OPTOBJS): $(OBJPATH)/%.cmx : $(SRCPATH)/%.ml
	$(CAMLOPT) -o $@ $(INCLUDE) -c $<

clean:
	rm -f $(OBJPATH)/*.cm[iox] 
	rm -f $(OBJPATH)/$(NAME).o

fclean: clean
	rm -rf $(OBJPATH)
	rm -f $(NAME)
	rm -f .depend

depend: .depend
	mkdir -p $(OBJPATH)
	$(CAMLDEP) $(SOURCES) >.depend

re: fclean all

$(shell touch .depend)
include .depend
