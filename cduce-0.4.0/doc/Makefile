# MAN = cduce.1 dtd2cduce.1 cduce_validate.1 cdo2ml.1 cduce_mktop.1
INSTALL_MAN = cduce.1 cduce_mktop.1 dtd2cduce.1
MAN = $(INSTALL_MAN)

all: $(MAN)

.SUFFIXES: .1 .docbook

.docbook.1:
	docbook-to-man $< > $@

install:
	@echo "Install man pages"
	$(HIDE)$(INSTALL) -m644 $(INSTALL_MAN) $(MANDIR)/man1/
