# include ../../Makefile.distrib
include ../../VERSION
JAVA_HOME=/usr/java/jre1.5.0
BUILD_DIR:=$(PWD:/doc/fomanual=)
CDUCE=$(BUILD_DIR)/cduce
FOP=fop-0.20.5/fop.sh 
#FOP=fop

all: manual.pdf tutorial.pdf 

# tutorial:
# 	(cd ../../web && $(CDUCE) ../doc/fomanual/xml2fo.cd  \
#             --arg tutorial.xml tutorial_fo.xml $(VERSION))

# manual:
# 	(cd ../../web && $(CDUCE) ../doc/fomanual/xml2fo.cd  \
#             --arg manual.xml manual_fo.xml $(VERSION))

# tutorial.fo: xml2fo.cd ../../web/tutorial.xml ../../web/tutorial/*.xml ../../web/siteTypes.cd
# 	(cd ../../web && ${CDUCE} ../doc/fomanual/xml2fo.cd \
# 		--arg tutorial.xml tutorial.fo ${VERSION})

# manual.fo: xml2fo.cd ../../web/manual.xml ../../web/manual/*.xml ../../web/siteTypes.cd
# 	(cd ../../web && ${CDUCE} ../doc/fomanual/xml2fo.cd  \
# 		--arg manual.xml manual.fo ${VERSION})

echo:
	@echo ${PWD}
	@echo ${BUILD_DIR}
	@echo ${CDUCE}

xml2fo.cdo: xml2fo.cd ../../web/siteTypes.cd
	${CDUCE} --compile -I ../../web xml2fo.cd

manual.fo: ../../web/manual.xml ../../web/manual/* xml2fo.cdo
	(cd ../../web && ${CDUCE} --run -I ../doc/fomanual xml2fo.cdo \
		--arg manual.xml manual.fo ${VERSION})

tutorial.fo: ../../web/tutorial.xml ../../web/tutorial/* xml2fo.cdo
	(cd ../../web && ${CDUCE} --run -I ../doc/fomanual xml2fo.cdo \
		--arg tutorial.xml tutorial.fo ${VERSION})

# tutorial_pdf: tutorial
# 	$(FOP) tutorial_fo.xml tutorial.pdf

# manual_pdf: manual
# 	$(FOP) manual_fo.xml manual.pdf 

install:
	scp manual.pdf tutorial.pdf cduce@di.ens.fr:public_html/papers/

.SUFFIXES: .fo .pdf

.fo.pdf:
	${FOP} $< $*.pdf

clean::
	rm -f *~
	rm -f xml2fo.cdo
	rm -f tutorial_fo.xml manual_fo.xml
	rm -f tutorial.fo manual.fo
	rm -f tutorial.pdf manual.pdf

#
# END
#
