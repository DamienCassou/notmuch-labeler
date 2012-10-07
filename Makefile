# package.el multi-file package install

# These are the variables that are specific to the package
NAME=notmuch-labeler
VERSION=0.1
DOC="Improves notmuch way of displaying labels through fonts, pictures, and hyperlinks."
REQUIREMENTS=requirements.txt
package_parts=*.el README.md COPYING AUTHORS resources

# Everything beyond here should be generic
PACKAGE=$(NAME)-$(VERSION)
TARBALL=$(PACKAGE).tar

all: tarball

clean:
	rm -rf .elpa
	rm -rf $(TARBALL)
	rm -rf $(PACKAGE)
	rm -rf $(NAME)-pkg.el

tarball: $(TARBALL)

$(TARBALL): $(PACKAGE) $(PACKAGE)/$(NAME)-pkg.el
	tar cf $@ $<

$(PACKAGE): $(package_parts)
	mkdir $@
	cp -R $(package_parts) $@

$(PACKAGE)/$(NAME)-pkg.el:
	echo "(define-package \"$(NAME)\" \"$(VERSION)\" \"$(DOC)\" `cat $(REQUIREMENTS)`)" > $@

# End
