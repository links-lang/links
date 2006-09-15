Summary: a language for fast and type safe processing of XML documents.
Name: cduce
Version: 0.1.1
Release: 1
URL: http://www.cduce.org/
Source : http://www.cduce.org/download/%{name}-%{version}.tar.gz
License: MIT
Group: Development/Languages
BuildRoot: %{_tmppath}/%{name}-root
Requires: ocaml >= 3.06    
Requires: findlib >= 0.8   
Requires: pcre-ocaml 
Requires: ocamlnet  >= 0.96
Requires: pxp >= 1.1.9

%description

CDuce,  is a functional language for fast and type safe
processing  of XML documents.

A  complete  documentation,  reference  manual,  tutorial,
technical   articles  on  implementation  and  theoretical
issues, benchmarks, source code, mailing lists and  forums
can be found on the CDuce website: http://www.cduce.org.

%prep
rm -rf $RPM_BUILD_ROOT
%setup -q

%build
make cduce dtd2cduce webpages PHP=false NATIVE=true PXP_WLEX=false

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT%{_bindir}
mkdir -p $RPM_BUILD_ROOT%{_mandir}/man1

install -s -m 755 cduce $RPM_BUILD_ROOT%{_bindir}/cduce
install -s -m 755 dtd2cduce $RPM_BUILD_ROOT%{_bindir}/dtd2cduce
install -m 644 doc/cduce.1 $RPM_BUILD_ROOT%{_mandir}/man1/cduce.1

%post
cd %{_docdir}/%{name}-%{version}/www
ln -s ../img ./
cd ..
ln -s www/manual.html ./


%postun
rm -rf  %{_docdir}/%{name}-%{version}

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%doc README CHANGES web/www web/img

%{_bindir}/cduce
%{_bindir}/dtd2cduce
%{_mandir}/man1/cduce.1*

%changelog
* Mon Sep  1 2003 Giuseppe Castagna <Giuseppe.Castagna@ens.fr>
0.1.1
  * Various bug fixes  (expat might now work)
  * Sequencing operator   e1;e2  (equivalent to: let [] = e1 in e2)
  * Encoded references

* Fri Jul  4 2003 Giuseppe Castagna <Giuseppe.Castagna@ens.fr>
0.1.0
  * Support for XML Namespaces
  * Better support for expat; clean Makefile
  * Get rid of ;; and let fun in examples
  * Optional ; for record in attribute position (values and types/patterns)
  * Split --dump into --load and --save
  * Better handling of strings (specific node in AST)
  * Map, transform: tail-recursive implementation
  * Preliminary support for XML Schema
  * Various bug fixes



