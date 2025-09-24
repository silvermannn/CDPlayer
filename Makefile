libsupport.so:
	make -C Support libsupport.so
	mv Support/libsupport.so libsupport.so

editorFilesHS != find ./Editor/ -type f -name '*.hs'

cddbFilesHS != find ./CDDB/ -type f -name '*.hs'

editorFiles = libsupport.so ${libSupportHS} ${cddbFilesHS} ${editorFilesHS}

hsDefaultFlags = -no-keep-hi-files -no-keep-o-files -Wall -Wextra -static

editor: ${editorFiles}
	ghc ${hsDefaultFlags} ${linkLibSupport} -lsupport -L. -O4 Editor/Main.hs -o editor
	strip editor

editor-prof: ${editorFiles}
	ghc ${hsDefaultFlags} ${linkLibSupport} -rtsopts -prof -fprof-auto -ticky -fdistinct-constructor-tables -O4 Editor/Main.hs -o editor-prof
	./editor-prof +RTS -l-augT -hc
	eventlog2html editor-prof.eventlog

hlint:
	hlint -v --report --with-group=generalise-for-conciseness --with-group=extra --with-group=teaching .
	open report.html

clean:
	rm -fv libsupport*.so
	rm -fv test*
	rm -fv editor*
	rm -fv analisys*.txt
	rm -fv *.eventlog
	rm -fv gmon.out
	rm -fv report.html
	rm -fv *.dot
	rm -fv *.svg
	rm -fv *.log
