editorFilesHS != find ./Editor/ -type f -name '*.hs'

cddbFilesHS != find ./CDDB/ -type f -name '*.hs'

editorFiles = ${cddbFilesHS} ${editorFilesHS}

hsDefaultFlags = -no-keep-hi-files -no-keep-o-files -Wall -Wextra -static

editor: ${editorFiles}
	ghc ${hsDefaultFlags} -O4 Editor/Main.hs -o editor
	strip editor

editor-prof: ${editorFiles}
	ghc ${hsDefaultFlags} -rtsopts -prof -fprof-auto -ticky -fdistinct-constructor-tables -O4 Editor/Main.hs -o editor-prof
	./editor-prof +RTS -l-augT -hc
	eventlog2html editor-prof.eventlog

hlint:
	hlint -v --report --with-group=generalise-for-conciseness --with-group=extra --with-group=teaching .
	open report.html

clean:
	rm -fv editor*
	rm -fv report.html
