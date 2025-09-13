libSupportFiles != find ./Support/ -type f -name '*.c*'

libSupportHeaders != find ./Support/ -type f -name '*.h'

libSupportHS != find ./Support/ -type f -name '*.hs'

libSupportLibs = `pkg-config --libs --cflags icu-uc icu-io` -lz -lspdlog -lfmt

libSupportCFLAGS = -Wextra -Wall -Wpedantic -shared -fPIC -std=c++23

libsupport-d.so: $(libSupportFiles) $(libSupportHeaders)
	g++ $(libSupportFiles) ${libSupportCFLAGS} ${libSupportLibs} -pg -g -o libsupport-d.so

libsupport.so: $(libSupportFiles) $(libSupportHeaders)
	g++ $(libSupportFiles) ${libSupportCFLAGS} ${libSupportLibs} -O4 -o libsupport.so

testFilesCPP != find ./Tests/ -type f -name '*.cpp'

testFilesHS != find ./Tests/ -type f -name '*.hs'

linkLibSupport = -lsupport -L.

testCFLAGS = -Wextra -Wall -Wpedantic -std=c++23 -lz -lgtest

tests-debug: libsupport-d.so ${testFilesCPP}
	g++ ${testFilesCPP} ${testCFLAGS} -L. -lsupport-d -g -pg -o tests-debug
	LD_LIBRARY_PATH=. time -v ./tests-debug
	gprof tests gmon.out > analisys.txt

tests: libsupport.so ${testFilesCPP}
	g++ ${testFilesCPP} ${testCFLAGS} ${linkLibSupport} -O4 -o tests
	LD_LIBRARY_PATH=. time -v ./tests

hsDefaultFlags = -no-keep-hi-files -no-keep-o-files -Wall -Wextra -static

tests-hs: libsupport.so ${libSupportHS} ${testFilesHS}
	ghc ${hsDefaultFlags} ${linkLibSupport} Tests/Main.hs -o tests-hs
	LD_LIBRARY_PATH=. time -v ./tests-hs

editorFilesHS != find ./Editor/ -type f -name '*.hs'

cddbFilesHS != find ./CDDB/ -type f -name '*.hs'

editorFiles = libsupport.so ${libSupportHS} ${cddbFilesHS} ${editorFilesHS}

editor: ${editorFiles}
	ghc ${hsDefaultFlags} ${linkLibSupport} -O4 Editor/Main.hs -o editor
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
