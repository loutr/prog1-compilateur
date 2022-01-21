#!/usr/bin/env bash
# Performs various tests to detect false negatives and
# false positives in the typing process

CALL_PATH="$(dirname "$0")"

PGOC="$CALL_PATH/../src/pgoc"
GO_EXEC="go run"
TYPING_FLAGS="--debug --type-only"

pgoc_exec () { $PGOC "$1" && gcc -no-pie "${1%.go}.s"; }

test_code () {
    total_count=0
    success_count=0
    
    for filetest in $(find "$CALL_PATH/$1" -name '*.go'); do
	filename=${filetest%".go"}
	pgoc_exec "$filetest" > comp.log 2>&1 || {
	    tput setaf 1; echo "FAILED - $filetest"
	    echo "compiling scheme responded:"; echo "-------------------"; tput setaf 7
	    cat comp.log
	    tput setaf 1; echo "-------------------"; tput setaf 7
	}
	./a.out > pgoc.log 2>&1
	$GO_EXEC "$filetest" > goc.log 2>&1
	if diff pgoc.log goc.log >/dev/null 2>&1; then
	    tput setaf 2; echo "    OK - $filetest"; tput setaf 7
	    ((success_count++))
	else
	    tput setaf 1; echo "FAILED - $filetest"
	    echo "produced output:"; echo "-------------------"; tput setaf 7
	    cat pgoc.log
	    tput setaf 1
	    echo -e "\nexpected output is:"; echo "-------------------"; tput setaf 7
	    cat goc.log
	    tput setaf 1; echo -e "\n-------------------"; tput setaf 7
	fi
	rm -f "${filename}.s"
	((total_count++))
    done
    echo "($(date)) SCORE FOR EXECUTIONS: $success_count/$total_count" >> pgoc_results
    rm pgoc.log goc.log comp.log a.out
}

test_typing () {
    total_count=0
    success_count=0
    
    for filetest in $(find "$CALL_PATH/$2" -name '*.go'); do
	filename=${filetest%".go"}
	$PGOC $TYPING_FLAGS "$filetest" > pgoc.log 2>&1
	ret=$?
	if [ $ret -eq "$3" ]; then
	    tput setaf 2; echo "    OK - $filetest"; tput setaf 7
	    ((success_count++))
	else
	    tput setaf 1; echo "FAILED - $filetest"
	    echo "compiler responded:"; echo "-------------------"; tput setaf 7
	    cat pgoc.log
	    tput setaf 1; echo "-------------------"; tput setaf 7
	fi
	rm -f "${filename}_ast.dot" "${filename}_tast.dot"
	((total_count++))
    done
    echo "($(date)) SCORE FOR $1 TESTS: $success_count/$total_count" >> pgoc_results
    rm pgoc.log
}

#test_typing GOOD good 0
#test_typing BAD bad 1
test_code good

cat pgoc_results
rm pgoc_results
