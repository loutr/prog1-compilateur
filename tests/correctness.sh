#!/usr/bin/env bash
# Performs various tests to detect false negatives and
# false positives in the typing process

CALL_PATH="$(dirname "$0")"

GOC="$CALL_PATH/../src/pgoc --debug --type-only"

test_category () {
    total_count=0
    success_count=0
    
    for filetest in $(find "$CALL_PATH/$2" -name '*.go'); do
	filename=${filetest%".go"}
	$GOC "$filetest" > pgoc.log 2>&1
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
	rm -f "${filename}_ast.dot" "${filename}_tast.dot" "${filename}.s"
	((total_count++))
    done
    echo "($(date)) SCORE FOR $1 TESTS: $success_count/$total_count" >> pgoc_results
    rm pgoc.log
}

test_category GOOD good 0
test_category BAD bad 1

cat pgoc_results
rm pgoc_results
