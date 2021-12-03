#!/usr/bin/env bash
# Performs various tests to detect false negatives and
# false positives in the typing process

GOC="../src/pgoc --debug --type-only"

GOOD=./good
BAD=./bad

test_category () {
    total_count=0
    success_count=0
    
    for filetest in $2/*.go; do
	$GOC "$filetest" > pgoc.log 2>&1
	ret=$?
	if [ $ret -eq "$3" ]; then
	    tput setaf 2; echo "    OK - $filetest"; tput setaf 7
	    ((success_count++))
	else
	    tput setaf 1; echo "FAILED - $filetest"
	    echo "compiler responded:"; tput setaf 7
	    cat pgoc.log
	fi
	((total_count++))
    done
    echo "($(date)) SCORE FOR $1 TESTS: $success_count/$total_count" >> pgoc_results
    rm pgoc.log
}

test_category GOOD $GOOD 0
test_category BAD $BAD 1

cat pgoc_results
rm pgoc_results
