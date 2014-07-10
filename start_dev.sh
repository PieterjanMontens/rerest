#!/bin/bash
# rerest development startup script 
# USE REBAR RELEASE FOR PRODUCTION
#
# Pieterjan Montens

###### CONFIG VARIABLES ######
##############################
## Cookies enable erlang nodes to communicate
COOKIE=$(<cookie)

## Just so you can run script from outside its own directory
DIR="$( cd "$( dirname "$0" )" && pwd )"

## Environment environment variable
ENV="development"

###### STARTUP ###############
##############################
PROMPT=yes
case $1 in
    fast)
        PROMPT=no
        ;;
    normal)
        ;;
    *)
        echo "Usage: start_dev.sh OPTION

Options:
    fast   : Run without possibility to cancel startup
    normal : Run with possibility to cancel startup procedure
"
        exit 0
        ;;        
esac


###### MAKE ##################
##############################
echo
echo \>\>\>\>\>\>\>\>\>\> Compiling Dependencies and source files
cd `dirname $0`
make deps compile

if [ "$PROMPT" = yes ]; then
    echo \>\>\>\>\>\>\>\>\>\> Press ctrl-c to cancel startup, any other key to continue
    read line
fi

echo
echo \>\>\>\>\>\>\>\>\>\> Testing 
rebar skip_deps=true eunit
 
if [ "$PROMPT" = yes ]; then
    echo \>\>\>\>\>\>\>\>\>\> Press ctrl-c to cancel startup, any other key to continue
    read line
fi

###### RUN ##################
##############################
erl -pa $DIR/ebin $DIR/deps/*/ebin \
    -sname rerest@$HOSTNAME \
    -setcookie $COOKIE \
    -smp auto \
    -rerest environment "$ENV" \
    -s rerest
