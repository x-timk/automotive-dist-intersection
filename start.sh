#!/bin/bash
# start.sh
#
# Default params
ENV_NODE="nodetest"
ENV_IP="$(ifconfig en0 | grep inet | grep -v inet6 | awk '{print $2}')"

GUI_JAR="erlDIM.jar"
# do not change
AUTH_COOKIE="guitar"

# calc possible ip addresses
IPADDRESSES="$(ifconfig | grep inet | grep -v inet6 | grep -v "127.0.0.1" | awk '{print $2}')"
# IPADDRESSES="$(ifconfig | grep inet | grep -v inet6 | awk '{print $2}' | tr '\n' ' ')" 
IPADDRESSES_N="${IPADDRESSES//$'\n'/ }"
echo "Your Ip Addresses are: $IPADDRESSES_N"

if [ "$#" -lt 1 ]; then
    echo "Illegal number of parameters"
    echo "Usage: ./start.sh -e <erlang-node-name> -i <ip-address>"
    echo "Example: ./start.sh -e envnode -i \"$IPADDRESSES\""
	echo "With this example you will start the environment on an erlang node called envnode@$IPADDRESSES"
    exit 1
fi

OPTIND=1
while getopts "e:i:h?:" option; do
	case "${option}" in
		e) ENV_NODE=$OPTARG;;
		i) ENV_IP=$OPTARG;;
		h) echo "Help page"
    	echo "Usage: ./start.sh -e <erlang-node-name> -i <ip-address>"
		echo "Example: ./start.sh -e envnode -i \"$IPADDRESSES\""
		echo "With this example you will start the environment on an erlang node called envnode@$IPADDRESSES"
		exit 0
	esac
done
shift $((OPTIND-1))


NODE="$ENV_NODE@$ENV_IP"
echo $NODE

echo "Compiling erlang sources..."
erl -make

echo "Starting env..."
erl -name $NODE -setcookie $AUTH_COOKIE -pa ebin -eval "dim_env:start()" -noshell -detached

echo "Starting GUI..."
java -jar $GUI_JAR

PID="$(ps aux | grep erl | grep envnode |  awk '{print $2}')"
echo "Killing pid $PID"
kill $PID