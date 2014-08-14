#/usr/bin/bash

if [[ "$1" == "test" ]]; then
  export TX_OUT='--TX-input=dummy --TX-dummy-samples=12800'
else
  export TX_OUT='--TX-output=ip'
fi

if [[ $# -eq 2 ]]; then
  export TX_RATE="--TX-PHY-rate=$2"
else
  export TX_RATE=''
fi 

if [[ "$3" == "debug" ]]; then
  export TX_DEBUG='--DEBUG=1'
else
  export TX_DEBUG='--DEBUG=0'
fi

./tx.out --MAC-type=TX-only $TX_DEBUG $TX_RATE $TX_OUT --TX-output=sora --TX-sora-gain=4096 --TX-sora-radio-id=0 --TX-sora-central-frequency=739



