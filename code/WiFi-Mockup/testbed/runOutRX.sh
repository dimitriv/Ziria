#/usr/bin/bash

if [[ "$1" == "test" ]]; then
  export RX_OUT='--RX-output=dummy'
else
  export RX_OUT='--RX-output=ip --TX-PC=10.190.102.72'
fi

if [[ "$2" == "debug" ]]; then
  export RX_DEBUG='--DEBUG=1'
else
  export RX_DEBUG='--DEBUG=0'
fi

./rx.out --MAC-type=RX-only $RX_DEBUG --RX-input=sora $RX_OUT --RX-sora-radio-id=1 --RX-sora-central-frequency=739
