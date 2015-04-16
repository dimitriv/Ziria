Currently, only BPSK and QPSK are tested in full TX/RX chain.
To enable 16QAM and 64QAM, more tunning of AGC is most likely required.

Also, test_real_rx.blk is currently disabled by being renamed to test_real_rx.disable
until we get a real WiFi snapshot because the current channel snapshot is obsolete.
