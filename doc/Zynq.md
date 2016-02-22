# Using Ziria on Zynq

Run Ziria compiler on x86 Linux machine. For instruction on how to run Ziria on Linux, consult Linux.md

To run Ziria on Zynq, a two stage process is needed:
1) Generate the C file with Ziria compiler
2) Move the C file to Zynq, compile and run with gcc.

In code/arm_neon, there are some test codes with a Makefile.host which generates C code and copies them over to Zynq.


# How to setup Zynq before running Ziria

The current code only supports Zynq and AD-FMCOMMS SDR boards. It is supported by ADI through libiio API. AD-FMCOMMS SDR package ships with an SD-card which has proper FPGA images, Linaro OS and Libiio pre-installed. For more detailed instructions on how to use the SD-card, see: https://wiki.analog.com/resources/tools-software/linux-software/zynq_images







