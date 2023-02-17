#!bin/bash

fname=$(basename "$1" .wacc)

[ ! -e ${fname}.s ] && exit 1
arm-linux-gnueabi-gcc -o "${fname}" -mcpu=arm1176jzf-s -mtune=arm1176jzf-s "${fname}.s"
qemu-arm -L /usr/arm-linux-gnueabi/ "${fname}"
