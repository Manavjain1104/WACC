# shellcheck disable=SC2068
java -jar wacc-25-compiler.jar $@

ecode=$?
if [ $ecode -ne 0 ]
then
    exit $ecode
fi

fname=$(basename "$1" .wacc)
arm-linux-gnueabi-gcc -o "${fname}" -mcpu=arm1176jzf-s -mtune=arm1176jzf-s "${fname}.s"
qemu-arm -L /usr/arm-linux-gnueabi/ "${fname}"
mv ${fname} build
exit $?