#!/bin/bash

barebox_tty=/dev/ttyUSB0

function wait_for_tty_device()
{
	local again=10

	while [ $again -gt 0 ]; do
		if [ -r ${barebox_tty} ]; then
			exec 3<> ${barebox_tty}
			return 1
		fi
		let again-=1
		sleep 1
	done
	return 0
}

# Waits for barebox prompt
# Returns 1 if prompt found, 0 overwise
function wait_for_prompt()
{
	local in
	local again
	TMOUT=1
	again=10

	while [ $again -gt 0 ]; do
		echo "" >&3
		read in <&3
		[ ${#in} -gt 0 ] && return 1
		let again-=1
	done
	return 0
}

function upload_file()
{
	filename="$1"

	echo "loady" >&3
	sb --ymodem ${filename} <&3 >&3
}

wait_for_tty_device && exit 1
case "$1" in
	upload_file)
		wait_for_prompt && exit 2
		shift
		file=${1:-/home/rj/mio_linux/kernel/arch/arm/boot/zImage}
		upload_file ${file}
		;;
	wait_prompt)
		wait_for_prompt && exit 2
		;;
	command)
		wait_for_prompt && exit 2
		shift
		echo "$@" >&3
		;;
esac
exit 0
