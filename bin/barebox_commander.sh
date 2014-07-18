#!/bin/bash

barebox_tty=/dev/serial/by-id/usb-barebox_Scoter_Mitac_Mio_A701-if00-port0

function progressing()
{
	local filesize
	local progress
	local last_progress

	filesize=$1
	last_progress=0
	while read -d "" line; do
		echo "$line" >> /tmp/rjk_was_here
		if [ "${line::14}" = "Ymodem sectors" -a "${line: -1}" = "k" ]; then
			line=${line##*\/}
			line=${line::-1}
			progress=$(($line * 1024 * 20 / filesize))
			[ $last_progress != $progress ] && {
				 last_progress=$progress;
				echo -n "#"
			}
		fi
	done
	echo
}

function wait_for_tty_device()
{
	local again=10

	while [ $again -gt 0 ]; do
		if [ -r ${barebox_tty} ]; then
			stty -F ${barebox_tty} >/dev/null 2>/dev/null &&
				exec 3<> ${barebox_tty} && return 1
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
	local in_full
	local again
	TMOUT=1
	again=10
	in_full=""
	in=""

	while [ $again -gt 0 ]; do
		[ ${#in} -eq 0 ] && echo -n -e "\r\n" >&3
		in=""
		read -t 1 in <&3
		in_full="${in_full}${in}"
		if [ "${in_full: -9:9}" = "barebox:/" -o "${in_full: -10:9}" = "barebox:/" -o "${in_full: -11:9}" = "barebox:/" ]; then
			#echo "In_Full=${in_full}"
			return 1
		fi
		let again-=1
	done
	return 0
}

function upload_file()
{
	local filename
	local filesize
	filename="$1"
	filesize=$(stat -c "%s" ${filename})

	echo "loady" >&3
	sleep 1
	(sb --ymodem ${filename} <&3 >&3) |& progressing $filesize
	#sb --ymodem ${filename} <&3 >&3
}

wait_for_tty_device && exit 1
case "$1" in
	upload_file)
		echo "" >&3
		wait_for_prompt && exit 2
		shift
		file=${1:-/home/rj/mio_linux/kernel/arch/arm/boot/zImage}
		upload_file ${file}
		;;
	wait_prompt)
		echo "" >&3
		wait_for_prompt && exit 2
		;;
	command)
		echo "" >&3
		wait_for_prompt && exit 2
		shift
		echo "$@" >&3
		wait_for_prompt && exit 2
		;;
esac
exit 0
