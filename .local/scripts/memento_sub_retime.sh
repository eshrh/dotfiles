#!/usr/bin/env bash
set -euo pipefail
set -x

extract() {
    echo "extract"
    baseVid="${1%.*}"
    baseSub="${2%.*}"
    echo "basevid: ${baseVid} \n basesub: ${baseSub}"
    ffmpeg -i "$1" -map 0:s:0 "/tmp/${baseVid}.srt" -y
    ffsubsync "/tmp/${baseVid}.srt" -i "$2" -o "${baseSub}_retimed.srt" &> /dev/null
    memento "$1" --sub-file="${baseSub}_retimed.srt"
}

audio() {
    baseVid=$(basename "$1")
    baseSub=$(basename "$2")
    ffsubsync "$1".srt -i "$2" -o "${baseSub}_retimed.srt"
    memento "$1" --sub-file="${baseSub}_retimed.srt"
}

while getopts 'av' flag; do
	case "${flag}" in
		a) audio "${@:2}" ;;
        v) extract "${@:2}" ;;
	esac
done

if (( $OPTIND == 1 )); then
   extract "$@"
fi

