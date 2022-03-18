record() {
    # this section is a heavily modified version of the linux audio script written by salamander on qm's animecards.
    local -r recordingToggle="/tmp/ffmpeg-recording-audio"

    if [[ ! -f /tmp/ffmpeg-recording-audio ]]; then
        local -r audioFile=$(mktemp "/tmp/ffmpeg-recording.XXXXXX.$AUDIO_FORMAT")
        echo "$audioFile" >"$recordingToggle"

        if [ "$OUTPUT_MONITOR" == "" ]; then
            local -r output=$(pactl info | grep 'Default Sink' | awk '{print $NF ".monitor"}')
        else
            local -r output="$OUTPUT_MONITOR"
        fi

        ffmpeg -nostdin \
            -y \
            -loglevel error \
            -f pulse \
            -i "$output" \
            -ac 2 \
            -af 'silenceremove=1:0:-50dB' \
            -ab $AUDIO_BITRATE \
            "$audioFile" 1>/dev/null &
	    echo "$!" >> "$recordingToggle"

        if [[ "$LANG" == en* ]]; then
            notify-send --hint=int:transient:1 -t 500 -u normal "Recording started..."
        fi
        if [[ "$LANG" == ja* ]]; then
            notify-send --hint=int:transient:1 -t 500 -u normal "録音しています..."
        fi
    else
        local audioFile="$(sed -n "1p" "$recordingToggle")"
        local pid="$(sed -n "2p" "$recordingToggle")"

        rm "$recordingToggle"
        kill -15 "$pid"

	while [ $(du $audioFile | awk '{ print $1 }') -eq 0 ]; do
		true
	done
        cp "${audioFile}" "$1"

        if [[ "$LANG" == en* ]]; then
            notify-send --hint=int:transient:1 -t 500 -u normal "Recording added"
        fi
        if [[ "$LANG" == ja* ]]; then
            notify-send --hint=int:transient:1 -t 500 -u normal "録音付けました"
        fi
    fi
}

record "$1"
