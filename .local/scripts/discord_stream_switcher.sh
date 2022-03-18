output=$(pacmd list-source-outputs |
	tr '\n' '\r' |
	perl -pe 's/ *index: ([0-9]+).+?application\.name = "([^\r]+)"\r.+?(?=index:|$)/\2:\1\r/g' |
	tr '\r' '\n')

if [ $(echo $output | grep -c "WEBRTC VoiceEngine") -eq 0 ] 
then
       exit 0
fi

discord=$(pactl list source-outputs | grep -B23 "Discord" | egrep "Source Output #" | egrep -o "[0-9]+")

mic=$(pactl list sources | grep -E -B2 "echocancel$" | egrep "Source #" | egrep -o "[0-9]+")
monitor=$(pactl list sources | grep -E -B2 "alsa_output.pci-0000_2f_00.4.analog-stereo.monitor$" | egrep "Source #" | egrep -o "[0-9]+")

mic(){
	pactl move-source-output $discord $mic
}
desktop(){
	pactl move-source-output $discord $monitor
}

while getopts 'md' flag; do
	case "${flag}" in
		m) mic ;;
		d) desktop ;;
	esac
done
