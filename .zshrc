cd ~
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="gallois"
plugins=(z)

source $ZSH/oh-my-zsh.sh

alias jrnl=" jrnl"
alias ssh="TERM=xterm-256color ssh"
alias dotfiles="git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"
alias lock="sh $HOME/.i3lock-script.sh"
alias uninstallorphans="sudo pacman -Rns $(pacman -Qtdq)"
alias osu="wine $HOME/games/osu/osu\!.exe"
alias doom="~/.emacs.d/bin/doom"
alias mem="sh ~/prog/scripts/memento_sub_retime.sh"
alias droidcamaudio="pacmd load-module module-alsa-source device=hw:Loopback,1,0"

cpanki(){
    cp "$HOME/.local/share/Anki2/User 1/collection.media/$1" .
}

emcs(){
    emacsclient $1 &
    disown
}

startjpynb(){
    cd $1
    nohup jupyter notebook --allow-root >.jpyerrors & echo $!>~/.jpypid
}
killjpynb(){
    kill -9 $(cat ~/.jpypid)
}

spectrogram(){
	sox $1 -n spectrogram
	sxiv spectrogram.png	
	rm spectrogram.png
}
jcar() {
  f=$1
  f2=${f%.*}
  trap "rm $f2.class" SIGINT SIGTERM
  javac $1
  java $f2
  rm $f2.class
}


eval
	(cat ~/.cache/wal/sequences &)
	setopt HIST_IGNORE_SPACE

export LC_ALL=en_US.UTF-8
# ja_JP

export PATH="$HOME/.local/bin:$PATH"

export DOTNET_CLI_TELEMETRY_OPTOUT=1
#export WINEPREFIX="$HOME/.wine"
export WINEPREFIX="$HOME/.wine2"
export WINEARCH=win32
export PATH=/opt/wine-osu/bin:$PATH
export PATH="$HOME/.cabal/bin:$PATH"

export VISUAL="emacsclient -c -a ''"
export EDITOR="emacsclient -c -a ''"

if [[-d "$HOME/.config/broot/"]]; then
    source $HOME/.config/broot/launcher/bash/br
fi

function vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# Completely clear the buffer. With this, everything that is not on screen
# is erased.
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'
fi

# With vterm_cmd you can execute Emacs commands directly from the shell.
# For example, vterm_cmd message "HI" will print "HI".
# To enable new commands, you have to customize Emacs's variable
# vterm-eval-cmds.
vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}

# This is to change the title of the buffer based on information provided by the
# shell. See, http://tldp.org/HOWTO/Xterm-Title-4.html, for the meaning of the
# various symbols.
autoload -U add-zsh-hook
add-zsh-hook -Uz chpwd (){ print -Pn "\e]2;%m:%2~\a" }

# Sync directory and host in the shell with Emacs's current directory.
# You may need to manually specify the hostname instead of $(hostname) in case
# $(hostname) does not return the correct string to connect to the server.
#
# The escape sequence "51;A" has also the role of identifying the end of the
# prompt
vterm_prompt_end() {
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)";
}

setopt PROMPT_SUBST
PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'
