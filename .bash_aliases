#!/bin/bash

# load thirukural script to shell
source ~/projects/.ThirukkuralAPI/src/kural.sh

# command to check weather
function wtr() {
    curl wttr.in/$1
}

# various aliases to quickly access important directory
alias cl="cd /home/veera/classes/s24"
alias uca="cd /home/veera/classes/s22/uca"
alias 365="cd /home/veera/classes/s22/digital-forensics"
alias 410="cd /home/veera/classes/s22/compilers"
alias 453="cd /home/veera/classes/s22/networking"
alias 220="cd /home/veera/classes/s22/220"
alias 325="cd /home/veera/classes/s23/hci"
alias 151="cd /home/veera/classes/s23/physics"
alias 373="cd /home/veera/classes/s23/graphics"
alias 305="cd /home/veera/classes/s23/writing"
alias 491="cd /home/veera/classes/s23/networking"
alias proj="cd /home/veera/projects"
alias t="cd /home/veera/projects/test" # alias to go straight into test dir
alias e="exit" # alias to quickly exit terminal
alias books="cd /home/veera/books"
alias lock="loginctl lock-session"
alias edlab="ssh vsivarajan@elnux.cs.umass.edu"
alias gs="git status" 
alias blog="cd /home/veera/projects/blog/local"
alias lard="du -hs */ | sort -hr | head" # view 10 largest dirs in current dir 
alias mr="make && make run" # make and run program
alias mrv="make && make runv" # make and run program with valgrind
alias car="cargo run"
alias cab="cargo build"
alias pap="cd /home/veera/books/papers/pl"
alias casm="gcc -O0 -fverbose-asm -S"
# quickly build stage 1 rust compiler while using existing std library
alias rcb="./x build --stage 1 library --keep-stage-std 1"
# shortened command for ~rustc +stage1~
alias rs="rustc +stage1"
# open a random wikipedia page on firefox
alias wiki="firefox https://en.wikipedia.org/wiki/Special:Random"

# alias ld='ls -d */' # list all directories

# create directory and cd into it
function mcd() {  
    mkdir $1
    cd $1
}

# change directory and list the current directory
function cdl() {   
    cd $1
    ls
}

# display last 3 directories in prompt
PROMPT_DIRTRIM=3 

# copy working directory address to clipboard 
function cwd() {
    pwd | xclip -selection clipboard
}

# log off command for KDE5 Plasma
function bye() {
    qdbus org.kde.ksmserver /KSMServer logout 0 0 0
}

# git add all files and commit 
function gac() {
    git add .
    git commit -m "$1"
}

# alias for adding dotfiles to git
alias dfiles='/usr/bin/git --git-dir=$HOME/.files/ --work-tree=$HOME'

# short command to add and commit all dot files
function dfg() {
    dfiles add /home/veera/.bash_aliases
    dfiles add /home/veera/.bashrc
    dfiles add /home/veera/.emacs.d/init.el
    dfiles commit -m "$1"
    dfiles push -u origin master
}

# function to send files from local to edlab
function sendfile() {
    scp -r $1 vsivarajan@elnux.cs.umass.edu:$2
}

# function to receive files from edlab
function getfile() {
    scp -r vsivarajan@elnux.cs.umass.edu:$1 /home/veera/$2
}

# quickly compile and execute C programs
function crun() {
    gcc $1
    ./a.out
}

# set external monitor brightness
function emb() {
    xrandr --output HDMI-1-0 --brightness $1
}

# open anything using desired programs and push it to background           
function open () {
    xdg-open $1
}

# display battery details
function bat () {
    upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep -E "state|to\ full|percentage"
}

# open a file in already running Emacs instance
function ff () {
    is_open=`ps -fe | grep "/usr/bin/emacs" | grep -vc grep`
    if [ $is_open -eq 0 ]; then # if emacs is not open already
        echo "Opening new Emacs instance"
        (emacs)
        (sleep 2.5)  # wait for emacs to load config file and server-start  
        (emacsclient -n $1)
    else
        emacsclient -n $1          
    fi
    wmctrl -a "Emacs"  # switch focus to Emacs window 
}

# split window vertically and open a file in the other window  
function rff () {
    emacsclient -e '(progn (split-right-and-switch))'
    ff $1
}

# split window horizontally and open a file in the other window  
function dff () {
    emacsclient -e '(progn (split-down-and-switch))'
    ff $1
}


# command to get the top 10 commands from history
function hiso () {
    history | awk '{print $4}' | sort | uniq -c | sort -rn | head
}

# list number of journal entries for each month
function dstat () {
    if [ $# -eq 0 ] # if no arguments 
    then
        file='/home/veera/diary.org'
    else
        file=$1
    fi
    count=$(wc "$file")
    temp=$(sed -n '/^* /p' "$file" | awk '{ print $2 }' | uniq -c | awk '{t=$1; $1=$2; $2=t; print;}')
    echo "$temp"
    total_entries=$(echo -n "$temp" | awk '{s+=$2} END {print s}')
    total_lines=$(echo -n "$count" | awk '{ print $1 }')
    total_words=$(echo -n "$count" | awk '{ print $2 }')
    avg_word_count=$(echo "$total_words / $total_entries" | bc)
    echo "----------------------------------------------"
    echo "Total: $total_entries entries"
    echo "       $total_lines lines"
    echo "       $total_words words"
    echo "                         "
    echo "On an average $avg_word_count words per entry."
    echo "----------------------------------------------"
}

# display total number of commits in a repo
function comc () {
    git log --format='%an' | sort | uniq -c | awk '{s+=$1} END {printf "%.0f\n", s}'
} 

# download audio from Youtube using youtube-dl
function dl {
    python3 $(which youtube-dl) -U --extract-audio --audio-format mp3 $1
}

# copy the rust code from clipboard into a file and execute it
function rr {
    output_file='/home/veera/projects/test/clipboard.rs'
    xclip -selection clipboard -o > $output_file
    echo -e "\nfn main() {}" >> $output_file
    rustc $output_file
    # .~/projects/test/clipboard
}

function music() {
    cd /home/veera/music/
    cmus *.mp3
}

function audio_length {
    ffmpeg -i $1 2>&1 | egrep "Duration" |  cut -d ' ' -f 4 | sed s/,//
}

function minfo() {
    for file in /home/veera/music/*.mp3
    do
        len=$(audio_length $file)
        echo -e $file $len
    done
}

export LD_LIBRARY_PATH=/usr/lib/gcc/x86_64-linux-gnu/10/
