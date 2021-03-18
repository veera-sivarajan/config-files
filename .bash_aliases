#!/bin/bash

# load thirukural script to shell
source ~/Projects/.ThirukkuralAPI/src/kural.sh

# command to check weather
function wtr() {
    curl wttr.in/$1
}

# various aliases to quickly access important directory
alias cl='cd /home/veera/Classes/Spring21'
alias 220='cd /home/veera/Classes/Spring21/220'
alias 311='cd /home/veera/Classes/Spring21/311'
alias proj='cd /home/veera/Projects'
alias t='cd /home/veera/Projects/Test' # alias to go straight into test dir
alias e='exit' # alias to quickly exit terminal
alias books='cd /home/veera/Books'
alias lock='loginctl lock-session'
alias edlab='ssh vsivarajan@elnux.cs.umass.edu'

# create directory and cd into it
function modr() {  
    mkdir $1
    cd $1
}

# change directory and list the current directory
function cs() {   
  cd $1
  ls
}

# display last 3 directories in prompt
PROMPT_DIRTRIM=3 

# copy working directory address to clipboard 
function cwd() {
  pwd | xclip -selection clipboard
}

# log off command for KDE5
function bye() {
  qdbus org.kde.ksmserver /KSMServer logout 0 0 0
}

# screen lock command for KDE5
# function lock() {
 # loginctl lock-session
# } 

# execute vtags from ~/Projects/ 
function vtags() {
  path=$(pwd)
  java -jar ~/Projects/vtags.jar $path/$1   
}

# compile and run java source code 
function j() {
  javac $1.java
  java $1
}

# git add all files and commit 
function gac() {
  git add .
  git commit -m "$1"
}

# alias for adding dotfiles to git
alias dfiles='/usr/bin/git --git-dir=$HOME/.files/ --work-tree=$HOME'

# shortened command for executing python3 programs quickly
function p() {
  python3 "$1"
}

# short command to add and commit all dot files
function dfg() {
  dfiles add .bash_aliases
  dfiles add .bashrc
  dfiles add .vimrc
  dfiles commit 
  dfiles push -u origin master
}

# git and commit Emacs init file
function edfg() {
    cd ~/.emacs.d
    dfiles add init.el
    dfiles commit
    dfiles push -u origin master
    cd
}

# script for jekyll installs Ruby Gems to ~/gems
export GEM_HOME="$HOME/gems"
export PATH="$HOME/gems/bin:$PATH"

# function to send files from local to edlab
function sendfile() {
    scp -r $1 vsivarajan@elnux.cs.umass.edu:$2
}

# function to receive files from edlab
function getfile() {
    scp -r vsivarajan@elnux.cs.umass.edu:$1 /home/veera/$2
}

# quickly ssh into edlab
 #function edlab() {
#   ssh vsivarajan@elnux.cs.umass.edu
#}

# quickly compile and execute C programs
function crun() {
    gcc $1
    ./a.out
}

# set external monitor brightness
function emb() {
    xrandr --output HDMI-1 --brightness $1
}

# open anything using desired programs and push it to background           
function open () {
  xdg-open $1
}

# make and run C program
function mr () {
    make
    make run
}

# display battery details
function bat () {
    upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep -E "state|to\ full|percentage"
}
    
# always open emacs in background
emacs () {
  /usr/bin/emacs "$@" &
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
    
