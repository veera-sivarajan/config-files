#!/bin/bash
# command to check weather
function w() {
    curl wttr.in/$1
}

# various aliases to quickly access important directory
alias 187='cd /home/veera/Classes/Spring20/COMPSCI187/Projects'
alias class='cd /home/veera/Classes/Summer20'
alias 250='cd /home/veera/Classes/Summer20/COMPSCI250'
alias proj='cd /home/veera/Projects'

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
function lock() {
  loginctl lock-session
} 

# execute vtags from ~/Projects/ 
function vtags() {
  path=$(pwd)
  java -jar ~/Projects/vtags.jar $path/$1   
}

# execute jrun
function jrun() {
  javac $1.java
  java $1
}

# git add all files and commit 
function gac() {
  git add .
  git commit -m "$1"
}

# alias for adding dotfiles to git
alias dfiles='/usr/bin/git --git-dir=$HOME/.Files/ --work-tree=$HOME'
