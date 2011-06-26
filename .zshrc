# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory autocd extendedglob
unsetopt beep nomatch notify
bindkey -v
 
export ZSH=$HOME/.oh-my-zsh
export ZSH_THEME='pmcgee'

source $ZSH/oh-my-zsh.sh

bindkey '^u' history-beginning-search-backward
bindkey '^p' history-beginning-search-forward
bindkey '^J' push-line

zstyle ':completion:*' completer _expand _complete _match _prefix _approximate _list
zstyle ':completion:*' format $'%{\e[0;33m%}completing %B%d%b%{\e[0m%}'
zstyle ':completion:*:messages' format '%d'
zstyle ':completion:*:warnings' format $'%{\e[4;1;31m%}No matches for:%{\e[0m%} %d'
zstyle ":completion:*:descriptions" format $'%{\e[0;33m%}%d:%{\e[0m%}'
zstyle ":completion:*:corrections" format $'%{\e[0;31m%}%d (errors: %e)%}'

LS_COLORS='no=00:fi=00:di=1;37:ln=01;36:pi=40;33:so=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:ex=01;32:*.cmd=01;32:*.exe=01;32:*.com=01;32:*.btm=01;32:*.bat=01;32:*.sh=01;32:*.csh=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.bz=01;31:*.tz=01;31:*.rpm=01;31:*.cpio=01;31:*.jpg=01;35:*.gif=01;35:*.bmp=01;35:*.xbm=01;35:*.xpm=01;35:*.png=01;35:*.tif=01;35:'
export LS_COLORS

zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'
zstyle ':completion:*:kill:*' insert-ids single
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always

LISTMAX=0

autoload -Uz compinit
compinit

setopt correctall
setopt sharehistory
setopt nocheckjobs             # don't warn me about bg processes when exiting
setopt nohup                   # and don't kill them, either
setopt listpacked              # compact completion lists
setopt dvorak                  # with spelling correction, assume dvorak kb
setopt completeinword          # not just at the end
setopt correct                 # spelling correction
setopt cdablevars

# End of lines added by compinstall

alias spec='rspec --color'
alias cower='cower -cf -t $HOME/builds'
alias aurget='aurget --noedit --deps --discard --noconfirm'
alias ps='psgrep'
alias mplayer='mplayer -vo xv'
alias ssh='TERM=xterm; ssh'
alias slurpy='slurpy --save-to=/home/sean/builds'
alias vim='vim'
alias df='cdf'
alias ls='ls --color'
alias ll='ls -lh'
alias aoeu='xmodmap ~/.dvorak2qwerty.xmodmap'       
alias asdf='xmodmap ~/.qwerty2dvorak.xmodmap'
alias mp="./.mpcgrep"
alias lla='ls -al'
alias x='startx'
alias tee='tee -a'
alias kd='kill `ps dhcpcd`'
alias ncmpc='ncmpcpp'
alias screen='nocorrect screen' 
alias mv='nocorrect mv'
alias cp='nocorrect cp'
alias rm='nocorrect rm'
alias mkdir='nocorrect mkdir'
alias xwrits='xwrits typetime=55 break=5 flashtime=:2 after 5 clock multiply=5:1.4 +mouse'
alias t='python ~/bin/t.py --task-dir ~/tasks --list todo.txt'
alias td='python ~/bin/t.py  --task-dir ~/tasks --list todo.txt -f'
alias cdl='cd $1; ll'

export PATH=$PATH:/home/sean/cs350/bin/:/usr/bin/:bin/:/usr/sbin:/sbin:/usr/local/bin:/usr/local/sbin:/home/sean/.scripts:/home/sean/.gem/ruby/1.8/bin:/opt/:/opt/openoffice/program:/usr/share/eclipse:/home/sean/bin:/home/sean/bin/mlbviewer

export MPD_HOST='seandesktop'
export hostname='seandesktop'

export EDITOR='vim'
export FCEDIT='vim'
export PAGER='less'

export CVS_RSH="ssh"
export CVSROOT=":ext:ssorrell@student.cs.uwaterloo.ca:/u5/empuurunen/cvsroot/cs350/"

rationalise-dot() {
if [[ $LBUFFER = *.. ]]; then
    LBUFFER+=/..
    else
        LBUFFER+=.
fi
}
# get the name of the branch we are on
function git_prompt_info() {
  ref=$(git symbolic-ref HEAD 2> /dev/null) || return
echo "$ZSH_THEME_GIT_PROMPT_PREFIX${ref#refs/heads/}$(parse_git_dirty)$ZSH_THEME_GIT_PROMPT_SUFFIX"
}

parse_git_dirty () {
  if [[ -n $(git status -s 2> /dev/null) ]]; then
echo "$ZSH_THEME_GIT_PROMPT_DIRTY"
  else
echo "$ZSH_THEME_GIT_PROMPT_CLEAN"
  fi
}


zle -N rationalise-dot
bindkey . rationalise-dot

source ~/.rvm/scripts/rvm


[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

# hack to load .rvmrc if present
cd `pwd`
