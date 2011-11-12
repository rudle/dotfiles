# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory autocd extendedglob
unsetopt beep nomatch notify
bindkey -v
set -o vi

export ZSH=$HOME/.oh-my-zsh
export ZSH_THEME='pmcgee'

export LSCOLORS=gxfxbEaEBxxEhEhBaDaCaD
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

alias sw='cd ~/Dropbox/soywiki; soywiki'
alias mplayer='mplayer -vo xv'
alias ssh='TERM=xterm; ssh'
alias ll='ls -lh'
alias lla='ls -al'
alias tee='tee -a'
alias kd='kill `ps dhcpcd`'
alias screen='nocorrect screen'
alias mv='nocorrect mv'
alias cp='nocorrect cp'
alias rm='nocorrect rm'
alias mkdir='nocorrect mkdir'
alias cdl='cd $1; ll'

export CC=/usr/bin/gcc-4.2
export PATH=$PATH:/usr/bin/:bin/:/usr/sbin:/sbin:/usr/local/bin:/usr/local/sbin:/opt/:/usr/share/eclipse:/Users/ssorrell/bin


export EDITOR='vim'
export FCEDIT='vim'
export PAGER='less'

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

# get the name of the branch we are on
function rvm_prompt_info() {
  ruby_version=$(~/.rvm/bin/rvm-prompt 2> /dev/null) || return
  echo "($ruby_version)"
}

RPS1=$'$(git_prompt_info)$(rvm_prompt_info)'

source ~/.rvm/scripts/rvm
