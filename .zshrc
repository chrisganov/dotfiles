eval "$(starship init zsh)"
export LC_ALL=en_US.UTF-8

export NVM_DIR=~/.nvm
 [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"

autoload -U compinit
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

alias ls='ls -GH'
alias p='cd ~/Desktop/Projects'
alias tags='ctags -R --exclude=node_modules .'

source /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
