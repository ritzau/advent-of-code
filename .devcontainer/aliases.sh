alias bb="bazel build '...'"
alias bba="bazel build //..."
alias bt="bazel test '...' --test_output=all"
alias bta="bazel test //..."
alias aoc="bazel run //:aoc --"
alias bf="bazel run //tools:format"
alias gr="git rev-parse --show-toplevel"
alias cgr="cd $(git rev-parse --show-toplevel)"
alias gcan!='git commit --all --amend --no-edit'
alias gcof='git checkout "$(git branch --format=\"%(refname:short)" | fzf)"'
alias gd='git diff'
alias gds='git diff --stat'
alias gdt='git difftool --dir-diff --no-symlinks'
alias gdtm='git difftool --dir-diff --no-symlinks origin/main'
alias gdts='git difftool --no-prompt'
alias gdtsm='git difftool --no-prompt origin/main'
alias gdw='git diff --word-diff-regex=.'
alias gf='git fetch'
alias gl='git log --oneline --max-count=42'
alias gl1='git log --max-count=1'
alias glg='git log --graph --oneline --branches --max-count=24 @'
alias glga='git log --graph --oneline --branches @'
alias gmt='git mergetool'
alias gp='git push'
alias gpf='git push --force-with-lease'
alias gpsup='git push --set-upstream origin $(git branch --show-current)'
alias grasq='git rebase --interactive --autosquash origin/main'
alias gs='git status --ahead-behind'
