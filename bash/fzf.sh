_fzf_compgen_path() {
  fd --hidden . "$1"
}

_fzf_compgen_dir() {
  fd --type d --hidden . "$1"
}
