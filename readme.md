## A Emacs mode for running rspec specs

Just require the file in your configuration:

    (add-to-list 'load-path "<path_to_rspec_simple>")
    (require 'rspec-simple)

Then in `ruby-mode` and `enh-ruby-mode` you should have access to following keys:

    Ctrl-c l - Run spec on line
    Ctrl-c k - Run entire spec file
    Ctrl-c s - Run the spec file with zeus



The specs are run by starting your chosen shell and hence it should automatically
pick version of Ruby configured via `rbenv` or `rvm`.

If your shell is not being picked correctly you can as well try:

    (setq shell-file-name "zsh")
    (setq shell-command-switch "-c")



