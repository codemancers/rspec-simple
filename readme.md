## A Emacs mode for running rspec specs

Just require the file in your configuration:

    (add-to-list 'load-path "<path_to_rspec_simple>")
    (require 'rspec-simple)
    



### Available commands
`rspec-simple` provides following interactive functions that you can use:

* `rspec-compile-on-line ` - Run spec where the cursor is.
* `rspec-compile-file` - Run entire spec file.
* `rspec-find-related-file` - Toggle between spec and code. Implementation very different from `toggle.el`. Integrates with `ido-mode` and does depth first search of related file (In other words, much smarter than `toggle.el`)
* `rspec-display-file-outline` - Display rspec outline (bit like dry run). The generated outline is navigatable via mouse or keyboard! Implemented using ripper.


You can define following shortcut in your `ruby-mode-hook`:

    (add-hook 'ruby-mode-hook
         (lambda ()
            (local-set-key (kbd "C-c l") 'rspec-compile-on-line)
            (local-set-key (kbd "C-c k") 'rspec-compile-file)
            (local-set-key (kbd "s-t") 'rspec-find-related-file)


Then in `ruby-mode` you should have access to following keys:

    Ctrl-c l - Run spec on line
    Ctrl-c k - Run entire spec file


The specs are run by starting your chosen shell and hence it should automatically
pick version of Ruby configured via `rbenv` or `rvm`.

If your shell is not being picked correctly you can as well try:

    (setq shell-file-name "zsh")
    (setq shell-command-switch "-c")
    
## Example    

### Running specs

![Rspec simple](https://github.com/code-mancers/rspec-simple/raw/master/images/rspec-simple.gif "rspec simple")

### Rspec outline

![Rspec outline](https://github.com/code-mancers/rspec-simple/raw/master/images/outline.gif "rspec outline")
