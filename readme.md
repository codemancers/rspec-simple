## A Emacs mode for running rspec specs

Just require the file in your configuration:

    (add-to-list 'load-path "<path_to_rspec_simple>")
    (require 'rspec-simple)
    
`rspec-simple` provides following interactive functions that you can use:

```
;; run spec where the cursor is
rspec-compile-on-line 
;; Run entire file
rspec-compile-file
;; find related file for this spec file. This could be 
;; a spec file or corresponding code file.
;; Works with Rails model/controllers too. Unlike toggle.el
;; this integrates well with ido
rspec-find-related-file
;; Display outline of rspec file without running specs.
;; You can navigate too from the outline.
rspec-display-file-outline
```

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
