## A Emacs mode for running rspec specs

Just require the file in your configuration:

    (add-to-list 'load-path "<path_to_rspec_simple>")
    (require 'rspec-simple)

Then in `ruby-mode` and `enh-ruby-mode` you should have access to following keys:

    Ctrl-c l - Run spec on line
    Ctrl-c k - Run entire spec file
    Ctrl-c s - Run the spec file with zeus

