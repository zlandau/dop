
Introduction
------------

`dop` is a little tool allowing you to create aliases for commonly used
directories.

Configuration
-------------

Commands can be executed by running `dop <command>` but it is much easier to
create symlinks for the commonly used commands. For example:

      $ dop dlist

can be replaced by

      $ ln -s dop dlist
      $ dlist

Usage
-----

      dpush <key> [dir]  : save a directory under the given key
      dpop <key>         : remove the given key for a directory
      dlist              : show the list of saved directories
      dchange <key>      : changes to the directory specified by the key

Note that `dchange` is actually useless, because it will only change the
directory in the spawned `dop` process, not your shell. More likely you
want to use `_dch` with the alias specified at the top of the file.  This
will allow you to change the directory within your current shell.
