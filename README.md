ml2mxml
=======

ml2mxml is an OCaml library to generate
musicxml (http://www.musicxml.com/) files.

That library has been developed for two reasons:
- the lack of what 'git' offers, in music score editors,
- the hard time experienced when using the cut/copy/paste features
  of the Guitar Pro 6 music score editor.


With that library, music can be written as:
- write an OCaml program (see examples/hello_ode_of_joy.ml),
- compile/execute it (% omake ex),
- and open the generated musicxml file, with for example a midi
  player that understands the format (see http://www.musicxml.com/software/).

Up to now, the generated files were tested with Guitar Pro 6
(http://www.guitar-pro.com/en/index.php) only.
If you experience issues with another musicxml reader,
don't hesitate at creating issues (https://github.com/yetanotherion/ml2mxml/issues).



Instructions
============
The library can be installed with opam (http://opam.ocaml.org/):

```shell
% opam install ml2mxml
```

To build from source, the following dependencies
are needed

```shell
% opam install ocamlfind cow omake ounit
```

Then, the following omake targets are available:

* build the library:
```shell
% omake
```

* execute the tests:
```shell
% omake test
```

* generate the xml of the example:

```shell
% omake ex
```

* install (uninstall):

```shell
% omake install (uninstall)
```