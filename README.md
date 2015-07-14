ml2mxml
=======

ml2mxml is an OCaml library to generate
musicxml (http://www.musicxml.com/) files.

That library has been developed for two reasons:
- the lack of what 'git' offers, in music partition editors,
- the personal lack of skills to use cut/copy/paste features of Guitar Pro 6,
  needed to copy/move partitions around measure/instruments.

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
* To build the library:
```shell
% omake
```

* To execute the tests:
```shell
% omake test
```

* To generate the xmls of the examples:

```shell
% omake ex
```
