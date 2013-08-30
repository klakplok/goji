Goji: OCaml-JavaScript bindings generator
=========================================

Goji is a multi-tool for OCaml-JavaScript interoperability.

For now, its is able to:
- generate bindings from high level descriptions
- grab JavaScript dependencies and include them in you application code

Its main features are:
 - An OCaml-centric approach, including the possibility to map complex
   data types and to use OCaml's optional and labeled arguments
 - Concise ans high level descriptions partially based on lenses
 - Meta-generation of binding descriptions (built as an embedded DSL)
 - Customizable generation scheme, in particular to handle events
   either by hand or with your concurrency library of choice
 - Some static checking and optional insertion of dynamic cheks for
   debugging purposes
 - Automated handling of JavaScript dependencies

Some sample bindings are available at:
  https://github.com/klakplok/goji-bindings

HOWTO start writing your first library binding
----------------------------------------------

 * Build and install Goji. This installs a `goji` command and two
   OCamlFind packages: `goji_lib` and `goji_runtime`.
 * Bindings descriptions must be defined in OCaml source files, using
   binding description pimitives provided by the `goji_lib` package.
   In other words, you use predefined OCaml functions and types to
   build an intermediate representation of your binding. Goji then
   takes this intermediate representation to generate your binding as
   new OCaml source files (along with METAs, Makefiles, etc.). This is
   similar to CamlIDL, and different from ocaml-ctypes. So open up a
   new OCaml source file in which to write your description, for
   instance with the same name as the library you want to bind.
 * The first step is to declare a package for your library using
   `Goji.register_package`, providing its version and a short
   documentation. Once done, you can start building the architecture
   of your library by adding top level modules using
   `Goji.register_component`. If you have a lot of toplevel modules,
   you can define them in separate OCaml source files for convenience,
   but you'll have to be careful abour their order when feeding them
   to `goji`. You are free to architecture your binding as you wish,
   in particular, you do not have to respect the structure of the
   underlying library.
 * For each component, you have to define meta-information. Some are
   optional, such as your name or OCamlFind dependencies. Some are
   required, such as the license. This is required because most of the
   time, you will include some parts of the original library, in
   particular in documentation strings, and in such case, you have to
   respect the original license. `Goji.License` contains predefined
   licenses, and you can define your own. You can also explain how to
   obtain the sources of the JavaScript library by using
   `Goji.Grab`. This is explained in a dedicated section below.
 * Describe the internal structure as a list of toplevel binding
   elements (type `Goji_ast.binding`). You can define sub-modules,
   types and values as described in a following section.
 * When you are ready and that your descriptions successfully compile
   against the `goji_lib` package, you can feed them to the `goji
   generate` command. By default, this will produce a folder for each
   of the packages you registered, with the generated sources, METAs
   and Makefiles inside. You can call `goji generate --help` for more
   options.

Top level binding description
-----------------------------

TODO

Describing data mappings
------------------------

TODO

Describing functions / methods mappings
---------------------------------------

TODO

JavaScript dependency handling
------------------------------

TODO

License
-------

 - The code generator (sources in the toplevel directory) is placed
   under the CeCILL license, which ressembles the GPL
 - The AST and DSL (sources in goji_lib) are placed under the CeCILL-B
   license, which ressembles the LGPL, so you can give the license you
   want to your own binding descriptions, but any modification to the
   library must be redistributed
 - The runtime library (sources in goji_runtime) is released with an
   extremely permissive library (the WTFPL), so you can give the
   license you want to your bindings and still link them with the
   runtime
