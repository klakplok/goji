Goji: OCaml-JavaScript bindings generator
=========================================

Goji is a multi-tool for OCaml-JavaScript interoperability.

For now, its is able to:
 - Generate bindings from high level descriptions
 - Grab JavaScript dependencies and include them in you application code

Its main features are:

 - An OCaml-centric approach, including the possibility to map complex
   data types and to use OCaml's optional and labeled arguments
 - Concise ans high level descriptions based on lenses
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

 - Build and install Goji. This installs a `goji` command and two
   OCamlFind packages: `goji_lib` and `goji_runtime`.
 - Bindings descriptions must be defined in OCaml source files, using
   binding description pimitives provided by the `goji_lib` package.
   In other words, you use predefined OCaml functions and types to
   build an intermediate representation of your binding. Goji then
   takes this intermediate representation to generate your binding as
   new OCaml source files (along with METAs, Makefiles, etc.). This is
   similar to CamlIDL, and different from ocaml-ctypes. So open up a
   new OCaml source file in which to write your description, for
   instance with the same name as the library you want to bind.
 - The first step is to declare a package for your library using
   `Goji.register_package`, providing its version and a short
   documentation. Once done, you can start building the architecture
   of your library by adding top level modules using
   `Goji.register_component`. If you have a lot of toplevel modules,
   you can define them in separate OCaml source files for convenience,
   but you'll have to be careful abour their order when feeding them
   to `goji`. You are free to architecture your binding as you wish,
   in particular, you do not have to respect the structure of the
   underlying library.
 - For each component, you have to define meta-information. Some are
   optional, such as your name or OCamlFind dependencies. Some are
   required, such as the license. This is required because most of the
   time, you will include some parts of the original library, in
   particular in documentation strings, and in such case, you have to
   respect the original license. `Goji.License` contains predefined
   licenses, and you can define your own. You can also explain how to
   obtain the sources of the JavaScript library by using
   `Goji.Grab`. This is explained in a dedicated section below.
 - Describe the internal structure as a list of toplevel binding
   elements (type `Goji_ast.binding`). You can define sub-modules,
   types and values as described in a following section.
 - When you are ready and that your descriptions successfully compile
   against the `goji_lib` package, you can feed them to the `goji
   generate` command. By default, this will produce a folder for each
   of the packages you registered, with the generated sources, METAs
   and Makefiles inside. You can call `goji generate --help` for more
   options.

Writing binding descriptions using the AST or the DSL
-----------------------------------------------------

You have two options to build binding descriptions.

 - Directly write values from Goji's intermediate representation (AST
   nodes), by calling the constructors defined by the various types of
   the `Goji_ast` module. The AST is made public, documented and
   should be fairly stable. Even if you don't use it directly, it is a
   good idea to browse its definition for understanding how
   descriptions are structured.
 - The AST has been designed for being actually writable by hand, but
   since it encode features only needed by complex bindings, writing
   simple descriptions can be a little verbose or confusing. For this,
   the `Goji_dsl` module defines functions that correspond to AST
   constructors but with some parameters made optional. For instance,
   documentation can be passed with `~doc:"..."` or omitted (**but
   should not**).

Apart from providing basic constructor functions, the DSL also
defines more high level functions which generate complex AST parts for
common non trivial bindings constructs (such as binding an enum to a
sum type). The idea is to use OCaml as the meta language to encode
custom binding constructs as OCaml functions that produce AST parts or
combine other DSL constructs.

If you write new DSL constructs which seem useful outside of your
specific binding, don't hesitate to ask for their integration.

Top level binding description
-----------------------------

The `Goji.register_component` takes a list of `Goji_ast.binding`
elements. This list contains descriptions of the top level elements of
the generated OCaml module.

 - **Type definitions** explain how an OCaml data type is mapped to a
   JavaScript data structure, as explained in the following
   section. It produces an OCaml type definition and two conversion
   functions and two converters for internal use (these functions
   appear in the signature for cross-library interoperability, but are
   omitted for the OCamlDoc).
 - **Function definitions** explain how to map an OCaml function to
   some JavaScript primitive (not mandatorily a JavaScript function
   call). A later section is dedicated to this topic.
 - Some **object oriented features** such as inheritance between types
   and method definitions are also available. By default, these do not
   produce object oriented code. Inheritance produces explicit
   corecion functions and methods produce functions to whom the object
   is passed as parameter. An object oriented back-end is planned in
   which an abstract type, associated methods and inheritance
   declarations are actually mapped to an OCaml class. It is thus a
   good idea not to forget to these primitives.
 - Local **exceptions** can be declared, so they can be raised either
   when a JavaScript exception is thrown in an external code or when
   some value cannot be converted (see the section on guards).
 - The **structure** of the OCaml code can be defined by using the
   `Structure` AST node, which produces a submodule. The `Section`
   node simply adds a documentation header to a series of
   bindings. The `Group` constructor is on the other hand completely
   transparent, its content is flattened into its parent. This is
   useful for writing macros that have to produce several AST nodes
   but must only return one.


Describing data mappings
------------------------

In order for the library user to see only OCaml values, values have to
be converted back and forth between their OCaml and JavaScript
representations. For simple types, Goji has predefined construct
defined by the type `Goji.AST.mapping`. When converting values of
complex, structured types, the binding has to explain the mappings
between the elements of the OCaml structure and those of the
JavaScript one, though the type `Goji.AST.value`. This can often be
done inline (for instance when describing the return value of a
JavaScript function), or in two steps by using Goji's type definition
construct and then by to refering this definition by name. This second
option is also the only possibility when mapping records or variants.

The goal of a type definition is twofold:

 1. Produce an OCaml type definition / abbreviation which helps having
    a clean and documented interface.
 2. Explain how a value of this type is converted to a JavaScript
    value.

The second task is done by attaching two convertion functions to the
type:

- The **injector** takes an OCaml value and converts it to
   JavaScript. In the case of type definitions, the result is a single
   JavaScript value, but in other contexts, various effects can be
   performed in the JavaScript world (for instance assigning function
   parameters or globals), hence the name. We say that the function
   injects an OCaml value in the JavaScript context.
 - The **extractor** performs the opposite conversion: it extracts an
   OCaml value from the JavaScript context. In the case of a type
   definition, this context is actually a single JavaScript value.

### Lenses ###

The conversion functions are automatically generated from a single
declarative description of the relations between the OCaml type
definition and the JavaScript structure. These definitions are OCaml
oriented, consistently with the rest of Goji, and are naturally read
as extractions. However, they are actually reversible and one
definition is enough to generate both converters. They can actually be
seen as a sort of **lenses**.

A lense is described using the following three AST node types.

 - `Goji.AST.value` is the top level part of the description. It
   describes the structure of the OCaml type, for instance `Tuple`, or
   `Variant`. The leaves are described using the `Value` case, which
   associates a `mapping` to a `storage` to describe the conversion of
   a single JavaScript value.
 - The `Goji.AST.storage` gives the location of the JavaScript
   value. It is basically a path inside the JavaScript context, and in
   the case of a type definition, a path from the root of the
   JavaScript value. For instance, `Field ("x", Field ("b", Var
   "root"))`. is equivalent to JavaScript `root.b.x`. The elements of
   the JavaScript context are accessed through the `Var` construct. In
   a type definition context, only the `"root"` Goji variable is
   defined.
 - The `Goji.AST.mapping` explains how to convert the JavaScript
   value. Predefined cases (such as `Int`) are given for basic types,
   are mapped to their native equivalents in both languages. This
   means in particular that string are passed by copy by default. For
   composite objects such as arrays, a `value` description has to be
   provided to describe how each element has to be converted. Be aware
   that in this sub-description, the `"root"`variable is hidden and
   now describes the `"root"`of the element being converted, not the
   root of the collection.

The DSL provides a notation `@@` to describe a single `Value`.

 - On the left is the `mapping`.
 - On the right is the `storage`.

### Examples ###

 - `(int @@ field root "x")` means that the value is an `int` and is
   located in the `"x"` field of the JavaScript value. If such an
   expression is used in a type definition, it means that a value
   `1234` in OCaml will be converted as an object `{ x: 1234 }` in
   JavaScript, and vice versa.
 - `tuple [ int @@ field root "x" ; int @@ field root "y" ]` maps a
   pair `(12, 34)` of integers to a JavaScript object `{ x: 12, y: 34 }`.

Describing functions / methods mappings
---------------------------------------

TODO

JavaScript dependency handling
------------------------------

For the **user**, handling of external JavaScript sources basically
works as follows. When compiling a program using bindings `a` `b` and
`c`, by simply using `goji jslink a b c -o mylibs.js`, the programmer
obtains a single JavaScript entry point `mylibs.js` to refer to in its
HTML source code.

For this to work, binding **authors** have to explain how to grab
library sources in binding descriptions. This is done by writing a
script using primitives from the `Goji_grab` module which will
download / unpack the sources and put them in a specific form, as
explained just after. Such a script can be provided for every
registered component using the `~grab` parameter.

At generation time, goji runs these scripts and packs the resulting
files in an archive inside the OCamlFind directory. The `jslink`
command uses OCamlFind to locate these archives, before unpacking them
and merging their contents.

Scripts have to respect the following rules:

 - Scripts of the same package are executed in an intially empty
   temporary directory. They can download, write or move any file they
   want into this directory, including subdirectories, but should not
   perform other side effects.
 - After all scripts are run, there are two options:
    - If a file named `goji_entry.js` is present, all the files are
      archived and stored in the OCamlFind package directory.
    - If not, files are completely discarded. This is for instance the
      case when binding browser built-ins (in which no external JavaScript
      has to be inserted into your app) or for mobile apps wrappers (in
      which JavaScript dependencies are inserted by the tool).

At `jslink` time, all archives are extracted into the current
directory (unless the option is passed). An exception is made for
`goji_entry.js` files, which are not extracted directly but
concatenated into the file specified with `-o`.

**N.B.** It is up to the script to ensure that there is no filename
clash between libraries, for instance by using subdirectories. This is
a bit of a shame, but there is no simple and uniform way to handle
modularity in JavaScript, so this task has to be done manually.

License
-------

- The AST and DSL (sources in goji_lib) are placed under the CeCILL-B
  license, which ressembles the LGPL, so you can give the license you
  want to your own binding descriptions, but any modification to the
  library must be redistributed
- The code generator (sources in the toplevel directory) is placed
  under the CeCILL license, which ressembles the GPL
- The runtime library (sources in goji_runtime) is released with an
  extremely permissive library (the WTFPL), so you can give the
  license you want to your bindings and still link them with the
  runtime
