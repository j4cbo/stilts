SML On Stilts
=============

Stilts is a framework for robust web development based on multistage
programming. Rather than performing extensive introspection and processing at
runtime, Stilts compiles portions of the application together in multiple
passes to ensure strict correctness between components. For example, templates
are never parsed on the fly; instead, they are pre-serialized as much as
possible into code. Type-checking all invocations of the template ensures that
the application always passes templates the parameters they expect. Similarly,
SQL statements are wrapped together with their input and output types, and
escaping code inserted automatically.

The framework is written in [Standard ML](http://en.wikipedia.org/wiki/Standard_ML). Both
[SML/NJ](http://www.smlnj.org/) and [MLton](http://mlton.org) are supported, with parallel CM
and ML Basis build systems.

Components
----------
- Core Web application types, signatures, and utility functions.
- A high-performance userspace threading system.
- Server implementations of FastCGI, SCGI, and HTTP, allowing Stilts applications to be invoked from a variety of front-end Web servers or as a standalone daemon.
- Smelt, an HTML / XML templating system.
- Squall, a SQL query wrapper function generator.
- Infrastructure for interfacing with MySQL and SQLite.
- A simple Wiki, as a demo application.

See each component's README file for more documentation.

Installation
------------
Some notes:

- On debian, you will need to install the following packages in addition to smlnj:
    libckit-smlnj libmlnlffi-smlnj ml-nlffigen ml-lex ml-yacc ml-lpt

- ml-yacc may fail like so:

        Usage: ml-yacc filename
        ../db/squall/squall.cm:5.2-5.11 Error: tool "ML-Yacc" failed: "/usr/lib/smlnj/bin/ml-yacc" "" "input.grm"

  Run ml-yacc filename by hand. (Argh, SML/NJ.)
