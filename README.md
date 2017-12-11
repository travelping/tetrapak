# tetrapak
===========

tetrapak is an extensible build system for Erlang/OTP applications.

Installation
============

    make install

Developing with tetrapak
========================

-------------------------------------------------------------------------

There are default tasks that are available in every directory.

    tetrapak
    == tetrapak:info =================
    ** version your_tetrapak_version

    Available Tasks
      new      - Generate a skeleton for a new Erlang application
      shell    - Start the Erlang shell

-------------------------------------------------------------------------

You can start a new project with a simple sceleton.

    tetrapak new -app new_app
    == new ===========================
    create: your_path/new_app
    create: your_path/new_app/src
    create: your_path/new_app/tetrapak
    create: your_path/new_app/src/new_app.app.src
    create: your_path/new_app/src/new_app.erl

-------------------------------------------------------------------------

Now, there are more available tasks in a project directory.

    tetrapak
    == tetrapak:info =================
    ** version your_tetrapak_version

    Available Tasks
      build:appfile        - Generate the application resource file
      build:erlang         - Compile Erlang modules
      build:leex           - Compile lexical analysers (.xrl) to Erlang
      build:yecc           - Compile yecc parsers (.yrl) to Erlang
      check:appmodules     - Check app file module list
      check:packageable    - Check that application has all plugin dependencies
      check:xref           - Check inter-module calls
      clean:appfile        - Delete generated application resource file
      clean:ct             - Remove compile common_test units
      clean:dist           - Delete packages
      clean:edoc           - Delete generated documentation
      clean:erlang         - Delete compiled Erlang modules
      clean:leex           - Delete compiled lexical analysers
      clean:taskcache      - Delete the local task cache
      clean:testlog        - Delete common_test HTML logs
      clean:yecc           - Delete compiled yecc parsers
      config:appfile       - Read the application resource file
      config:vcs           - Gather information from the VCS
      doc:edoc             - Generate edoc documentation
      info:deps            - Get application deps as list
      info:deps:tree       - Get application deps as tree
      install:copy         - install a copy of the application into Erlang lib dir
      pkg:deb              - Create a binary debian package
      pkg:debsrc           - Create a debian source package
      pkg:ipkg             - Create a binary ipkg
      shell                - Start the Erlang shell
      start:dev            - Start the application in Erlang shell
      test:ct              - Run unit tests with common_test

-------------------------------------------------------------------------

Tasks have this structure: 'build:appfile'. This means that tetrapak tries to execute a build task. If it fails find atleast one build task, it tries to execute all 'build:*' tasks

An example is: tetrapak info:deps. Note that you will get output from both tasks, even if you execute only info.

All tasks have their own tasks dependency.

Example: tetrapak build:appfile execute build:erlang tasks

-------------------------------------------------------------------------

Sometimes you might need an extra configuration file while development in application environments.
It is possible to specify this in a folder:

tetrapak/dev.config:

    [
     {app1,
        [{env1, value1}]
     },
     {app2,
        [{env1, value1}]
     }
    ].

or

    {app1,
        [{env1, value1}]
    }.

    {app2,
        [{env1, value1}]
    }.

Both styles are working.
Config would be read if you start tetrapak start:dev (or tetrapak start, which invokes start:dev).

## tetrapak's shell

-------------------------------------------------------------------------

There are extra functions available in shell such as:

    1> help().
    ...
    l()        -- reloads changed modules
    start()    -- starts the current application
    start(App) -- starts an application and all its dependencies
    bl()       -- runs "build"
    bll()      -- runs "build" and reloads modules
    bls()      -- runs "build", reloads modules and starts the current application
    dbg(M)     -- enable dbg tracer on all functions in module M
    dbg(M, F)  -- enable dbg tracer on M:F functions
    dbgl(M)    -- enable dbg tracer on all local functions in module M
    dbgdel(M)  -- disable call tracer for module M
    dbgdel(M,F)-- disable call tracer for function M:F
    dbgoff()   -- disable dbg tracer (calls dbg:stop_clear/0) to delete all debug information

If you have installed the OTP application [redbug] (https://github.com/liveforeverx/redbug) in your ERL_LIBS pathes for a dbg/1 command you can use:

    dbg(S)     -- enable dbg tracer with redbug RTP(restricted trace pattern)
                  Please note, that not all original patterns are supported
                  the RTP has the form: "<mfa> when <guards>"
                  "mod", "mod:fun", "mod:fun/3" or "mod:fun('_',atom,X)"
                  <guard> is something like:
                  "X==1" or "is_atom(A)" or "(X==2) or (Y==2)"

If you want to debug some function by tetrapak or tetrapak tasks yourself (or if your
application shows different behaviour in a test case for example):

It is possible invoke a redbug-like RTP pattern with a tetrapak start:

    DEBUG_SPEC="tetrapak_task"

Or a throw command option, where it possible to define more than one debug spec:

    -debug_spec "lists:seq(1,20)" "tetrapak"

There is a debug command for tetrapak itself.

    DEBUG=1 tetrapak

Or:

    DEBUG=true tetrapak

## Config Options

    build.erlc_options :: [atom() | {atom(), term()}]

Options are passed to the erlang compiler. Please note that there is
no support for rebar-like platform defines at the moment.

-------------------------------------------------------------------------

    build.version :: string()

This option describes what goes into the `vsn` field of the application
resource file. A format-string-like syntax is used to pull information
from the VCS (like git). The directives are:

    ~~       -- a literal ~
    ~t       -- the most recent tag
    ~c       -- the last commit's abbreviated name
    ~o       -- the last commit's offset from the tag
    ~b       -- the current branch
    ~d       -- a timestamp (YYYYMMDDhhmmss)

In addition to the directives above the version string may also include
the following conditional directives:

    ~O{...}  -- expands to ... if the offset from the latest tag is non-zero
    ~D{...}  -- expands to ... if the source tree is dirty
    ~T{...}  -- expands to ... if there is a latest tag

Non-Conditional directives may be used inside of a conditional directive,
but nesting them is not supported.

-------------------------------------------------------------------------

    edoc.outdir :: string()

The directory where edoc-generated files are placed. Defaults to `"doc"`.

-------------------------------------------------------------------------

    edoc.private :: boolean()

Whether private documentation shall be generated for this application.
Defaults to `false`.

-------------------------------------------------------------------------

    edoc.hidden :: boolean()

Whether documentation for hidden functions should be generated.
Defaults to `false`.

-------------------------------------------------------------------------

    edoc.todo :: boolean()

Whether `@todo` notes should be included in the documentation.
Defaults to `false`.

-------------------------------------------------------------------------

    edoc.sort_functions :: boolean()

Whether functions should be sorted alphabetically.
Defaults to `false`.

-------------------------------------------------------------------------

    edoc.pretty_print :: boolean()

Whether type specs and signatures should be pretty-printed.
Defaults to `true`.

-------------------------------------------------------------------------

    package.outdir :: string()

The directory where built packages are placed. This directory
is _deleted_ by the clean:dist task, don't use it for anything you
want to keep. Defaults to `"dist"`.

-------------------------------------------------------------------------

    package.maintainer :: string()

This goes into the Maintainer Field of generated packages.
Defaults to `"Joe User <joe@example.com>"`.

-------------------------------------------------------------------------

    package.exclude :: string()

A regular expression. Any path that matches the expression will be excluded
from packages.

-------------------------------------------------------------------------

    package.extra_apps :: [atom()]

List of OTP applications the application depends on in addition to
the ones specified in the application resource file.
Defaults to `[]`.

-------------------------------------------------------------------------

    package.extra_build_apps :: [atom()]

List of OTP applications the application depends on _at build time_,
in addition to the applications specified in the application resource file and
through the `package.extra_apps` option.
Defaults to `[]`.

One case where this might be useful is when your application uses 'eunit'
for testing and some of the application's runtime modules include the eunit header.
You obviously don't want to depend on eunit on the target system, but the build will
fail when eunit is not installed. In that case, adding

    [package]
    extra_build_apps = [eunit]

to the configuration file will make the build work.

-------------------------------------------------------------------------

    package.include_src :: boolean()

Whether the source code of the application shall be included in the package.
If true the `tetrapak/` directory is also included.
Defaults to `false`.

-------------------------------------------------------------------------

    package.use_erlrc :: boolean()

If this flag is set, the package will have a dependency on the erlrc application.
The package scripts will try to start or upgrade the application at
installation time.

-------------------------------------------------------------------------

    package.deb.section :: string()

The debian section in which the package is placed. Defaults to `"misc"`.

-------------------------------------------------------------------------

    package.deb.priority :: string()

The priority of the package. Valid values are `"required"`, `"important"`,
`"standard"`, `"optional"`, `"extra"`.
Please consult the [Debian Policy](http://www.debian.org/doc/debian-policy/ch-archive.html#s-priorities)
to learn more about the definition of those terms. Defaults to `"optional"`.

-------------------------------------------------------------------------

    package.deb.erlang_base_apps :: [atom()]

This list contains the OTP applications that are included in the `erlang-base`
package. You probably don't want to change this.

-------------------------------------------------------------------------

    package.deb.dependencies :: [string()]

A list of additional debian packages this application depends on at runtime.
Don't specify Erlang dependencies here, use `package.extra_apps` for that.

-------------------------------------------------------------------------

    package.deb.build_dependencies :: [string()]

A list of additional debian packages this application depends on _at build time_.
Don't specify Erlang build dependencies here, use `package.extra_build_apps` for that.

-------------------------------------------------------------------------

    test.ct.logdir :: string()

The directory where common test's HTML reports are placed.
Defaults to `"test-log"`.

-------------------------------------------------------------------------

    test.ct.srcdir :: string()

The directory in which to look for common test suite modules.
Defaults to `"test"`.

-------------------------------------------------------------------------

    test.ct.suite :: string()

The common test suite that should be run. The main purpose of this
config option is running a specific suite from the command line,
as in:

    $ tetrapak test -o 'test.ct.suite' 'my_SUITE'

Defaults to `"all"`.

-------------------------------------------------------------------------

    xref.ignore_undef :: [{atom(), atom(), integer()}]

The entries of this list specify functions ({Module, Function, Arity}).
The 'check:xref' task will fail if there are any calls to undefined
functions that are not a member of this list.
Defaults to `[]`.

Extensibility
=============
