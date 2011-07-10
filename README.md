# ~~ tetrapak

tetrapak is an extensible build system for Erlang/OTP applications.

## Config Options

    build.erlc_options :: [atom() | {atom(), term()}]

Options passed to the erlang compiler. Please note that there is
no support for rebar-like platform defines at the moment.

-------------------------------------------------------------------------

    build.version :: string()

This option describes what goes into the `vsn` field of the application
resource file. A format-string like syntax is used to pull information
from the VCS (like git). The directives are:

    ~~       -- a literal ~
    ~t       -- the most recent tag
    ~c       -- the last commit's abbreviated name
    ~o       -- the last commit's offset from the tag
    ~b       -- the current branch

In addition to the directives above, the version string may also include
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

Whether documentation for hidden functions shall be generated.
Defaults to `false`.

-------------------------------------------------------------------------

    edoc.hidden :: boolean()

Whether documentation for hidden functions shall be generated.
Defaults to `false`.

-------------------------------------------------------------------------

    edoc.todo :: boolean()

Whether `@todo` notes should be included in the documentation.
Defaults to `false`.

-------------------------------------------------------------------------

    package.outdir :: string()

The directory where built packages are placed. This directory
is _deleted_ by the clean:dist task, don't use it for anything you
want to keep. Defaults to `"dist"`.

-------------------------------------------------------------------------

    package.maintainer :: string()

This goes into the Maintainer field of generated packages.
Defaults to `"Joe User <joe@example.com>"`.

-------------------------------------------------------------------------

    package.exclude :: string()

A regular expression. Any path that matches the expression will be excluded
from packages.

-------------------------------------------------------------------------

    package.include_src :: boolean()

Whether the source code of the application shall be included in the package.
If true, the `tetrapak/` directory is also included.
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
to learn more about the meaning of those.
Defaults to `"optional"`.

-------------------------------------------------------------------------

    package.deb.erlang_base_apps :: [atom()]

This list contains the OTP applications that are included in the `erlang-base`
package. You probably don't want to change this.

-------------------------------------------------------------------------

    test.ct.logdir :: string()

The directory where common test's HTML reports are placed.  
Defaults to `"test-log"`.

-------------------------------------------------------------------------

    test.ct.srcdir :: string()

The directory in which to look for common test suite modules.
Defaults to `"test"`.
