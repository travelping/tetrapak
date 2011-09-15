# planned for 0.3.1

- config objects API
  there is now support for GIT-like config objects in the parser
  so it can read them but there is no API to access them

          [build.escript "foo"]
          mfa = {module, func, []}
          include_application = true

- NIF/driver support
- modularize packaging:
    - we need a "pkg:stage" task that decides what to include and puts that stuff in a temp dir
    - "pkg:deb", "pkg:ipkg" just archive that temp dir
- task library support (read tasks from an applications environment)
- support for eunit and quickcheck tests
