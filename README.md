An example of GUI using reflex solving a problem given in a famuos internship

run natively (gtk) with

```
  stack build
  stack exec Roles
```

run in browser with

```
  stack --stack-yaml  stack.js.yaml build
  firefox .stack-work/dist/x86_64-linux/Cabal-1.24.0.0_ghcjs/build/Roles/Roles.jsexe/index.html 
```

