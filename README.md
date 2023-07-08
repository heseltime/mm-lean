# mm_lean for WSS2023: Meta-Mathematics Project

# Things to Implement




# Things to Show

See also: https://writings.stephenwolfram.com/2020/09/the-empirical-metamathematics-of-euclid-and-beyond/




# Notes on Setup (using Lean version 3.21.0 and Windows)

* Lean installation: I went with Lean 4 per https://leanprover-community.github.io/install/windows.html and specifically 
```
curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh
```
(Using gitbash.) It makes things easier to add lean to the system path.
* switch to src dir after downloading this directory: e.g. C:\Public\WSS2023\mm-lean
* the .toml file in here will specify the correct (downwards compatible) Lean version, but Mathlib still needs to be installed:
```
leanpkg build
```
This will add a leanpkg.path: On Windows I had a forward-slash issue that required me to change the forward slashes to backward slashes! If you see an error when running your Mathematica .nb file after setup, you should try this. 

* Finally, open src/Leanproof.nb and see if the cells execute: This step probably also requires customizing the directories to your machine.

Notes on the setup:
* The "leanproject" command is only available in Lean 3! (Referenced in the upstream project.) See: https://leanprover-community.github.io/leanproject.html
* "lean --path" to check correct path settings