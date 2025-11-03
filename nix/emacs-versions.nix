# Adjust to add new Emacs versions to CI
# Emacs 29.x support has been removed due to the use of new font-lock faces
# Also https://search.nixos.org/packages?channel=25.05&query=emacs
# shows that the lowest currently supported version of emacs in nixpkgs is 30.2
[
  "29.1"
  "29.2"
  "29.3"
  "30.1"
  "30.2"
  "snapshot"
  "release-snapshot"
]
