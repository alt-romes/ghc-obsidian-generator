# ghc-obsidian-generator

Generate an Obsidian Vault from the Notes in GHC Haskell

GHC Haskell has throughout its source code multiple Notes explaining bits of the compiler.
It's sometimes quite useful to search through these notes only, and to see the connections between notes.

The Obsidian program to create and maintain knowledge bases.

`ghc-obsidian` basically bridges the Haskell GHC Notes to an Obsidian Vault by extracting notes from the source code and creating a note in a new Obsidian vault for each GHC note.

One can then load the vault into Obsidian and search through and visualize the Notes of the GHC compiler.

TODO: Notes in single line comments don't seem to be parsed correctly (see GHC/CmmToC.hs)

## How to use

Install the `ghc-obsidian` executable with cabal by cloning this repository and running `cabal install` inside.

Run `ghc-obsidian` in the root of the ghc source code. This will create a folder called `NotesVault`.

Choose "Open Vault" within the "File" tab in Obsidian, and select the `NotesVault` folder in the ghc tree root.

## Screenshots


<img width="1440" alt="Screenshot 2023-02-22 at 18 18 31" src="https://user-images.githubusercontent.com/21295306/220725750-0f061a9d-66de-411b-baeb-0bade08745c0.png">

<img width="1440" alt="Screenshot 2023-02-22 at 18 21 07" src="https://user-images.githubusercontent.com/21295306/220725771-8dbdd632-394e-4611-a1b5-9e461677f208.png">
