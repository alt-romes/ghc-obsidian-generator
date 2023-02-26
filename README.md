# ghc-obsidian-generator

Generate an Obsidian Vault from the Notes in GHC Haskell

GHC Haskell has throughout its source code multiple Notes explaining bits of the compiler.
It's sometimes quite useful to search through these notes only, and to see the connections between notes.

The Obsidian program to create and maintain knowledge bases.

`ghc-obsidian` basically bridges the Haskell GHC Notes to an Obsidian Vault by extracting notes from the source code and creating a note in a new Obsidian vault for each GHC note.

One can then load the vault into Obsidian and search through and visualize the Notes of the GHC compiler.

## How to use

Install the `ghc-obsidian` executable with cabal by cloning this repository and running `cabal install` inside.

Run `ghc-obsidian` in the root of the ghc source code. This will create a folder called `NotesVault`.

Choose "Open Vault" within the "File" tab in Obsidian, and select the `NotesVault` folder in the ghc tree root.

## Screenshots


![Screenshot 2023-02-26 at 13 32 24](https://user-images.githubusercontent.com/21295306/221413824-5ef05ed5-c8c8-4b94-8656-298c636c276e.png)
![Screenshot 2023-02-26 at 13 37 19](https://user-images.githubusercontent.com/21295306/221413825-5118eb6b-6c64-4f6b-bf38-61109ac63413.png)
![Screenshot 2023-02-26 at 13 31 43](https://user-images.githubusercontent.com/21295306/221413827-621b4c5f-99e2-42f5-9f38-ca74f2ad3c09.png)
![Screenshot 2023-02-26 at 13 31 46](https://user-images.githubusercontent.com/21295306/221413828-a3d3f881-fc0b-4f46-afb3-f97c5566e48f.png)
