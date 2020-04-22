# dauntless-equipment

Loadout optimizer for Dauntless.

You tell it which perks / cells you want, it gives you all loadouts that have those.
If you specify too much, you get 0 loadouts.
If you specify too little you get hundreds of loadouts.
The idea is to refine your specification, and sort the results, to explore what's possible.

This program is not perfect.
The database is incomplete, I'm using mostly chain blades and pikes, so those are supported.
No support for exotic weapons / equipment.
I wrote it for myself to explore loadouts, you can use it at your own risk.

This program has no graphical user interface. You have to modify the main function and run it.


## How to use this

- You need a Haskell compiler; I'm using the stack build system
- Install stack https://docs.haskellstack.org/en/stable/README/
- Clone the source code using git
- On the command line, go to the source directory and run stack
- I'm using linux, I run it like this:

    $ stack run | less

## Example:

Ember survival build: built around Fortress and Fireproof for chain blades,
which I like playing with Conditioning and Agility. I found playing with
randoms online requires at least +4 medic for the revive speed.
Has at least 3 blaze pieces and no frost piece.
Has a frost weapon.

You specify:

    >>= has Defensive Fortress
    >>= has Defensive Fortress
    >>= has Defensive Fireproof
    >>= has Mobility Conditioning
    >>= has Mobility Agility
    >>= has Utility Medic
    >>= has Utility Medic
    , num Blaze resist >= 3
    , num Frost advantage >= 1

gives 145 options, sorted by number of remaining slots

    145
    Revolution of Boreus
    Fiery Helm
    Shocking Heart
    Fiery Gauntlets
    Hellplate Greaves
    Any Lantern
    blz:3 fro:-3 sho:1 ter:-1
    has:
    pow:2 mob:1 def:2 utl:1 tec:1
    Fireproof:1 Fortress:1 Conditioning:1 EvasiveFury:1 Medic:1
    after required:
    pow:2 def:1 tec:1
    EvasiveFury:1

    Revolution of Boreus
    Shocking Gaze
    Fiery Breastplate
    Fiery Gauntlets
    Hellplate Greaves
    Any Lantern
    blz:3 fro:-3 sho:1 ter:-1
    has:
    pow:2 mob:1 def:1 utl:2 tec:1
    Fireproof:1 Fortress:1 Conditioning:1 EvasiveFury:1 Medic:1
    after required:
    pow:2 utl:1 tec:1
    EvasiveFury:1

    ...

From there you can refine more, or just look through the results and pick
whatever you like.
