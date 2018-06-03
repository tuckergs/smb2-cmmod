# smb2-cmmod

## Wat?

This is a program that modifies the challenge mode difficulties in mkb2.main_loop.rel. It can change what levels a difficulty contains, and it could set the maximum alloted time for the levels.

## Compiling

This program is written in Haskell. If you don't already have Haskell, I'd recommend getting Haskell Platform.

You can then compile the code using
```
ghc Main.hs
```

## Usage

The usage is as follows
```
./Main [input REL] [config] [output REL]
```

Many sample configs can be found in the cfgs folder.

## Sample configs

simpleConfig.txt is a config that gives you the difficulties that you see in vanilla SMB2. The entries take up significantly less space than with the vanilla REL

halfTimeConfig.txt keeps the level order the same as in vanilla SMB2, but halves the time alloted for each level. The entries almost take up the whole space alloted for the entries (28 bytes less than max size)

modRelConfig.txt follows the stage ID conventions set forth by the alternative REL that separates the story stage IDs from the challenge stage IDs, and avoids Totalitarianism and Nintendo. I highly recommend that if you use this config, use the alternative REL as your input REL.


## Credits

I would like to thank various people from the SMB Level Workshop for their documentation about SMB2, like miss mecha, CraftSpider, bobjrsenior, et al. I would also like to thank CraftedCart for his repo, smbinfo, that is a compilation of this data.

