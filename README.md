# smb2-cmmod

## Wat?

This is a program that modifies the challenge mode difficulties in mkb2.main_loop.rel. It can change what levels a difficulty contains, and it could set the maximum alloted time for ANY level

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

simpleConfig.new.txt is a config that gives you the difficulties that you see in vanilla SMB2. The entries take up significantly less space than with the vanilla REL

halfTimeConfig.new.txt keeps the level order the same as in vanilla SMB2, but halves the time alloted for each level. The entries almost take up the whole space alloted for the entries (28 bytes less than max size)

modRelConfig.new.txt follows the stage ID conventions set forth by the alternative REL that separates the story stage IDs from the challenge stage IDs, and avoids Totalitarianism and Nintendo. It is intended to be a template for your use. I highly recommend that if you use this config, use the alternative REL as your input REL.

stupidConfig.new.txt shows more of what my config format can do. It shows you that you could make your difficulties have arbitrary size, although there are problems with making a difficulty bigger than normal; I found that after playing my fifteen level Advanced Extra, I had five levels of Expert Extra unlocked when I hadn't touched Expert. It also shows that you could specify the entries for Beginner, Advanced, and Advanced Extra and not specify the other difficulties. If you don't specify a difficulty, then that difficulty uses the first entry list such that there is a difficulty declaration that mentions its name. Order is in respect to the position of the entry list in the config. Separately, you could also manually set two difficulties to point to the same entry list

## Notes

There is a limited amount of space that could be used for the challenge mode entry area. The size of that area is 0x3E3C bytes; a normal entry, with no time alloted portion, is 0x54 bytes while each time alloted portion is 0x1C bytes. Also, the end of entries portion for a difficulty is 0x1C bytes. Therefore, the formula for calculating the size needed is 0x54\*(number of levels) + 0x1C\*(number of levels with time alloted portions) + 0x1C\*(number of entry lists that have a difficulty declaration associated with them). My half time config uses 0x3E20 space while my simple config uses 0x2F08 space.

## Credits

I would like to thank various people from the SMB Level Workshop for their documentation about SMB2, like miss mecha, CraftSpider, bobjrsenior, et al. I would also like to thank CraftedCart for his repo, smbinfo, that is a compilation of this data.

