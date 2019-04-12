# smb2-cmmod

## Wat?

This is a program that modifies the challenge mode difficulties in mkb2.main_loop.rel. It can change what levels a difficulty contains, and it could set the maximum alloted time for ANY level

## Compiling

This program is written in Haskell. If you don't already have Haskell, I'd recommend getting Haskell Platform.

I have a simple Makefile. You can compile the code using
```
make
```

As per the L2 norm, you can also run
```
make clean
```
to get rid of the executable and compilation files

If you don't have make, you can alternatively run
```
ghc Main.hs
```

## Usage

The usage is as follows
```
./Main [input REL] [config] [output REL]
```
You would replace the "./Main" with "Main.exe" if you are using Windows prompts

Many sample configs can be found in the cfgs folder.

## Sample configs

simpleConfig.new.txt is a config that gives you the difficulties that you see in vanilla SMB2. The entries take up significantly less space than with the vanilla REL

modRelConfig.new.txt follows the stage ID conventions set forth by the alternative REL that separates the story stage IDs from the challenge stage IDs, and avoids Totalitarianism and Nintendo. It is intended to be a template for your use. I highly recommend that if you use this config, use the alternative REL as your input REL.

stupidConfig.new.txt shows more of what my config format can do. It shows you that you could make your difficulties have arbitrary size, although there are problems with making a difficulty bigger than normal; I found that after playing my fifteen level Advanced Extra, I had five levels of Expert Extra unlocked when I hadn't touched Expert. It also shows that you could specify the entries for Beginner, Advanced, and Advanced Extra and not specify the other difficulties. If you don't specify a difficulty, then that difficulty uses the first entry list such that there is a difficulty declaration that mentions its name. Order is in respect to the position of the entry list in the config. Separately, you could also manually set two difficulties to point to the same entry list

testJumpDistanceSlots.new.txt shows you that you could specify a range of level slots that let you use goals that have arbitrary warp distance; that is, these slots allow you to make a red goal that allows you to warp from Level 18 to Level 25. Note that in these slots, you must specify every goal in the level. Read docs/cmentries.txt before using jump distance slots

bareBonesSimpleConfig.new.txt writes the standard SMB2 difficulties, but with barebone entries. Check out my smb2-relmod repo for more details on those, and read docs/cmentries.txt before using these

sampleStableBareBoneConfig.new.txt is a sample config that shows you that you can extend the difficulties without requiring a different save file with stable barebone entries, as opposed to normal, optimized barebone entries, which requires a different save file. Note that you could only extend the difficulties by 20 levels. As above, read docs/cmentries.txt in my smb2-relmod repo before using

halfTimeConfig.new.txt halves the times for all of the levels. This config makes cmmod output stable barebone entries, so you should read docs/cmentries.txt in my smb2-relmod repo before using

## Notes

There is a limited amount of space that could be used for the challenge mode entry area. The size of that area is 0x3C7C bytes; a normal entry, with no time alloted portion, is 0x54 bytes while each time alloted portion is 0x1C bytes. Also, the end of entries portion for a difficulty is 0x1C bytes. Therefore, the formula for calculating the size needed is 0x54\*(number of levels) + 0x1C\*(number of levels with time alloted portions) + 0x1C\*(number of entry lists that have a difficulty declaration associated with them). Barebone entries are always 8 bytes long, and end of entries portions for barebone entries is also 8 bytes. My simple config uses 0x2F08 space.

Before specifying jump distance or using barebone entries, you should read docs/cmentries.txt

## Credits

I would like to thank various people from the SMB Level Workshop for their documentation about SMB2, like miss mecha, CraftSpider, bobjrsenior, et al. I would also like to thank CraftedCart for his repo, smbinfo, that is a compilation of this data.

