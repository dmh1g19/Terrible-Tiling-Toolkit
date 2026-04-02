# The Terrible Tiling Toolkit (TTTT)
A relatively pointless language used to experiment with writing parsers in Haskell! 

Based on our uni group project: https://github.com/dmh1g19/Tiling-Language

The above link contains more nitty gritty detail about the base language.

Thank you to the miso team for making this possible, it otherwise would have been very painful!

Check it out! https://domain-specific-language.web.app/

## DSL Interpreter:
The core of the project is implemented in Haskell. The interpreter reads TSL source code, tokenizes it, parses the grammar, and evaluates the expressions. The evaluation process supports various tile operations like combining, rotating, and scaling.

## Tile Operations:
TSL provides a rich set of operations to manipulate tiles:

## Combining Tiles:
Merge tiles either horizontally or vertically.

## Rotation: 
Rotate tiles by 90°, 180°, or 270°.

## Duplication: 
Duplicate tiles along the horizontal or vertical axis.

## Reflection: 
Reflect tiles across the X axis, Y axis, or the line Y = X.

## Sub-tiling: 
Create sub-tiles by extracting portions of a tile.

# Dual Interfaces:

## Command-Line Interface (CLI): 
Use the traditional CLI to run TSL programs by compiling with GHC.
```
ghc Tsl.hs -o Tsl
```
```
./Tsl < myprogram.tsl
```

## Browser-based Interface: 
An interactive web interface powered by the Miso framework allows you to experiment with TSL code and view tile outputs as SVG.

## Example Programs:
The project includes several pre-loaded examples demonstrating various operations and creative tile designs, from simple checkerboards to complex compositions involving boolean operations and gradients.
