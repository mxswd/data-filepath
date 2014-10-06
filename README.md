data-filepath
=============

A type safe file path data structure.

File paths describe a path that can be:

- to a file or a directory
- either be from the root of a system or be relative to something else

We never want to mix up these types but we want to be able to compose values of these types.

The representation we use is a GADT parametric on

- `Path`: what the path is pointing too (file or directory)
- `From`: if the path is relative or absolute

The append function is defined as:

```haskell
(</>) :: FilePath a Directory -> FilePath Relative b -> FilePath a b
p </> RelativePath = p
p </> (DirectoryPath u s) = DirectoryPath (p </> u) s
p </> (FilePath u s) = FilePath (p </> u) s
```

As such, you can only append to a Directory and you can only append relative paths.

The library also provides tools for working with values, such as safe to / from conversion functions
and a quasi-quoter.
