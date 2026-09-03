# Deprecated functions in mapsf

These functions and features still work but will be removed in the next
major version of the package.

### `mf_map` sub-functions

Instead of using the following deprecated functions, one can use
[mf_map](https://riatelab.github.io/mapsf/reference/mf_map.md) with the
corresponding type:

- [`mf_base()`](https://riatelab.github.io/mapsf/reference/mf_base.md)
  =\> **base**

- [`mf_choro()`](https://riatelab.github.io/mapsf/reference/mf_choro.md)
  =\> **choro**

- [`mf_prop()`](https://riatelab.github.io/mapsf/reference/mf_prop.md)
  =\> **prop**

- [`mf_typo()`](https://riatelab.github.io/mapsf/reference/mf_typo.md)
  =\> **typo**

- [`mf_symb()`](https://riatelab.github.io/mapsf/reference/mf_symb.md)
  =\> **symb**

- [`mf_grad()`](https://riatelab.github.io/mapsf/reference/mf_grad.md)
  =\> **grad**

- [`mf_prop_typo()`](https://riatelab.github.io/mapsf/reference/mf_prop_typo.md)
  =\> **prop_typo**

- [`mf_prop_choro()`](https://riatelab.github.io/mapsf/reference/mf_prop_choro.md)
  =\> **prop_choro**

- [`mf_symb_choro()`](https://riatelab.github.io/mapsf/reference/mf_symb_choro.md)
  =\> **symb_choro**

### `mf_init`

`mf_init` is deprecated. It is possible to use `mf_map` instead
(`mf_map(x, type = "base", col = NA, border = NA)`.

It is also possible to use the `extent` argument of
[mf_map](https://riatelab.github.io/mapsf/reference/mf_map.md).

### `mf_export`

`mf_export` is deprecated, use
[mf_png](https://riatelab.github.io/mapsf/reference/mf_png.md) or
[mf_svg](https://riatelab.github.io/mapsf/reference/mf_svg.md) instead.

### `mf_annotation`

`mf_annotation` is deprecated, use
[mf_text](https://riatelab.github.io/mapsf/reference/mf_text.md)
instead.

### Double legends

The use of separated legends for map types **prop_choro**, **prop_typo**
and **symb_choro** is deprecated. Use `leg_pos = NA` and
[mf_legend](https://riatelab.github.io/mapsf/reference/mf_legend.md) if
separated legends are needed.

### Theming system

In [mf_theme](https://riatelab.github.io/mapsf/reference/mf_theme.md),
the following themes are deprecated: "default", "brutal", "ink", "dark",
"agolalight", "candy", "darkula", "iceberg", "green", "nevermind", "jsk"
and "barcelona".  
The following arguments are also deprecated: "bg", "fg", "tab", "pos",
"inner", "line", "cex" and "font".  

Although the map theming system has been radically changed in version
1.0.0 of the package, you can still use the old themes by referencing
them by name. If you need to use the *pre* v1.0.0 default theme, set `x`
to "default".  
If an old theme is set, only deprecated arguments are used and others
are ignored.  
If current and deprecated arguments are mixed, only deprecated arguments
are used and others are ignored.  
All references and usages of the old theming system will be removed in
the next major version.
