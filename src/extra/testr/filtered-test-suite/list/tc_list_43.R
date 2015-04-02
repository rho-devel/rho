expected <- eval(parse(text="structure(list(raster = structure(\"#000000\", .Dim = c(1L, 1L), class = \"raster\"), x = structure(0, unit = \"npc\", valid.unit = 0L, class = \"unit\"), y = structure(0.5, unit = \"npc\", valid.unit = 0L, class = \"unit\"), width = NULL, height = NULL, just = \"centre\", hjust = NULL, vjust = NULL, interpolate = TRUE, name = NULL, gp = structure(list(), class = \"gpar\"), vp = NULL), .Names = c(\"raster\", \"x\", \"y\", \"width\", \"height\", \"just\", \"hjust\", \"vjust\", \"interpolate\", \"name\", \"gp\", \"vp\"))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(raster = structure(\"#000000\", .Dim = c(1L, 1L), class = \"raster\"), x = structure(0, unit = \"npc\", valid.unit = 0L, class = \"unit\"), y = structure(0.5, unit = \"npc\", valid.unit = 0L, class = \"unit\"), width = NULL, height = NULL, just = \"centre\", hjust = NULL, vjust = NULL, interpolate = TRUE, name = NULL, gp = structure(list(), class = \"gpar\"), vp = NULL)"));                  
do.call(`list`, argv);                  
}, o=expected);                  

