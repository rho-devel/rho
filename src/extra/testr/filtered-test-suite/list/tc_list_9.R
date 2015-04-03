expected <- eval(parse(text="structure(list(label = \"\", x = structure(0.5, unit = \"npc\", valid.unit = 0L, class = \"unit\"), y = structure(0.5, unit = \"npc\", valid.unit = 0L, class = \"unit\"), just = \"centre\", hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE, name = NULL, gp = structure(list(), class = \"gpar\"), vp = NULL), .Names = c(\"label\", \"x\", \"y\", \"just\", \"hjust\", \"vjust\", \"rot\", \"check.overlap\", \"name\", \"gp\", \"vp\"))"));         
test(id=0, code={         
argv <- eval(parse(text="list(label = \"\", x = structure(0.5, unit = \"npc\", valid.unit = 0L, class = \"unit\"), y = structure(0.5, unit = \"npc\", valid.unit = 0L, class = \"unit\"), just = \"centre\", hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE, name = NULL, gp = structure(list(), class = \"gpar\"), vp = NULL)"));         
do.call(`list`, argv);         
}, o=expected);         

