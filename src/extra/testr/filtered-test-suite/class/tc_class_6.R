expected <- eval(parse(text="\"viewport\""));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(list(x = structure(0.5, unit = \"npc\", valid.unit = 0L, class = \"unit\"), y = structure(0.5, unit = \"npc\", valid.unit = 0L, class = \"unit\"), width = structure(1, unit = \"npc\", valid.unit = 0L, class = \"unit\"), height = structure(1, unit = \"npc\", valid.unit = 0L, class = \"unit\"), justification = \"centre\", gp = structure(list(), class = \"gpar\"), clip = TRUE, xscale = c(-15.89, 356.89), yscale = c(0.683750615306643, 5.8340977374556), angle = 0, layout = NULL, layout.pos.row = c(21L, 21L), layout.pos.col = c(17L, 17L), valid.just = c(0.5, 0.5), valid.pos.row = c(21L, 21L), valid.pos.col = c(17L, 17L), name = \"plot_01.panel.3.1.vp\"), .Names = c(\"x\", \"y\", \"width\", \"height\", \"justification\", \"gp\", \"clip\", \"xscale\", \"yscale\", \"angle\", \"layout\", \"layout.pos.row\", \"layout.pos.col\", \"valid.just\", \"valid.pos.row\", \"valid.pos.col\", \"name\"), class = \"viewport\"))"));              
do.call(`class`, argv);              
}, o=expected);              

