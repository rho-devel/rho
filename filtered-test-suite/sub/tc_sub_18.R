expected <- eval(parse(text="\"UTF-8\""));               
test(id=0, code={               
argv <- eval(parse(text="list(\".*Content-Type:[^\\\\]*charset=([^\\\\[:space:]]*)[[:space:]]*\\\\\\\\n.*\", \"\\\\1\", \"Project-Id-Version: lattice 0.20-10\\\\nReport-Msgid-Bugs-To: bugs@r-project.org\\\\nPOT-Creation-Date: 2012-03-10 14:42\\\\nPO-Revision-Date: 2012-08-31 16:36+0100\\\\nLast-Translator: \\305\\201ukasz Daniel <lukasz.daniel@gmail.com>\\\\nLanguage-Team: \\305\\201ukasz Daniel <lukasz.daniel@gmail.com>\\\\nLanguage: pl_PL\\\\nMIME-Version: 1.0\\\\nContent-Type: text/plain; charset=UTF-8\\\\nContent-Transfer-Encoding: 8bit\\\\nPlural-Forms: nplurals=3; plural=(n==1 ? 0 : n%10>=2 && n%10<=4 && (n%100<10 || n%100>=20) ? 1 : 2)\\\\nX-Poedit-SourceCharset: iso-8859-1\\\\n\", FALSE, FALSE, FALSE, FALSE)"));               
.Internal(sub(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));               
}, o=expected);               

