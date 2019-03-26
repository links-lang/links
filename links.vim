syn keyword linksTodo contained TODO FIXME XXX NOTE
syn match linksComment "#.*$" contains=linksTodo

"----------------------------------------------------------------
" Links files (based off http://vim.wikia.com/wiki/Creating_your_own_syntax_files)
" Also some regexes pinched from the Idris vim mode.
"----------------------------------------------------------------

" Regular int like number with - + or nothing in front
syn match linksNumber '\d\+' display
syn match linksNumber '[-+]\d\+' display

" Floating point number with decimal no E or e (+,-)
syn match linksNumber '\d\+\.\d*' display
syn match linksNumber '[-+]\d\+\.\d*' display

syn match linksType '[A-Z][A-Za-z0-9_]*' display
syn match linksIdentifier "[a-z][a-zA-z0-9_]*\('\)*"
syn region linksXml start='<#>' end='</#>'
syn region linksString start='"' end='"'

syn keyword linksKeywords alien as case client database default delete
syn keyword linksKeywords escape for forall from fun formlet
syn keyword linksKeywords in yields insert linfun mu mutual native
syn keyword linksKeywords nu offer orderby op page query readonly
syn keyword linksKeywords receive returning select server set sig spawn
syn keyword linksKeywords spawnAngel spawnClient spawnDemon spawnWait switch
syn keyword linksKeywords table TableHandle typename update values var
syn keyword linksKeywords where with tablekeys

" Library functions which kinda act like keywords
syn keyword linksLibWord addRoute servePages new serveWebsockets spawnAt
syn keyword linksLibWord newAP here send

syn keyword linksModule module open

syn keyword linksConditional if else true false

highlight def link linksXml PreProc
highlight def link linksComment Comment
highlight def link linksNumber Number
highlight def link linksString String
highlight def link linksModule Keyword
highlight def link linksConditional Conditional
highlight def link linksTodo Todo
highlight def link linksKeywords Keyword
highlight def link linksLibWord Identifier
highlight def link linksIdentifier Normal
highlight def link linksType Type
