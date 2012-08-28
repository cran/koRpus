# this is an internal file providing language support.
# please refer to inst/README.languages for details

set.lang.support("hyphen",
	list(
		"it"="it"
	)
)

set.lang.support("treetag",
	list("it-utf8"=list(
		## preset: "it-utf8"
		# tags "utf-8" encoded text files
		# Alberto Mirisola added this Italian section
		lang="it",
		encoding="UTF-8",
		preset=function(TT.cmd, TT.bin, TT.lib, unix.OS){
			if(isTRUE(unix.OS)){
				# preset for unix systems
				TT.abbrev		<- file.path(TT.lib, "italian-abbreviations")
				return(
					list(
						TT.tokenizer 			= file.path(TT.cmd, "utf8-tokenize.perl"),
						TT.tagger 				= file.path(TT.bin, "tree-tagger"),
						TT.abbrev 				= TT.abbrev,
						TT.params 				= file.path(TT.lib, "italian-utf8.par"),

						TT.tknz.opts.def		= c(),
						TT.tknz.opts 			= paste("-a", TT.abbrev),
						TT.lookup.command		= c(),
						TT.filter.command		= c()
					)
				)
			} else {
				# preset for windows systems
				TT.abbrev		<- file.path(TT.lib, "italian-abbreviations")
				return(
					list(
						TT.tokenizer 			= file.path(TT.cmd, "utf8-tokenize.perl"),
						TT.tagger 				= file.path(TT.bin, "tree-tagger.exe"),
						TT.abbrev				= TT.abbrev,
						TT.params				= file.path(TT.lib, "italian-utf8.par"),

						TT.tknz.opts.def		= c(),
						TT.tknz.opts			= paste("-a", TT.abbrev),
						TT.lookup.command		= c(),
						TT.filter.command		= c()
					)
				)
			}
		}),
		"it"=list(
			# tags "latin1" encoded text files
			lang="it",
			encoding="Latin1",
			preset=function(TT.cmd, TT.bin, TT.lib, unix.OS){
				if(isTRUE(unix.OS)){
					# preset for unix systems
					TT.abbrev <- file.path(TT.lib, "italian-abbreviations")
					return(
						list(
							TT.tokenizer			= file.path(TT.cmd, "tokenize.pl"),
							TT.tagger				= file.path(TT.bin, "tree-tagger"),
							TT.abbrev				= TT.abbrev,
							TT.params				= file.path(TT.lib, "italian.par"),

							TT.tknz.opts.def		= c(),
							TT.tknz.opts			= paste("-a", TT.abbrev),
							TT.lookup.command		= c(),
							TT.filter.command		= c()
						)
					)
				} else {
					# preset for windows systems
					TT.abbrev <- file.path(TT.lib, "italian-abbreviations")
					return(
						list(
							TT.tokenizer			= file.path(TT.cmd, "tokenize.pl"),
							TT.tagger				= file.path(TT.bin, "tree-tagger.exe"),
							TT.abbrev				= TT.abbrev,
							TT.params				= file.path(TT.lib, "italian.par"),

							TT.tknz.opts.def		= c(),
							TT.tknz.opts			= paste("-a", TT.abbrev),
							TT.lookup.command		= c(),
							TT.filter.command		= c()
						)
					)
				}
			}
		)
	)
)

set.lang.support("kRp.POS.tags",
	## tag and class definitions
	# it -- italian
	# see http://www.ims.uni-stuttgart.de/ftp/pub/corpora/italian-tagset.txt
	# Alberto Mirisola added these Italian tags
	list("it"=list(
		tag.class.def.words=matrix(c(
			"ABR", "abbreviation", "Abbreviation",
			"ADJ", "adjective", "Adjective",
			"ADV", "adverb", "Adverb",
			"CON", "conjunction", "Conjunction",
			"DET:def", "article", "Definite article",
			"DET:indef", "article", "Indefinite article",
			"INT", "interjection", "Interjection",
			"NOM", "noun", "Noun",
			"NPR", "name" ,"Proper noun",
			"NUM", "number", "Number",
			"ORD", "number", "Ordinal number",
			"PRE", "preposition", "Preposition",
			"PRE:det", "preposition", "Preposition + article",
			"PRO", "pronoun", "Pronoun",
			"PRO:demo", "pronoun", "Demonstrative pronoun",
			"PRO:indef", "pronoun", "Indefinite pronoun",
			"PRO:inter", "pronoun", "Interrogative pronoun",
			"PRO:pers", "pronoun", "Personal pronoun",
			"PRO:poss", "pronoun", "Possessive pronoun",
			"PRO:refl", "pronoun", "Reflexive pronoun",
			"PRO:rela", "pronoun", "Relative pronoun",
			"SYM", "symbol", "Symbol",
			"VER:cimp", "verb", "Verb conjunctive imperfect",
			"VER:cond", "verb", "Verb conditional",
			"VER:cpre", "verb", "Verb conjunctive present",
			"VER:futu", "verb", "Verb future tense",
			"VER:geru", "verb", "Verb gerund",
			"VER:impe", "verb", "Verb imperative",
			"VER:impf", "verb", "Verb imperfect",
			"VER:infi", "verb", "Verb infinitive",
			"VER:pper", "verb", "Verb participle perfect",
			"VER:ppre", "verb", "Verb participle present",
			"VER:pres", "verb", "Verb present",
			"VER:refl:infi", "verb", "Verb reflexive infinitive",
			"VER:remo", "verb", "Verb simple past"
			), ncol = 3, byrow = TRUE, dimnames = list(c(), c("tag", "wclass", "desc"))),
		tag.class.def.punct=matrix(c(
			"PON", "punctuation", "Punctuation"
			), ncol = 3, byrow = TRUE, dimnames = list(c(), c("tag", "wclass", "desc"))),
		tag.class.def.sentc=matrix(c(
			"SENT", "fullstop", "Sentence ending punctuation"
			), ncol = 3, byrow = TRUE, dimnames = list(c(), c("tag", "wclass", "desc")))
		)
	)
)
