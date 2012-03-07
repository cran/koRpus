// TODO: show sysllables in freq results, if available

function preprocess () {
	// we need the main package
	echo("require(koRpus)\n\n");
}

function calculate () {
	// let's read all values into php variables for the sake of readable code
	// tagging vars
	var radCustomTT					= getValue("radCustomTT");
	var TTroot							= getValue("TTroot");
	var TTpreset						= getValue("drpPreset");
	var txtToTag						= getValue("txtToTag");
	var varTTCustom					= getValue("varTTCustom");
	var chkDetectHeadlines			= getValue("chkDetectHeadlines");
	var chkDetectParagraphs			= getValue("chkDetectParagraphs");
	var chkDetect = new Array();
	chkDetect.push(chkDetectParagraphs, chkDetectHeadlines);
	chkDetect = chkDetect.filter(String);
	if(chkDetect.length > 0) {
		var tknzDetect = ", detect=c("+chkDetect.join(", ")+")";
	} else {
		var tknzDetect = "";
	}

	if(TTpreset == "de" || TTpreset == "de-utf8") {
		var TTlang = "de";
	} else if(TTpreset == "es" || TTpreset == "es-utf8") {
		var TTlang = "es";
	} else {
		var TTlang = TTpreset;
	}

	// readability vars
	var chkReadability				= getValue("chkReadability");
	var chkFlesch						= getValue("Flesch");
	var chkFleschKincaid				= getValue("FleschKincaid");
	var chkFarrJenkinsPaterson		= getValue("FarrJenkinsPaterson");
	var chkFOG							= getValue("FOG");
	var chkColemanLiau				= getValue("ColemanLiau");
	var chkSMOG							= getValue("SMOG");
	var chkLIX							= getValue("LIX");
	var chkRIX							= getValue("RIX");
	var chkWheelerSmith				= getValue("WheelerSmith");
	var chkFORCAST						= getValue("FORCAST");
	var chkARI							= getValue("ARI");
	var chkStrain						= getValue("Strain");
	var chkLinsearWrite				= getValue("LinsearWrite");
	var chknWS							= getValue("nWS");
	var chkColeman						= getValue("Coleman");
	var chkDaleChall					= getValue("DaleChall");
	var chkBormuth						= getValue("Bormuth");
	var chkDRP							= getValue("DRP");
	var chkSpache						= getValue("Spache");
	var chkELF							= getValue("ELF");
	var chkDickesSteiwer				= getValue("DickesSteiwer");
	var chkAmdahl						= getValue("Amdahl");
	var chkTRI							= getValue("TRI");
	var chkFucks						= getValue("Fucks");
	var chkDanielsonBryan			= getValue("DanielsonBryan");
	var chkARINRI						= getValue("ARINRI");
	var chkFleschPSK					= getValue("FleschPSK");
	var chkFarrJenkinsPatersonPSK	= getValue("FarrJenkinsPatersonPSK");
	var chkFOGPSK						= getValue("FOGPSK");
	var chkFOGNRI						= getValue("FOGNRI");
	var chkQu							= getValue("Qu");
	var chkWheelerSmithDe			= getValue("WheelerSmithDe");
	var chkDaleChallPSK				= getValue("DaleChallPSK");
	var chkDaleChallOld				= getValue("DaleChallOld");
	var chkSpacheOld					= getValue("SpacheOld");
	var WLldc							= getValue("readbWLldc");
	var WLsdc							= getValue("readbWLsdc");
	var readabIndices = new Array();
	readabIndices.push(chkFlesch, chkFleschPSK, chkFleschKincaid, chkFarrJenkinsPaterson,
		chkFarrJenkinsPatersonPSK, chkFOG, chkFOGPSK, chkFOGNRI, chkColemanLiau,
		chkSMOG, chkQu, chkLIX, chkRIX, chkWheelerSmith, chkWheelerSmithDe, chkFORCAST,
		chkARI, chkARINRI, chkStrain, chkLinsearWrite, chknWS, chkColeman,
		chkDaleChall, chkDaleChallPSK, chkDaleChallOld, chkBormuth, chkDRP, 
		chkSpache, chkSpacheOld, chkELF, chkDickesSteiwer, chkAmdahl, chkTRI, chkFucks, chkDanielsonBryan);
	readabIndices = readabIndices.filter(String);

	// hyphenation vars
	var chkCustomHyph					= getValue("chkCustomHyph");
	var radCustomHyph					= getValue("radCustomHyph");
	var varHyphenCustom				= getValue("varHyphenCustom");

	// lexical diversity vars
	var chkLexDiv						= getValue("chkLexDiv");
	var chkTTR							= getValue("TTR");
	var chkMSTTR						= getValue("MSTTR");
	var chkCld							= getValue("Cld");
	var chkRld							= getValue("Rld");
	var chkCTTR							= getValue("CTTR");
	var chkUld							= getValue("Uld");
	var chkSld							= getValue("Sld");
	var chkKld							= getValue("Kld");
	var chkMaas							= getValue("Maas");
	var chkHDD							= getValue("HDD");
	var chkMTLD							= getValue("MTLD");
	var chkTTRChar						= getValue("TTRChar");
	var chkCldChar						= getValue("CldChar");
	var chkRldChar						= getValue("RldChar");
	var chkCTTRChar					= getValue("CTTRChar");
	var chkUldChar						= getValue("UldChar");
	var chkSldChar						= getValue("SldChar");
	var chkKldChar						= getValue("KldChar");
	var chkMaasChar					= getValue("MaasChar");
	var chkHDDChar						= getValue("HDDChar");
	var chkMTLDChar					= getValue("MTLDChar");
	var showTypes						= getValue("showTypes");
	var lexDivIndices = new Array();
	lexDivIndices.push(chkTTR, chkMSTTR, chkCld, chkRld, chkCTTR, chkUld,
		chkSld, chkKld, chkMaas, chkHDD, chkMTLD);
	lexDivIndices = lexDivIndices.filter(String);
	var lexDivChars = new Array();
	lexDivChars.push(chkTTRChar, chkCldChar, chkRldChar, chkCTTRChar,
		chkUldChar, chkSldChar, chkKldChar, chkMaasChar, chkHDDChar, chkMTLDChar);
	lexDivChars = lexDivChars.filter(String);
	
	// frequency analysis vars
	var chkFreq							= getValue("chkFreq");
	var drpFreqDB						= getValue("drpFreqDB");
	var corpDBdir						= getValue("corpDBdir");
	var inpCelexRunWd					= getValue("inpCelexRunWd");
	var varCorpFreqObj				= getValue("varCorpFreqObj");

	// this is all TreeTagger configuration, just like in its own shell scripts
/*  var TTbin				= TTroot+"/bin";
  var TTcmd				= TTroot+"/cmd";
  var TTlib				= TTroot+"/lib";
  var TToptions		= "-token -lemma -sgml -pt-with-lemma";
  var TTtokenizer		= TTcmd+"/tokenize.pl";
  var TTtagger			= TTbin+"/tree-tagger";
  var TTabbrev			= TTlib+"/german-abbreviations-utf8";
  var TTparams			= TTlib+"/german-utf8.par";
  var TTlexicon		= TTlib+"/german-lexicon-utf8.txt";
  var TTfilter			= TTcmd+"/filter-german-tags";
  var TTlookup			= "perl "+TTcmd+"/lookup.perl "+TTlexicon;
*/

	if(radCustomTT == "file"){
		echo("tagged.text.obj <- tokenize(\""+txtToTag+"\", lang=\""+TTlang+"\""+tknzDetect+")\n");
	} else if(radCustomTT == "fileTreeTagger"){
		echo("tagged.text.obj <- treetag(\""+txtToTag+"\", treetagger=\"manual\", lang=\""+TTlang+"\", TT.options=list(path=\""+TTroot+"\", preset=\""+TTpreset+"\"))\n");
	} else if(radCustomTT == "object"){
		echo("tagged.text.obj <- "+varTTCustom+"\n");
	} else {};

	if(chkCustomHyph && radCustomHyph == "custom" && varHyphenCustom){
		echo("hyphenated.text.obj <- suppressMessages(correct.hyph("+varHyphenCustom+"))\n");
		var localHyphObject	= "hyphen=hyphenated.text.obj, ";
	} else {
		var localHyphObject	= "";
	};

	if(chkCustomHyph && radCustomHyph == "auto"){
		echo("hyphenated.text.obj <- hyphen(tagged.text.obj, hyph.pattern=\""+TTlang+"\", quiet=TRUE)\n");
		var localHyphObject	= "hyphen=hyphenated.text.obj, ";
	} else {
		var localHyphObject	= "";
	};

	if(chkReadability && readabIndices){
		// check for word lists, if needed
		var readabWordLists =  new Array();
		if(chkDaleChall && WLldc){
			readabWordLists.push("Dale.Chall=\""+WLldc+"\"");
		} else {};
		if(chkBormuth && WLldc){
			readabWordLists.push("Bormuth=\""+WLldc+"\"");
		} else {};
		if(chkSpache && WLsdc){
			readabWordLists.push("Spache=\""+WLsdc+"\"");
		} else {};
		if(readabWordLists.length > 0){
			var addWordLists = ", word.lists=c("+readabWordLists.join(", ")+")";
		} else {
			var addWordLists = "";
		};
		
		echo("readability.obj <- readability(tagged.text.obj, "+localHyphObject+"index=c(\""+readabIndices.join("\", \"")+"\")"+addWordLists+", quiet=TRUE)\n");
	} else {};

	if(chkLexDiv && lexDivIndices){
		if(lexDivChars.length > 0){
			var calcChars = "\"), char=c(\""+lexDivChars.join("\", \"");
		} else {
			var calcChars = "";
		}
		if(showTypes){
			var keepTypes = ", keep.tokens=TRUE";
		} else {
			var keepTypes = "";
		}
		echo("lexical.diversity.obj <- lex.div(tagged.text.obj, measure=c(\""+lexDivIndices.join("\", \"")+calcChars+"\")"+keepTypes+", quiet=TRUE)\n");
	} else {};

	if(chkFreq){
		if(drpFreqDB == "LCC" && corpDBdir){
			echo("corp.freq.obj <- read.corp.LCC(\""+corpDBdir+"\")\n");
			var addCorpFreq = ", corp.freq=corp.freq.obj";
		} else if(drpFreqDB == "celex" && corpDBdir && inpCelexRunWd){
			echo("corp.freq.obj <- read.corp.celex(\""+corpDBdir+", running.words="+inpCelexRunWd+"\")\n");
			var addCorpFreq = ", corp.freq=corp.freq.obj";
		} else if(drpFreqDB == "corpObj" && varCorpFreqObj){
			echo("corp.freq.obj <- "+varCorpFreqObj+"\n");
			var addCorpFreq = ", corp.freq=corp.freq.obj";
		} else {
			var addCorpFreq = "";
		};
		echo("frequency.analysis.obj <- kRp.freq.analysis(tagged.text.obj"+addCorpFreq+")\n");
	} else {};
	
	echo("\n");
}
		
function printout () {
	// save options
	var radCustomTT			= getValue("radCustomTT");
	var saveTagged				= getValue("saveTaggedText.active");
	var saveTaggedName		= getValue("saveTaggedText");
	var saveTaggedEnv			= getValue("saveTaggedText.parent");
	var chkReadability		= getValue("chkReadability");
	var saveReadb				= getValue("saveReadb.active");
	var saveReadbName			= getValue("saveReadb");
	var saveReadbEnv			= getValue("saveReadb.parent");
	var chkCustomHyph			= getValue("chkCustomHyph");
	var radCustomHyph			= getValue("radCustomHyph");
	var saveHyphen				= getValue("saveHyphen.active");
	var saveHyphenName		= getValue("saveHyphen");
	var saveHyphenEnv			= getValue("saveHyphen.parent");
	var chkLexDiv				= getValue("chkLexDiv");
	var saveLexDiv				= getValue("saveLexDiv.active");
	var saveLexDivName		= getValue("saveLexDiv");
	var saveLexDivEnv			= getValue("saveLexDiv.parent");
	var chkFreq					= getValue("chkFreq");
	var saveFreq				= getValue("saveFreq.active");
	var saveFreqName			= getValue("saveFreq");
	var saveFreqEnv			= getValue("saveFreq.parent");
	var saveCorpFreq			= getValue("saveCorpFreq.active");
	var saveCorpFreqName		= getValue("saveCorpFreq");
	var saveCorpFreqEnv		= getValue("saveCorpFreq.parent");
	
	// show options
	var showTagged				= getValue("showTagged");
	var showHyphenation		= getValue("showHyphenation");
	var showTypes				= getValue("showTypes");
	var showFreqWClasses		= getValue("showFreqWClasses");

	echo("rk.header(\"Text analysis results\", level=2)\n");
	if(showTagged){
		echo("rk.header(\"POS Tagging\", level=3)\n"+
		"rk.print(tagged.text.obj@TT.res)\n");
	} else {};
	if(chkCustomHyph && showHyphenation){
		echo("rk.header(\"Hyphenation\", level=3)\n"+
		"rk.print(hyphenated.text.obj@hyphen)\n");
	} else {};
	if(chkReadability){
		echo("rk.header(\"Readability\", level=3)\n"+
			"rk.print(summary(readability.obj))\n");
	} else {};
	if(chkLexDiv){
		echo("rk.header(\"Lexical diversity\", level=3)\n"+
		"rk.print(summary(lexical.diversity.obj))\n");
		if(showTypes){
			echo("rk.header(\"Identified types in text\", level=4)\n"+
			"rk.print(lexical.diversity.obj@tt[[\"types\"]])\n");
		} else {};

	} else {};
	if(chkFreq){
		echo("rk.header(\"Frequency analysis\", level=4)\n"+
			"rk.print(summary(frequency.analysis.obj))\n");
		if(showFreqWClasses){
			echo("rk.header(\"Frequencies of word classes\", level=4)\n"+
				"rk.print(data.frame(freq=frequency.analysis.obj@desc$freq.wclass))\n");
		} else {};
	} else {};

	if (radCustomTT == "file" && saveTagged) {
		echo("assign(\""+saveTaggedName+"\", tagged.text.obj, envir="+saveTaggedEnv+")\n");
	}
	if (radCustomHyph == "auto" && saveHyphen) {
		echo("assign(\""+saveHyphenName+"\", hyphenated.text.obj, envir="+saveHyphenEnv+")\n");
	}
	if (saveReadb && chkReadability) {
		echo("assign(\""+saveReadbName+"\", readability.obj, envir="+saveReadbEnv+")\n");
	}
	if (saveLexDiv && chkLexDiv) {
		echo("assign(\""+saveLexDivName+"\", lexical.diversity.obj, envir="+saveLexDivEnv+")\n");
	}
	if (saveCorpFreq && chkFreq) {
		echo("assign(\""+saveCorpFreqName+"\", corp.freq.obj, envir="+saveCorpFreqEnv+")\n");
	}
	if (saveFreq && chkFreq) {
		echo("assign(\""+saveFreqName+"\", frequency.analysis.obj, envir="+saveFreqEnv+")\n");
	}
}