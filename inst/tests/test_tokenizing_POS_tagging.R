# testing basic tokenizing and POS tagging

context("environment")

test_that("setting environment variables", {
	# we cannot really test the treetag function without a local TreeTagger installation,
	# however, we can check if setting the environment works as expected
	set.kRp.env(TT.cmd="manual", lang="en", TT.options=list(path=".", preset="en"))

	expect_that(get.kRp.env(TT.cmd=TRUE),
		matches("manual"))
	expect_that(get.kRp.env(lang=TRUE),
		matches("en"))
	expect_that(get.kRp.env(TT.options=TRUE),
		is_identical_to(list(path=".", preset="en")))
})


context("tokenizing")

test_that("basic tokenizing", {
	sampleTextFile <- normalizePath("sample_text.txt")
	sampleTextStandard <- dget("sample_text_tokenized_dput.txt")
	sampleTextObj <- readLines(sampleTextFile)

	# without a local TreeTagger installation, these tests will be limited
	# to what is possible with tokenize()
	tokenizedTextFile <- tokenize(sampleTextFile, lang="en")
	tokenizedTextObj <- tokenize(sampleTextObj, format="obj", lang="en")

	textToTag <- file(sampleTextFile)
	tokenizedTextConnection <- tokenize(textToTag, lang="en")
	close(textToTag)

	# we can't compare with "is_identical_to() because the percentages may slightly differ
	expect_that(tokenizedTextFile,
		equals(sampleTextStandard))
	expect_that(tokenizedTextObj,
		equals(sampleTextStandard))
	expect_that(tokenizedTextConnection,
		equals(sampleTextStandard))

})
