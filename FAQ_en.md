# FAQ Frequently Asked Questions

## Q. What are the features of SumibiGPT?

A. It is a modeless Japanese conversion system.

Standard Japanese input systems for Windows and macOS require switching between Japanese input mode on and off, but SumibiGPT converts the previous romanized text to Japanese by pressing the Ctrl-J key in any situation.

## Q. Which OS is compatible with SumibiGPT?

A. It works on Windows/Linux/macOS.

## Q. Which Emacs version does it support?

A. It supports Emacs 28.x or later.

## Q. Is there any additional software required besides Emacs?

A. No, it is not necessary.

## Q. Is an OpenAI account required?

A. Yes, it is required. Please contract for OpenAI API and obtain an API key.

## Q. How much does it cost to use SumibiGPT with OpenAI?

A. If using gpt-3.5-turbo, it costs about 1 to 5 yen to write one document. If changing SumibiGPT settings to GPT-4, the cost is roughly 10 times higher.

## Q. The expected kanji does not appear. Is there any way to convert it properly?

A. The best way is to input a longer document if possible.

Also, gpt-3.5-turbo cannot yet produce the desired kanji, but switching to gpt-4 significantly improves the conversion accuracy. If only gpt-3.5-turbo is available, please create a longer document and convert it.

## Q. If using GPT, it seems possible to translate from English to Japanese. How can I translate it?

A. Instead of romanized text, input Ctrl-J at the end of the English sentence to convert it to a Japanese sentence.

## Q. If using GPT, it seems possible to summarize long documents. How can I do this?

A. Summarizing long documents is not supported in SumibiGPT 1.1. It is planned to be supported in future versions.