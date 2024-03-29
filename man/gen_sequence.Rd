% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/system_splines.R
\name{gen_sequence}
\alias{gen_sequence}
\title{Generate text sequences with various properties.}
\usage{
gen_sequence(
  seed = NULL,
  method = "lipsum",
  scale = "sentence",
  n_paragraph = 1,
  n_sentence = 1,
  n_word = 3:5,
  n_chr = 3:8
)
}
\arguments{
\item{seed}{random seed for the character sequence.}

\item{method}{method used for sequence generation
\itemize{
\item lipsum, generate long sentences latin-based text, with the option to subset the text into smaller elements (scale argument).
\item sentence, draw short sentences of comparable length from the Revised List of Phonetically Balanced Sentences \href{https://www.cs.columbia.edu/~hgs/audio/harvard.html}{Harvard Sentences}
\item random, recurvively generate sentences from random length, with words of given size range by randomly sampling letters.
}}

\item{scale}{scale of the returned elements, for the lipsum method. Control how the generated text is
divided into word, sentence, or paragraph.}

\item{n_paragraph, n_sentence, n_word, n_chr}{number of elements at each organizational level, i.e. number of paragraphs, sentences, words, and characters. The last two levels can be specified as a vector of length to sample in.}
}
\value{
a character string.
}
\description{
Generate text sequences with various properties.
}
