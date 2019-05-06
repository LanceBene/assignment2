# Die Funktion pattern_count mit den Variablen "text" und "pattern" wird definiert, 
# nach den geschweiften Klammern folgt, was die Funktion tut. 
# Ziel der Funktion ist es, zu zählen, wie oft eine bestimmte Zeichenfolge im Text vorkommt
pattern_count <- function(text, pattern) {
  # count wird der Wert "0" zugewiesen
  count <- 0
  # pattern_length wird als Anzahl der Zeichen von pattern definiert
  # Referenz nchar: https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/nchar
  pattern_length <- nchar(pattern)
  # Eine for-Schleife wird erstellt, mit den Wiederholungen von 0 bis zur Zahl der Zeichen des textes minus der zuvor definierten pattern_length
  for (i in 0:(nchar(text) - pattern_length)) {
    # Der Text wird von Beginn an nach Zeichenfolgen mit der zuvor definierten pattern_length durchsucht. 
    # Wenn folgende Bedingung erfüllt wird, wird "count" um +1 erhöht: Eine Zeichenfolge entspricht der zuvor definierten "pattern", siehe Funktion unten
    # Referenz substr: https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/substr
    # Referenz rep: https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/rep
    if (substr(rep(text), i + 1, i + pattern_length) == pattern)
      count <- count + 1
  }
  # Der Wert von "count" wird ausgegeben
  return(count)
}


# Die Funktion frequent_word mit den Variablen "text" und "k" wird definiert, nach den geschweiften Klammern folgt, was die Funktion tut
frequent_words <- function(text, k) {
  # frequent_patterns wird als leere Liste definiert
  frequent_patterns <- {}
  # count ebenfalls
  count <- {}
  # Eine for-Schleife wird erstellt, mit den Wiederholungen von 0 bis zur Zahl 
  # der Basen minus der Länge der gesuchten Sequenz im verwendeten Abschnitt
  for (i in 0:(nchar(text) - k)) {
    # Der Vektor "pattern" wird definiert, er enthält fortlaufend alle Elemente von text mit der Länge k
    pattern <- substr(text, i + 1, i + k)
    # Der Vektor "count" wird für jeden Wert von i+1 mit dem Ergebnis der Funktion pattern_count befüllt
    count[i + 1] <- pattern_count(text, pattern)
  }
  # "max_count" wird definiert als höchster Wert des Vektors "count"
  # Referenz max: https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/Extremes
  max_count <- max(count)
  # Eine weitere for-Schleife wird erstellt, Wiederholungen s.o.
  for (i in 0:(nchar(text) - k)) {
    # Wenn ein Wert von count gleich max_count ist...
    if (count[i + 1] == max_count)
      #...wird der entsprechende Ausschnitt aus "text" dem Vektor "frequent_patterns" hinzugefügt
      frequent_patterns <- append(frequent_patterns, substr(text, i + 1, i + k))
  }
  # Der Vektor "frequent_patterns" wird ausgegeben, Duplikate werden dabei entfernt. 
  # Die Funktion gibt einen oder mehrere Ausschnitte aus "text" mit der Länge k aus, welche mit der höchsten Häufigkeit vorkommen.
  # Referenz unique: https://www.rdocumentation.org/packages/base/versions/3.6.0/topics/unique
  return(unique(frequent_patterns))
}

# Text wird mit dem Sample-Dataset von Rosalind definiert
text <- c("ACGTTGCATGTCGCATGATGCATGAGAGCT")
# k mit 4
k <- 4
# Die Funktion frequent_words wird ausgeführt und als result1 gespeichert und sortiert
frequent_words(text, k)
result1 <- frequent_words(text, k)
result1 <- sort(result1)
# Der vorgegebene Output wird in output1 gespeichert
output1 <- c("CATG", "GCAT")
output1 <- sort(output1)
# Das Ergebnis der Funktion ist richtig
result1==output1

# Und nochmal mit dem anderen Datensatz
text2 <- c("CGGAAGCGAGATTCGCGTGGCGTGATTCCGGCGGGCGTGGAGAAGCGAGATTCATTCAAGCCGGGAGGCGTGGCGTGGCGTGGCGTGCGGATTCAAGCCGGCGGGCGTGATTCGAGCGGCGGATTCGAGATTCCGGGCGTGCGGGCGTGAAGCGCGTGGAGGAGGCGTGGCGTGCGGGAGGAGAAGCGAGAAGCCGGATTCAAGCAAGCATTCCGGCGGGAGATTCGCGTGGAGGCGTGGAGGCGTGGAGGCGTGCGGCGGGAGATTCAAGCCGGATTCGCGTGGAGAAGCGAGAAGCGCGTGCGGAAGCGAGGAGGAGAAGCATTCGCGTGATTCCGGGAGATTCAAGCATTCGCGTGCGGCGGGAGATTCAAGCGAGGAGGCGTGAAGCAAGCAAGCAAGCGCGTGGCGTGCGGCGGGAGAAGCAAGCGCGTGATTCGAGCGGGCGTGCGGAAGCGAGCGG")
k2 <- 12
frequent_words(text2, k2)
result2 <- frequent_words(text2, k2)
result2 <- sort(result2)
output2 <- c("CGGCGGGAGATT", "CGGGAGATTCAA", "CGTGCGGCGGGA", "CGTGGAGGCGTG", "CGTGGCGTGCGG", "GCGTGCGGCGGG", "GCGTGGAGGCGT", "GCGTGGCGTGCG", "GGAGAAGCGAGA", "GGAGATTCAAGC", "GGCGGGAGATTC", "GGGAGATTCAAG", "GTGCGGCGGGAG", "TGCGGCGGGAGA")
output2 <- sort(output2)
result2==output2
