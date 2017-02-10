# Spell check for Clojure comments
grep -rin ";;" ok.clj | cut -d ";" -f3 | hunspell -d en_US
