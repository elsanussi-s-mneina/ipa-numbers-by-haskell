
# Runs module named Haskell, without warning about tabs.
# Passes in the flag to convert from numbers to characters.

echo "Enter numbers like 123 124, separated by spaces"
echo "They will be converted to characters of the international phonetic alphabet"
runghc -Wno-tabs Main.hs --nc
