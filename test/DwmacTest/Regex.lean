import DwmacCore.Regex

/-! Regular expressions: found anywhere in the string, ignoring case -/

namespace DwmacCore.Regex.Test

#guard search "LG" "LG UltraFine" && search "lg" "LG UltraFine" && !search "Dell" "LG UltraFine"
#guard search "^Built-in" "Built-in Retina Display" && !search "^Retina" "Built-in Retina Display"
#guard search "Display$" "Built-in Retina Display" && !search "Retina$" "Built-in Retina Display"
#guard search "LG.*Fine" "LG UltraFine" && search "a.c" "abc" && !search "a.c" "ac"
#guard search "colou?r" "color" && search "colou?r" "colour" && !search "colou?r" "colouur"
#guard search "^a+$" "aaa" && !search "^a+$" "" && search "^a*$" ""
#guard search "^a{2}$" "aa" && !search "^a{2}$" "aaa" && search "^a{2,}$" "aaaa" && !search "^a{2,3}$" "aaaa"
#guard search "^(ab)+$" "ababab" && !search "^(ab)+$" "aba" && search "^(?:ab|cd)+$" "abcdab"
#guard search "cat|dog" "hotdog" && !search "^(cat|dog)$" "hotdog"
#guard search "^[a-c]+$" "abcCBA" && !search "^[a-c]+$" "abd" && search "^[^0-9]+$" "abc" && !search "[^0-9]" "123"
#guard search "\\d+" "Monitor 2" && !search "\\d" "Monitor" && search "^\\w+$" "ab_1" && search "a\\sb" "a b"
#guard search "\\." "a.b" && !search "\\." "ab" && search "^\\D+$" "ab" && search "[\\d]" "x9"
#guard search "^(a*)*$" "aaa" && search "^(a|)+$" "aa" -- empty repetitions terminate
#guard (parse "(ab").toOption.isNone && (parse "ab)").toOption.isNone && (parse "*a").toOption.isNone
  && (parse "[b-a]").toOption.isNone && (parse "a{3,1}").toOption.isNone

end DwmacCore.Regex.Test
