; # Win
; ! Alt
; ^ Control
; + Shift
; & Combine two and only two keys into a custom hotkey
; $ Don't use when called from other hotkey, avoid recursion
; < left key, e.g. <^ is left control
; > right key

$<^p:: Send, {up}
$<^n:: Send, {down}
$<^b:: Send, {left}
$<^f:: Send, {right}
$<^a:: Send, {home}
$<^e:: Send, {end}
$<^g:: Send, {esc}
$<^s:: Send, {f3}
$<^r:: Send, +{f3}
$<^d:: Send, {delete}

$<^+p:: Send, +{up}
$<^+n:: Send, +{down}
$<^+b:: Send, +{left}
$<^+f:: Send, +{right}
$<^+a:: Send, +{home}
$<^+e:: Send, +{end}


$<!n:: Send, {pgdn}
$<!p:: Send, {pgup}
$<!b:: Send, ^{left}
$<!f:: Send, ^{right}
$<!d:: Send, ^{delete}

$<!+n:: Send, +{pgdn}
$<!+p:: Send, +{pgup}
$<!+b:: Send, ^+{left}
$<!+f:: Send, ^+{right}


$<!up:: Send, {pgup}
$<!down:: Send, {pgdn}
$<!left:: Send, ^{left}
$<!right:: Send, ^{right}

$<!+up:: Send, +{pgup}
$<!+down:: Send, +{pgdn}
$<!+left:: Send, ^+{left}
$<!+right:: Send, ^+{right}


$<^up:: Send, ^{home}
$<^down:: Send, ^{end}
$<^left:: Send, {home}
$<^right:: Send, {end}

$<^+up:: Send, ^+{home}
$<^+down:: Send, ^+{end}
$<^+left:: Send, +{home}
$<^+right:: Send, +{end}
