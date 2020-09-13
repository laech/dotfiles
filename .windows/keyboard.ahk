; # Win
; ! Alt
; ^ Control
; + Shift
; & Combine two and only two keys into a custom hotkey
; $ Don't use when called from other hotkey, avoid recursion
; < left key, e.g. <^ is left control
; > right key

$<^p:: send, {up}
$<!p:: send, {pgup}
$<^+p:: send, +{up}
$<!+p:: send, +{pgup}

$<^n:: send, {down}
$<!n:: send, {pgdn}
$<^+n:: send, +{down}
$<!+n:: send, +{pgdn}

$<^b:: send, {left}
$<!b:: send, ^{left}
$<^+b:: send, +{left}
$<!+b:: send, ^+{left}

$<^f:: send, {right}
$<!f:: send, ^{right}
$<^+f:: send, +{right}
$<!+f:: send, ^+{right}

$<^a:: send, {home}
$<^+a:: send, +{home}

$<^e:: send, {end}
$<^+e:: send, +{end}

$<^g:: send, {esc}

$<^d:: send, {delete}
$<!d:: send, ^{delete}

$<^backspace:: send, +{home}{backspace}
$<!backspace:: send, ^{backspace}

$<^delete:: send, +{end}{delete}
$<!delete:: send, ^{delete}

$<^up:: send, ^{home}
$<!up:: send, {pgup}
$<^+up:: send, ^+{home}
$<!+up:: send, +{pgup}

$<!down:: send, {pgdn}
$<^down:: send, ^{end}
$<!+down:: send, +{pgdn}
$<^+down:: send, ^+{end}

$<!left:: send, ^{left}
$<^left:: send, {home}
$<!+left:: send, ^+{left}
$<^+left:: send, +{home}

$<!right:: send, ^{right}
$<^right:: send, {end}
$<!+right:: send, ^+{right}
$<^+right:: send, +{end}

$<!+,:: send, ^{home}
$<!+.:: send, ^{end}

$<!space:: send, !{space}
