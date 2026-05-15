#!/bin/bash
# random-video.sh — pick a random short inspiring video and open in mpv
# Searches YouTube via yt-dlp across curated themes, filters to ≤5 min,
# picks one at random, prints title to stdout, opens in mpv.

THEMES=(
  "Krishnamurti talk on thinking and freedom"
  "Osho discourse consciousness mind"
  "Ramana Maharshi self inquiry silence"
  "Vivekananda lecture wisdom strength"
  "Vimalananda aghori teaching"
  "eastern philosophy consciousness life"
  "zen story insight daily life"
  "advaita vedanta explained simply"
  "Carl Jung shadow archetype explained"
  "Alfred Adler individual psychology inferiority"
  "why people think the way they do psychology"
  "psychology of belief behavior story"
  "Jungian psychology dream unconscious"
  "human behavior explained science"
  "how calculus was invented history"
  "mathematics discovery history story"
  "Ramanujan mathematics genius life"
  "mathematical theorem origin story"
  "Godel incompleteness theorem explained"
  "beauty of mathematics elegance"
  "how probability thinking emerged history"
  "philosopher biography story life"
  "stoicism philosophy daily lesson"
  "how one idea changed history"
  "ancient wisdom modern life insight"
  "history of consciousness thought"
  "history untold human story"
  "spoken word poem philosophy life"
  "personal story transformation insight"
  "running ultramarathon mindset story"
  "silence boredom creativity insight"
  "meditation insight story practice"
  "neuroscience mind consciousness insight"
  "buddhism wisdom teaching life"
  "science discovery human story insight"
  "habit behavior psychology story"
  "creative process artist story"
  "Jiddu Krishnamurti freedom observer"
  "eastern thinker philosophy life meaning"
  "mindfulness attention awareness story"
)

# Use AI-generated query if passed as $1, otherwise pick from built-in list
if [ -n "$1" ]; then
  THEME="$1"
else
  THEME="${THEMES[$RANDOM % ${#THEMES[@]}]}"
fi

# Search YouTube and collect results: id <TAB> duration <TAB> title
RESULTS=$(yt-dlp "ytsearch20:${THEME}" \
  --flat-playlist \
  --print "%(id)s	%(duration)s	%(title)s" \
  2>/dev/null)

if [ -z "$RESULTS" ]; then
  echo "ERROR: No results for theme: ${THEME}"
  exit 1
fi

# Filter to videos ≤ 300 seconds (5 min); durations come as floats (e.g. 278.0)
FILTERED=$(echo "$RESULTS" | awk -F'\t' '$2+0 > 0 && $2+0 <= 300 {print}')

if [ -z "$FILTERED" ]; then
  echo "ERROR: No short videos found for: ${THEME}"
  exit 1
fi

# Pick one at random (macOS-compatible; shuf not available)
CHOSEN=$(echo "$FILTERED" | perl -MList::Util=shuffle -e '@l=<STDIN>; print((shuffle @l)[0])')
ID=$(echo "$CHOSEN" | cut -f1)
TITLE=$(echo "$CHOSEN" | cut -f3-)

# Output ID, duration (seconds), and title for Emacs — Emacs launches mpv directly
printf "%s\t%s\t%s\n" "$ID" "$(echo "$CHOSEN" | cut -f2)" "$TITLE"
