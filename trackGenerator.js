const ARTISTS = ["lana del rey", "toto", "moto boy", "khalid", "ariana grande", "the xx", "lorde", "bon iver", "billie eilish", "kaytranada"]
const BPM_UPPER = 125
const BPM_LOWER = 140
const MINUTES = [2, 3, 4, 5]

const generateTrack = i => {
  const artist = ARTISTS[Math.floor(Math.random()*ARTISTS.length)]
  const bpm = Math.floor(Math.random() * (BPM_UPPER - BPM_LOWER)) + BPM_LOWER
  const length = MINUTES[Math.floor(Math.random()*MINUTES.length)]
  return 'track(' + i + ',"' + artist + '",' + bpm + "," + length + ')'
}

const generateTracks = amount => {
  let tracks = '['
  for (var i = 0; i < amount; i++) {
    tracks += generateTrack(i)
    if (i !== amount - 1) {
      tracks += ','
    }
  }
  tracks += ']'
  return tracks
}


console.log(generateTracks(14))