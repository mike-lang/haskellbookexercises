

-- Left unfinished WTF is with this exercise?

data Button = Button Digit String

data DaPhone = 
  DaPhone (Button, Button, Button,
           Button, Button, Button,
           Button, Button, Button,
           Button, Button, Button)
--- ???


convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol lol",
   "Lol ya",
   "Wow ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Haha thanks just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
type Digit = Char

-- validPresses: 1 and up
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps = 





