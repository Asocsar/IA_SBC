# IA_SBC

**Ideas:**
```
- Books need attribute for language
- Preference: National or foreign author?
- Age of the reader, ex. if age < 14 => difficulty = easy
- Where to read, ex. if collective transport => difficulty = easy or medium
- How often to read, ex. daily => difficulty = easy or medium
- Genres that are related somehow, ex. magic => sword and sorcery
- If empty (no preferences) => popularity = best sellers
- If empty (no preferences) and if have a friend => use friend's data
- If an experienced reader => difficulty = hard and popularity = critics or classic
```

More attributes:
  - Number of pages
  - 

Other things:
Does the book "Seven deadly sins" have author?
In the onthology put a constraint => not possible be friend to yourself
The nationality for all authors is not correct.
What can we use with the information "time of the day the reader reads?"

------------------------------------------------------------------------
I1: Age
I2: Number of books read
I3: Number of hours dedicated to read
I4: Number of pages
I5: Nationality / What languages do you understand?
I6: Do you like some of these writers?

---   RULES ---
R1: 
		; 5 <= AGE <= 17 -> YOUNG
		; 18 <= AGE <= 30 -> ADULT
		; 31 <= AGE -> OLD ADULT

R2: 
		; 0 <= Nº HOURS <= 2 -> OCASIONALLY
		; 3 <= Nº HOURS <= 7 -> CASUAL
		; 8 <= Nº HOURS <= 14 -> REGULAR
		; 14 <= Nº HOURS -> DEDICATED
    
R3: 
		; 0 <= Nº PAGES <= 150 -> SHORT
		; 151 <= Nº PAGES <= 400 -> NORMAL
		; 400 > Nº PAGES -> LONG
    
R4: 
		; NARRATIVE ACTION AND SETTING MEDIAVAL -> ADVENTURE, EPIC, MAGIC, SWORD_AND_SORCERY
		; NARRATIVE DEVELOPMENT AND SETTING MEDIAVAL -> FABULA
		; NARRATIVE ACTION AND SETTING URBAN -> SUPERHEROES, CYBERPUNK, SCI-FI
		; NARRATIVE DEVELOPMENT AND SETTING URBAN -> ROMANTIC, DARK, SPOOKY

R5:
		; 0 <= Nº OF BOOKS <= 5 -> BEGINNER
		; 6 <= Nº OF BOOKS <= 15 -> AMATEUR
		; 15 <= Nº OF BOOKS -> ADVANCED

---



X1: If Young and Beginner -> Difficulty = Easy and Length = Short
X2: If Young and Amateur OR Advanced -> Difficulty =  Normal and Length = Normal


Difficulty / Length / Genero

---

# X1: If Young and Beginner -> Difficulty = Easy and Length = Short
# X2: If Young and Amateur -> Difficulty = Easy and Length = Normal
# X3: If Young and Advanced -> Difficulty = Medium and Length = Normal
X4: If Adult and Beginner -> Difficulty = Easy and Length = Normal
X5: If Adult and Amateur -> Difficulty = Easy and Length = Long
X6: If Adult and Advanced -> Difficulty = Medium and Length = Long
X7: If Old Adult and Beginner -> Difficulty = Medium and Length = Normal
X8: If Old Adult and Amateur -> Difficulty -> Difficulty = Hard and Length = Normal
X9: If Old Adult and Advanced -> Difficulty -> Difficulty = Hard and Length = Long


X13: If Occasionaly -> Length = Short
X14: If Casual -> Length = Normal
X15: If Regular or dedicated -> Length = Long


