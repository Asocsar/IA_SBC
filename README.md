# IA_SBC

### Ideas:

- [ ] Books need attribute for language
- [ ] Preference: National or foreign author?
- [ ] Where to read, ex. if collective transport => difficulty = easy or medium
- [x] How often to read, ex. daily => difficulty = easy or medium
- [x] Genres that are related somehow, ex. magic => sword and sorcery
- [ ] If empty (no preferences) => popularity = best sellers
- [ ] If empty (no preferences) and if have a friend => use friend's data
- [x] If an experienced reader => difficulty = hard and popularity = critics or classic
- [ ] Dou you prefer best Sellers or another kind of book ?


**More attributes:**
  1. Language of the book
  2. 


### Initial Information
1. Age
2. Number of books read
3. Number of hours dedicated to read
4. Number of pages
5. Nationality / What languages do you understand?
6. Do you like some of these writers?

### RULES

1. R1: 
	+ 5 <= AGE <= 17 -> YOUNG
	+ 18 <= AGE <= 30 -> ADULT
	+ 31 <= AGE -> OLD ADULT

2. R2: 
	+ 0 <= Nº HOURS <= 2 -> OCASIONALLY
	+ 3 <= Nº HOURS <= 7 -> CASUAL
	+ 8 <= Nº HOURS <= 14 -> REGULAR
	+ 14 <= Nº HOURS -> DEDICATED
    
3. R3: 
	+ 0 <= Nº PAGES <= 150 -> SHORT
	+ 151 <= Nº PAGES <= 400 -> NORMAL
	+ 400 > Nº PAGES -> LONG
    
4. R4: 
	+ NARRATIVE ACTION AND SETTING MEDIAVAL -> ADVENTURE, EPIC, MAGIC, SWORD_AND_SORCERY
	+ NARRATIVE DEVELOPMENT AND SETTING MEDIAVAL -> FABULA
	+ NARRATIVE ACTION AND SETTING URBAN -> SUPERHEROES, CYBERPUNK, SCI-FI
	+ NARRATIVE DEVELOPMENT AND SETTING URBAN -> ROMANTIC, DARK, SPOOKY

5. R5:
	+ 0 <= Nº OF BOOKS <= 5 -> BEGINNER
	+ 6 <= Nº OF BOOKS <= 15 -> AMATEUR
	+ 15 <= Nº OF BOOKS -> ADVANCED

### Abstraction

1. EASY DIFFICULTY IF:
```
	+ IF YOUNG AND OCASIONALLY OR CASUAL
	+ IF ADULT AND OCASIONALLY
```

2. MEDIUM DIFFICULTY IF:
```
	+ IF YOUNG AND AMATEUR
	+ IF YOUNG AND BEGINNER AND REGULAR OR DEDICATED
	+ IF YOUNG AND ADVANCED AND OCASIONALLY OR CASUAL OR REGULAR
	+ IF ADULT AND BEGINNER AND CASUAL OR REGULAR OR DEDICATED
	+ IF ADULT AND AMATEUR OR ADVANCED AND OCASIONALLY
	+ IF OLD ADULT AND OCASIONALLY OR CASUAL AND BEGINNER OR AMATEUR
```

3. HARD DIFFICULTY IF:
```
	+ IF YOUNG AND DEDICATED AND ADVANCED
	+ IF ADULT AND CASUAL OR REGULAR OR DEDICATED AND AMATEUR OR ADVANCED
	+ IF OLD ADULT AND (REGULAR OR DEDICATED) OR (OCASIONALLY OR CASUAL AND ADVANCED)
```

4. SHORT EXTENSION IF:
```
	+ IF AMATEUR AND OCASIONALLY
	+ IF BEGINNER AND OCASIONALLY OR CASUAL OR REGULAR
```

5. NORMAL EXTENSION IF:
```
	+ IF BEGINNER AND DEDICATED
	+ IF AMATEUR AND CASUAL OR REGULAR
	+ IF ADVANCED AND OCASIONALLY
```

6.LONG EXTENSION IF:
```
	+ IF AMATEUR AND DEDICATED
	+ IF ADVANCED AND CASUAL OR REGULAR OR DEDICATED
```

7. BEST SELLER AND POPULAR BOOK IF:
```
	+ BEGINNER AND NORMAL
	+ BEGGINER AND EASY
```

8. CRITIC
```
	+ AMATEUR AND DEDICATED
	+ ADVANCED AND DEDICATED
```
9. NORMAL
```
	+ REGULAR AND AMATEUR
	+ REGULAR AND BEGGINER
```
10. NON-POPULAR
```
	+BEGINNER AND DEDICATED
	+
```


## Other things:
* Delete the friends, I want to be alone without any friends......Hello darkness my old friend, I'm glad to see you again.....
* The nationality for all authors is not correct.
* What can we use with the information "time of the day the reader reads?"

