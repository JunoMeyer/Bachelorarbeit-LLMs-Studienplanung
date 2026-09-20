# Beschreibung der Variablen und Kodierungen 

## Variablen

- `case`: Nummer des Durchlaufs der Ansteuerung von GPT-5.5 über die OpenAI-API (aus Zeitgründen wurde für die Analyse nur ein Drittel aller produzierten cases verwendet)
- `repl`: Nummer der 10 Replikationen für die vier Bedingungen 
- `condition`: vier Bedingungen (Ausprägungen: 9 = Reflexionsanweisung und Hypothesen im Prompt; 10 = Reflexionsanweisung im Prompt, aber keine Hypothesen; 11 = Hypothesen im Prompt, aber keine Reflexionsanweisung; 12 = beides nicht im Prompt) 
- `type`: Reflexionsanweisung (Ausprägungen: `yes` = Reflexionsanweisung im Prompt , `no` = Reflexionsanweisung nicht im Prompt)
- `hypo`: Hypothesen (Ausprägungen: `yes` = Hypothesen im Prompt, `no` = Hypothesen nicht im Prompt)
- `prompt`: User-Prompt
- `llm_output`: LLM-Output generiert von GPT-5.5
- `Krit_Studiendesign_rating`: enspricht Bewertungsskala 2 (Entscheidungshilfe in Bezug auf das Studiendesign)
- `Krit_Messinstr_rating`: entspricht Bewertungsskala 4 (Entscheidungshilfe in Bezug auf die Messinstrumente)
- `Krit_Kontrollvar_rating`: entspricht Bewertungsskala 6 (Entscheidungshilfe in Bezug auf Kontrollvariablen)
- `Kritische Reflexion Begründung`: Begründungen für die Kodierung von Bewertungsdimension 2, 4 & 6
- `Studiendesign_Rating`: entspricht Bewertungsskala 1 (Angemessenheit des vorgeschlagenen Studiendesign)
- `Studiendesign_Begründung`: Begründung der Kodierung von Bewertungsdimension 1
- `Messinstrumente_Rating`: enstpricht Bewertungsskala 3 (Angemessenheit der vorgeschlagenen Messinstrumente)
- `Messinstrumente_Begründung`: Begründung der Kodierung von Bewertungsskala 3
- `Kontrollvariablen_Rating`: enstpricht Bewertungsskala 5 (Angemessenheit der vorgeschlagenen Kontrollvariablen)
- `Kontrollvariablen_Begründung`: Begründung der Kodierung von Bewertungsskala 5
  

## Kodierungen 

- 1: LLM-Output als "schlecht" bewertet
- 2: LLM-Output als "mittel" bewertet
- 3: LLM-Output als "gut" bewertet

