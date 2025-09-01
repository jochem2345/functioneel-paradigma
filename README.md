# Belangrijke informatie

Student: Jochem Jansen
Studentnummer: 1673402
Gekozen taal: Haskell
Challenge: Markdown to HTML converter

Handige bronnen:
- https://www.haskell.org/documentation/

# Opdracht Functioneel Paradigma

## Inleiding

Functioneel programmeren wordt steeds belangrijker in het softwarelandschap vanwege de voordelen op het gebied van eenvoud, onderhoudbaarheid en parallelliteit. In deze opdracht ga je je verdiepen in het functionele paradigma door zelf een algoritme of programma te implementeren in een functionele taal naar keuze. Het doel is dat je functionele concepten leert herkennen, begrijpen en toepassen in een zelfgekozen context.

## Opdrachtomschrijving

Je gaat een zelfbedachte challenge ontwerpen en implementeren in een zuiver functionele programmeertaal. Denk hierbij aan talen zoals:

- Haskell
- Erlang
- Clojure
- Elixir

Het doel van de opdracht is om:

- De werking van functionele programmeertalen te begrijpen en functionele concepten in praktijk te brengen.
- Een oplossing te programmeren die gebaseerd is op het functionele paradigma.
- Functionele concepten bewust te koppelen aan specifieke onderdelen van je uitgeprogrammeerde algoritme.
- Je bevindingen helder te verwoorden in een rapport dat voldoet aan de AIM-controlekaart.

## Eisen aan de opdracht

### 1. Onderzoek

- Onderzoek de gekozen programmeertaal en de functionele concepten die in deze taal centraal staan, zoals:
  - Zuiverheid (pure functions)
  - First-class functions
  - Higher-order functions
  - Immutability
  - Recursie
  - Lazy evaluation
  - Pattern matching

### 2. Challenge

- Bedenk een inhoudelijk interessante challenge die geschikt is om functionele concepten te onderzoeken en toe te passen.
- De challenge moet van voldoende complexiteit zijn om aan te tonen dat je functionele concepten begrijpt en kunt toepassen.
- Zorg dat de challenge aansluit bij wat jij zelf boeiend vindt en wat jij graag wil leren.

### 3. Implementatie

- Implementeer je challenge volledig in een functionele programmeertaal.
- Gebruik overwegend functionele constructies (pure functions, higher-order functions, pattern matching, etc.).
- Zorg voor duidelijke code en documentatie. Laat zien dat je je code begrijpt door deze volledig uit te leggen. Zet de uitleg als commentaar in de code.
- Gebruik Git bij het maken van de implementatie. Commit regelmatig. Op deze manier krijgt de docent beter inzicht in je programmeerproces.

### 4. Reflectie

- Leg uit welke functionele concepten je hebt toegepast en hoe ze bijdragen aan de oplossing.
- Reflecteer op het gebruik van functionele concepten:
  - Waarom zijn bepaalde constructies handig of juist lastig?
  - Wat zijn de voordelen of nadelen ten opzichte van imperatieve of objectgeoriënteerde talen?

### 5. Rapport

Schrijf een kort rapport (ongeveer 4-6 pagina’s) dat voldoet aan de richtlijnen van de AIM-controlekaart. Het rapport bevat:

- Inleiding – Doel van de opdracht en korte uitleg over de gekozen programmeertaal.
- Onderzoek – Functionele concepten en kenmerken van de gekozen taal.
- Challenge – Beschrijving van de bedachte challenge en waarom deze uitdagend is.
- Implementatie – Korte samenvatting van de implementatie en gebruikte functionele concepten.
- Reflectie – Wat heb je geleerd over het functionele paradigma? Wat werkte goed of juist niet?
- Conclusie – Samenvatting van de belangrijkste leerpunten.
- Bronvermelding – Vermeldt alle bronnen die je gebruik hebt. Als je gebruik maakt van GenAI (zoals ChatGPT) vermeld dit dan en neem links op naar de volledige conversaties. Geef dan ook in je verslag aan hoe je GenAI hebt gebruikt (waarom heb je welke prompt gebruikt en wat deed je met de output).

## Goedkeuring door docent

- Je moet uiterlijk eind week 2 de gekozen challenge en programmeertaal ter goedkeuring voorleggen aan de docent.

## Voorbeelden van mogelijke challenges

- De onderstaande voorbeelden zijn eenvoudig genoeg om binnen de beschikbare tijd af te ronden, maar uitdagend genoeg om functionele concepten toe te passen:

### JSON Parser

- Schrijf een JSON-parser die JSON-bestanden inleest en omzet naar een datastructuur.
- Maak gebruik van recursie en pattern matching om geneste objecten en arrays correct te verwerken.

### LZW-compressie

- Implementeer het LZW-compressiealgoritme in een functionele taal.
- Gebruik immutability en lazy evaluation voor efficiëntie.

### Chatbot met Higher-Order Functions

- Bouw een eenvoudige chatbot die reageert op specifieke patronen in invoer.
- Gebruik higher-order functions om reacties dynamisch te genereren.

### Pathfinding met Lazy Evaluation

- Implementeer een simpel A\* of Dijkstra-pathfinding-algoritme.
- Gebruik lazy evaluation om grote grafen efficiënt te doorzoeken.

### Parallelle verwerking van grote datasets

- Schrijf een programma dat een grote dataset verwerkt (bijvoorbeeld een CSV-bestand).
- Verwerk de data parallel met behulp van immutability en higher-order functions.

### Tekstanalyse

- Implementeer een programma dat woordfrequentie analyseert in een tekstbestand.
- Gebruik recursie en higher-order functions.

### Boekingsysteem voor een bioscoop

- Bouw een boekingsysteem waarbij de state immutabel blijft.
- Gebruik pattern matching voor het verwerken van boekingsacties.

### RLE-compressie (Run-Length Encoding)

- Implementeer run-length encoding en decoding.
- Gebruik pattern matching voor het comprimeren en decomprimeren van data.

### Markdown-to-HTML Converter

- Bouw een Markdown-to-HTML converter die simpele syntax omzet.
- Gebruik pattern matching om Markdown-elementen te herkennen.

### Tic-Tac-Toe AI

- Implementeer een minimax-algoritme voor Tic-Tac-Toe.
- Gebruik immutability voor het spelbord.

## Wat NIET is toegestaan

- Overmatige hulp van AI-tools zoals ChatGPT, Copilot of andere code generators.
- Het hergebruiken van bestaande functionele implementaties zonder toegevoegde waarde.
- Het beperken van de oplossing tot imperatieve of objectgeoriënteerde constructies.

## Oplevering

- Je levert in iSAS in:
  Een zip van de map waarin je code (met uitleg!) staat. Zorg dat je de verborgen .git/-map ook meezipt.
  Je rapport.
  Zorg ervoor dat je op verzoek een demo kunt geven waarin je de werking van je algoritme of programma laat zien.

# Nakijkmodel Functioneel Paradigma

## Beoordelingscriteria

| Categorie                           | Criteria                                                                                                                                           | Knockout |
|-------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------|----------|
| Kwaliteit van de challenge          | De gekozen challenge is inhoudelijk interessant en uitdagend                                                                                       | Nee      |
| Implementatie                       | De code is volledig, correct en maakt duidelijk gebruik van functionele constructies. De code is volledig uitgelegd. Er is gebruik gemaakt van Git | Nee      |
| Koppeling aan functionele concepten | De gekozen functionele concepten worden correct gekoppeld aan de oplossing                                                                         | Ja       |
| Reflectie                           | Kritische analyse van het functionele paradigma en de gekozen taal                                                                                 | Ja       |
| Rapport                             | Helder, gestructureerd en volledig rapport volgens de AIM-controlekaart                                                                            | Nee      |

- Op alle criteria kun je Onvoldoende, Voldoende of Goed scoren.
- Indien bij knockout "Ja" staat moet je V of G scoren om te slagen. De O is dus de knockout.

## Berekening score

1 of meerdere knockouts: 4
Alle scores een V: 7
O: -1
G: +1
Max een 10, min een 4.
