# Abschlussprojekt-DP2016

Für die Vorlesung Deskriptive Programmierung an der Universität Bonn gilt es als Vorraussetzung zur Prüfungszulassung ein kleines Projekt in Elm zu realisieren. Hierfür würde ich gerne einen Gold Miner Klon programmieren.

Gold Miner ist ein Flashgame bei dem mit Hilfe eines Greifhakens Goldklumpen eingesammelt werden müssen. Der Greifhaken schwingt immer von links nach rechts und wieder zurück. Mit einem Klick der linken Maustaste schnellt der Greifarm in die Richtung, in welche der Haken zum Zeitpunkt des Klicks ausgerichtet ist. Trifft der Haken hierbei auf einen Goldklumpen so wird dieser an die Oberfläche gezogen. Abhängig von der Größe dauert das hochziehen länger oder kürzer. Größere Klumpen benötigen mehr Zeit hochgezogen zu werden, bringen aber auch mehr Geld ein. Ziel eines Levels ist es innerhalb einer bestimmten Zeit einen vorgegebenen Betrag an Geld zu erreichen.

(Neben Goldklumpen können auch noch andere Gegenstände erscheinen, wie z.B. Diamanten oder Steinklumpen)


-- 13.07.2016
- wenig erfolgreich bisher, schiessen mit Spacetaste möglich, Zeit wird angezeigt, Punkte Stand wird angezeigt, ein Felsbroken ermöglicht   Punkte zu sammeln. Konzept leicht verändert, kleinere Broken geben mehr Punkte, größere Broken weniger Punkte

-- 15.07.2016
- Level 1 und 2 spielbar, Level 2 erweitert das Spiel um in der Luft fliegende "Snitches" 

-- 19.07.2016
- Collide von subscription zu update
- allgemeinere Modellierung begonnen (iron, sivler, gold, snitches als Listen modelliert)

https://Dinendal92.github.io/Abschlussprojekt-DP2016/index.html
