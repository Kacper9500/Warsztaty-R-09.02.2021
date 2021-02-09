#WCZYTANIE BIBLIOTEK
library(dplyr)  #biblioteka umożliwiająca manipulację ramkami danych 
library(flextable) #biblioteka umożliwiająca wygenerowanie tabeli z ramki danych

#PRZEDSTAWIENIE ZBIORU DANYCH
#Baza danych pochodzi ze strony fbref.com i przedstawia statystyki piłkarzy występujących w Bundeslidze.
#Zbiór składa się z 20 zmiennych i 457 obserwacji

#OPIS ISTOTNYCH ZMIENNYCH
#Number - ID
#Name - imię i nazwisko
#Club - klub, w którym występuje piłkarz
#Note - średnia nota w prasie

#WCZYTUJEMY ZBIÓR DANYCH

data <- read.csv("warsztatyR.csv", header = T, sep = ",") 

glimpse(data)

#1. WYBIERANIE ZMIENNYCH
#Korzystamy z funkcji select i wybieramy interesujące nas zmienne

data <- data %>% select(Number, Name, Club, Note)
          
#2. FILTROWANIE OBSERWACJI
#Funkcja filter - wybieramy tylko tych zawodników, którzy nie posiadają braków danych 
#w zmiennej Note

data <- data %>% filter(!is.na(Note))

#3. TWORZENIE NOWEJ ZMIENNEJ
#Z użyciem funkcji mutate towrzymy nową zmienną - przeliczenie not w skali niemieckiej 6-1
#na skalę polską 1-6

data <- data %>% mutate(NotePL = 7 - Note)

#4. GRUPOWANIE ZMIENNEJ WEDŁUG KLUCZA I PODSTAWOWE STATYSTYKI
#Grupujemy zawodników według klubów i wyliczamy dla nich średnią notę

mNote <- data %>%  
  group_by(Club) %>%
  summarise(mean_Note = round(mean(NotePL),2)) 

#Wczytujemy kolejny zbiór danych, który przyda nam się w późniejszym etapie

table <- read.csv("warsztatyR2.csv", header = T, sep = ";")

#5. ŁĄCZENIE BAZ DANYCH
#Przy pomocy funkcji left_join łączymy ze sobą dwa zbiory według klucza, którym będzie zmienna Club

mNote <- left_join(mNote, table, by = "Club")

#6. SORTOWANIE BAZY WEDŁUG WYBRANEJ ZMIENNEJ
#Z wykorzystaniem funkcji arrange sortujemy naszą bazę malejąco według zmiennej mean_Note

mNote <- mNote %>% arrange(desc(mean_Note))

#7. Tworzymy nową zmienną NotePos, która przypisze do posortowanego zbioru pozycje w tabeli
#według zmiennej mean_Note oraz zmienną diff, która wyliczy różnicę między dwiema zmiennymi

mNote <- mNote %>% mutate(NotePos = c(1:18))

mNote <- mNote %>%  
  mutate(diff = NotePos - TabPos)

#8. FUNKCJA CASE_WHEN
#Przy użyciu funkcji case_when na podstawie zmiennej diff przypisujemy odpowiednie
#kategorie, żeby ocenić czy pozycje klubów w tabeli odzwierciedlają ich pozycję według
#not prasowych

mNote <- mNote %>%
  mutate(
    Ocena = case_when(
      diff > 0  ~ "niedoceniani",
      diff < 0      ~ "przeceniani",
      TRUE                      ~ "bez zmian"
    )
  )

#9. ZMIANA NAZWY ZMIENNEJ
#Korzystamy z funkcji rename zmieniamy nazwy zmiennych na polskie, 
#aby wyświetlić je w tabeli

mNote <- mNote %>% rename(Klub = Club, `Średnia nota` = mean_Note, 
                          `Pozycja w tabeli` = TabPos,
                          `Pozycja według not` = NotePos, `
                          Różnica pozycji` = diff)

#10. TWORZENIE TABELI
#Z użyciem funkcji flextable tworzymy tabelę wraz z podpisem

mNote %>%
  flextable() %>%
  autofit() %>% 
  flextable::set_caption("Tabela 1. Średnie noty zawodników w poszczególnych klubach.")

#WSZYSTKIE OPERACJE W JEDNYM KROKU
#przechodzimy do wykonania poleceń z poprzednich 10 etapów w jednym kroku
#przy użyciu operatora %>%

  mNote2 <- data %>%
  select(Number, Club, Note) %>%
  filter(!is.na(Note)) %>%
  mutate(NotePL = 7 - Note) %>%
  group_by(Club) %>%
  summarise(mean_Note = round(mean(NotePL),2)) %>%
  left_join(table, by = "Club") %>%
  arrange(desc(mean_Note)) %>%
  mutate(NotePos = c(1:18)) %>%
  mutate(diff = NotePos - TabPos) %>%
  mutate(
    Ocena = case_when(
      diff > 0  ~ "niedoceniani",
      diff < 0      ~ "przeceniani",
      TRUE                      ~ "bez zmian"
    )
  ) %>%
  rename(Klub = Club, `Średnia nota` = mean_Note, `Pozycja w tabeli` = TabPos,
         `Pozycja według not` = NotePos, `Różnica pozycji` = diff)
  
  
mNote2 %>%
  flextable() %>%
  autofit() %>% 
  flextable::set_caption("Tabela 1. Średnie noty zawodników w poszczególnych klubach.")
  
  
#Z użyciem funkcji sample_n i sample_frac możemy również losować obserwacje ze zbioru danych
#albo z użyciem funkcji top_n wybrać najlepsze obserwacje 

sample_data <- sample_n(data, 50)

sample_data2 <- sample_frac(data, 0.11)

top_data <- top_n(data, 10, NotePL)

#Możemy także sprawdzić ile osberwacji występuje w obu wylosowanych zbiorach i wykorzystamy
#do tego funkcję inner_join

inner_join(sample_data, sample_data2)

#Za pomocą funkcji filter i operatora %in% możemy także wybierać obserwacje pojawiające się
#w danej kolumnie

data1<-data%>% filter(Club %in% c("Schalke","Union"))
