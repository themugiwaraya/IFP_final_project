---------------------------------
-- Alibi
---------------------------------
--Создаем типы данных :))
data Event = Event {
    eventID :: Int,
    name :: String,
    startTime :: Float,
    endTime :: Float,
    eventLocation :: String,
    speaker :: String
} deriving (Show)

data ConferenceSchedule = ConferenceSchedule {
    scheduleID :: Int,
    location :: String,
    events :: [Event]
} deriving (Show)

--Функция для проверки схожести времени 
timeOverlap :: Event -> Event -> Bool
timeOverlap e1 e2 = startTime e1 < endTime e2 && startTime e2 < endTime e1

--Функция для добавления Ивента в Расписание
addEventToSchedule :: Event -> ConferenceSchedule -> Either String ConferenceSchedule
addEventToSchedule newEvent schedule =
    if overlapDetected 
        then Left "Error: Event conflicts with another event at the same location." 
        else Right schedule { events = newEvent : events schedule } 
  where
    overlapDetected = checkOverlap newEvent (events schedule) --функция overlapDetected которая применяет checkOverlap для расписания
    checkOverlap :: Event -> [Event] -> Bool -- функция checkOverlap которая применяет timeOverlap для списка 
    checkOverlap _ [] = False 
    checkOverlap newEvent (e:es) 
        | eventLocation newEvent == eventLocation e && timeOverlap newEvent e = True 
        | otherwise = checkOverlap newEvent es 

----------------------------------
-- Ruslan
----------------------------------

findEventById :: Int -> [Event] -> Maybe Event --функция для поиска event по id
findEventById _ [] = Nothing 
findEventById id (e:es) 
    | eventID e == id = Just e 
    | otherwise = findEventById id es 

updateEventInSchedule :: Event -> ConferenceSchedule -> Either String ConferenceSchedule --функция для обновления ивента
updateEventInSchedule updatedEvent schedule =
    case findEventById (eventID updatedEvent) (events schedule) of 
        Nothing -> Left "Error: Event with the given ID not found." 
        Just updatedEvent -> Right schedule { events = updateEventList (events schedule) updatedEvent } 
  where
    updateEventList :: [Event] -> Event -> [Event] -- функция для обновления листа
    updateEventList [] _ = [] 
    updateEventList (e:es) updated 
        | eventID e == eventID updated = updated : es
        | otherwise = e : updateEventList es updated
------------------------------------
-- Alibi
------------------------------------
checkTimeSlot :: Event -> [Event] -> Either String String --функция для проверки свободного времени
checkTimeSlot newEvent [] = Right "The time slot is available." 
checkTimeSlot newEvent (e:es) 
    | eventLocation newEvent == eventLocation e && timeOverlap newEvent e = 
        Left "Error: The time slot is already booked." 
    | otherwise = checkTimeSlot newEvent es

---функции для формирования отчета

countEventsAtLocation :: String -> [Event] -> Int -- функция для подсчета количества событий для конкретной локации
countEventsAtLocation _ [] = 0
countEventsAtLocation location (e:es)
    | eventLocation e == location = 1 + countEventsAtLocation location es
    | otherwise = countEventsAtLocation location es

uniqueLocations :: [Event] -> [String] -- получение списка уникальных локаций
uniqueLocations [] = []
uniqueLocations (e:es)
    | eventLocation e `elem` map eventLocation es = uniqueLocations es --если локация имеется(используем elem) в списке то мы ее пропускаем  мап используем чтобы извлечь названия локаций
    | otherwise = eventLocation e : uniqueLocations es 

generateLocationReport :: [Event] -> String -- генерация отчета по локациям
generateLocationReport [] = ""
generateLocationReport events = locationReport (uniqueLocations events) events
  where
    locationReport :: [String] -> [Event] -> String
    locationReport [] _ = ""
    locationReport (loc:locs) events =
            loc ++ ": " ++ show (countEventsAtLocation loc events) ++ " events\n" ++
            locationReport locs events

--------------------------------
-- Ruslan
--------------------------------
countEventsBySpeaker :: String -> [Event] -> Int -- функция для подсчета количества событий для конкретного спикера
countEventsBySpeaker _ [] = 0
countEventsBySpeaker spkr (e:es)
    | speaker e == spkr = 1 + countEventsBySpeaker spkr es
    | otherwise = countEventsBySpeaker spkr es

eventsBySpeakerList :: String -> [Event] -> [String] -- функция для получения списка событий для конкретного спикера
eventsBySpeakerList _ [] = []
eventsBySpeakerList spkr (e:es)
    | speaker e == spkr = name e : eventsBySpeakerList spkr es
    | otherwise = eventsBySpeakerList spkr es

uniqueSpeakers :: [Event] -> [String] --функция для получения уникальных спикеров
uniqueSpeakers [] = []
uniqueSpeakers (e:es)
    | speaker e `elem` map speaker es = uniqueSpeakers es --также используется елем для проверки наличия и мап для получения названия
    | otherwise = speaker e : uniqueSpeakers es

generateSpeakerReport :: [Event] -> String -- функция для генерации отчета по спикерам
generateSpeakerReport [] = ""
generateSpeakerReport events = speakerReport (uniqueSpeakers events) events
  where
    speakerReport :: [String] -> [Event] -> String
    speakerReport [] _ = ""
    speakerReport (spkr:spkrs) events =
        spkr ++ ": " ++ show (countEventsBySpeaker spkr events) ++ " events\n  Events: " ++
        unwords (eventsBySpeakerList spkr events) ++ "\n" ++ --анвордс соднияет список в строку
        speakerReport spkrs events

generateReport :: ConferenceSchedule -> String
generateReport schedule =
    "Total number of events: " ++ show totalEvents ++ "\n\n" ++
    "Events by location:\n" ++ locationReport ++ "\n" ++
    "Events by speaker:\n" ++ speakerReport
  where
    totalEvents = length (events schedule)
    locationReport = generateLocationReport (events schedule)
    speakerReport = generateSpeakerReport (events schedule)

  
main :: IO ()
main = do
    --создаем список ивеентов и расписание
    let events = [Event 1 "ITFP Final" 9.0 10.0 "C1.1.251" "Olzhas Aimukhambetov",
                  Event 2 "COA Final" 10.30 12.0 "C1.1.360" "Sandibek Umirov"]
    let schedule = ConferenceSchedule 1 "AITU" events
    
    --как выглядит расписание
    putStrLn "Initial Schedule:"
    print schedule

    -- добавляем новое событие
    let newEvent = Event 3 "WEB Technologies Final" 13.0 14.30 "C1.3.366" "Aldiyar Salkenov"
    putStrLn "\nAttempting to add a new event:"
    case addEventToSchedule newEvent schedule of
        Left err -> putStrLn err --обработка ошибок
        Right updatedSchedule -> do
            putStrLn "Event added successfully."
            print updatedSchedule

   --обновляем существующее событие
    let updatedEvent = Event 2 "Analytics in CS" 10.0 11.30 "C1.3.370" "Al-Tarazi Assaubai"
    putStrLn "\nAttempting to update an existing event:"
    case updateEventInSchedule updatedEvent schedule of
        Left err -> putStrLn err
        Right updatedSchedule -> do
            putStrLn "Event updated successfully."
            print updatedSchedule

     --проверяем доступность временного слота для нового ивента
    let newEvent2 = Event 4 "Kazakh Language" 9.30 10.30 "C1.1.251" "Sabira Sapina"
    putStrLn "\nChecking time slot for C1.1.251 (9:30 to 10:30):"
    case checkTimeSlot newEvent2 events of --ошибка
        Left err -> putStrLn err
        Right msg -> putStrLn msg
    
    let newEvent3 = Event 5 "Kazakh Language" 12.00 13.00 "C1.1.251" "Sabira Sapina"
    putStrLn "\nChecking time slot for C1.1.251 (12:00 to 13:00):" --успешно
    case checkTimeSlot newEvent3 events of
        Left err -> putStrLn err
        Right msg -> putStrLn msg

    --создаем новый список и расписание для демонстрации отчета
    let events2 = [
            Event 1 "ITFP Final" 9.0 10.0 "C1.1.251" "Olzhas Aimukhambetov",
            Event 2 "COA Final" 10.30 12.0 "C1.1.360" "Sandibek Umirov",
            Event 3 "WEB Technologies Final" 13.0 14.30 "C1.3.366" "Aldiyar Salkenov",
            Event 4 "Kazakh Language" 9.30 10.30 "C1.1.251" "Sabira Sapina",
            Event 5 "Kazakh Language" 12.0 13.0 "C1.1.251" "Sabira Sapina"
          ]
    let schedule = ConferenceSchedule 1 "AITU" events2

    --генерация и вывод отчета
    putStrLn "\nConference Schedule Report:"
    putStrLn (generateReport schedule)

