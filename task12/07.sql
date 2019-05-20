-- Выведите названия стран в лексикографическом порядке, большинство населения
-- которых не проживает в городах (имеются в виду города, информация о которых
-- есть в базе данных), если в базе данных для страны нет ни одного города,
-- то ее городское население считается равным 0 (будьте внимательны, при этом
-- население страны тоже может быть равным 0, в этом случае выводить такую
-- страну не нужно). (0,5 баллов)
SELECT Country.Name
FROM Country
LEFT JOIN City ON Country.Code = City.CountryCode
GROUP BY Country.Code
HAVING COALESCE(SUM(City.Population), 0) < (Country.Population - COALESCE(SUM(City.Population), 0))
ORDER BY Country.Name;
