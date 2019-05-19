-- Выведите названия 20 городов с самым большим процентом от населения страны в
-- порядке убывания процента. В случае равенства процентов, отсортировать города
-- в порядке обратном лексикографическому (вывод: название города, его
-- население, население страны). (0,5 баллов)
SELECT City.Name, City.Population, Country.Population
FROM City
JOIN Country ON Country.Code = City.CountryCode
ORDER BY CAST (City.Population AS FLOAT) / CAST (Country.Population AS FLOAT) DESC, City.Name DESC
LIMIT 20;