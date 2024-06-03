DROP TABLE IF EXISTS #persistence_eras;
  
SELECT ENDS.person_id,
	min(drug_exposure_start_date) AS era_start_date,
	DATEADD(day, 0, ENDS.era_end_date) AS era_end_date
INTO #persistence_eras
FROM (
	SELECT de.person_id,
		de.drug_exposure_start_date,
		MIN(e.END_DATE) AS era_end_date
	FROM #drug_exposure DE
	JOIN (
		--cteEndDates
		SELECT PERSON_ID,
			DATEADD(day, - 1 * @persistence_pad, EVENT_DATE) AS END_DATE -- unpad the end date
		FROM (
			SELECT PERSON_ID,
				EVENT_DATE,
				EVENT_TYPE,
				MAX(START_ORDINAL) OVER (
					PARTITION BY PERSON_ID ORDER BY event_date,
						event_type,
						START_ORDINAL ROWS UNBOUNDED PRECEDING
					) AS start_ordinal,
				ROW_NUMBER() OVER (
					PARTITION BY PERSON_ID ORDER BY EVENT_DATE,
						EVENT_TYPE,
						START_ORDINAL
					) AS OVERALL_ORD -- this re-numbers the inner UNION so all rows are numbered ordered by the event date
			FROM (
				-- select the start dates, assigning a row number to each
				SELECT PERSON_ID,
					DRUG_EXPOSURE_START_DATE AS EVENT_DATE,
					0 AS EVENT_TYPE,
					ROW_NUMBER() OVER (
						PARTITION BY PERSON_ID ORDER BY DRUG_EXPOSURE_START_DATE
						) AS START_ORDINAL
				FROM #drug_exposure D
				
				UNION ALL
				
				-- add the end dates with NULL as the row number, padding the end dates by @persistence_pad to allow a grace period for overlapping ranges.
				SELECT PERSON_ID,
					DATEADD(day, @persistence_pad, DRUG_EXPOSURE_END_DATE),
					1 AS EVENT_TYPE,
					NULL
				FROM #drug_exposure D
				) RAWDATA
			) E
		WHERE 2 * E.START_ORDINAL - E.OVERALL_ORD = 0
		) E
		ON DE.PERSON_ID = E.PERSON_ID
			AND E.END_DATE >= DE.DRUG_EXPOSURE_START_DATE
	GROUP BY de.person_id,
		de.drug_exposure_start_date
	) ENDS
GROUP BY ENDS.person_id,
	ENDS.era_end_date;