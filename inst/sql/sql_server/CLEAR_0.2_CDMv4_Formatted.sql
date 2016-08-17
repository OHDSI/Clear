/* 
	CLEAR Query - 0.2
	
	Written and Confirmed by SUNGJAE JUNG
	Created at 2016.08.03
	Edited 1st: 2016.08.15
	Confirmed 1st edition: 2016.08.15
	Edited 1st: 2016.08.17
	Confirmed 1st edition: 2016.08.17

*/
{DEFAULT @target_database = 'SJ_CLEAR_CDM4' }
{DEFAULT @cdm_database = 'SCAN' }
{DEFAULT @date_from = '2001-01-01'}
{DEFAULT @date_to = '2010-03-31'}

USE [@target_database];

-- Running
-- # CASE: ATC(Anatomical Therapeutic Chemical Classification)
IF OBJECT_ID('[@target_database].[dbo].TARGET_DRUG', 'U') IS NULL
CREATE TABLE TARGET_DRUG(
	DRUG_NAME			VARCHAR(50) NOT NULL,
	DRUG_CLASS			VARCHAR(50) NOT NULL,
	DRUG_CODE			VARCHAR(50) NOT NULL
);
IF NOT EXISTS(SELECT 1 FROM [@target_database].[dbo].TARGET_DRUG)
INSERT INTO TARGET_DRUG VALUES('CIPROFLOXACIN','Anatomical Therapeutic Chemical Classification','J01MA02');

IF OBJECT_ID('[@target_database].[dbo].DRUG_LIST', 'U') IS NOT NULL
	DROP TABLE DRUG_LIST;
SELECT DISTINCT B.DRUG_NAME, A.DESCENDANT_CONCEPT_ID DRUG_ID
INTO DRUG_LIST
FROM [@cdm_database].[DBO].CONCEPT_ANCESTOR A
	INNER JOIN (
		SELECT BB.DRUG_NAME, AA.CONCEPT_ID, AA.CONCEPT_CLASS, AA.CONCEPT_CODE
		FROM [@cdm_database].[DBO].CONCEPT AA
			INNER JOIN TARGET_DRUG BB
			ON AA.CONCEPT_CLASS=BB.DRUG_CLASS
				AND AA.CONCEPT_CODE=BB.DRUG_CODE
	) B
	ON A.ANCESTOR_CONCEPT_ID=B.CONCEPT_ID

-- Running
-- LABTEST_LIST: LOINC Code
IF OBJECT_ID('[@target_database].[dbo].LABTEST_LIST', 'U') IS NULL
CREATE TABLE LABTEST_LIST(
	LAB_ID			INT NOT NULL,
	LAB_NAME		VARCHAR(50) NOT NULL,
	ABNORM_TYPE		VARCHAR(20) NOT NULL
);
IF NOT EXISTS(SELECT 1 FROM [@target_database].[dbo].LABTEST_LIST)
INSERT INTO LABTEST_LIST VALUES(3018677,'aPTT','Both')

/* Use Exposure */
-- VISIT
-- Running
IF OBJECT_ID('[@target_database].[dbo].VISIT_EXPOSURE_TMP', 'U') IS NOT NULL
	DROP TABLE VISIT_EXPOSURE_TMP;
SELECT DISTINCT A.PERSON_ID, A.VISIT_START_DATE, A.VISIT_END_DATE, A.VISIT_SEQ
	, MIN(B.DRUG_EXPOSURE_START_DATE) OVER(PARTITION BY A.PERSON_ID, A.VISIT_SEQ, B.DRUG_NAME) FIRST_DRUG_ORDDATE
	, B.DRUG_EXPOSURE_START_DATE, B.DRUG_NAME
INTO VISIT_EXPOSURE_TMP
FROM (
		SELECT PERSON_ID, VISIT_START_DATE, VISIT_END_DATE
			, ROW_NUMBER() OVER(PARTITION BY PERSON_ID ORDER BY VISIT_START_DATE, VISIT_END_DATE) VISIT_SEQ
		FROM [@cdm_database].[DBO].VISIT_OCCURRENCE 
		WHERE PLACE_OF_SERVICE_CONCEPT_ID IN (9201)
			AND VISIT_START_DATE BETWEEN '@date_from' AND '@date_to'
			AND VISIT_END_DATE BETWEEN '@date_from' AND '@date_to'
	) A --391090
	LEFT JOIN (
		SELECT AA.PERSON_ID, AA.DRUG_EXPOSURE_START_DATE
			, BB.DRUG_NAME
		FROM [@cdm_database].[DBO].DRUG_EXPOSURE AA
			INNER JOIN DRUG_LIST BB
			ON AA.DRUG_CONCEPT_ID=BB.DRUG_ID
	) B
	ON A.PERSON_ID=B.PERSON_ID
		AND B.DRUG_EXPOSURE_START_DATE BETWEEN A.VISIT_START_DATE AND A.VISIT_END_DATE

IF OBJECT_ID('[@target_database].[dbo].VISIT_EXPOSURE', 'U') IS NOT NULL
	DROP TABLE VISIT_EXPOSURE;
SELECT DISTINCT PERSON_ID, VISIT_START_DATE, VISIT_END_DATE, VISIT_SEQ
	, FIRST_DRUG_ORDDATE, DRUG_NAME
INTO VISIT_EXPOSURE
FROM VISIT_EXPOSURE_TMP

-- LAB
-- Running
IF OBJECT_ID('[@target_database].[dbo].LAB_EXPOSURE_TMP', 'U') IS NOT NULL
	DROP TABLE LAB_EXPOSURE_TMP;
SELECT DISTINCT A.PERSON_ID, A.VISIT_START_DATE, A.VISIT_END_DATE, A.VISIT_SEQ, A.FIRST_DRUG_ORDDATE, A.DRUG_NAME
	, B.OBSERVATION_DATETIME, B.LAB_NAME, B.OBSERVATION_CONCEPT_ID, B.ABNORM_TYPE, B.RANGE_LOW, B.RANGE_HIGH, B.RESULT
	, ROW_NUMBER() OVER(PARTITION BY B.PERSON_ID, B.OBSERVATION_CONCEPT_ID ORDER BY B.OBSERVATION_DATETIME, B.RESULT) OBSERVATION_SEQ
	, CASE WHEN A.FIRST_DRUG_ORDDATE IS NULL THEN 'NON' 
		WHEN OBSERVATION_DATETIME <= A.FIRST_DRUG_ORDDATE THEN 'Y'
		ELSE 'N'
	END IS_BEFORE
INTO LAB_EXPOSURE_TMP
FROM VISIT_EXPOSURE A
	INNER JOIN (
		SELECT AA.PERSON_ID, AA.OBSERVATION_CONCEPT_ID
			, CAST(AA.OBSERVATION_DATE AS DATETIME)+CAST(AA.OBSERVATION_TIME AS DATETIME) OBSERVATION_DATETIME
			, AA.VALUE_AS_NUMBER RESULT, AA.RANGE_LOW, AA.RANGE_HIGH
			, BB.LAB_NAME, BB.ABNORM_TYPE
		FROM (
				SELECT PERSON_ID, OBSERVATION_CONCEPT_ID, OBSERVATION_DATE, OBSERVATION_TIME
					, VALUE_AS_NUMBER, RANGE_LOW, RANGE_HIGH
				FROM [@cdm_database].[DBO].OBSERVATION
				WHERE RANGE_LOW IS NOT NULL
					AND RANGE_HIGH IS NOT NULL
					AND OBSERVATION_DATE BETWEEN '@date_from' AND '@date_to'
			) AA
			INNER JOIN LABTEST_LIST BB
			ON AA.OBSERVATION_CONCEPT_ID=BB.LAB_ID 
	) B
	ON A.PERSON_ID=B.PERSON_ID
		AND B.OBSERVATION_DATETIME BETWEEN A.VISIT_START_DATE AND A.VISIT_END_DATE

IF OBJECT_ID('[@target_database].[dbo].LAB_EXPOSURE', 'U') IS NOT NULL
	DROP TABLE LAB_EXPOSURE;
SELECT 
--DISTINCT *
	A.PERSON_ID, A.VISIT_START_DATE, A.VISIT_END_DATE, A.VISIT_SEQ, A.FIRST_DRUG_ORDDATE, A.DRUG_NAME
	, A.LAB_NAME, A.OBSERVATION_CONCEPT_ID
	, A.ABNORM_TYPE, A.RANGE_LOW, A.RANGE_HIGH, A.RESULT, A.IS_BEFORE
INTO LAB_EXPOSURE
FROM LAB_EXPOSURE_TMP A
	INNER JOIN (
		SELECT PERSON_ID, VISIT_SEQ, FIRST_DRUG_ORDDATE, DRUG_NAME, OBSERVATION_CONCEPT_ID, MIN(OBSERVATION_SEQ) FIRST_OBSERVATION_SEQ
		FROM LAB_EXPOSURE_TMP
		GROUP BY PERSON_ID, VISIT_SEQ, FIRST_DRUG_ORDDATE, DRUG_NAME, OBSERVATION_CONCEPT_ID
	) B
	ON A.PERSON_ID=B.PERSON_ID
		AND A.VISIT_SEQ=B.VISIT_SEQ
		AND ((A.FIRST_DRUG_ORDDATE=B.FIRST_DRUG_ORDDATE) OR (A.FIRST_DRUG_ORDDATE IS NULL AND B.FIRST_DRUG_ORDDATE IS NULL))
		AND ((A.DRUG_NAME=B.DRUG_NAME) OR (A.DRUG_NAME IS NULL AND B.DRUG_NAME IS NULL))
		AND A.OBSERVATION_CONCEPT_ID=B.OBSERVATION_CONCEPT_ID
	INNER JOIN (
		SELECT PERSON_ID, VISIT_SEQ, FIRST_DRUG_ORDDATE, DRUG_NAME, OBSERVATION_SEQ, OBSERVATION_CONCEPT_ID, RANGE_LOW, RANGE_HIGH, RESULT
		FROM LAB_EXPOSURE_TMP
	) C
	ON A.PERSON_ID=C.PERSON_ID
		AND A.VISIT_SEQ=C.VISIT_SEQ
		AND ((A.FIRST_DRUG_ORDDATE=C.FIRST_DRUG_ORDDATE) OR (A.FIRST_DRUG_ORDDATE IS NULL AND C.FIRST_DRUG_ORDDATE IS NULL))
		AND ((A.DRUG_NAME=C.DRUG_NAME) OR (A.DRUG_NAME IS NULL AND C.DRUG_NAME IS NULL))
		AND A.OBSERVATION_CONCEPT_ID=C.OBSERVATION_CONCEPT_ID
		AND B.FIRST_OBSERVATION_SEQ=C.OBSERVATION_SEQ
		AND C.RESULT BETWEEN C.RANGE_LOW AND C.RANGE_HIGH

-- CLEAR Dataset
-- Running
IF OBJECT_ID('[@target_database].[dbo].CLEAR_EXPOSURE', 'U') IS NOT NULL
	DROP TABLE CLEAR_EXPOSURE;
SELECT A.*
--, B.Y, B.N, B.NON
INTO CLEAR_EXPOSURE
FROM LAB_EXPOSURE A --1846157
	INNER JOIN (
		SELECT *
		FROM (
			SELECT PERSON_ID, VISIT_SEQ, FIRST_DRUG_ORDDATE, DRUG_NAME, OBSERVATION_CONCEPT_ID
				, RANGE_LOW, RANGE_HIGH, IS_BEFORE
			FROM LAB_EXPOSURE
		) A
		PIVOT (
			COUNT(IS_BEFORE) FOR IS_BEFORE IN (Y,N,NON)
		) PV
		WHERE (Y>0 AND N>0) OR (NON>=2)) B --273757
	ON A.PERSON_ID=B.PERSON_ID
		AND A.VISIT_SEQ=B.VISIT_SEQ
		AND ((A.FIRST_DRUG_ORDDATE=B.FIRST_DRUG_ORDDATE) OR (A.FIRST_DRUG_ORDDATE IS NULL AND B.FIRST_DRUG_ORDDATE IS NULL))
		AND ((A.DRUG_NAME=B.DRUG_NAME) OR (A.DRUG_NAME IS NULL AND B.DRUG_NAME IS NULL))
		AND A.OBSERVATION_CONCEPT_ID=B.OBSERVATION_CONCEPT_ID
		AND A.RANGE_LOW=B.RANGE_LOW
		AND A.RANGE_HIGH=B.RANGE_HIGH

/*
-- Demographics
IF OBJECT_ID('[@target_database].[dbo].DEMOGRAPHICS', 'U') IS NOT NULL
	DROP TABLE DEMOGRAPHICS;
*/
-- Summary result
IF OBJECT_ID('[@target_database].[dbo].SUMMARY', 'U') IS NOT NULL
	DROP TABLE SUMMARY;
SELECT PERSON_ID, VISIT_START_DATE, VISIT_END_DATE, VISIT_SEQ, DRUG_NAME, LAB_NAME, OBSERVATION_CONCEPT_ID, ABNORM_TYPE
	, RANGE_LOW, RANGE_HIGH, RESULT_AFTER=N, RESULT_NON=NON, RESULT_TYPE
	,CASE WHEN NON IS NULL THEN
		CASE WHEN RESULT_TYPE IN ('MAX') AND N<RANGE_HIGH THEN 'NORMAL'
			WHEN RESULT_TYPE IN ('MIN') AND N>RANGE_LOW THEN 'NORMAL'
			ELSE 'ABNORMAL'
		END
		ELSE
		CASE WHEN RESULT_TYPE IN ('MAX') AND NON<RANGE_HIGH THEN 'NORMAL'
			WHEN RESULT_TYPE IN ('MIN') AND NON>RANGE_LOW THEN 'NORMAL'
			ELSE 'ABNORMAL'
		END
	END JUDGE
INTO SUMMARY
FROM(
	SELECT *
	FROM(
		SELECT *, 'MAX' RESULT_TYPE
		FROM CLEAR_EXPOSURE
		WHERE ABNORM_TYPE IN ('HYPER','BOTH')
	) A
	PIVOT(
		MAX(RESULT) FOR IS_BEFORE IN (Y,N,NON)
	) PV
	UNION
	SELECT *
	FROM(
		SELECT *, 'MIN' RESULT_TYPE
		FROM CLEAR_EXPOSURE
		WHERE ABNORM_TYPE IN ('HYPO','BOTH')
	) A
	PIVOT(
		MIN(RESULT) FOR IS_BEFORE IN (Y,N,NON)
	) PV
) T

-- Matching data
IF OBJECT_ID('[@target_database].[dbo].MATCHING_TMP', 'U') IS NOT NULL
	DROP TABLE MATCHING_TMP;
WITH
PERSON( --102326
	PERSON_ID, VISIT_START_DATE
)
AS(
	SELECT DISTINCT PERSON_ID, VISIT_START_DATE
	FROM CLEAR_EXPOSURE
),
BDAYSEX(
	PERSON_ID, BIRTHDAY, SEX
)
AS(
	SELECT DISTINCT A.PERSON_ID
		, CAST(
			CAST(YEAR_OF_BIRTH AS VARCHAR(4))+
			RIGHT('0'+CAST(MONTH_OF_BIRTH AS VARCHAR(2)),2)+
			RIGHT('0'+CAST(DAY_OF_BIRTH AS VARCHAR(2)),2) 
			AS DATETIME
		) BIRTHDAY
		, CASE WHEN A.GENDER_CONCEPT_ID IN (8507) THEN 'M'
			WHEN A.GENDER_CONCEPT_ID IN (8532) THEN 'F'
			ELSE NULL
		END SEX
	FROM [@cdm_database].[DBO].PERSON A
		INNER JOIN PERSON B
		ON A.PERSON_ID=B.PERSON_ID
),
AGE(
	PERSON_ID, VISIT_START_DATE, AGE, SEX
)
AS(
	SELECT DISTINCT A.PERSON_ID, B.VISIT_START_DATE
		, DATEDIFF(YY, A.BIRTHDAY, B.VISIT_START_DATE) -
		CASE WHEN DATEADD(YY, DATEDIFF(YY, A.BIRTHDAY, B.VISIT_START_DATE), A.BIRTHDAY)
					> B.VISIT_START_DATE THEN 1
			ELSE 0
		END AGE
		, SEX
	FROM BDAYSEX A
		INNER JOIN PERSON B
		ON A.PERSON_ID=B.PERSON_ID
),
DIAG(
	PERSON_ID, VISIT_START_DATE, AGE, SEX, CONDITION_START_DATE, CONDITION_CONCEPT_ID
)
AS(
	SELECT DISTINCT T1.PERSON_ID, T1.VISIT_START_DATE, T1.AGE, T1.SEX, T2.CONDITION_START_DATE, T2.CONDITION_CONCEPT_ID
	FROM AGE T1
		CROSS APPLY (
			SELECT TOP 1 *
			FROM [@cdm_database].[DBO].CONDITION_OCCURRENCE
			WHERE PERSON_ID=T1.PERSON_ID
				AND CONDITION_START_DATE>=T1.VISIT_START_DATE
			ORDER BY CONDITION_START_DATE, CONDITION_OCCURRENCE_ID
		) T2
)
SELECT A.*
	, B.AGE, B.SEX
--	, C.AGE, C.SEX, C.CONDITION_CONCEPT_ID
INTO MATCHING_TMP
FROM SUMMARY A --231121
	INNER JOIN AGE B --231121
	ON A.PERSON_ID=B.PERSON_ID
		AND A.VISIT_START_DATE=B.VISIT_START_DATE
--	INNER JOIN DIAG C --559223
--	ON A.PERSON_ID=C.PERSON_ID
--		AND A.VISIT_START_DATE=C.VISIT_START_DATE

