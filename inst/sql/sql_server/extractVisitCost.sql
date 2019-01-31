SELECT cohort_definition_id, DATEDIFF(@min_date_unit,COHORT_START_DATE,visit.visit_start_date) as date_unit, visit.visit_concept_id, COUNT(visit.visit_occurrence_id) AS visit_count, COUNT(cohort.subject_id) AS subject_count, SUM(cost.paid_by_payer) AS paid_by_payer_sum, SUM(cost.paid_by_patient) AS paid_by_patient_sum, SUM(cost.total_charge) AS total_charge_sum--,concept.concept_name as visit_concept_name
    FROM @target_database_schema.@target_cohort_table cohort
    INNER JOIN @cdm_database_schema.VISIT_OCCURRENCE visit
        ON cohort.subject_id = visit.person_id
        {@target_cohort_id != -1}? {AND  cohort.cohort_definition_id = @target_cohort_id}
        AND visit.visit_start_date >= DATEADD(DAY,@cost_window_start,cohort.cohort_start_date)
        AND visit.visit_start_date <= DATEADD(DAY,@cost_window_end,cohort.cohort_end_date)
    INNER JOIN @cdm_database_schema.COST cost 
        ON visit.visit_occurrence_id = cost.cost_event_id 
        AND cost.cost_domain_id = 'visit'
    --INNER JOIN @vocabulary_database_schema.concept
    --    ON visit.visit_concept_id = concept.concept_id
    {@specify_condition}?{WHERE visit.visit_occurrence_id IN (select distinct condition.visit_occurrence_id 
                                                                FROM @cdm_database_schema.condition_occurrence condition
                                                                JOIN (SELECT * FROM @target_database_schema.@target_cohort_table
                                                                		{@target_cohort_id != -1}? {WHERE  cohort.cohort_definition_id = @target_cohort_id}) AS cohort
                                                                  ON cohort.subject_id = condition.person_id
                                                                  AND condition_concept_id IN (@condition_concept_ids)
                                                                  AND condition.condition_start_date >= DATEADD(DAY,@cost_window_start,cohort.cohort_start_date)
        														  AND condition.condition_start_date <= DATEADD(DAY,@cost_window_end,cohort.cohort_end_date))
                                                                }
    GROUP BY cohort_definition_id, DATEDIFF(@min_date_unit,COHORT_START_DATE,visit.visit_start_date), visit.visit_concept_id --, concept.concept_name
    ;