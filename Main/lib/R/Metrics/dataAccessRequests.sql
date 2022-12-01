select '20'||SUBSTR(start_date,8,2) year,  
         count(1) as total_number, 
         count(case when approval_status_id = 0 then 1 end) as approved_number,
         count(case when approval_status_id = 2 then 1 end) as denied_number
    from studyaccess.end_users
    group by SUBSTR(start_date,8,2)
    order by year