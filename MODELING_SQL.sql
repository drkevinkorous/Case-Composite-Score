--Population of interest
WITH cte_a as (
    SELECT 
        a.unique_id_a,
        a.date_1,
        ceil(trunc(max_date_2)- TRUNC(a.date_1)) as outcome,
        CASE 
            WHEN round(months_between(a.date_1, a.date_3) / 12, 2)<=12 
            THEN 1
            ELSE 0
        END AS x_a,
        CASE
            WHEN (trunc(add_months(a.date_3,216)) - trunc(a.date_1))/7 >=0 and (trunc(add_months(a.date_3,216)) - trunc(a.date_1))/7<=3  
            THEN 1
            ELSE 0
        END AS x_b,
        CASE    
            WHEN group_id_a IS NULL
            THEN 0
            ELSE 1
        END x_c,
        CASE
            WHEN  regexp_substr(lower(a.text_a), 'bi(-|)national') is not null 
            THEN 1
            ELSE 0
        END as x_d,
        case
            when ltct.unique_id_a is not null then 1
            else 0
        end as x_e
    FROM a.rd_a a
    LEFT JOIN (
        SELECT a.unique_id_a, MAX(a.date_2) AS max_date_2 
        FROM a.rd_b a
        LEFT JOIN a.rd_e b on a.unique_id_d = b.unique_id_d
        LEFT JOIN a.rd_h c ON a.unique_id_c = c.unique_id_c
        WHERE list_a <> 9 AND unique_id_e <> 13
        group by unique_id_a) b ON a.unique_id_a = b.unique_id_a
    LEFT JOIN (
        SELECT  
            a.unique_id_a,
            max(a.date_4) as latest_date
        from a.rd_i a
        where a.unique_id_f in (3,5) and a.selected = '1'
        group by a.unique_id_a
    ) c on a.unique_id_a = c.unique_id_a
    WHERE TRUNC(a.date_1) >= DATE'2022-11-01'
        AND TRUNC(max_date_2) <= DATE'2024-11-30' 
        AND x_f = 'DISCHARGED'
),

cte_b AS (
    SELECT 
        a.unique_id_a, 
        CASE 
            WHEN MIN(NVL(c.date_4, sysdate) - a.date_1) > 5 THEN 1
            ELSE 0 
        END as x_g
    FROM a.rd_a a
    LEFT JOIN a.rd_b b ON a.unique_id_a = b.unique_id_a
    LEFT JOIN a.rd_j c ON b.unique_id_c = c.unique_id_c
    GROUP BY a.unique_id_a
),

cte_c AS (
    SELECT
        a.unique_id_a,
        SUM(coalesce(b.item_a,0)   
        + coalesce(b.item_b,0)  
        + coalesce(b.item_c,0) 
        + coalesce(b.item_d,0)  
        + coalesce(b.item_e,0) 
        + coalesce(b.item_f,0) 
        + coalesce(b.item_g,0) 
        + coalesce(b.item_h,0) 
        + coalesce(b.item_i,0) 
        + coalesce(b.item_j,0) 
        ) AS count_x_h,
        SUM(CASE
                WHEN coalesce(b.item_k,0) = 0 
                THEN 1
                ELSE 0
            END
        ) AS count_x_i,
        SUM(coalesce(b.item_l,0)  
        + coalesce(b.item_m,0)  
        + coalesce(b.item_n,0)  
        + coalesce(b.item_n,0)
        ) AS count_x_j,
        SUM(coalesce(b.item_o,0) 
        + coalesce(b.item_p ,0) 
        + coalesce(b.item_q,0) 
        + coalesce(b.item_r,0) 
        + coalesce(b.item_s,0) 
        + coalesce(b.item_t,0) 
        + coalesce(b.item_u,0) 
        + coalesce(b.item_v,0) 
        + coalesce(b.item_w,0) 
        ) AS count_x_k
    FROM a.rd_b a 
    LEFT JOIN a.rd_k b on a.unique_id_c = b.unique_id_c
    WHERE coalesce(isdeleted,0) = 0 
    GROUP BY a.unique_id_a
),

cte_d AS (
    SELECT
        a.unique_id_a,
        CASE
            WHEN COUNT(
                CASE 
                    WHEN c.date_5 IS NOT NULL 
                    THEN 1
                END) > 0 
            THEN 1
            ELSE 0
        END AS x_l
    FROM
        a.rd_a a
        LEFT JOIN a.rd_b b ON a.unique_id_a = b.unique_id_a
        LEFT JOIN a.rd_l c ON b.unique_id_c = c.unique_id_c
    GROUP BY
        a.unique_id_a
),

cte_i AS (
    SELECT 
        unique_id_a,
        CASE
            WHEN count(unique_id_g) > 0 
            THEN 1 
            ELSE 0 
        END AS x_m
    FROM a.rd_m a
    WHERE "Higher Category" = 'String' AND "Unique ID F" IS NULL
    GROUP by unique_id_a
),

cte_e AS (
    SELECT
        a.unique_id_a,
        a.unique_id_b,
        rank() over (partition by a.unique_id_b order by trunc(date_2)) AS x_n
    FROM
        (select a.*, rank() over (partition by a.unique_id_a order by a.date_6) AS x_o from a.rd_c a where coalesce(is_deleted, 0) = 0) a
        LEFT JOIN a.rd_b b on a.unique_id_a = b.unique_id_a
        LEFT JOIN a.rd_n c on b.unique_id_c = c.unique_id_c 
        LEFT JOIN (select * from a.rd_q where coalesce(isdeleted,0) = 0) d on a.unique_id_b = d.unique_id_b and c.unique_id_c = d.unique_id_c 
    WHERE a.x_p = 1 AND list_a = 3
),

cte_f as (
    SELECT
        a.unique_id_a,
        a.unique_id_b,
        a.date_7,
        a.date_6,
        round(TRUNC(a.date_6) - TRUNC(a.date_7),0) as x_q,
        CASE 
            WHEN d.x_r = 1 
            THEN 1 
            ELSE 0 
        END as x_r_1,
        CASE 
            WHEN d.x_r = 2 
            THEN 1 
            ELSE 0 
        END as x_r_2,
        CASE 
            WHEN d.x_r = 3 
            THEN 1 
            ELSE 0 
        END as x_r_3,
        si.x_s,
        si.x_t,
        CASE
            WHEN a.x_u IS NOT NULL  
                AND count(DISTINCT a.x_u||', '|| COALESCE(a.x_u_2, '')||', '|| a.x_v||', '|| 
                    LPAD(substr(a.x_w, 1, 5), 5, '0')||', '||  a.x_x) over (partition by a.unique_id_b) > 1 
            THEN 1
            ELSE 0
        END AS x_y,
        CASE
            WHEN trunc(a.date_6) <= trunc(date_2)
                AND trim(e.x_z) = trim(a.x_x)
            THEN 0
            WHEN a.x_x is null
                OR e.x_z is null
            THEN NULL
            ELSE 1
        END AS x_ab,
        CASE 
            WHEN si.x_aa IN (23,18,25,20,28,29,12)
            THEN 1 
            ELSE 0 
        END AS x_ac,
        CASE 
            WHEN si.x_aa IN (27,21,1,2)
            THEN 1 
            ELSE 0 
        END AS x_ad,
        CASE 
            WHEN si.x_aa IN (24,26,11)
            THEN 1 
            ELSE 0 
        END AS x_ae,
        CASE 
            WHEN si.x_aa IN (22,30,31,32)
            THEN 1 
            ELSE 0 
        END as x_af,
        NVL(x_ag, 0) AS x_ag,
        x_o,
        x_p,
        max(date_2) over (partition by a.unique_id_a) AS date_2_last,
        CASE 
            WHEN count(distinct a.unique_id_a) over (partition by a.unique_id_b, trunc(a.date_4, 'MONTH')) > 1
                OR count(distinct a.unique_id_a) over (partition by a.unique_id_b, trunc((a.date_4 - 14), 'MONTH')) > 1
            THEN 1
            ELSE 0
        END AS x_ah,
        CASE 
            WHEN a.x_p = 1 AND list_a = 3 AND x_n = 1
            THEN 1
            ELSE 0
        END AS x_ai,
        CASE 
            WHEN a.x_p = 1 AND list_a = 3 AND x_n = 2
            THEN 1
            ELSE 0
        END AS x_aj,
        CASE 
            WHEN a.x_p = 1 AND list_a = 3 AND x_n > 2
            THEN 1
            ELSE 0
        END AS x_ak,
        CASE 
            WHEN a.x_u IS NOT NULL  
                AND COUNT(distinct CASE WHEN a.x_p = 1 AND list_a = 3 THEN a.unique_id_a END) 
                    over (partition by a.x_u||', '|| COALESCE(a.x_al, '')||', '|| a.x_v||', '|| 
                        LPAD(substr(a.x_w, 1, 5), 5, '0')||', '||  a.x_x) > 1 
            THEN 1 
            ELSE 0 
        END AS x_am,
        CASE 
            WHEN g.date_4 IS NOT NULL
            THEN rank() over (partition by g.unique_id_c order by g.date_4)
            ELSE 0
        END AS x_an,
        CASE 
            WHEN g.date_4 IS NOT NULL
            THEN min(g.date_4) over (partition by a.unique_id_a) 
        END AS x_ao,
        CASE 
            WHEN x_ap = 5 
                OR x_aq = 5
            THEN 1
            ELSE 0
        END as x_ar,
        CASE 
            WHEN x_ap IN (6,10,11,12,13,14,15,16)
                OR x_aq IN (6,10,11,12,13,14,15,16)
            THEN 1
            ELSE 0
        END as x_as,
        CASE 
            WHEN x_ap IN (7,8,9)
            THEN 1
            ELSE 0
        END as x_at,
        row_number() over (partition by a.unique_id_a, g.unique_id_b order by g.time_a desc) as x_bm_rn
    FROM
        (select a.*, CASE WHEN date_6 IS NULL THEN 0 ELSE rank() over (partition by a.unique_id_a order by a.date_6) END AS x_o from a.rd_c a where coalesce(is_deleted, 0) = 0) a
        LEFT JOIN cte_e b on a.unique_id_a = b.unique_id_a and a.unique_id_b = b.unique_id_b
        LEFT JOIN (select a.*, row_number() over (partition by unique_id_b order by time_a desc) as c_rn from a.rd_o a where coalesce(isdeleted,0) = 0 and x_ag = 1) c on a.unique_id_a = c.unique_id_a and a.unique_id_b = c.unique_id_b
        LEFT JOIN a.rd_p d on a.x_au = d.unique_id_h
        LEFT JOIN (SELECT a.unique_id_c, a.unique_id_a, a.date_2, b.x_z FROM a.rd_b a left join a.rd_e b on a.unique_id_d = b.unique_id_d) e on a.unique_id_a = e.unique_id_a
        LEFT JOIN (select a.*, row_number() over (partition by unique_id_c order by time_a desc) as f_rn from a.rd_n a) f on e.unique_id_c = f.unique_id_c 
        LEFT JOIN (select a.*, row_number() over (partition by unique_id_c, unique_id_b order by time_a desc) as g_rn from a.rd_q a where coalesce(isdeleted,0) = 0) g on a.unique_id_b = g.unique_id_b and f.unique_id_c = g.unique_id_c 
        LEFT JOIN (SELECT a.*, row_number() over (partition by unique_id_b order by time_a desc) as h_rn FROM a.rd_f a) h on a.unique_id_b = h.unique_id_b
        WHERE coalesce(g_rn,1) = 1 and f_rn = 1 and h_rn = 1 and coalesce(c_rn, 1) = 1
),

cte_g as (
    SELECT 
        a.unique_id_a,
        a.unique_id_b,
        CASE
            WHEN SUM(CASE WHEN c.x_ax = 1
                AND c.date_5 IS NOT NULL
                AND c.unique_id_i IS NULL
                AND c.unique_id_j IS NULL
            THEN 1 END) > 0
        THEN 1 ELSE 0
        END as x_av,
            CASE
            WHEN SUM(CASE WHEN c.x_ax = 1
                AND c.date_5 IS NOT NULL
                AND c.unique_id_i IS NOT NULL
                AND c.unique_id_j IS NOT NULL
            THEN 1 END) > 0
        THEN 1 ELSE 0
        END as x_aw,
        CASE
            WHEN SUM(CASE WHEN c.x_ay = 1
                AND c.date_5 IS NOT NULL
                AND c.unique_id_i IS NULL
                AND c.unique_id_j IS NULL
            THEN 1 END) > 0
        THEN 1 ELSE 0
        END as x_az,
            CASE
            WHEN SUM(CASE WHEN c.x_ay = 1
                AND c.date_5 IS NOT NULL
                AND c.unique_id_i IS NOT NULL
                AND c.unique_id_j IS NOT NULL
            THEN 1 END) > 0
        THEN 1 ELSE 0
        END as x_ba,
        CASE
            WHEN SUM(CASE WHEN c.x_bb = 1
                AND c.unique_id_k = 2
                AND c.unique_id_i IS NULL
                AND c.unique_id_j IS NULL
            THEN 1 END) > 0
        THEN 1 ELSE 0
        END as x_bc,
            CASE
            WHEN SUM(CASE WHEN c.x_bb = 1
                AND c.unique_id_k = 2
                AND c.unique_id_i IS NOT NULL
                AND c.unique_id_j IS NOT NULL
            THEN 1 END) > 0
        THEN 1 ELSE 0
        END as x_bd,
        CASE
            WHEN SUM(CASE WHEN c.x_be = 1
                AND c.unique_id_k = 2
                AND c.unique_id_i IS NULL
                AND c.unique_id_j IS NULL
            THEN 1 END) > 0
        THEN 1 ELSE 0
        END as x_bf,
            CASE
            WHEN SUM(CASE WHEN c.x_be = 1
                AND c.unique_id_k = 2
                AND c.unique_id_i IS NOT NULL
                AND c.unique_id_j IS NOT NULL
            THEN 1 END) > 0
        THEN 1 ELSE 0
        END as x_bg
    FROM a.rd_c a
    LEFT JOIN a.rd_f b on a.unique_id_b = b.unique_id_b
    LEFT JOIN a.rd_g c ON b.unique_id_b = c.unique_id_b
    GROUP BY a.unique_id_a, a.unique_id_b
    ),

cte_h AS (
    SELECT
        a.unique_id_a,
        a.unique_id_b,
        CASE
            WHEN count(DISTINCT a.x_u||', '|| COALESCE(a.x_al, '')||', '|| a.x_v||', '|| 
                LPAD(substr(a.x_w, 1, 5), 5, '0')||', '||  a.x_x) > 1 
            THEN 1
            ELSE 0
        END AS x_bh
    FROM a.rd_d a
    WHERE a.x_u IS NOT NULL
    GROUP BY a.unique_id_a, a.unique_id_b
)

SELECT
    cte_a.unique_id_a,
    cte_f.unique_id_b,
    cte_a.outcome,
    x_a,
    x_b,
    x_c,
    x_c*x_a AS x_cXx_a,
    x_d,
    x_e,
    NVL(x_l, 0) AS x_l,
    NVL(x_m, 0) AS x_m,
    CASE 
        WHEN count_x_h > 0 
        THEN 1
        ELSE 0
    END x_h,
    CASE 
        WHEN count_x_i > 0 
        THEN 1
        ELSE 0
    END x_i,
    CASE 
        WHEN count_x_j > 0 
        THEN 1
        ELSE 0
    END x_j,
    CASE 
        WHEN count_x_k > 0 
        THEN 1
        ELSE 0
    END x_k,
    CASE
        WHEN min(TRUNC(NVL(cte_f.date_7, sysdate)) - TRUNC(cte_a.date_1)) over (partition by cte_a.unique_id_a)  > 7 
        THEN 1 
        ELSE 0
    END as x_bi,
    NVL(x_bj, 0) AS x_bj,
    NVL(x_r_1, 0) AS x_r_1,
    NVL(x_r_2, 0) AS x_r_2,
    NVL(x_r_3, 0) AS x_r_3,
    x_a*NVL(x_r_3,0) AS x_aXx_r_3,
    NVL(x_s, 0) AS x_s,
    NVL(x_t, 0) AS x_t,
    NVL(x_y, 0) AS x_y,
    NVL(x_ab, 0) AS x_ab,
    NVL(x_ac, 0) AS x_ac,
    NVL(x_ad, 0) AS x_ad,
    NVL(x_ae, 0) AS x_ae,
    NVL(x_af, 0) AS x_af,
    NVL(x_ag, 0) AS x_ag,
    NVL(x_ah, 0) AS x_ah,
    NVL(x_ah,0)*NVL(x_r_3,0) AS x_ahXx_r_3,
    NVL(x_ah,0)*x_c AS x_c,
    NVL(x_ai, 0) AS x_ai,
    NVL(x_aj, 0) AS x_aj,
    NVL(x_ak, 0) AS x_ak,
    NVL(x_am, 0) AS x_am,
    NVL(x_ar, 0) AS x_ar,
    NVL(x_as, 0) AS x_as,
    NVL(x_at, 0) AS x_at,
    NVL(x_av, 0) AS x_av,
    NVL(x_aw, 0) AS x_aw,
    NVL(x_az, 0) AS x_az,
    NVL(x_ba, 0) AS x_ba,
    CASE 
        WHEN NVL(x_bc,0) = 1 or NVL(x_bf,0) = 1
        THEN 1
        ELSE 0
    END AS x_bk,
    CASE 
        WHEN NVL(x_bd,0) = 1 or NVL(x_bg,0) = 1
        THEN 1
        ELSE 0
    END AS x_bl,
    NVL(x_bh, 0 ) AS x_bh,
    NVL(x_o,0) AS x_o,
    NVL(x_p,0) AS x_p,
    NVL(x_an,0) AS x_an
FROM ucl
LEFT JOIN cte_b on cte_a.unique_id_a = cte_b.unique_id_a
LEFT JOIN cte_c on cte_a.unique_id_a = cte_c.unique_id_a
LEFT JOIN cte_d on cte_a.unique_id_a = cte_d.unique_id_a
LEFT JOIN cte_i on cte_a.unique_id_a = cte_i.unique_id_a
LEFT JOIN
    (SELECT 
        cte_f.*, 
        SUM (
            CASE 
                WHEN trunc(date_6) < trunc(x_ao)
                THEN 1 
                ELSE 0
            END) over (partition by unique_id_a) AS x_bj
        FROM cte_f 
        WHERE x_bm_rn = 1) cte_f on cte_a.unique_id_a = cte_f.unique_id_a
LEFT JOIN cte_g on cte_a.unique_id_a = cte_g.unique_id_a and cte_f.unique_id_b = cte_g.unique_id_b
LEFT JOIN cte_h on cte_a.unique_id_a = cte_h.unique_id_a and cte_f.unique_id_b = cte_h.unique_id_b