# printed log is stable

    Code
      view_log(p)
    Output
      <tidyplot log> schema 0.1 | 7 steps
      figure    tp-000000000000
      created   2026-01-01 12:00:00 UTC
      software  R x.y.z | tidyplots x.y.z | ggplot2 x.y.z
      data      d | hash 00000000 | 20 rows x 7 cols
      metadata  y_unit: points
      scope     steps from tidyplot() onward; data processing before tidyplot() is not recorded
                ggplot2 components added with + are not recorded, use add() to record them
      plotted   19 of 20 rows | dropped 1 (NA in score: 1)
      rows per  treatment: A 4, B 5, C 5, D 5
      
       1 data       tidyplot(data = d, x = treatment, y = score, color = treatment)
                    x: treatment (discrete) | y: score (continuous, points) | colour: treatment (discrete)
       2 metadata   add_metadata(y_unit = "points")
                    y unit: points
       3 note       "Each point represents one participant."
       4 summary    add_mean_bar(alpha = 0.4)
                    statistic: mean | geom: bar
       5 summary    add_sem_errorbar()
                    statistic: mean | dispersion: standard error of the mean | geom: errorbar
       6 layer      add_data_points()
                    shows: individual values | geom: points
       7 external   add(object = ggplot2::geom_hline(yintercept = 20))

