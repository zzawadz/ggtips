# geom_bar, geom_col, data pre-aggregated are handled properly - no factes

    Code
      tts_data[[1]]
    Output
        Auto/Manual Cylinders Value
      1           0         4     3
      2           0         6     4
      3           0         8    12
      4           1         4     8
      5           1         6     3
      6           1         8     2

---

    Code
      tts_data[[2]]
    Output
        Auto/Manual Cylinders Value
      1           0         4     3
      3           0         6     4
      5           0         8    12
      2           1         4     8
      4           1         6     3
      6           1         8     2

---

    Code
      tts_data[[3]]
    Output
        Auto/Manual Cylinders Value
      1           0         4     3
      3           0         6     4
      5           0         8    12
      2           1         4     8
      4           1         6     3
      6           1         8     2

# geom_bar, geom_col, data pre-aggregated are handled properly - factes

    Code
      tts_data[[1]]
    Output
         Auto/Manual Cylinders Value
      1            0         4     1
      4            0         4     2
      2            0         6     2
      5            0         6     2
      3            0         8    12
      8            1         4     2
      6            1         4     6
      9            1         6     1
      7            1         6     2
      10           1         8     2

---

    Code
      tts_data[[2]]
    Output
         Auto/Manual Cylinders Value
      1            0         4     1
      4            0         4     2
      2            0         6     2
      6            0         6     2
      3            0         8    12
      8            1         4     2
      5            1         4     6
      9            1         6     1
      7            1         6     2
      10           1         8     2

---

    Code
      tts_data[[3]]
    Output
         Auto/Manual Cylinders Value
      1            0         4     1
      4            0         4     2
      2            0         6     2
      6            0         6     2
      3            0         8    12
      8            1         4     2
      5            1         4     6
      9            1         6     1
      7            1         6     2
      10           1         8     2

# geom_bar, geom_col, data pre-aggregated are handled properly - grid

    Code
      tts_data[[1]]
    Output
         Auto/Manual Cylinders Value
      1            0         4     1
      4            0         4     2
      2            0         6     2
      6            0         6     2
      3            0         8    12
      8            1         4     2
      5            1         4     6
      9            1         6     1
      7            1         6     2
      10           1         8     2

---

    Code
      tts_data[[2]]
    Output
         Auto/Manual Cylinders Value
      1            0         4     1
      4            0         4     2
      2            0         6     2
      6            0         6     2
      3            0         8    12
      8            1         4     2
      5            1         4     6
      9            1         6     1
      7            1         6     2
      10           1         8     2

---

    Code
      tts_data[[3]]
    Output
         Auto/Manual Cylinders Value
      1            0         4     1
      4            0         4     2
      2            0         6     2
      6            0         6     2
      3            0         8    12
      8            1         4     2
      5            1         4     6
      9            1         6     1
      7            1         6     2
      10           1         8     2

# missing data is handled properly - random cells

    Code
      tt
    Output
        Auto/Manual Cylinders Value
      1           0         4     2
      2           0         6     4
      3           0         8    10
      4           0      <NA>     2
      5           1         4     5
      6           1         6     3
      7           1         8     2
      8           1      <NA>     1
      9        <NA>         4     3

# missing data is handled properly - whole group all variables

    Code
      tt
    Output
        Auto/Manual Cylinders Value
      1           0         6     4
      2           0         8    12
      3           1         6     3
      4           1         8     2
      5        <NA>      <NA>    11

# missing data is handled properly - whole group single variable

    Code
      tt
    Output
        Auto/Manual Cylinders Value
      1           0         4     1
      2           0         6     2
      3           0         8    12
      4           1         4     2
      5           1         6     1
      6           1         8     2
      7        <NA>         4     8
      8        <NA>         6     4

# missing data is handled properly - random cells 2

    Code
      tt
    Output
         Auto/Manual Cylinders Value
      1            0         4     1
      5            0         4     1
      2            0         6     1
      6            0         6     1
      14           0         6     2
      15           0         8     3
      3            0         8     7
      4            0      <NA>     1
      16           0      <NA>     1
      7            1         4     5
      10           1         6     1
      8            1         6     2
      11           1         8     2
      12           1      <NA>     1
      9         <NA>         4     1
      13        <NA>         4     1
      17        <NA>         4     1

# missing data is handled properly - random cells 3

    Code
      tt
    Output
        Auto/Manual Cylinders Value
      1           0         6     2
      3           0         6     2
      2           0         8    12
      5           1         6     1
      4           1         6     2
      6           1         8     2
      7        <NA>      <NA>    11

# position dodge - no missing data

    Code
      tt
    Output
        Age Class Mean Height Sex
      2        <5       0.808   M
      1        <5       0.859   F
      5       >12       1.637   F
      6       >12       1.668   M
      3      5-12       1.126   F
      4      5-12       1.239   M

# position dodge - no missing data, incl. faceting

    Code
      t
    Output
        Age Class Mean Height Sex
      2        <5       0.808   M
      1        <5       0.859   F
      5       >12       1.637   F
      6       >12       1.668   M
      3      5-12       1.126   F
      4      5-12       1.239   M

---

    Code
      t
    Output
        Age Class Mean Height Sex
      4        <5       0.808   M
      1        <5       0.859   F
      3       >12       1.637   F
      6       >12       1.668   M
      2      5-12       1.126   F
      5      5-12       1.239   M

---

    Code
      t
    Output
        Age Class Mean Height Sex
      2        <5       0.808   M
      1        <5       0.859   F
      5       >12       1.637   F
      6       >12       1.668   M
      3      5-12       1.126   F
      4      5-12       1.239   M

# missing data is handled properly - random cells - position dodge

    Code
      tt
    Output
        Auto/Manual Cylinders Value
      1           0         4     2
      2           0         6     4
      3           0         8    10
      4           0      <NA>     2
      5           1         4     5
      6           1         6     3
      7           1         8     2
      8           1      <NA>     1
      9        <NA>         4     3

# missing data is handled properly - whole group all variables - position dodge

    Code
      tt
    Output
        Auto/Manual Cylinders Value
      1           0         6     4
      2           0         8    12
      3           1         6     3
      4           1         8     2
      5        <NA>      <NA>    11

# missing data is handled properly - whole group single variable - position dodge

    Code
      tt
    Output
        Auto/Manual Cylinders Value
      1           0         4     1
      2           0         6     2
      3           0         8    12
      4           1         4     2
      5           1         6     1
      6           1         8     2
      7        <NA>         4     8
      8        <NA>         6     4

