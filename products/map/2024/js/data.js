/**
 * Loads data.
 * 
 *
 * @summary Loads data, create key-value object for populating charts.
 * 
 */



const indicators = [
  // econ
  {
    domain: "econ1",
    label: ["Share >200% Poverty"],
    name: "pct_above_200_pov",
    threshName: "pct_above_200_pov_median",
  },
  {
    domain: "econ1",
    label: "Share with Bachelors+",
    name: "pct_bachelors_plus",
    threshName: "pct_bachelors_plus_median",
  },
  {
    domain: "econ1",
    label: "Employment Rate",
    name: "pct_employed",
    threshName: "pct_employed_median",
  },
  {
    domain: "econ2",
    label: "Median Home Value",
    name: "home_value",
    threshName: "home_value_median",
  },
  // educ
  {
    domain: "edu",
    label: "Share Proficient in Math",
    name: "math_prof",
    threshName: "math_prof_median",
  },
  {
    domain: "edu",
    label: "Share Proficient in Reading",
    name: "read_prof",
    threshName: "read_prof_median",
  },
  {
    domain: "edu",
    label: "High School Grad Rate",
    name: "grad_rate",
    threshName: "grad_rate_median",
  },
  {
    domain: "edu",
    label: "Students Not in Poverty",
    name: "pct_not_frpm",
    threshName: "pct_not_frpm_median",
  },
  // high pov
  {
    domain: "pov",
    label: [["Share Below"], "Poverty Level"],
    name: "pct_below_pov",
    threshName: "high_pov_thresh",
  },
  // seg
  {
    domain: "seg",
    label: "Asian",
    name: "pct_asian",
    threshName: "asian_seg_thresh",
  },
  {
    domain: "seg",
    label: "Black",
    name: "pct_black",
    threshName: "black_seg_thresh",
  },
  {
    domain: "seg",
    label: "Hispanic",
    name: "pct_hispanic",
    threshName: "hispanic_seg_thresh",
  },
  {
    domain: "seg",
    label: "All POC",
    name: "pct_poc",
    threshName: "poc_seg_thresh",
  },
  // change
  {
    domain: "change1",
    label: [["Non-Hispanic White"], "Share Change (pp)"],
    name: "trct_raceeth_chng0021",
    threshName: "raceeth_half0021",
  },
  {
    domain: "change1",
    label: [["High-Income Share"], "Change (pp)"],
    name: "trct_inc_chng0021",
    threshName: "inc_half0021",
  },
  {
    domain: "change1b",
    label: [["Non-Hispanic White"], "Share Change (pp)"],
    name: "trct_raceeth_chng1321",
    threshName: "raceeth_quarter1321",
  },
  {
    domain: "change1b",
    label: [["High-Income Share"], "Change (pp)"],
    name: "trct_inc_chng1321",
    threshName: "inc_quarter1321",
  },
  {
    domain: "change2",
    label: [["Non-Hispanic White"], "Share Change (pp)"],
    name: "trct_raceeth_chng1321",
    threshName: "raceeth_half1321",
  },
  {
    domain: "change2",
    label: [["High-Income Share"], "Change (pp)"],
    name: "trct_inc_chng1321",
    threshName: "inc_half1321",
  },
  {
    domain: "change3",
    // label: "Relative Rent Change (%)",
    label: [["Relative Rent"], "Change (%)"],
    name: "trct_pctchng_medrent1321",
    threshName: "rent_half1321",
  },
  {
    // domain: "change4",
    domain: "change3",
    label: [["Home Value/"], "Income Gap (pp)"],
    name: "pct_gap",
    threshName: "gap_thresh",
  },
];

// ajax call to postgres db
// let oppDB;
//         $.ajax({
//             url: 'database.php',
//             type: 'POST',
//             success: function (response) {
//                 if (oppDB) {
//                     map.removeLayer(oppDB);
//                 };
//                 oppDB = L.geoJSON(JSON.parse(response));
//             }
//         });





